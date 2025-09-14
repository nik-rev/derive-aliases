//! Derive aliases

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
};

/// `#[derive]` that supports derive aliases
///
/// See the [crate-level](crate) documentation for more info
#[proc_macro_attribute]
pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    // #[::std::prelude::v1::derive(<list of derive macros>)]
    [
        TokenTree::Punct(Punct::new('#', Spacing::Joint)),
        TokenTree::Group(Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter([
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("std", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("prelude", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("v1", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("derive", Span::call_site())),
                TokenTree::Group(Group::new(Delimiter::Parenthesis, expand_aliases(attr))),
            ]),
        )),
    ]
    .into_iter()
    .chain(item)
    .collect::<TokenStream>()
}

/// Expand derive aliases recursively and deduplicate
fn expand_aliases(input: TokenStream) -> TokenStream {
    let map = get_aliases();

    let mut seen = HashSet::<String>::new();
    let mut results = Vec::<String>::new();

    let mut tokens = input.into_iter().peekable();

    while let Some(tt) = tokens.next() {
        match &tt {
            // Skip commas (we will insert them later ourselves)
            TokenTree::Punct(p) if p.as_char() == ',' => {}

            // Alias: starts with `..Alias`
            TokenTree::Punct(p) if p.as_char() == '.' => {
                if let Some(TokenTree::Punct(p2)) = tokens.peek() {
                    if p2.as_char() == '.' {
                        tokens.next(); // consume second dot
                        if let Some(TokenTree::Ident(alias_ident)) = tokens.next() {
                            expand_name(&alias_ident.to_string(), map, &mut seen, &mut results);
                        }
                        continue;
                    }
                }
                results.push(".".to_string());
            }

            // Identifier → could be an alias or a path
            TokenTree::Ident(id) => {
                let mut path = id.to_string();

                loop {
                    let mut clone_iter = tokens.clone();
                    match (clone_iter.next(), clone_iter.next(), clone_iter.next()) {
                        (
                            Some(TokenTree::Punct(p1)),
                            Some(TokenTree::Punct(p2)),
                            Some(TokenTree::Ident(next_id)),
                        ) if p1.as_char() == ':' && p2.as_char() == ':' => {
                            // consume 2x punct + ident
                            tokens.next();
                            tokens.next();
                            tokens.next();
                            path.push_str("::");
                            path.push_str(&next_id.to_string());
                        }
                        _ => break,
                    }
                }

                expand_name(&path, map, &mut seen, &mut results);
            }

            // Anything else: just push its string form
            other => {
                let s = other.to_string();
                if seen.insert(s.clone()) {
                    results.push(s);
                }
            }
        }
    }

    // Convert results into tokens again
    let mut out = Vec::new();
    for (i, path) in results.into_iter().enumerate() {
        if i > 0 {
            out.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
        }
        out.extend(make_path(&path));
    }

    out.into_iter().collect()
}

/// Recursively expand alias -> paths
fn expand_name(
    name: &str,
    map: &HashMap<String, Vec<String>>,
    seen: &mut HashSet<String>,
    results: &mut Vec<String>,
) {
    if let Some(expansion) = map.get(name) {
        for path in expansion {
            if let Some(alias_name) = path.strip_prefix("..") {
                // recurse into alias
                expand_name(alias_name, map, seen, results);
            } else if seen.insert(path.clone()) {
                // treat as a real derive path
                results.push(path.clone());
            }
        }
    } else if let Some(alias_name) = name.strip_prefix("..") {
        // direct reference like `..Alias`
        expand_name(alias_name, map, seen, results);
    } else if seen.insert(name.to_string()) {
        // not an alias, just push the raw name
        results.push(name.to_string());
    }
}

/// Convert a string like "zerocopy::IntoBytes" into tokens representing the path
fn make_path(path: &str) -> Vec<TokenTree> {
    let mut out = Vec::new();
    for (i, part) in path.split("::").enumerate() {
        if i > 0 {
            out.push(TokenTree::Punct(Punct::new(':', Spacing::Joint)));
            out.push(TokenTree::Punct(Punct::new(':', Spacing::Joint)));
        }
        out.push(TokenTree::Ident(Ident::new(part, Span::call_site())));
    }
    out
}

/// Contains all derive aliases
static ALIASES: OnceLock<HashMap<String, Vec<String>>> = OnceLock::new();

/// Retrieve the map that contains derive aliases
fn get_aliases() -> &'static HashMap<String, Vec<String>> {
    ALIASES.get_or_init(|| {
        let Ok(manifest_dir) = std::env::var("CARGO_WORKSPACE_DIR") else {panic!(concat!(
            "\n\n`CARGO_WORKSPACE_DIR` environment variable must be set, which points to the directory containing the\n",
            "workspace `Cargo.toml`. Since cargo currently doesn't set this variable, in your workspace create `.cargo/config.toml` with contents:\n",
            "\n",
            "[env]\n",
            "CARGO_WORKSPACE_DIR = {{ value = \"\", relative = true }}\n",
        ))};

        let path = std::path::Path::new(&manifest_dir).join("derive_aliases.rs");

        let content = std::fs::read_to_string(&path).unwrap_or_else(|err| {
            panic!(
                "expected {} to exist and contain derive aliases. error: {err}.\nhere's an example of syntax in `derive_aliases.rs` file:\n\n{EXAMPLE_DERIVE_ALIASES_RS}\n",
                path.display()
            )
        });

        parse_my_little_rust(&content)
    })
}

const EXAMPLE_DERIVE_ALIASES_RS: &str = "\
// Simple derive aliases
//
// `#[derive(..Copy, ..Eq)]` expands to `#[std::derive(Copy, Clone, PartialEq, Eq)]`

Copy = Copy, Clone;
Eq = PartialEq, Eq;

// You can nest them!
//
// `#[derive(..Ord, std::hash::Hash)]` expands to `#[std::derive(PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]`

Ord = PartialOrd, Ord, ..Eq;";

/// Parse the `derive_aliases.rs` file, which uses syntax that is a subset of Rust. `EXAMPLE` contains an example of this syntax.
fn parse_my_little_rust(s: &str) -> HashMap<String, Vec<String>> {
    let mut cleaned_from_block_comments = String::new();
    let mut chars = s.chars().peekable();

    // 1. Remove all block comments
    while let Some(c) = chars.next() {
        // found block comment
        if c == '/' && chars.peek() == Some(&'*') {
            // eat '*'
            chars.next();

            // Skip until closing */
            while let Some(d) = chars.next() {
                if d == '*' && chars.peek() == Some(&'/') {
                    // consume '/'
                    chars.next();
                    break;
                }
            }
        } else {
            cleaned_from_block_comments.push(c);
        }
    }

    // 2. Remove all lines comments
    let mut cleaned = String::new();
    for line in cleaned_from_block_comments.lines() {
        if let Some(idx) = line.find("//") {
            cleaned.push_str(&line[..idx]);
            cleaned.push('\n');
        } else {
            cleaned.push_str(line);
            cleaned.push('\n');
        }
    }

    let mut aliases = HashMap::new();

    // 3. Split on ';'
    for stmt in cleaned.split(';').map(str::trim).filter(|s| !s.is_empty()) {
        if let Some((alias_name, alias_value)) = stmt.split_once('=') {
            let alias_name = alias_name.trim().to_string();

            // Each RHS is `A , B , C`
            let expansions: Vec<String> = alias_value
                .split(',')
                .map(|part| part.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();

            aliases.insert(alias_name, expansions);
        }
    }

    aliases
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn my_little_rust() {
        assert_eq!(
            parse_my_little_rust(EXAMPLE_DERIVE_ALIASES_RS),
            HashMap::from([
                (
                    "Copy".to_string(),
                    vec!["Copy".to_string(), "Clone".to_string()]
                ),
                (
                    "Eq".to_string(),
                    vec![
                        "PartialEq".to_string(),
                        "Eq".to_string(),
                        "Copy".to_string()
                    ]
                ),
                (
                    "Ord".to_string(),
                    vec![
                        "PartialOrd".to_string(),
                        "Ord".to_string(),
                        "Copy".to_string()
                    ]
                ),
                (
                    "All".to_string(),
                    vec!(
                        "..Copy".to_string(),
                        "..Eq".to_string(),
                        "..FastHash".to_string(),
                        "std::hash::Hash".to_string()
                    )
                )
            ])
        );
    }
}
