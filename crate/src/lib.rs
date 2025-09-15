//! `#[derive]` aliases for reducing code boilerplate
//!
//! Aliases are defined in a special file `derive_aliases.rs`, located next to your **crate**'s `Cargo.toml`:
//!
//! ```rust
//! // Simple derive aliases
//! //
//! // `#[derive(..Copy, ..Eq)]` expands to `#[std::derive(Copy, Clone, PartialEq, Eq)]`
//!
//! Copy = Copy, Clone;
//! Eq = PartialEq, Eq;
//!
//! // You can nest them!
//! //
//! // `#[derive(..Ord, std::hash::Hash)]` expands to `#[std::derive(PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]`
//!
//! Ord = PartialOrd, Ord, ..Eq;
//! ```
//!
//! This file uses a tiny domain-specific language for defining the derive aliases (the parser is less than 20 lines of code!). `.rs` is used just for syntax highlighting.
//! These aliases can then be used in Rust like so:
//!
//! ```rust
//! // This globally overrides `std::derive` with `derive_aliases::derive` across the whole crate! Handy.
//! #[macro_use]
//! extern crate derive_aliases;
//!
//! #[derive(..Copy, ..Ord, std::hash::Hash)]
//! struct HelloWorld;
//! ```
//!
//! This expands to:
//!
//! ```rust
//! #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]
//! struct HelloWorld;
//! ```
//!
//! # Single `derive_aliases.rs` in Cargo Workspaces
//!
//! If you want to use the same `derive_aliases.rs` for all crates in your Cargo workspace, enable the `workspace` feature then define the `CARGO_WORKSPACE_DIR` env variable in `.cargo/config.toml`:
//!
//! ```toml
//! [env]
//! CARGO_WORKSPACE_DIR = { value = "", relative = true }
//! ```
//!
//! # `use` other alias files in `derive_aliases.rs`
//!
//! `use` followed by a path will inline the derive aliases located in that file.
//!
//! If `../my_other_aliases.rs` contains:
//!
//! ```rust
//! Ord = PartialOrd, Ord, ..Eq;
//! ```
//!
//! And your `derive_aliases.rs` has:
//!
//! ```rust
//! use "../my_other_aliases.rs";
//!
//! Eq = PartialEq, Eq;
//! ```
//!
//! Then it will inline the aliases in the other file, expanding to:
//!
//! ```rust
//! Ord = PartialOrd, Ord, ..Eq;
//! Eq = PartialEq, Eq;
//! ```

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    sync::{atomic::AtomicUsize, LazyLock},
};

/// `#[derive]` that supports derive aliases: `..Alias`
///
/// See the [crate-level](crate) documentation for more info
#[proc_macro_attribute]
pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    expand_aliases(attr).into_iter().chain(item).collect()
}

/// Generates `#[doc = $doc]`
fn add_doc(ts: &mut TokenStream, doc: impl AsRef<str>) {
    ts.extend(TokenStream::from_iter([
        TokenTree::Punct(Punct::new('#', Spacing::Joint)),
        TokenTree::Group(Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new("doc", Span::call_site())),
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
                TokenTree::Literal(Literal::string(doc.as_ref())),
            ]),
        )),
    ]));
}

fn compile_error(ts: &mut TokenStream, span: Span, msg: impl AsRef<str>) {
    ts.extend([
        TokenTree::Ident(Ident::new("compile_error", span)),
        TokenTree::Punct({
            let mut punct = Punct::new('!', Spacing::Alone);
            punct.set_span(span);
            punct
        }),
        TokenTree::Group({
            let mut group = Group::new(Delimiter::Brace, {
                TokenStream::from_iter(vec![TokenTree::Literal({
                    let mut string = Literal::string(msg.as_ref());
                    string.set_span(span);
                    string
                })])
            });
            group.set_span(span);
            group
        }),
    ]);
}

/// Expand derive aliases recursively and deduplicate
fn expand_aliases(input: TokenStream) -> TokenStream {
    // Derives that come from expansion of aliases which were already seen
    //
    // We'll just ignore them. We allow using several derive aliases that expand to the same derives,
    // by removing all duplicates
    //
    // This stores a map of `Derive => Alias that created it`
    let mut seen_expanded = HashSet::new();

    // Stuff we receive as derive input, aka everything inside of `#[derive(<--here-->)]`
    let mut input = input.into_iter().peekable();

    // The stuff our proc macro outputs
    let mut output = TokenStream::new();

    // This is the `struct` that contains documentation about all
    let mut documented_items = Vec::new();

    while let Some(tt) = input.next() {
        let TokenTree::Punct(ref p) = tt else {
            output.extend([tt]);
            continue;
        };

        if p.as_char() != '.' {
            output.extend([tt]);
            continue;
        }

        let Some(next) = input.next_if(|tt| {
            if let TokenTree::Punct(dot) = tt {
                dot.as_char() == '.'
            } else {
                false
            }
        }) else {
            compile_error(
                &mut output,
                p.span(),
                "after `.` we expect `.` followed by a derive alias, for example: `..Alias`",
            );
            continue;
        };

        // At this point, we have a `..` so we can absolutely certain that it is an alias

        // consume the identifier
        let Some(TokenTree::Ident(alias)) = input.next() else {
            compile_error(
                &mut output,
                next.span(),
                "after `..` we expect a derive alias, for example: `..Alias`",
            );
            continue;
        };

        let alias_string = alias.to_string();

        // All aliased that this alias aliases (lol)
        let Some(list_of_aliased) = DERIVE_ALIASES.get(&alias_string) else {
            let available = DERIVE_ALIASES
                .keys()
                .enumerate()
                .flat_map(|(i, key)| {
                    // intersperse
                    if i == 0 {
                        vec![key.as_str()]
                    } else {
                        vec![", ", key.as_str()]
                    }
                })
                .collect::<String>();
            compile_error(
                &mut output,
                alias.span(),
                format!(
                    "the alias {alias_string} is not defined.\n\navailable aliases: {available}",
                ),
            );
            continue;
        };

        let mut alias_documentation = TokenStream::new();

        alias_documentation.extend([
            // generate: #[doc(hidden)]
            TokenTree::Punct(Punct::new('#', Spacing::Joint)),
            TokenTree::Group(Group::new(
                Delimiter::Bracket,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("doc", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([TokenTree::Ident(Ident::new(
                            "hidden",
                            Span::mixed_site(),
                        ))]),
                    )),
                ]),
            )),
            // generate: #[allow(non_camel_case_types)]
            TokenTree::Punct(Punct::new('#', Spacing::Joint)),
            TokenTree::Group(Group::new(
                Delimiter::Bracket,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("allow", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenStream::from_iter([TokenTree::Ident(Ident::new(
                            "non_camel_case_types",
                            Span::mixed_site(),
                        ))]),
                    )),
                ]),
            )),
        ]);

        // Single line of documentation at the top

        add_doc(
            &mut alias_documentation,
            format!("Derive alias `..{alias_string}` expands to the following derives:\n"),
        );

        // Generate a single line of Documentation `#[doc = ...]" for each Derive that this Alias expands to, recursively
        // Also push all of those derives into a token stream

        let mut is_first_derive = true;
        for derive in list_of_aliased {
            // This is not an alias, it is a concrete derive

            let contains = seen_expanded.contains(derive);
            // if it's starting with a "::" remove it, since a `::foo::Bar` is considered to be the same as `foo::Bar` for
            // the purposes of duplication
            seen_expanded.insert(derive);

            // 1. If this derive has already been generated by an alias, ignore it
            if contains {
                continue;
            }

            // 2. Add a single line of documentation
            add_doc(&mut alias_documentation, format!("- [`{derive}`]"));

            // 3. Add the actual derive into the `#[std::derive(...)]` token stream
            for (i, part) in derive.split("::").enumerate() {
                // if this is NOT the first derive, AND it is the first segment, add a leading comma
                //
                // so e.g
                //
                // `derive(std::marker::Copy, std::hash::Hash)` will only insert a comma in the beginning
                //                          ^ this comma
                //
                // The outer loop that would iterate over the `std::marker::Copy` and `std::hash::Hash`
                // The inner loop (THIS one) would iterate over [`std`, `marker`, `copy`] and the 2nd iteration [`std`, `hash`, `Hash`]
                if !is_first_derive && i == 0 {
                    output.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
                }
                // Add path segment if we are NOT at the beginning
                if i > 0 {
                    output.extend([
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    ]);
                }
                output.extend([TokenTree::Ident(Ident::new(part, Span::call_site()))]);
                is_first_derive = false;
            }
        }

        #[cfg(not(feature = "workspace"))]
        let whos = "crate's";
        #[cfg(feature = "workspace")]
        let whos = "workspace's";

        add_doc(
            &mut alias_documentation,
            format!("\nDerive aliases are defined in file `derive_aliases.rs` next to the {whos} `Cargo.toml`"),
        );

        // This is the `struct` who's entire purpose is just to document what this alias expands to
        alias_documentation.extend([
            TokenTree::Ident(Ident::new("struct", Span::call_site())),
            TokenTree::Ident(Ident::new(
                &format!(
                    "{alias_string}__derive__alias__{}",
                    // when we use the same alias multiple times in the same module, we don't want names of the generated documentation
                    // items to clash
                    ALIASES_OUTPUTTED.fetch_add(1, std::sync::atomic::Ordering::Acquire)
                ),
                // when user hovers over the alias, they'll actually see docs for what it produces!
                alias.span(),
            )),
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ]);

        documented_items.push(alias_documentation);
    }

    // #[::std::prelude::v1::derive(<list of derive macros>)]
    let output = TokenStream::from_iter([
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
                TokenTree::Group(Group::new(Delimiter::Parenthesis, output)),
            ]),
        )),
    ]);

    documented_items
        .into_iter()
        .flatten()
        .chain(output)
        .collect()
}

/// Map from `Alias => Vec<Derive>`, where `Alias` expands to a bunch of derive macros (represented as plain strings).
///
/// We don't store the `TokenTree`s directly because they are `!Sync`
// TODO: Replace panics here with `compile_error!()` generations
static DERIVE_ALIASES: LazyLock<HashMap<String, Vec<String>>> = LazyLock::new(|| {
    #[cfg(feature = "workspace")]
    let Ok(dir) = std::env::var("CARGO_WORKSPACE_DIR") else {
        panic!(concat!(
            "\n\n`CARGO_WORKSPACE_DIR` environment variable must be set, which points to the directory containing the\n",
            "workspace `Cargo.toml`. Since cargo currently doesn't set this variable, in your workspace create `.cargo/config.toml` with contents:\n",
            "\n",
            "[env]\n",
            "CARGO_WORKSPACE_DIR = {{ value = \"\", relative = true }}\n",
        ))
    };
    #[cfg(not(feature = "workspace"))]
    let Ok(dir) = std::env::var("CARGO_MANIFEST_DIR") else {
        panic!("env variable `CARGO_MANIFEST_DIR` must be set, which points to the directory containing your crate's `Cargo.toml`. Cargo supplies this env variable by default")
    };

    let path = std::path::Path::new(&dir).join("derive_aliases.rs");

    let content = std::fs::read_to_string(&path).unwrap_or_else(|err| {
            panic!(
                "expected {} to exist and contain derive aliases. error: {err}.\nhere's an example of syntax in `derive_aliases.rs` file:\n\n{EXAMPLE_DERIVE_ALIASES_RS}\n",
                path.display()
            )
        });

    // This is a map from alias to derive or alias
    //
    // It is recursive
    let alias_map = parse_my_little_rust(&content);

    // Let's resolve the map so each alias maps exactly to a list of derives

    /// Given an alias, recursively expands it into a list of derives
    fn resolve_derives(
        alias: &str,
        alias_map: &HashMap<String, Vec<String>>,
        current: &mut Vec<String>,
    ) {
        let Some(list_of_aliased) = alias_map.get(alias) else {
            panic!("failed to parse aliases file: Alias `{alias}` does not exist");
        };

        for aliased in list_of_aliased {
            if let Some(alias) = aliased.strip_prefix("..") {
                resolve_derives(alias, alias_map, current);
            } else {
                current.push(aliased.clone());
            }
        }
    }

    let mut aliases_to_derives = HashMap::new();

    for alias in alias_map.keys() {
        let mut derives = vec![];
        resolve_derives(alias, &alias_map, &mut derives);

        aliases_to_derives.insert(alias.to_string(), derives);
    }

    aliases_to_derives
});

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
        if let Some(path) = stmt.strip_prefix("use") {
            let path = path.trim();
            let path = path.strip_prefix('"').expect("`use` expects a string");
            let path = path.strip_suffix('"').expect("`use` expects a string");
            let other_contents = std::fs::read_to_string(path)
                .unwrap_or_else(|err| panic!("failed to read derive aliases at {path}: {err}"));
            aliases.extend(parse_my_little_rust(&other_contents));
        } else if let Some((alias_name, alias_value)) = stmt.split_once('=') {
            let alias_name = alias_name.trim().to_string();

            // Each RHS is `A , B , C`
            let expansions: Vec<String> = alias_value
                .split(',')
                .map(|part| part.trim().to_string())
                .filter(|s| !s.is_empty())
                .collect();

            aliases.insert(alias_name, expansions);
        } else {
            panic!("invalid derive alias file")
        }
    }

    aliases
}

static ALIASES_OUTPUTTED: AtomicUsize = AtomicUsize::new(0);

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
                    vec!["PartialEq".to_string(), "Eq".to_string(),]
                ),
                (
                    "Ord".to_string(),
                    vec![
                        "PartialOrd".to_string(),
                        "Ord".to_string(),
                        "..Eq".to_string()
                    ]
                ),
            ])
        );
    }
}
