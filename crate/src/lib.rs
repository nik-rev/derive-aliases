//! `#[derive]` aliases for reducing code boilerplate
//!
//! Aliases are defined in a special file `derive_aliases.rs`, located next to your **crate**'s `Cargo.toml`:
//!
//! ```ignore
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
//! ```ignore
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
//! ```ignore
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
//! # Documentation on hover
//!
//! With `Ord` alias defines as follows:
//!
//! ```ignore
//! Eq = PartialEq, Eq;
//! Ord = PartialOrd, Ord, ..Eq;
//! ```
//!
//! Hovering over `..Ord` will show that it expands to `PartialOrd, Ord, PartialEq, Eq`:
//!
//! ![hovering shows docs](https://github.com/nik-rev/derive-aliases/blob/main/docs.png?raw=true)
//!
//! # `use` other alias files in `derive_aliases.rs`
//!
//! `use` followed by a path will inline the derive aliases located in that file.
//!
//! If `../my_other_aliases.rs` contains:
//!
//! ```ignore
//! Ord = PartialOrd, Ord, ..Eq;
//! ```
//!
//! And your `derive_aliases.rs` has:
//!
//! ```ignore
//! use "../my_other_aliases.rs";
//!
//! Eq = PartialEq, Eq;
//! ```
//!
//! Then it will inline the aliases in the other file, expanding to:
//!
//! ```ignore
//! Ord = PartialOrd, Ord, ..Eq;
//! Eq = PartialEq, Eq;
//! ```

use alias_map::ALIAS_MAP;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    iter,
    path::Path,
    sync::{atomic::AtomicUsize, LazyLock},
};

mod alias_map;
mod dsl;

/// `#[derive]` that supports derive aliases: `..Alias`
///
/// See the [crate-level](crate) documentation for more info
#[proc_macro_attribute]
pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    expand_aliases(attr).into_iter().chain(item).collect()
}

/// Expand derive aliases recursively and deduplicate
fn expand_aliases(input: TokenStream) -> TokenStream {
    // Derives that come from expansion of aliases which were already seen
    //
    // We'll just ignore them. We allow using several derive aliases that expand to the same derives,
    // by removing all duplicates
    let mut seen_expanded = HashSet::new();

    // Stuff we receive inside our attribute macro's input, aka everything inside of `#[derive(<--here-->)]`
    let mut input = input.into_iter().peekable();

    // This is what will get passed to `#[std::derive(...)]`
    let mut std_derive_input = TokenStream::new();

    // These are the temporary unit `struct`s that contains documentation about each `..Alias`
    //
    // The `span` of each `struct` is set to the `..Alias`, so hovering over the `..Alias` shows you
    // documentation about what it expands to
    let mut documentation_for_all_aliases = Vec::new();

    // We collect all errors in a single stream so we can report them all at once,
    // and rust-analyzer allows to continue us to work with other derives - even though we may have syntax errors somewhere
    let mut compile_errors = TokenStream::new();

    while let Some(tt) = input.next() {
        // We manually have to handle all `..Alias`es, but regular derives will be passed to `std::derive` verbatim
        let TokenTree::Punct(ref dot) = tt else {
            std_derive_input.extend([tt]);
            continue;
        };

        if dot.as_char() != '.' {
            std_derive_input.extend([tt]);
            continue;
        }

        let Some(dot_dot) = input.next_if(|tt| {
            if let TokenTree::Punct(dot) = tt {
                dot.as_char() == '.'
            } else {
                false
            }
        }) else {
            compile_errors.extend(CompileError::new(
                dot.span(),
                "after `.` we expect `.` followed by a derive alias, for example: `..Alias`",
            ));
            continue;
        };

        // At this point, we have a `..` so we can absolutely certain that it is an alias

        // consume the identifier `Alias`
        let Some(TokenTree::Ident(alias)) = input.next() else {
            compile_errors.extend(CompileError::new(
                dot_dot.span(),
                "after `..` we expect a derive alias, for example: `..Alias`",
            ));
            continue;
        };

        // Parsed `..Alias`.

        let alias_string = alias.to_string();

        // All derives that this alias references, fully resolved.
        let Some(derives) = ALIAS_MAP.get(&alias_string) else {
            let available = format_list(ALIAS_MAP.keys());

            let most_similar = most_similar_alias(&alias_string)
                .map(|similar| {
                    format!(
                        "did you mean: `{similar}`? it expands to: {}\n\n",
                        format_list(ALIAS_MAP.get(similar).expect(
                            "it is an existing alias, we got it by iterating over the alias names"
                        ))
                    )
                })
                .unwrap_or_default();

            compile_errors.extend(CompileError::new(
                alias.span(),
                format!(
                    "The alias `{alias_string}` is undefined.\n\n{most_similar}All available aliases: {available}",
                ),
            ));

            continue;
        };

        // A single `struct` that holds documentation for the current alias
        let mut alias_documentation = TokenStream::new();

        alias_documentation.extend(attr_with_inner("doc", "hidden"));
        alias_documentation.extend(attr_with_inner("allow", "non_camel_case_types"));

        alias_documentation.extend(doc_comment(format!(
            "Derive alias `..{alias_string}` expands to the following derives:\n"
        )));

        // Generate a single line of Documentation `#[doc = ...]" for each Derive that this Alias expands to,
        // Also push all of those derives into the final token stream passed to `#[std::derive]`

        // If this is the first derive that this alias expands to
        //
        // derive(std::marker::Copy, std::hash::Hash, Copy)
        //        ^^^^^^^^^^^^^^^^^ true
        //                           ^^^^^^^^^^^^^^^ false
        //                                            ^^^^ false
        let mut is_first_derive = true;

        for derive in derives {
            let is_duplicate_derive = seen_expanded.contains(derive);

            seen_expanded.insert(derive);

            // 1. If this derive has already been generated by an alias, ignore it
            if is_duplicate_derive {
                continue;
            }

            // 2. Add a single bullet point of documentation for this alias
            alias_documentation.extend(doc_comment(format!("- [`{derive}`]")));

            // manually handle it, since `.split()` ignores it
            let has_leading_path_separator = derive.starts_with("::");

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
                let prepend_leading_comma = !is_first_derive && i == 0;

                if prepend_leading_comma {
                    std_derive_input.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]);
                }

                // Add path segment if we are NOT at the beginning

                // - A: If we have a leading path separator, we'll always prepend a path separator at every split of `::`
                // - B: If we don't have a leading path separator, we'll prepend a path separator at every split of `::`, SKIPPING the 1st one
                //
                // ::foo::bar -> A
                // foo::bar -> B
                let prepend_path_separator = has_leading_path_separator || (i > 0);

                if prepend_path_separator {
                    std_derive_input.extend([
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    ]);
                }

                std_derive_input.extend([TokenTree::Ident(Ident::new(part, Span::call_site()))]);
                is_first_derive = false;
            }
        }

        #[cfg(not(feature = "workspace"))]
        let root = "crate's";
        #[cfg(feature = "workspace")]
        let root = "workspace's";

        alias_documentation.extend(doc_comment(
            format!("\nDerive aliases are defined in file `derive_aliases.rs` next to the {root} `Cargo.toml`"),
        ));

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

        documentation_for_all_aliases.push(alias_documentation);
    }

    documentation_for_all_aliases
        .into_iter()
        .flatten()
        .chain(std_derive(std_derive_input))
        .chain(compile_errors)
        .collect()
}

/// Generate `#[doc = $doc]`
fn doc_comment(doc: impl AsRef<str>) -> impl Iterator<Item = TokenTree> {
    [
        TokenTree::Punct(Punct::new('#', Spacing::Joint)),
        TokenTree::Group(Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new("doc", Span::call_site())),
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
                TokenTree::Literal(Literal::string(doc.as_ref())),
            ]),
        )),
    ]
    .into_iter()
}

/// `.into_iter()` generates `compile_error!($message)` at `$span`
struct CompileError {
    /// Where the compile error is generates
    span: Span,
    /// Message of the compile error
    message: String,
}

impl CompileError {
    /// Create a new compile error
    fn new(span: Span, message: impl AsRef<str>) -> Self {
        Self {
            span,
            message: message.as_ref().to_string(),
        }
    }
}

impl IntoIterator for CompileError {
    type Item = TokenTree;
    type IntoIter = std::array::IntoIter<Self::Item, 3>;

    fn into_iter(self) -> Self::IntoIter {
        [
            TokenTree::Ident(Ident::new("compile_error", self.span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(self.span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(self.span);
                        string
                    })])
                });
                group.set_span(self.span);
                group
            }),
        ]
        .into_iter()
    }
}

/// Generate `#[$attr($ident)]`
fn attr_with_inner(
    attr: impl AsRef<str>,
    ident: impl AsRef<str>,
) -> impl Iterator<Item = TokenTree> {
    [
        TokenTree::Punct(Punct::new('#', Spacing::Joint)),
        TokenTree::Group(Group::new(
            Delimiter::Bracket,
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new(attr.as_ref(), Span::call_site())),
                TokenTree::Group(Group::new(
                    Delimiter::Parenthesis,
                    TokenStream::from_iter([TokenTree::Ident(Ident::new(
                        ident.as_ref(),
                        Span::mixed_site(),
                    ))]),
                )),
            ]),
        )),
    ]
    .into_iter()
}

/// Generate `#[::std::prelude::v1::derive($derives)]`
///
/// Our macro expands aliases then passes it to the standard library `derive`
fn std_derive(derives: TokenStream) -> TokenStream {
    TokenStream::from_iter([
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
                TokenTree::Group(Group::new(Delimiter::Parenthesis, derives)),
            ]),
        )),
    ])
}

/// Intersperse list with commas and show it as a string
fn format_list<'a>(list: impl IntoIterator<Item = &'a String>) -> String {
    list.into_iter()
        .enumerate()
        .flat_map(|(i, key)| {
            // intersperse
            if i == 0 {
                vec![key.as_str()]
            } else {
                vec![", ", key.as_str()]
            }
        })
        .collect::<String>()
}

/// Used in error messages to make good suggestions
fn most_similar_alias(alias: impl AsRef<str>) -> Option<String> {
    ALIAS_MAP
        .0
        .keys()
        .map(|it| {
            (
                it.0.name.clone(),
                strsim::normalized_damerau_levenshtein(&it.0.name, alias.as_ref()),
            )
        })
        .max_by(|a, b| a.1.total_cmp(&b.1))
        // if the 2 strings are not that similar, we don't have the most similar alias
        //
        // e.g. if we have "Foo", "Bar" and we input "Fooo" the most similar will be "Foo".
        // If we remove "Foo", the most similar will be "Bar". But "Foo" and "Bar" are not similar at all, so
        // we just ignore if we can't find above a certain similarity threshold
        .filter(|it| it.1 >= 0.70)
        .map(|it| it.0)
}

static ALIASES_OUTPUTTED: AtomicUsize = AtomicUsize::new(0);
