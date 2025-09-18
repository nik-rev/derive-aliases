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
use codegen::{punct, use_underscore_keep_span, CompileError};
use dsl::Alias;
use proc_macro::{Delimiter, Span, TokenStream, TokenTree};
use std::collections::HashMap;
use std::io::Write;
use std::iter::{self, once};
use std::sync::atomic::AtomicBool;
use std::{collections::HashSet, fs::File, io::BufWriter, path::PathBuf, sync::LazyLock};

mod alias_map;
mod codegen;
mod dsl;

macro_rules! chain {
    [] => {
        std::iter::empty()
    };
    [$first:expr $(, $rest:expr)* $(,)?] => {{
        let iter = std::iter::IntoIterator::into_iter($first);
        $(
            let iter = std::iter::Iterator::chain(iter, std::iter::IntoIterator::into_iter($rest));
        )*
        iter
    }};
}
pub(crate) use chain;

macro_rules! write {
    ($($tt:tt)*) => {
        std::write!($($tt)*).expect("won't happen")
    };
}

// TODO: Allow composing `derive_aliases` files from different crates
//
// Syntax:
//
//     extern use foo::bar;
//
// This expects `foo` to be a crate, and `foo::bar` to be the exact module path where the crate's
// `derive_aliases::define!` macro was invoked
//
// We will invoke `cargo metadata` to find the crate `foo`'s manifest directory.
// From here on, we will know where its location is in the `/tmp` directory
//
// We will read its `derive_aliases` file and inline it into ours.

// when doing `#[macro_use] extern crate proc` we will also globally import this macro
// and it will be suggested by rust_analyzer. but since this macro must be called just once,
// we don't want rust_analyzer to suggest it
#[cfg_attr(not(doc), doc(hidden))]
#[proc_macro]
pub fn define(derive_aliases: TokenStream) -> TokenStream {
    /// The module where documentation for all aliases will exist
    const DERIVE_ALIASES_MODULE_NAME: &str = "derive_aliases_doc";

    let file = File::create(&*DERIVE_ALIASES_FILE).unwrap_or_else(|err| {
        panic!(
            "failed to create file {}: {err}",
            DERIVE_ALIASES_FILE.display()
        )
    });
    let mut w = BufWriter::new(file);

    let mut tts = derive_aliases.into_iter().peekable();
    let mut errors = TokenStream::new();

    let mut doc_tokens = TokenStream::new();
    let mut dummy_imports = TokenStream::new();

    // Eat a path until the end (until we find ';' or ',')
    //
    // Alias = Foo, ..Bar, std::hash::Hash;
    //         ^ start
    //         ^^^ eaten
    //                  ^ comma = end
    //                     ^ start
    //                     ^^^^^^^^^^^^^^ eaten
    //                                   ^ semicolon = end
    macro_rules! eat_path {
        // span of the `tt` directly preceding this path, used for errors if the path is empty
        ($span_of_path_start:expr) => {{
            let mut derive_path = TokenStream::new();
            let mut last_token_span = None;

            // Build up the actual, full derive path like `std::hash::Hash`
            while let Some(tt) = tts.next_if(|tt| {
                // eat until we find ',' or ';', which indicates end of the path
                //
                // Alias = std::cmp::Ord, std::hash::Hash;
                //                      ^ end
                //
                // Alias = std::cmp::Ord, std::hash::Hash;
                //                                       ^ end
                if let TokenTree::Punct(punct) = tt {
                    // if ',' or ';' then we stop
                    *punct != ',' && *punct != ';'
                } else {
                    // continue eating
                    true
                }
            }) {
                write!(w, "{tt}");

                last_token_span = Some(tt.span());

                match tt {
                    TokenTree::Punct(ref punct) if *punct == ':' => {
                        let next_tt = tts.next();

                        match next_tt {
                            Some(TokenTree::Punct(ref punct_2)) if *punct_2 == ':' => {
                                derive_path.extend([
                                    tt,
                                    next_tt.expect("we just got `Some` for `.next()`"),
                                ]);
                            }
                            _ => errors.extend(CompileError::new(
                                punct.span(),
                                "expected `:` after this to form a `::`",
                            )),
                        }
                    }
                    TokenTree::Ident(_) => {
                        derive_path.extend([tt]);
                    }
                    _ => errors.extend(CompileError::new(
                        tt.span(),
                        "expected `::` or identifier after this",
                    )),
                }
            }

            // Check that a `,` or `;` is present after the path
            match tts.next() {
                Some(TokenTree::Punct(punct)) if punct == ',' || punct == ';' => {
                    // ok
                }
                _ => errors.extend(CompileError::new(
                    if let Some(last_span) = last_token_span {
                        last_span
                    } else {
                        $span_of_path_start
                    },
                    "expected `,` or `;` after this token",
                )),
            };

            derive_path
        }};
    }

    while let Some(tt) = tts.next() {
        match tt {
            // for `#[cfg(...)]`
            //       ^        ^ delimiters
            //      ^ started
            TokenTree::Punct(punct) if punct == '#' => {
                let next_tt = tts.next();
                let group = match next_tt {
                    Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Bracket => {
                        group
                    }
                    _ => {
                        errors.extend(CompileError::new(punct.span(), "expected `[...]` after the `#` to form a `cfg` attribute: `#[cfg(...)]`"));
                        continue;
                    }
                };
                // write the entire cfg without processing it
                write!(w, "[{}]", group.stream().to_string());
            }
            TokenTree::Ident(ref ident) => {
                write!(w, "{ident}");
                let ident_span = ident.span();

                match tts.next() {
                    // if the next token is '=', the current identifier is an alias definition
                    //
                    // Alias = Ord, PartialOrd;
                    // ^^^^^ we are here
                    //       ^ next
                    //
                    Some(TokenTree::Punct(punct)) if punct == '=' => {
                        let ident_string = ident.to_string();

                        // pub mod Alias {}
                        doc_tokens.extend(chain![
                            codegen::ident("pub", Span::call_site()),
                            codegen::ident("mod", Span::call_site()),
                            // when we hover over definition in the macro,
                            // we'll actually see what the alias will expand to
                            codegen::ident(ident_string, ident_span),
                            // mod has nothing in it; it's just for documentation
                            codegen::nested_in::<'{', '}'>(TokenStream::new()),
                        ]);
                    }
                    // if the next token is ',' the current derive is just a single identifier
                    //
                    // Alias = Ord, std::hash::Hash;
                    //         ^^^ we are here
                    //            ^ terminator
                    //
                    // ...or if the next token is ';' the current derive is just a single identifier
                    //
                    // Alias = Ord;
                    //         ^^^ we are here
                    //            ^ terminator
                    //
                    Some(TokenTree::Punct(terminator))
                        if matches!(terminator.as_char(), ';' | ',') =>
                    {
                        dummy_imports.extend(codegen::use_underscore_keep_span(
                            iter::once(tt.clone()),
                            tt.clone().span(),
                        ));
                    }
                    // if the next token is a ':' the current identifier is the beginning of a path to a derive
                    //
                    // Alias = Ord, std::hash::Hash, Foo;
                    //              ^^^ we are here
                    //                 ^ next
                    Some(TokenTree::Punct(colon)) if colon == ':' => {
                        let colon_colon = match tts.next() {
                            Some(TokenTree::Punct(punct_2)) if punct_2 == ':' => {
                                // ok
                                punct_2
                            }
                            _ => {
                                errors.extend(CompileError::new(
                                    colon.span(),
                                    "expected `:` after this to form `::` (path segment)",
                                ));
                                continue;
                            }
                        };

                        let rest_of_path = eat_path!(ident_span);

                        dummy_imports.extend(codegen::use_underscore_keep_span(
                            chain![
                                iter::once(tt),
                                once(TokenTree::Punct(colon)),
                                once(TokenTree::Punct(colon_colon)),
                                rest_of_path
                            ],
                            Span::call_site(),
                        ));
                    }
                    // Imports derive aliases from another crate
                    //
                    // extern use foo::bar;
                    Some(TokenTree::Ident(kw_use))
                        if ident.to_string() == "extern" && kw_use.to_string() == "use" =>
                    {
                        // extern use foo::bar;
                        //            ^^^^^^^^ the crate and module
                        let path = eat_path!(kw_use.span());

                        if path.is_empty() {
                            errors.extend(CompileError::new(kw_use.span(), "expected a path after this identifier, e.g. `extern use foo::bar;`"));
                        }

                        // pub use foo::bar::*;
                        //
                        // We use everything from the module so we can do "goto definition" or "hover"
                        // and still get the very same documentation, even though it's in a completely different
                        // crate
                        doc_tokens.extend(chain![
                            codegen::ident("pub", Span::call_site()),
                            codegen::ident("use", kw_use.span()),
                            path,
                            codegen::path_sep(),
                            codegen::ident(DERIVE_ALIASES_MODULE_NAME, Span::call_site()),
                            codegen::path_sep(),
                            codegen::punct('*'),
                            codegen::punct(';'),
                        ]);
                    }
                    // extern, but not followed by "use"
                    _ if ident.to_string() == "extern" => errors.extend(CompileError::new(
                        ident.span(),
                        "expected `use` after this `extern`",
                    )),
                    None => errors.extend(CompileError::new(
                        ident.span(),
                        "expected a ';' after this identifier",
                    )),
                    Some(any) => errors.extend(CompileError::new(any.span(), "unexpected token")),
                }
            }
            // we are at the beginning of a reference to a derive that uses an absolute path
            //
            // Alias = Foo, ::std::hash::Hash;
            //              ^ we are here
            //              ^^^^^^^^^^^^^^^^^ when user hovers here, we'll reference the
            //                                actul spans of `Hash`
            TokenTree::Punct(ref colon) if *colon == ':' => {
                let next_tt = tts.next();
                let colon_colon = match next_tt {
                    Some(TokenTree::Punct(ref colon_colon)) if *colon_colon == ':' => colon_colon,
                    _ => {
                        errors.extend(CompileError::new(
                            colon.span(),
                            "expected `:` after this to form a path separator: `::`",
                        ));
                        continue;
                    }
                };

                write!(w, "::");

                // Rest of the path
                let path = eat_path!(colon_colon.span());

                // same as before, eat until we find ',' or ';'
                // for documentation on hover
                dummy_imports.extend(use_underscore_keep_span(
                    chain![
                        once(tt),
                        once(next_tt.expect("would have `continue`d otherwise")),
                        path.into_iter()
                    ],
                    Span::call_site(),
                ))
            }
            // alias usage: ..Alias
            TokenTree::Punct(dot) if dot == '.' => {
                // Next token must be a `.`
                let dot_dot = match tts.next() {
                    Some(TokenTree::Punct(dot_dot)) if dot_dot == '.' => dot_dot,
                    _ => {
                        errors.extend(CompileError::new(
                            dot.span(),
                            "expected `.` after this to spread an alias: `..Alias` into derives",
                        ));
                        continue;
                    }
                };

                write!(w, "..");

                // we are in an alias usage site
                //
                // Alias = Foo, ..Another
                //               ^ we are here
                //                ^^^^^^^ then the next identifier must be an alias Usage site
                if let Some(TokenTree::Ident(alias_ident)) = tts.next() {
                    match tts.next() {
                        Some(TokenTree::Punct(punct)) if punct == ';' || punct == ',' => {
                            // ok
                        }
                        _ => errors.extend(CompileError::new(
                            alias_ident.span(),
                            "expected `;` or `,` after this alias",
                        )),
                    }

                    // When the user hovers over `..Another`, we want to display documentation
                    // of the alias and what it expands to. Basically, same as if we hovered over
                    // the definition
                    //
                    // pub use Alias as _;
                    //         ^^^^^ the span that we will set it to
                    doc_tokens.extend(chain![
                        codegen::ident("pub", alias_ident.span()),
                        codegen::ident("use", alias_ident.span()),
                        codegen::ident(alias_ident.to_string(), alias_ident.span()),
                        codegen::ident("as", alias_ident.span()),
                        codegen::ident("_", alias_ident.span()),
                        punct(';'),
                    ]);

                    write!(w, "{alias_ident}");
                } else {
                    errors.extend(CompileError::new(dot_dot.span(), "expected name of alias after `..` to spread an alias: `..Alias` into derives"));
                }
            }
            unexpected => {
                errors.extend(CompileError::new(unexpected.span(), "unexpected token"));
            }
        }
    }

    // Write to the file
    w.flush().unwrap();

    TokenStream::from_iter(chain![
        codegen::attr_fn("allow", "non_snake_case"),
        codegen::attr_fn("allow", "unused_imports"),
        codegen::attr_fn("doc", "hidden"),
        //
        codegen::ident("pub", Span::call_site()),
        codegen::ident("mod", Span::call_site()),
        codegen::ident(DERIVE_ALIASES_MODULE_NAME, Span::call_site()),
        codegen::nested_in::<'{', '}'>(
            chain![
                doc_tokens,
                codegen::ident("mod", Span::call_site()),
                codegen::ident("dummy_imports", Span::call_site()),
                codegen::nested_in::<'{', '}'>(dummy_imports)
            ]
            .collect()
        ),
        errors
    ])
}

/// A `#[derive]` that supports derive aliases
///
/// For example, the following code, which uses `#[derive_aliases::derive]` instead of `#[std::derive]`:
///
/// ```ignore
/// use derive_aliases::derive;
///
/// #[derive(..Ord)]
/// ```
///
/// Will expand to this:
///
/// ```ignore
/// use derive_aliases::derive;
///
/// #[std::derive(Ord, PartialOrd, Eq, PartialEq)]
/// ```
///
/// If `derive_aliases.rs` contains the following:
///
/// ```ignore
/// // Defines alias `..Eq` that expands to `PartialEq, Eq`
/// Eq = PartialEq, Eq;
/// // Defines alias `..ord` that expands to `PartialOrd, Ord, PartialEq, Eq`
/// Ord = PartialOrd, Ord, ..Eq;
/// ```
///
/// Derive aliases are defined in file `derive_aliases.rs` next to the crate's `Cargo.toml`
///
/// For more info, see the [crate-level](crate) documentation
#[proc_macro_attribute]
pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    expand_aliases(attr).into_iter().chain(item).collect()
}

/// Expand a list of derives and derive aliases into a list of derives
///
/// At a high-level, the `derive_aliases::derive` proc macro attribute:
///
/// - Takes a list of aliases (e.g. `..MyAlias`) and normal derives (e.g. `std::hash::Hash`)
/// - All aliases are expanded into a list of real Rust derives. These derives are then directly
///   passed to `std::derive`
///
/// # Exact algorithm
///
/// We do 99.9% of the work **once** per the entire compilation session, as all aliases are stored in a `static`.
///
/// The exact algorithm is the following:
///
/// 1. The `derive_aliases.rs` file is [parsed](alias_map::parse) into a map of `Alias => Vec<AliasDeclaration>`
///
///    - Each `Alias` is just a `String` which can be referenced as `..Alias`
///    - Each `AliasDeclaration` creates an `Alias`, which references a list of (`Alias` or `Derive`).
///
///    The map is self-referential, at this stage
///
/// 2. We flatten this map into a map of `Alias => Vec<Derive>`, resolving any aliases recursively.
///
///    Each `Derive` is then just a path to the real `#[proc_macro_derive(..)]` contained in Rust, and it will be passed
///    directly to `std::derive`
///
///    The `#[cfg]` predicates we encounter along the way are cumulated together, so each `Derive` contains a `Vec<Cfg>`
///    These predicates are passed to `#[cfg_attr(..)]` and Rust must evaluate them to `true` for them to be invoked
///
/// 3. If we emitted code with the current data, we would generate a `#[std::derive]` for every single derive alias, which would be inefficient.
///    We want our proc macro to be as fast
///
///    We therefore resolve this map yet again into a map of `Alias => { Vec<Cfg> => Vec<Derive> }`, where `Derive`s that have the
///    same `Vec<Cfg>` are placed together, and we will emit just a single `#[cfg_attr]` for each entry in the map.
///
/// 4. We're now done with 99.9% of the work, and we store the result in [`ALIAS_MAP`].
///
///    From here on, every invocation of `derive` will skip all of those steps and directly read from the `ALIAS_MAP` in order to convert
///    it's list of aliases and derives into a list of derives, which are all passed to `#[std::derive]`,
///
/// 5. When we receive an alias `..Alias`, we get a map of `Vec<Cfg> => Vec<Derive>` from `ALIAS_MAP` and iterate over
///    the entries, generating a single `#[std::derive]` (or `#[cfg_attr(.., std::derive(..))]` if there is at least 1 `Cfg` predicate)
///    for each entry.
///
/// # Notes
///
/// - If aliases expands to a derive that was already the result of an alias expansion, then it is ignored. This allows us to define aliases
///   that have overlapping derives and then use them together
///
/// - When we encounter any errors, we collect them into a bunch of `compile_error!(..)` invocations then report them all at once
fn expand_aliases(input: TokenStream) -> TokenStream {
    // Derives that come from expansion of aliases which were already seen
    //
    // We'll just ignore them. We allow using several derive aliases that expand to the same derives,
    // by removing all duplicates
    let mut seen_expanded = HashSet::new();

    // Stuff we receive inside our attribute macro's input, aka everything inside of `#[derive(<--here-->)]`
    let mut input = input.into_iter().peekable();

    // These are all of the `#[cfg_attr(.., std::derive(..))]` attributes
    // that we will generate.
    let mut cfg_attr_derives = TokenStream::new();

    // These are all arguments to the plain `#[std::derive(<-- here -->)]`s that doesn't require any `cfg`
    let mut no_cfg_derives = TokenStream::new();

    // These imports exist only for documentation. They look like this:
    //
    // use crate::derive_aliases_doc::SomeAlias as _;
    //                                ^^^^^^^^
    //
    // The `SomeAlias` will contain documentation about what the alias `..SomeAlias` expands to.
    // When user hovers over `..SomeAlias`, they will see the docs about it
    let mut dummy_imports_for_docs = TokenStream::new();

    // We collect all errors in a single stream so we can report them all at once,
    // and rust-analyzer allows to continue us to work with other derives - even though we may have syntax errors somewhere
    let mut compile_errors = TokenStream::new();

    let (alias_map, errors) = &*ALIAS_MAP;

    // Report all parse errors as compile errors
    compile_errors.extend(
        errors
            .iter()
            .flat_map(|err| CompileError::new(Span::call_site(), err)),
    );

    while let Some(tt) = input.next() {
        // We manually have to handle all `..Alias`es, but regular derives will be passed to `std::derive` verbatim
        let TokenTree::Punct(ref punct) = tt else {
            no_cfg_derives.extend([tt]);
            continue;
        };

        if *punct != '.' {
            // Do not add leading commas.
            //
            // This can happen when there is an alias at the beginning, but it is removed because it has `cfg`s
            //
            // e.g. from this:
            //
            // #[derive(..Eq, PartialEq)]
            //
            // To this:
            //
            // #[cfg_attr(feature = "eq", Eq, PartialEq)]
            // #[derive(, PartialOrd)]
            //          ^ this comma still exists, must be removed
            //
            if *punct == ',' && no_cfg_derives.is_empty() {
                // skip
            } else {
                no_cfg_derives.extend([tt]);
            }
            continue;
        }

        let Some(punct_next) = input.next_if(|tt| {
            if let TokenTree::Punct(punct) = tt {
                *punct == '.'
            } else {
                false
            }
        }) else {
            compile_errors.extend(CompileError::new(
                punct.span(),
                "after `.` we expect `.` followed by a derive alias, for example: `..Alias`",
            ));
            continue;
        };

        // At this point, we have a `..` so we can absolutely certain that it is an alias

        // consume the identifier `Alias`
        let Some(TokenTree::Ident(alias)) = input.next() else {
            compile_errors.extend(CompileError::new(
                punct_next.span(),
                "after `..` we expect a derive alias, for example: `..Alias`",
            ));
            continue;
        };

        // Parsed `..Alias`.

        let alias_string = Alias::new(alias.to_string());

        // All derives that this alias will expand to
        let Some(derives) = alias_map.get(&alias_string) else {
            let all_aliases = format_list(alias_map.keys().map(|key| &key.0.name));

            let most_similar_alias = most_similar_alias(&alias_string.0.name)
                .map(|similar| {
                    format!(
                        "did you mean: `{similar}`?",
                        // "did you mean: `{similar}`? it expands to: {}\n\n",
                        // format_list(alias_map.get(&Alias::new(similar)).expect(
                        //     "it is an existing alias, we got it by iterating over the alias names"
                        // ))
                    )
                })
                .unwrap_or_default();

            compile_errors.extend(CompileError::new(
                alias.span(),
                format!(
                    "The alias `{alias_string}` is undefined.\n\n{most_similar_alias}All available aliases: {all_aliases}",
                ),
            ));

            continue;
        };

        for (cfg, derives) in derives {
            if cfg.is_empty() {
                // These are stuff that goes inside of  `#[std::derive(<-- here -->)]` requiring no cfg
                // Doing it this way is necessary as we'll append regular derives that don't come from an alias
                // expansion into the same TokenStream. Also, it reduces how many tokens we output which makes the macro faster
                no_cfg_derives.extend(codegen::into_std_derive_arguments(
                    derives,
                    &mut seen_expanded,
                ));
            } else {
                // A list of attributes, each one is inside of `#[cfg_attr(.., std::derive(..))]`
                cfg_attr_derives.extend(codegen::cfg_std_derive_attr(
                    cfg,
                    derives,
                    &mut seen_expanded,
                ));
            }
        }

        // dummy `use` simply to reference the `alias` in the generated doc module, for documentation on hover
        // TODO: This span only covers the identifier part:
        //
        // ..Alias
        //   ^^^^^
        //
        // What we actually want is a span that also covers the 2 dots:
        //
        // ..Alias
        // ^^^^^^^
        //
        // This would be possible with `Span::join`, but it is unstable
        let span = alias.span();

        dummy_imports_for_docs.extend(codegen::use_underscore(
            iter::once(TokenTree::Ident(alias)),
            span,
        ));
    }

    // We split into 2 parts because we don't want to generate a `#[std::derive(..)]` if there are no non-cfg derives
    if no_cfg_derives.is_empty() {
        chain![dummy_imports_for_docs, cfg_attr_derives, compile_errors].collect()
    } else {
        chain![
            dummy_imports_for_docs,
            cfg_attr_derives,
            codegen::attr(codegen::std_derive(no_cfg_derives).collect()),
            compile_errors
        ]
        .collect()
    }
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

/// When the `derive_aliases::define!` macro is called, it processes the received
/// `TokenStream` and then dumps the tokens into this file.
///
/// The first call of the `derive_aliases::derive!` macro will then parse contents of this file
/// into an in-memory data structure [`ALIAS_MAP`], which contains a mapping from aliases to derives.
///
/// All other calls to `derive_aliases::derive!` will read directly from the efficient `ALIAS_MAP`
/// and will not touch this static.
///
/// Extra weird: `derive_aliases::define!` itself reads from the file to generate documentation for
/// all of the derive aliases. This obviously must be optional - if this variable is not set, the
/// `derive_aliases::derive!` macro must compile (in order to create it in the first place!)
static DERIVE_ALIASES_FILE: LazyLock<PathBuf> = LazyLock::new(|| {
    // HACK: Cargo does not expose `OUT_DIR` and hacky methods of retrieving it (e.g. via `std::env::args()`)
    // don't always work. So, what we do is create a unique filename in the temporary directory
    //
    // Every crate which depends on `derive_aliases` MUST have a unique file name that doesn't clash with
    // each other. If not, then bugs will happen: if 2 crates have the same `DERIVE_ALIASES_FILE` then
    // this derive macro will read from the same file for both of them, and compilation will fail
    //
    // To guarantee that this can never happen, we use `CARGO_MANIFEST_DIR` because it is unique for each crate
    std::env::temp_dir().join(format!(
        "derive_aliases_{}",
        std::env::var("CARGO_MANIFEST_DIR")
            .expect("expected env variable `CARGO_MANIFEST_DIR` to be defined")
            // replace all path separators so we don't refer to directories
            .replacen(std::path::MAIN_SEPARATOR, "_", usize::MAX)
    ))
});
