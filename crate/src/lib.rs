#![allow(warnings)]
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
use codegen::CompileError;
use dsl::Alias;
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    io::Chain,
    iter,
    path::Path,
    sync::{atomic::AtomicUsize, LazyLock},
};

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

    // These are the temporary unit `struct`s that contains documentation about each `..Alias`
    //
    // The `span` of each `struct` is set to the `..Alias`, so hovering over the `..Alias` shows you
    // documentation about what it expands to
    let mut documentation_for_all_aliases = Vec::new();

    // We collect all errors in a single stream so we can report them all at once,
    // and rust-analyzer allows to continue us to work with other derives - even though we may have syntax errors somewhere
    let mut compile_errors = TokenStream::new();

    let (alias_map, errors) = &*ALIAS_MAP;

    // Report all parse errors as compile errors
    compile_errors.extend(
        errors
            .iter()
            .flat_map(|err| CompileError::new(Span::mixed_site(), err)),
    );

    while let Some(tt) = input.next() {
        // We manually have to handle all `..Alias`es, but regular derives will be passed to `std::derive` verbatim
        let TokenTree::Punct(ref dot) = tt else {
            no_cfg_derives.extend([tt]);
            continue;
        };

        if dot.as_char() != '.' {
            no_cfg_derives.extend([tt]);
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

        let alias_string = Alias::new(alias.to_string());

        // All derives that this alias references, fully resolved.
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

        // A single `struct` that holds documentation for the current alias
        let mut alias_documentation = TokenStream::from_iter(chain![
            codegen::attr_with_inner("doc", "hidden"),
            codegen::attr_with_inner("allow", "non_camel_case_types"),
            codegen::doc_comment(&format!(
                "Derive alias `..{alias_string}` expands to the following derives:\n"
            ))
        ]);

        for (cfg, derives) in derives {
            if cfg.is_empty() {
                // These are stuff that goes inside of  `#[std::derive(<-- here -->)]` requiring no cfg
                // Doing it this way is necessary as we'll append regular derives that don't come from an alias
                // expansion into the same TokenStream. Also, it reduces how many tokens we output which makes the macro faster
                no_cfg_derives.extend(codegen::into_std_derive_arguments(
                    cfg,
                    derives,
                    &mut alias_documentation,
                    &mut seen_expanded,
                ));
            } else {
                // A list of attributes, each one is inside of `#[cfg_attr(.., std::derive(..))]`
                cfg_attr_derives.extend(codegen::cfg_std_derive_attr(
                    cfg,
                    derives,
                    &mut alias_documentation,
                    &mut seen_expanded,
                ));
            }
        }

        // This is the `struct` who's entire purpose is just to document what this alias expands to
        alias_documentation.extend([
            TokenTree::Ident(Ident::new("struct", Span::call_site())),
            TokenTree::Ident(Ident::new(
                &format!(
                    "{alias_string}__derive__alias__{}",
                    // when we use the same alias multiple times in the same module,
                    // we don't want names of the generated documentation items to clash
                    ALIASES_OUTPUTTED.fetch_add(1, std::sync::atomic::Ordering::Acquire)
                ),
                // when user hovers over the alias, they'll actually see docs for what it produces!
                //
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
                alias.span(),
            )),
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ]);

        documentation_for_all_aliases.push(alias_documentation);
    }

    documentation_for_all_aliases
        .into_iter()
        .flatten()
        .chain(cfg_attr_derives)
        .chain(codegen::attr(codegen::std_derive(no_cfg_derives).collect()))
        .chain(compile_errors)
        .collect()
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

/// Hovering on aliases shows documentation for them. This function generates 1 line of such documentation
///
/// This is the function that generates documentation
/// for a **single** derive that comes from the expansion of an alias.
/// The `cfgs` are the cumulative `cfg` predicates that will be rendered, if non-empty.
///
/// ```rs
/// Eq = PartialEq, #[cfg(bar)] Eq;
/// Ord = PartialOrd, Ord, #[cfg(foo)] ..Eq;
/// ```
///
/// When expanding `..Ord`, we will generate *something like* the following documentation on a throwaway
/// `struct` for which we just attach documentation comments:
///
/// ```ignore
/// /// - [`PartialOrd`]
/// /// - [`Ord`]
/// /// - [`PartialEq`] when `#[cfg(foo)]`
/// /// - [`Eq`] when `#[cfg(all(foo, bar))]`
/// struct throwaway_struct_only_for_documentation_of_alias_1425042150;
/// ```
///
/// Each bullet point corresponds to a single invocation of this function.
fn single_line_of_doc_comment_for_derive(cfgs: &[dsl::Cfg], derive: &dsl::Derive) -> String {
    let mut doc_line = format!("- [`{derive}`]");

    if cfgs.len() == 1 {
        // `#` will also render `#[cfg(...)]` attribute wrapper
        doc_line.push_str(&format!(" when `{doc_line:#}`"));
    } else if cfgs.len() > 2 {
        doc_line.push_str(" when ");
        doc_line.push_str("`#[cfg(all(");
        for (i, cfg) in cfgs.iter().enumerate() {
            // This renders a predicate, e.g. `feature = "serde"`
            doc_line.push_str(&cfg.to_string());

            let is_last = i + 1 == cfgs.len();

            if !is_last {
                // Intersperse all CFG predicates with commas, except the last one
                //
                // cfg, cfg2, cfg3
                //    ^^    ^^
                doc_line.push_str(", ");
            }
        }
        doc_line.push_str("))]`");
    }

    doc_line
}

/// How many aliases were generated
static ALIASES_OUTPUTTED: AtomicUsize = AtomicUsize::new(0);
