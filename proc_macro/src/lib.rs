//! Implementation detail of the `derive_aliases` crate

use crate::tokens::IntoTokens;
use crate::tokens::PathSeparator;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use proc_macro::{Literal, Spacing};
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};
use tokens::Path;
use tokens::TokensIter;

mod tokens;

use tokens::CompileError;

/// Returns `{ true }`
fn cfg_true() -> TokenTree {
    TokenTree::Group(Group::new(
        Delimiter::Brace,
        TokenStream::from_iter([TokenTree::Ident(Ident::new("true", Span::call_site()))]),
    ))
}

/// Visibility mode defines who can see derive aliases
///
/// It is a hack that is a necessary evil due to there being no way to export a `macro_rules!` macro
/// from the crate *without* using `#[macro_export]` on it
enum VisibilityMode {
    /// User specified `#![export_derive_aliases]`.
    ///
    /// `#[macro_export]` is applied to every generated `macro_rules!` item
    MacroExport {
        /// #![export_derive_aliases]
        ///    ^^^^^^^^^^^^^^^^^^^^^
        kw_export_derive_aliases: Span,
    },
    /// Default visibility mode for aliases: only accessible within the defining crate
    CrateLocal,
}

impl VisibilityMode {
    /// Parse #![export_derive_aliases], if it exists
    pub fn parse(ts: &mut TokensIter, compile_errors: &mut Vec<CompileError>) -> Self {
        // If we have this attribute:
        //
        // #![export_derive_aliases]
        //
        // Then we'll apply `#[macro_export]` to all `macro_rules!` aliases
        let export_derive_aliases = match ts.peek_char('#') {
            Some(_) => 'ret: {
                // #![export_derive_aliases]
                // ^
                ts.tt();

                // #![export_derive_aliases]
                //  ^
                if ts.char('!').is_none() {
                    compile_errors.push(ts.compile_error("expected `#![export_derive_aliases]`"));
                };

                // #![export_derive_aliases]
                //   ^^^^^^^^^^^^^^^^^^^^^^^
                let Some(stream) = ts.group(Delimiter::Bracket) else {
                    compile_errors.push(ts.compile_error("expected `#![export_derive_aliases]`"));
                    break 'ret None;
                };
                let mut stream = TokensIter {
                    stream: stream.into_iter().peekable(),
                    span: ts.span,
                };

                // #![export_derive_aliases]
                //    ^^^^^^^^^^^^^^^^^^^^^
                let Some(ident_span) = stream
                    .ident()
                    .filter(|ident| ident.to_string() == "export_derive_aliases")
                else {
                    compile_errors.push(ts.compile_error("expected `#![export_derive_aliases]`"));
                    break 'ret None;
                };

                Some(ident_span)
            }
            None => None,
        };

        match export_derive_aliases {
            Some(span) => Self::MacroExport {
                kw_export_derive_aliases: span.span(),
            },
            None => Self::CrateLocal,
        }
    }

    pub fn as_ident(&self) -> Ident {
        let mode = match self {
            VisibilityMode::MacroExport { .. } => "b",
            VisibilityMode::CrateLocal => "a",
        };

        Ident::new(mode, Span::call_site())
    }
}

#[cfg_attr(
    not(doc),
    doc = "\
Define derive aliases that can be used in [`#[derive]`](derive)

```rust
# mod derive_alias {
derive_aliases::define! {
    Copy = ::core::marker::Copy, ::core::clone::Clone;
    Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
    Ord = ..Eq, ::core::cmp::Ord, ::core::cmp::PartialOrd;
    StdTraits = ..Copy, ..Ord, ::core::hash::Hash, ::core::fmt::Debug;
}
# }
# fn main() {}
```

Alias declarations end with `;`. On the left of the `=` is name of the alias, and on the right are the derives it expands to, separated by commas.

See the [crate-level](https://docs.rs/derive_aliases/latest/derive_aliases) documentation for details"
)]
// NOTE on `#[cfg_attr(not(doc), doc = "...")]`:
//
// Documentation on an `#[doc(inline)] pub use item` concatenates with documentation of `item`
// in the generated HTML documentation. I want users to see documentation on hover and when they goto definition
// they see the actual documentation, but without having duplicate documentation in the HTML documentation
//
// ---
#[cfg_attr(not(feature = "show"), doc(hidden))]
#[allow(unused_assignments)]
#[proc_macro]
pub fn define(tts: TokenStream) -> TokenStream {
    // Whole input to the macro
    let mut ts = TokensIter {
        stream: tts.into_iter().peekable(),
        span: Span::call_site(),
    };

    // Compile errors to report all at once
    let mut compile_errors = Vec::new();

    // Parse #![export_derive_aliases]
    let visibility_mode = VisibilityMode::parse(&mut ts, &mut compile_errors);

    // First, let's create a Nested Alias Map:
    //
    // Alias => Derive OR Alias
    //
    // It's nested because each alias expands to a bunch of aliases+derives,
    // so it is recursive
    //
    // Later, we will normalize this by resolving all aliases, so we know exactly which derives
    // an alias expands into
    let nested_alias_map = parse_aliases(&mut ts, &mut compile_errors);

    // All of these are just `use ... as _` the only reason we have them
    // is to get access to the `Span` of whatever item they import, which we'll
    // use in documentation to get nice docs-on-hover and goto-definition
    //
    // #[doc = "..."]
    // #[allow(non_camel_case_types)]
    // struct export_derive_aliases;
    let mut dummy_use_statements = match visibility_mode {
        // doesn't have #![export_derive_aliases]
        VisibilityMode::CrateLocal => TokenStream::new(),
        // has #![export_derive_aliases]
        VisibilityMode::MacroExport {
            kw_export_derive_aliases,
        } => {
            TokenStream::from_iter([
                // #[doc = "..."]
                TokenTree::Punct(Punct::new('#', Spacing::Joint)),
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter([
                        TokenTree::Ident(Ident::new("doc", Span::call_site())),
                        TokenTree::Punct(Punct::new('=', Spacing::Joint)),
                        TokenTree::Literal(Literal::string(include_str!(
                            "export_derive_aliases.md"
                        ))),
                    ]),
                )),
                // #[allow(non_camel_case_types)]
                TokenTree::Punct(Punct::new('#', Spacing::Joint)),
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter([
                        TokenTree::Ident(Ident::new("allow", Span::call_site())),
                        TokenTree::Group(Group::new(
                            Delimiter::Parenthesis,
                            TokenTree::Ident(Ident::new("non_camel_case_types", Span::call_site()))
                                .into(),
                        )),
                    ]),
                )),
                // struct export_derive_aliases;
                TokenTree::Ident(Ident::new("struct", Span::call_site())),
                TokenTree::Ident(Ident::new(
                    "export_derive_aliases",
                    kw_export_derive_aliases,
                )),
                TokenTree::Punct(Punct::new(';', Spacing::Joint)),
            ])
        }
    };

    // This is a map `Alias => Derive`, with all nested aliases resolved
    let mut flat_alias_map = HashMap::new();

    struct DeriveData {
        alias_name_span: Span,
        flat_derives: HashSet<Path>,
        extern_aliases: Vec<Ident>,
    }

    // Build up the `flat_alias_map`
    for (alias_name, (alias_name_span, entities)) in &nested_alias_map {
        // A flat list of derives that the alias expands to
        let mut flat_derives = HashSet::new();

        // "external" aliases that were NOT defined by this macro,
        // we'll just nest the aliases then call the `__internal_derive_aliases_new_alias_with_externs` macro to de-duplicate
        let mut extern_aliases = Vec::new();

        // use crate::derive_alias::Foo as _;
        //
        // We do this so we get documentation when hovering over the alias
        dummy_use_statements.extend([
            TokenTree::Ident(Ident::new("use", Span::call_site())),
            TokenTree::Ident(Ident::new("crate", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Ident(Ident::new(alias_name, *alias_name_span)),
            TokenTree::Ident(Ident::new("as", Span::call_site())),
            TokenTree::Ident(Ident::new("_", Span::call_site())),
            TokenTree::Punct(Punct::new(';', Spacing::Joint)),
        ]);

        // Resolve the alias and the recursive list of its derives
        resolve_alias(
            &nested_alias_map,
            entities,
            &mut flat_derives,
            &mut dummy_use_statements,
            &mut extern_aliases,
        );

        for derive in &flat_derives {
            // use ::std::hash::Hash as _;
            //     ^^^^^^^^^^^^^^^^^ then we take this span and associate it with
            //                       what the user wrote in `define!` call, to
            //                       get documentation on hover
            dummy_use_statements.extend(
                [
                    // use ::std::hash::Hash as _;
                    // ^^^
                    TokenTree::Ident(Ident::new("use", Span::call_site())),
                ]
                .into_iter()
                // use ::std::hash::Hash as _;
                //     ^^^^^^^^^^^^^^^^^
                .chain(derive.clone().into_tokens())
                // use ::std::hash::Hash as _;
                //                       ^^^^^
                .chain([
                    // use ::std::hash::Hash as _;
                    //                       ^^
                    TokenTree::Ident(Ident::new("as", Span::call_site())),
                    // use ::std::hash::Hash as _;
                    //                          ^
                    TokenTree::Ident(Ident::new(
                        "_",
                        derive.components.last().map_or_else(
                            || derive.first_component.span(),
                            |(_, ident)| ident.span(),
                        ),
                    )),
                    // use ::std::hash::Hash as _;
                    //                           ^
                    TokenTree::Punct(Punct::new(';', Spacing::Joint)),
                ]),
            );
        }

        // A single alias. Done. We've resolved all of the actual aliases it points to
        //
        // derive_aliases::define! {
        //     Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
        // }
        // derive_aliases::define! {
        //     Copy = ::core::marker::Copy, ::core::clone::Clone;
        //     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
        // }
        //
        // For the `Ord` alias above:
        //
        // - flat_derives        =    ::core::marker::Copy, ::core::clone::Clone, ::core::cmp::PartialOrd, ::core::cmp::Ord
        // - extern_aliases      =    Eq
        flat_alias_map.insert(
            alias_name,
            DeriveData {
                alias_name_span: *alias_name_span,
                flat_derives,
                extern_aliases,
            },
        );
    }

    // Finally let's expand all of this to a bunch of invocations of the `__internal_derive_aliases_new_alias!` macro
    flat_alias_map
        .into_iter()
        .flat_map(
            |(
                alias,
                DeriveData {
                    alias_name_span,
                    flat_derives,
                    mut extern_aliases,
                },
            )| {
                // The Input passed into the `new_alias!`
                //
                // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                let input_into_new_alias_macro = [
                    // Visibility mode `a` or `b`
                    //
                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    // ^
                    TokenTree::Ident(visibility_mode.as_ident()),
                    // Dollar token: The macro creates macros, so we can't use '$' in it. This becomes `$_:tt` so
                    // we use `$_` inside the macro
                    //
                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //   ^
                    TokenTree::Punct(Punct::new('$', proc_macro::Spacing::Alone)),
                    // Real name of the alias
                    //
                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //     ^^^
                    //
                    // IMPORTANT: `alias_span` here allows us to associate definition of the actual alias
                    // with usages of it. This means when we do "goto definition" it takes us to the ACTUAL alias definition
                    // Very, very important to not remove this for good DX
                    TokenTree::Ident(Ident::new(alias, alias_name_span)),
                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //        ^
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                ]
                .into_iter()
                // The input is then followed by a bunch of paths to all the derives the macro expands to,
                // wrapped in `[...]`. so like `[ { cfg } ::core::hash::Hash]`. That's because we want to compare 2 paths,
                // but `:path` specifiers can't be compared, so we compare `$($tt)*` in `[ $cfg:tt $($tt:tt)*]` instead,
                // and ignore the `$cfg`
                //
                // That `cfg` is any `#[cfg(cfg)]` attributes the alias expects
                //
                // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                //          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                .chain(flat_derives.iter().flat_map(|derive| {
                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //            ^^^^^^^                        ^^^^^^^
                    let derive_cfg = cfg_true();

                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //                    ^^^^^^^^^^^^^^^^^^             ^^^^^^^^^^^^^^^^^^
                    let derive_path = derive.clone().into_tokens();

                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    let derive = Group::new(
                        Delimiter::Bracket,
                        TokenStream::from_iter([derive_cfg].into_iter().chain(derive_path)),
                    );

                    // a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],
                    //                                       ^                              ^
                    let comma = Punct::new(',', proc_macro::Spacing::Joint);

                    [TokenTree::Group(derive), TokenTree::Punct(comma)]
                }));

                // NOTE: Treat the last extern alias specially, because we'll actually invoke it.
                // The nested extern aliases will be invoked by this one, one-after-the-other
                if let Some(last_extern_alias) = extern_aliases.pop() {
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenStream::from_iter([
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        // ^^^^^
                        TokenTree::Ident(Ident::new("crate", Span::call_site())),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        //      ^^
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        //        ^^^^^^^^^^^^
                        TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        //                    ^^
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        //                      ^^^
                        TokenTree::Ident(last_extern_alias),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                        //                         ^
                        TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                        //                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        TokenTree::Group(Group::new(
                            Delimiter::Brace,
                            TokenStream::from_iter([
                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],]] }
                                //                            ^
                                TokenTree::Punct(Punct::new('%', Spacing::Joint)),
                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                TokenTree::Group(Group::new(
                                    Delimiter::Bracket,
                                    extern_aliases.into_iter().fold(
                                        TokenStream::from_iter(input_into_new_alias_macro),
                                        |acc, alias| {
                                            TokenStream::from_iter([
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                               ^^^^^
                                                TokenTree::Ident(Ident::new(
                                                    "crate",
                                                    Span::call_site(),
                                                )),
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                    ^^
                                                TokenTree::Punct(Punct::new(
                                                    ':',
                                                    proc_macro::Spacing::Joint,
                                                )),
                                                TokenTree::Punct(Punct::new(
                                                    ':',
                                                    proc_macro::Spacing::Joint,
                                                )),
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                      ^^^^^^^^^^^^
                                                TokenTree::Ident(Ident::new(
                                                    "derive_alias",
                                                    Span::call_site(),
                                                )),
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                                  ^^
                                                TokenTree::Punct(Punct::new(
                                                    ':',
                                                    proc_macro::Spacing::Joint,
                                                )),
                                                TokenTree::Punct(Punct::new(
                                                    ':',
                                                    proc_macro::Spacing::Joint,
                                                )),
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                                    ^^
                                                TokenTree::Ident(alias),
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                                      ^
                                                TokenTree::Punct(Punct::new(
                                                    ',',
                                                    proc_macro::Spacing::Joint,
                                                )),
                                                // Finally, add the input
                                                //
                                                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                                //                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                                TokenTree::Group(Group::new(
                                                    Delimiter::Bracket,
                                                    acc,
                                                )),
                                            ])
                                        },
                                    ),
                                )),
                            ]),
                        )),
                    ])
                } else {
                    // SIMPLE, and the most common case: There are no extern aliases referenced! That means
                    // we can create the alias fully from memory, including the documentation
                    //
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenStream::from_iter([
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        // ^^
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //   ^^^^^^^^^^^^^^
                        TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //                 ^^
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        TokenTree::Ident(Ident::new(
                            "__internal_derive_aliases_new_alias",
                            Span::call_site(),
                        )),
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //                                                      ^
                        TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        TokenTree::Group(Group::new(
                            Delimiter::Brace,
                            TokenStream::from_iter(
                                // The generated documentation. Which we can fully create from what we already know
                                //
                                // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                                //                                                          ^^^^^
                                [TokenTree::Literal(Literal::string(
                                    &generate_documentation_for_alias(alias, &flat_derives),
                                ))]
                                .into_iter()
                                // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                                //                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                .chain(input_into_new_alias_macro),
                            ),
                        )),
                    ])
                }
            },
        )
        // Compile errors, which we report all at once
        .chain(compile_errors.into_iter().flat_map(IntoTokens::into_tokens))
        .chain(TokenStream::from_iter([
            // #[allow(unused_imports)] - That's literally the point! (see below)
            TokenTree::Punct(Punct::new('#', Spacing::Joint)),
            TokenTree::Group(Group::new(
                Delimiter::Bracket,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("allow", Span::call_site())),
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        TokenTree::Ident(Ident::new("unused_imports", Span::call_site())).into(),
                    )),
                ]),
            )),
            // A bunch of dummy `use` statements. We only do this because we want to get the `Span` of whatever
            // they import, so we can have documentation-on-hover
            //
            // const _: () = {
            //     use foo::bar as _
            //     use bar::baz as _
            // };
            //
            // NOTE: We put it in an anonymous `const` declaration because then we need to emit just a single `#[allow(unused_imports)]`,
            // instead of having to put it on every import.
            TokenTree::Ident(Ident::new("const", Span::call_site())),
            TokenTree::Ident(Ident::new("_", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
            TokenTree::Punct(Punct::new('=', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Brace, dummy_use_statements)),
            TokenTree::Punct(Punct::new(';', Spacing::Joint)),
        ]))
        .collect()
}

fn parse_aliases(
    ts: &mut TokensIter,
    compile_errors: &mut Vec<CompileError>,
) -> HashMap<String, (Span, Vec<Entity>)> {
    let mut nested_alias_map = HashMap::new();

    // We parse each alias declaration token-by-token. If the current alias
    // declaration has a syntax error we'll just report it and skip parsing the current alias.
    // This allows us to collect as many errors as possible before reporting them.
    let is_entity_terminator = |char| char == ';' || char == ',';
    let is_alias_decl_terminator = |char| char == ';';

    // Loop that parses every alias declaration
    //
    //
    // Alias1 = Foo, Bar;
    // ^^^^^^^^^^^^^^^^^
    //
    // Alias_21 = ..Foo, Baz ..Bar;
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //
    // Another = Clone, ::std::hash::Hash;
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    'parse_alias_declaration: while let Some(tt) = ts.tt() {
        // Alias = ..Foo, Bar, baz::Baz;
        // ^^^^^
        let TokenTree::Ident(alias_name) = tt else {
            compile_errors.push(CompileError::new(
                tt.span(),
                "expected alias name (identifier)",
            ));
            ts.eat_until_char(is_alias_decl_terminator);
            continue 'parse_alias_declaration;
        };

        // Alias = ..Foo, Bar, baz::Baz;
        //       ^
        if ts.char('=').is_none() {
            compile_errors.push(ts.compile_error("expected `=`"));

            // Do this as our goal is to recover as much as possible. When rust-analyzer gets more
            // things to work with, DX is better
            nested_alias_map.insert(alias_name.to_string(), (alias_name.span(), Vec::new()));

            ts.eat_until_char(is_alias_decl_terminator);
            continue 'parse_alias_declaration;
        };

        // List of items, each one is either:
        //
        // - Aliases: expnads to a bunch of Aliases or Derives
        //
        // - Derive
        let mut entities = Vec::new();

        // Loop that parses the entire RHS
        //
        // Alias = ..Foo, Bar, baz::Baz;
        //         ^^^^^
        //                ^^^
        //                     ^^^^^^^^
        'parse_entity: loop {
            match ts.tt() {
                // Parsing Alias expansion
                //
                // Alias = std::hash::Hash, ..Foo;
                //                          ^^^^^
                Some(TokenTree::Punct(punct)) if punct == '.' => {
                    if ts.char('.').is_none() {
                        compile_errors
                            .push(ts.compile_error("expected `.` after `.` to form `..Alias`"));
                        ts.eat_until_char(is_entity_terminator);
                        continue 'parse_entity;
                    }

                    let Some(alias) = ts.ident() else {
                        compile_errors.push(
                            ts.compile_error("expected identifier after `..` to form `..Alias`"),
                        );
                        ts.eat_until_char(is_entity_terminator);
                        continue 'parse_entity;
                    };

                    entities.push(Entity::Alias(alias));

                    match ts.tt() {
                        Some(TokenTree::Punct(punct)) if punct == ';' => {
                            // reached end of current alias declaration
                            //
                            // Alias = std::hash::Hash, ..Foo;
                            //                               ^
                            break 'parse_entity;
                        }
                        Some(TokenTree::Punct(punct)) if punct == ',' => {
                            // parse next entity
                            //
                            // Alias = ..Foo, ::std::hash::Hash;
                            //              ^
                            continue 'parse_entity;
                        }
                        _ => {
                            compile_errors.push(ts.compile_error("expected `;`, or `,`"));
                            ts.eat_until_char(is_entity_terminator);
                            continue 'parse_entity;
                        }
                    }
                }
                // Parsing relative path to a derive
                //
                // Alias = std::hash::Hash, ..Foo;
                //         ^^^
                //
                // But this is disallowed to avoid surprises!
                Some(TokenTree::Ident(_)) => {
                    compile_errors.push(ts.compile_error(concat!(
                        "to avoid surprises, path to derive in alias ",
                        "definition must be absolute - meaning it must start with `::`",
                        "\n\nfor example, use `::std::hash::Hash` instead ",
                        "of `std::hash::Hash` ",
                        "and use `::core::marker::Copy` instead",
                        " of just `Copy`",
                    )));
                    ts.eat_until_char(is_entity_terminator);
                    continue 'parse_entity;
                }
                // Parsing absolute path to a derive
                //
                // Alias = ::std::hash::Hash, ..Foo;
                //         ^^^^^^^^^^^^^^^^^
                Some(TokenTree::Punct(colon)) if colon == ':' => {
                    // ::std::hash::Hash
                    //  ^
                    let Some(colon_colon) = ts.char(':') else {
                        compile_errors.push(
                            ts.compile_error(
                                "expected `:` to form a path like `::std::hash::Hash`",
                            ),
                        );
                        ts.eat_until_char(is_entity_terminator);
                        continue 'parse_entity;
                    };

                    // ::std::hash::Hash
                    //   ^^^^^^^^^^^^^^^
                    let mut path = match ts.path() {
                        Ok(path) => path,
                        Err(err) => {
                            compile_errors.push(err);
                            ts.eat_until_char(is_entity_terminator);
                            continue 'parse_entity;
                        }
                    };

                    // ::std::hash::Hash
                    // ^^
                    path.leading_colon = Some(PathSeparator {
                        first: colon.span(),
                        second: colon_colon.span(),
                    });

                    // Expect terminator.
                    //
                    // ::std::hash::Hash,
                    //                  ^
                    match ts.tt() {
                        Some(TokenTree::Punct(comma)) if comma == ',' => {
                            entities.push(Entity::Derive(path));

                            // There is an entity after this one
                            //
                            // ::std::hash::Hash, ..Alias
                            //                  ^
                            continue 'parse_entity;
                        }
                        Some(TokenTree::Punct(semi)) if semi == ';' => {
                            entities.push(Entity::Derive(path));

                            // This is the last entity in this alias declaration
                            //
                            // ::std::hash::Hash;
                            //                  ^
                            //
                            // We'll add all collected entities and parse the next alias declaration
                            break 'parse_entity;
                        }
                        _ => {
                            compile_errors.push(ts.compile_error("expected `;` or `,`"));
                            ts.eat_until_char(is_entity_terminator);
                            continue 'parse_entity;
                        }
                    }
                }
                _ => {
                    compile_errors.push(ts.compile_error("expected absolute path like `::std::hash::Hash`, alias like `..Alias` or `;` signifying end of alias declaration"));
                    ts.eat_until_char(is_alias_decl_terminator);
                    continue 'parse_alias_declaration;
                }
            }
        }

        nested_alias_map.insert(alias_name.to_string(), (alias_name.span(), entities));
    }

    nested_alias_map
}

fn resolve_alias(
    nested_alias_map: &HashMap<String, (Span, Vec<Entity>)>,
    entities: &[Entity],
    derives: &mut HashSet<Path>,
    dummy_use_statements: &mut TokenStream,
    extern_aliases: &mut Vec<Ident>,
) {
    for entity in entities {
        match entity {
            Entity::Alias(ident) => {
                // Maybe this alias has been created inside of this `define!` call (it is NOT an extern alias)
                let Some((_span, entities)) = nested_alias_map.get(&ident.to_string()) else {
                    // THIS macro invocation did not define this alias,
                    // but another one could have. Or it could have been imported from another
                    // module/crate
                    //
                    extern_aliases.push(ident.clone());
                    continue;
                };

                // IMPORTANT: This must only exist if it is NOT an extern alias, so do NOT
                // move it before the `let...else` above
                //
                // use crate::derive_alias::Alias as _;
                dummy_use_statements.extend([
                    TokenTree::Ident(Ident::new("use", Span::call_site())),
                    TokenTree::Ident(Ident::new("crate", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Ident(ident.clone()),
                    TokenTree::Ident(Ident::new("as", Span::call_site())),
                    TokenTree::Ident(Ident::new("_", Span::call_site())),
                    TokenTree::Punct(Punct::new(';', Spacing::Joint)),
                ]);

                // The entity is an alias, which expands to more entities.
                // Resolve the alias by getting the path to its derives recursively
                resolve_alias(
                    nested_alias_map,
                    entities,
                    derives,
                    dummy_use_statements,
                    extern_aliases,
                );
            }
            // A "leaf": No more expansion, end of recursion
            Entity::Derive(path) => {
                derives.insert(path.clone());
            }
        }
    }
}

#[cfg_attr(
    not(doc),
    doc = "\
Like [`#[std::derive]`](https://doc.rust-lang.org/reference/attributes/derive.html), but with support for derive aliases generated by [`#[define]`](define)

```rust
mod derive_alias {
    // 1. Define the aliases
    derive_aliases::define! {
        Copy = ::core::marker::Copy, ::core::clone::Clone;
        Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
        Ord = ..Eq, ::core::cmp::Ord, ::core::cmp::PartialOrd;
        StdTraits = ..Copy, ..Ord, ::core::hash::Hash, ::core::fmt::Debug;
    }
}

use derive_aliases::derive;

// 2. Use the aliases
#[derive(..StdTraits)]
struct Example;
# fn main() {}
```

The above expands to:

```
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, ::core::hash::Hash, Debug)]
struct Example;
```

See the [crate-level](https://docs.rs/derive_aliases/latest/derive_aliases) documentation for details"
)]
// NOTE on `#[cfg_attr(not(doc), doc = "...")]`:
//
// Documentation on an `#[doc(inline)] pub use item` concatenates with documentation of `item`
// in the generated HTML documentation. I want users to see documentation on hover and when they goto definition
// they see the actual documentation, but without having duplicate documentation in the HTML documentation
//
// ---
#[proc_macro_attribute]
#[cfg_attr(not(feature = "show"), doc(hidden))]
pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut compile_errors = Vec::new();
    let ExtractedDerives {
        regular_derives,
        derive_aliases,
    } = extract_derives(attr, &mut compile_errors);

    let mut regular_derives: Vec<(TokenStream, Vec<Path>)> =
        vec![(TokenStream::from_iter([cfg_true()]), regular_derives)];

    let mut derive_aliases: Vec<(TokenStream, Ident)> = derive_aliases
        .into_iter()
        .map(|alias| (TokenStream::from_iter([cfg_true()]), alias))
        .collect();

    // This currently holds the entire item.
    //
    // All attributes `#[attr()]` at the start of the stream will be extracted,
    // and the non-processed ones will be placed into `other_attrs`
    //
    // It will be our job to put those attributes back when generating the code, before this stream
    let mut item_tokens = item.into_iter().peekable();

    // Attributes that we don't process, instead, they are kept as-is
    let mut other_attrs = TokenStream::new();

    // Loop through every attribute on the item
    //
    // #[attr1]
    // #[attr2]
    // #[attr3]
    // #[attr4]
    // struct Item;
    //
    // Attributes that are #[derive(..)]-like attributes or #[cfg_attr(predicate, derive(..))]-like will
    // have their contents parsed and the derive aliases / derives will all be handled by a single proc macro call
    //
    // Yes, this means that multiple #[derive_aliases::derive] calls will "merge" into a single one:
    //
    // #[derive(..Eq, Clone)]
    // #[cfg_attr(feature = "serde", deny_unknown_fields)]
    // #[derive(..Ord, Copy)]
    // #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
    // struct Item;
    //
    // All of those attributes will actually act as if the user wrote this instead:
    //
    // #[derive(..Eq, Clone, ..Ord, Copy)]
    // #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
    // #[cfg_attr(feature = "serde", deny_unknown_fields)]
    // struct Item;
    //
    // Notably:
    //
    // - What the user wrote should fail compilation. The `serde` helper attribute is only available
    //   *after* the derive macro, but the user wrote them in reverse order. Because of our macro, it will compile
    // - The order in which the aliases/derives are actually evaluated is completely unspecified
    //
    // Why do this?
    //
    // Because we're mimicking what the `core::derive` macro actually does, we need to consider helper attributes.
    // Derive helper attributes work via name resolution. But in a macro context, we don't have access to the name resolution,
    // so we must try best-effort.
    //
    // Earlier, we would have each `derive_aliases::derive` attribute handle its own logic and wrapping.
    // This didn't work because there were issues with helper attribute namespace being invalidated after the first
    // attribute was expanded, so instead all work is done by a single attribute.
    //
    // This does mean that the user can't actually use a different name for the `derive` attribute,
    // only a hard-coded set of paths is read from.

    loop {
        match item_tokens
            .peek()
            .expect("we only parse attrs, not actual item")
        {
            // An attribute
            TokenTree::Punct(punct) if *punct == '#' => {
                // #[attr]
                // ^
                let attr_hash = item_tokens.next().unwrap();
                // #[attr(ibute)]
                //   ^^^^^^^^^^^
                let Some(TokenTree::Group(attr_brackets)) = item_tokens.next() else {
                    unreachable!()
                };
                let mut attr_stream = TokensIter {
                    stream: attr_brackets.stream().into_iter().peekable(),
                    span: Span::call_site(),
                };

                // #[attr(ibute)]
                //   ^^^^
                let attr_path = attr_stream.path().expect("attribute always has a path");

                // NOTE: used in pattern position
                macro_rules! is_derive {
                    () => {
                        "derive" | "derive_aliases::derive" | "::derive_aliases::derive"
                    };
                }

                // The step where, if the the attribute is #[derive(..)] or #[cfg_attr(.., derive(..))] - the contents are parsed
                // If it is not, the attribute is re-emitted
                match attr_path.to_string().as_str() {
                    // A #[cfg_attr(predicate, derive(..))] attribute
                    "cfg_attr" => {
                        // #[cfg_attr(predicate, derive(..))]
                        //            ^^^^^^^^^^^^^^^^^^^^^
                        let Some(cfg_attr_stream) = attr_stream.group(Delimiter::Parenthesis)
                        else {
                            // No error on purpose
                            continue;
                        };
                        let mut cfg_attr_stream = TokensIter {
                            stream: cfg_attr_stream.into_iter().peekable(),
                            span: Span::call_site(),
                        };

                        let mut cfg_predicate = TokenStream::new();

                        // #[cfg_attr(predicate, derive(A, B))]
                        //            ^^^^^^^^^
                        loop {
                            match cfg_attr_stream.tt() {
                                // #[cfg_attr(predicate, derive(A, B))]
                                //                     ^
                                Some(TokenTree::Punct(p)) if p == ',' => break,
                                Some(tt) => {
                                    // A single token of the cfg predicate, for example:
                                    //
                                    // #[cfg_attr(any(), derive(A, B))]
                                    //               ^^
                                    cfg_predicate.extend([tt]);
                                }
                                None => unreachable!(),
                            }
                        }

                        // #[cfg_attr(predicate, derive(..))]
                        //                       ^^^^^^
                        let cfg_attr_inner_path = cfg_attr_stream
                            .path()
                            .expect("attributes always start with a path");

                        match cfg_attr_inner_path.to_string().as_str() {
                            // This is definitely like #[cfg_attr(predicate, derive(..))]
                            is_derive!() => {
                                let Some(derive_stream) = attr_stream.group(Delimiter::Parenthesis)
                                else {
                                    // No error on purpose.
                                    continue;
                                };
                                let derives = extract_derives(derive_stream, &mut compile_errors);
                                regular_derives
                                    .push((cfg_predicate.clone(), derives.regular_derives));
                                for alias in derives.derive_aliases {
                                    derive_aliases.push((cfg_predicate.clone(), alias));
                                }
                            }
                            // The attribute inside of #[cfg_attr] is different., e.g. #[cfg_attr(predicate, doc(hidden))]
                            _ => {
                                // re-construct the original attribute
                                //
                                // #[cfg_attr(predicate, doc(hidden))]
                                //            ^^^^^^^^^^^^^^^^^^^^^^^
                                let cfg_attr_inner = TokenStream::from_iter(
                                    // #[cfg_attr(predicate, doc(hidden))]
                                    //            ^^^^^^^^^
                                    cfg_predicate
                                        .into_iter()
                                        // #[cfg_attr(predicate, doc(hidden))]
                                        //                     ^
                                        .chain([TokenTree::Punct(Punct::new(',', Spacing::Joint))])
                                        // #[cfg_attr(predicate, doc(hidden))]
                                        //                       ^^^
                                        .chain(cfg_attr_inner_path.into_tokens())
                                        // #[cfg_attr(predicate, doc(hidden))]
                                        //                          ^^^^^^^^
                                        .chain(cfg_attr_stream.stream),
                                );

                                // #[cfg_attr(predicate, doc(hidden))]
                                //   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                let attr_inner = TokenStream::from_iter(
                                    attr_path.into_tokens().chain(cfg_attr_inner),
                                );
                                // #[cfg_attr(predicate, doc(hidden))]
                                //  ^                                ^
                                let mut group = Group::new(Delimiter::Bracket, attr_inner);
                                group.set_span(attr_brackets.span());

                                // Add back the entire attribute
                                other_attrs.extend([attr_hash, TokenTree::Group(group)]);
                                continue;
                            }
                        }
                    }
                    // A #[derive(..)] attribute
                    is_derive!() => {
                        let Some(derive_stream) = attr_stream.group(Delimiter::Parenthesis) else {
                            // No error on purpose.
                            continue;
                        };
                        let derives = extract_derives(derive_stream, &mut compile_errors);

                        let cfg_predicate = TokenStream::from_iter([cfg_true()]);
                        regular_derives.push((cfg_predicate.clone(), derives.regular_derives));
                        for alias in derives.derive_aliases {
                            derive_aliases.push((cfg_predicate.clone(), alias));
                        }
                    }
                    // This is not a #[derive(..)], nor is it a #[cfg_attr(predicate, derive(..))]
                    //
                    // Restore the original attribute that the user wrote
                    _ => {
                        // Re-construct the original attribute contents: [...]
                        let mut group = Group::new(
                            Delimiter::Bracket,
                            TokenStream::from_iter(
                                attr_path.into_tokens().chain(attr_stream.stream),
                            ),
                        );
                        group.set_span(attr_brackets.span());

                        other_attrs.extend([attr_hash, TokenTree::Group(group)]);
                        continue;
                    }
                }
            }
            // The item has been reached
            //
            // #[attrs]
            // struct Item;
            // ^^^^^^
            _ => break,
        }
    }

    // all the tokens for "compile_error!(...)" invocations, to be inserted
    // alongside all other input
    let compile_errors = compile_errors
        .into_iter()
        .flat_map(|compile_error| compile_error.into_tokens());

    // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])) [] }
    //
    // We treat the last alias specially
    if let Some((first_alias_cfg, first_alias)) = derive_aliases.pop() {
        // Every regular derive and its `cfg` value, which is just for now will be
        // always `true`, so basically #[cfg_attr(true, DERIVE)]
        //
        // [{ true } ::core::marker::Copy] [{ true } ::core::clone::Clone]
        let regular_derives = regular_derives.into_iter().flat_map(|(cfg, derives)| {
            derives.into_iter().map(move |derive| {
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter(cfg.clone().into_iter().chain(derive.into_tokens())),
                ))
            })
        });

        let innermost_ts = TokenStream::from_iter([
            TokenTree::Punct(Punct::new('@', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Bracket, regular_derives.collect())),
            TokenTree::Group(Group::new(Delimiter::Bracket, item_tokens.collect())),
        ]);

        // Build up nesting
        //
        // @ [Debug,] [struct Foo;]
        // crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])
        // crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;]))
        let inner =
            derive_aliases
                .into_iter()
                .fold(innermost_ts, |acc, (current_cfg, current_alias)| {
                    TokenStream::from_iter([
                        TokenTree::Ident(Ident::new("crate", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(current_alias),
                        TokenTree::Punct(Punct::new(',', Spacing::Joint)),
                        TokenTree::Group(Group::new(Delimiter::Parenthesis, acc)),
                    ])
                });

        // Wrap in a final invocation
        //
        // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])) [] }
        let stream = TokenStream::from_iter(inner.into_iter().chain([TokenTree::Group(
            Group::new(Delimiter::Bracket, TokenStream::new()),
        )]));

        TokenStream::from_iter(
            [
                TokenTree::Ident(Ident::new("crate", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(first_alias),
                TokenTree::Punct(Punct::new('!', Spacing::Joint)),
                TokenTree::Group(Group::new(Delimiter::Brace, stream)),
            ]
            .into_iter()
            .chain(compile_errors),
        )
    } else {
        // No derive aliases used.
        // Just pass all derives to the standard library's

        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
        let derive_macro_input = regular_derives.into_iter().flat_map(|(cfg, derives)| {
            derives.into_iter().flat_map(move |derive| {
                [
                    // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                    // ^
                    TokenTree::Punct(Punct::new('#', Spacing::Joint)),
                    // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                    //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Bracket,
                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                        //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        TokenStream::from_iter([
                            // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                            //   ^^^^^^^^
                            TokenTree::Ident(Ident::new("cfg_attr", Span::call_site())),
                            // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                            //           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            TokenTree::Group(Group::new(
                                Delimiter::Parenthesis,
                                TokenStream::from_iter(
                                    // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                    //            ^^^^
                                    cfg.clone().into_iter().chain([
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                ^
                                        TokenTree::Punct(Punct::new(',', Spacing::Alone)),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                  ^^
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                    ^^^^
                                        TokenTree::Ident(Ident::new("core", Span::call_site())),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                        ^^
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                          ^^^^^^^
                                        TokenTree::Ident(Ident::new("prelude", Span::call_site())),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                                 ^^
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                                   ^^
                                        TokenTree::Ident(Ident::new("v1", Span::call_site())),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                                     ^^
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                                       ^^^^^^
                                        TokenTree::Ident(Ident::new("derive", Span::call_site())),
                                        // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                        //                                             ^^^^^^^
                                        TokenTree::Group(Group::new(
                                            Delimiter::Parenthesis,
                                            // #[cfg_attr(true, ::core::prelude::v1::derive(Trait))]
                                            //                                              ^^^^^
                                            derive.into_tokens().collect(),
                                        )),
                                    ]),
                                ),
                            )),
                        ]),
                    )),
                ]
            })
        });

        TokenStream::from_iter(derive_macro_input.chain(item_tokens).chain(compile_errors))
    }
}

/// The extracted derive data from a `derive` invocation that
/// permits derive aliases
///
/// Example:
///
/// ```ignore
/// #[derive(..Eq, Serialize, ..Clone, Deserialize)]
/// ```
struct ExtractedDerives {
    /// This is just the raw TokenStream passed directly to `#[std::derive(..)]`
    ///
    /// For that Example, this is:
    ///
    /// ```ignore
    /// [parse_quote!(Serialize), parse_quote!(Deserialize)]
    /// ```
    regular_derives: Vec<Path>,
    /// A list of derive aliases, which we will expand at `crate::derive_alias::{alias}`
    ///
    /// For that Example, this is:
    ///
    /// ```ignore
    /// [parse_quote!(Eq), parse_quote!(Clone)]
    /// ```
    derive_aliases: Vec<Ident>,
}

/// Extracts derives and derive aliases from a #[derive] attribute.
///
/// Given:
///
/// ```ignore
/// #[derive(..Copy, std::hash::Hash, ..StdTraits, Clone)]
/// ```
///
/// Extracts all the "regular derives" (derives that are not aliases),
/// and also all the "derive aliases" (derives with `..Alias` syntax) into the
/// second field of tuple
///
/// The result for the above will look like this:
///
/// ```ignore
/// (
///     quote!(std::hash::Hash, Clone,),
///     vec![parse_quote!(Copy), parse_quote!(Clone)]
/// )
/// ```
///
/// Notably, each regular derive will always be followed by a comma,
/// even if that comma wasn't in the original input
fn extract_derives(attr: TokenStream, compile_errors: &mut Vec<CompileError>) -> ExtractedDerives {
    // Contains both regular derives and derive aliases
    //
    // ..Alias, Derive1, ..Alias2, std::Derive2
    let mut attr = TokensIter {
        stream: attr.into_iter().peekable(),
        span: Span::call_site(),
    };

    let mut regular_derives = Vec::new();
    let mut derive_aliases = Vec::new();

    while let Some(tt) = attr.peek_tt() {
        if matches!(tt, TokenTree::Punct(dot) if *dot == '.') {
            attr.tt();

            if attr.char('.').is_none() {
                compile_errors.push(attr.compile_error("expected `..Alias`"));
                break;
            }

            let Some(alias) = attr.ident() else {
                compile_errors.push(attr.compile_error("expected `..Alias`"));
                break;
            };

            derive_aliases.push(alias);

            match attr.peek_tt() {
                // Comma after alias
                //
                // #[derive(..Copy, std::hash::Hash, ..StdTraits,)]
                //                ^                             ^
                Some(TokenTree::Punct(punct)) if *punct == ',' => {
                    attr.tt();
                }
                // Unexpected token
                Some(_) => {
                    compile_errors.push(attr.compile_error("expected `,` or end of input"));
                    break;
                }
                // end of input, no more aliases or derives
                //
                // #[derive(..Copy, std::hash::Hash, ..StdTraits)]
                //                                              ^
                None => (),
            }
        } else {
            // part of a derive path,
            //
            // #[derive(..Copy, std::hash::Hash, ..StdTraits)]
            //                  ^^^^^^^^^^^^^^^

            let path = match attr.path() {
                Ok(path) => path,
                Err(err) => {
                    compile_errors.push(err);
                    continue;
                }
            };

            regular_derives.push(path);

            match attr.tt() {
                Some(TokenTree::Punct(punct)) if punct == ',' => {
                    // A comma in the derive input
                }
                Some(_) => {
                    compile_errors.push(attr.compile_error("expected `,` or end of input"));
                    continue;
                }
                None => {
                    // End of derive input
                }
            }
        }
    }

    ExtractedDerives {
        regular_derives,
        derive_aliases,
    }
}

/// A single entity that appears on the RHS of an alias declaration
#[core::prelude::v1::derive(Clone, Debug)]
enum Entity {
    /// An alias, which will be expanded into more `Aliased`s
    ///
    /// ```txt
    /// MyAlias = ..AnotherAlias, Foo;
    ///             ^^^^^^^^^^^^
    /// ```
    Alias(Ident),
    /// A path to a derive
    ///
    /// ```txt
    /// MyAlias = ..AnotherAlias, std::hash::Hash;
    ///                           ^^^^^^^^^^^^^^^
    /// ```
    Derive(Path),
}

/// The macro **created** by `new_alias!` handles de-duplication just fine using a TT muncher. But the derives passed to `new_alias!` **must not**
/// have any duplicates in them. This can happen if the alias refers to an "extern alias" (alias defined outside of this `define!` call), like here:
///
/// ```ignore
/// derive_aliases::define! {
///     Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
/// }
/// derive_aliases::define! {
///     EqEq = ..Eq, ..Eq;
/// }
/// ```
///
/// The above generates this, which contains duplicated derive paths:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias! {
///     "..." a $ EqEq! [ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],[ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],
/// }
/// ```
///
/// So instead of that, we nest the arguments inside of a call to this macro, which de-duplicates the derives we pass to `new_alias!`:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias_with_externs! {
///     a $ EqEq!
///     [ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],[ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],
/// }
/// ```
///
/// This expands to this:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias! {
///     "..." a $ EqEq! [ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],
/// }
/// ```
///
/// Originally, this macro was just a TT muncher. However, to accurately generate the documentation `"..."` I made it
/// into a proc macro, so it also generates the `"..."` based off the aliases it contains
#[doc(hidden)]
#[proc_macro]
pub fn __internal_derive_aliases_new_alias_with_externs(ts: TokenStream) -> TokenStream {
    let mut ts = ts.into_iter().peekable();

    // a
    let visibility_mode = match ts.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => unreachable!("first token is the visibility mode of the `macro_rules!` alias"),
    };

    // $
    let dollar = match ts.next() {
        Some(TokenTree::Punct(punct)) if punct == '$' => punct,
        _ => unreachable!("second token is dollar for generating macro from macro"),
    };

    // EqEq
    let alias = match ts.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => unreachable!("third token is name of the actual alias"),
    };

    // !
    let exclam = match ts.next() {
        Some(TokenTree::Punct(punct)) if punct == '!' => punct,
        _ => unreachable!("fourth token is the `!`"),
    };

    // [ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],[ { true } ::core::cmp::Eq],[ { true } ::core::cmp::PartialEq],
    let mut paths = HashSet::new();

    // consume `[$($path:tt)*],` until there are none left
    while let Some(tt) = ts.next() {
        let mut group = match tt {
            TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket => TokensIter {
                stream: group.stream().into_iter().peekable(),
                span: group.span(),
            },
            _ => unreachable!("every path is enclosed inside of `[...]`"),
        };

        // ignore the `cfg` for now
        let _ = group
            .group(Delimiter::Brace)
            .expect("first token is always `{ ... }`, containing the `cfg`");

        paths.insert(
            group
                .path()
                .expect("inside of `[...]` has an absolute path"),
        );

        match ts.next() {
            Some(TokenTree::Punct(punct)) if punct == ',' => (),
            _ => unreachable!("after the path `[...]` we always have a comma `,`"),
        }
    }

    // the inner stream passed to the actual `new_alias!`
    // with these changes:
    // - de-duplicated alias
    // - contains the real documentation
    let stream = TokenStream::from_iter(
        // lets re-construct the actual path
        [
            TokenTree::Literal(Literal::string(&generate_documentation_for_alias(
                &alias.to_string(),
                &paths,
            ))),
            TokenTree::Ident(visibility_mode),
            TokenTree::Punct(dollar),
            TokenTree::Ident(alias),
            TokenTree::Punct(exclam),
        ]
        .into_iter()
        // now at the end we just add all of the tokens
        .chain(paths.into_iter().flat_map(|path| {
            // every path and its comma
            [
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter([cfg_true()].into_iter().chain(path.into_tokens())),
                )),
                TokenTree::Punct(Punct::new(',', Spacing::Joint)),
            ]
        })),
    );

    TokenStream::from_iter([
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Ident(Ident::new(
            "__internal_derive_aliases_new_alias",
            Span::call_site(),
        )),
        TokenTree::Punct(Punct::new('!', Spacing::Joint)),
        TokenTree::Group(Group::new(Delimiter::Brace, stream)),
    ])
}

/// Given an `alias` and its `expansion` (all derives it expands to, resolved recursively),
/// generates documentation about the alias to put in a `///` comment
fn generate_documentation_for_alias(alias: &str, expansion: &HashSet<Path>) -> String {
    const PRELUDE_PATHS: &[&str] = &[
        "core::marker::Copy",
        "core::clone::Clone",
        "core::fmt::Debug",
        "core::default::Default",
        "core::cmp::PartialEq",
        "core::cmp::Eq",
        "core::cmp::PartialOrd",
        "core::cmp::Ord",
    ];

    // We want to group each path by where it is located,
    // if we import a bunch of derives from `num_traits` then we want to group those
    // into a single `use`
    //
    // To do this we hash the path to the parent module/crate that contains the alias
    let mut grouped_by_parent = HashMap::new();

    let mut derive_contents = BTreeSet::new();

    for path in expansion {
        let mut components = path.components.clone();
        let derive = components
            .pop()
            .expect("Derive cannot be located at crate root")
            .1
            .to_string();

        let first_component = path.first_component.to_string();
        let first_component = if first_component == "std" {
            "core".to_string()
        } else {
            first_component
        };

        let path = Path {
            leading_colon: None,
            first_component: Ident::new(&first_component.to_string(), Span::call_site()),
            components,
        };

        let path_string = path.to_string();
        if PRELUDE_PATHS.contains(&format!("{path_string}::{derive}").as_str()) {
            // insert derive directly, without the `use`
            derive_contents.insert(derive);
            continue;
        }

        grouped_by_parent
            .entry(path)
            .or_insert_with(Vec::new)
            .push(derive);
    }

    derive_contents.extend(grouped_by_parent.values().flatten().cloned());

    let uses = grouped_by_parent
        .into_iter()
        .map(|(key, uses)| match &uses[..] {
            [single] => {
                format!("use {key}::{single}")
            }
            [..] => {
                if uses.len() >= 10 {
                    let content = uses.into_iter().collect::<Vec<_>>();
                    // format long list of derives in a more nice manner
                    let derives = content
                        .chunks(5)
                        .map(|derives| format!("    {},\n", derives.join(", ")))
                        .collect::<String>();
                    // remove trailing comma
                    if let Some(uses) = derives.strip_suffix(",\n") {
                        format!("\nuse {key}::{{{uses}}}\n")
                    } else {
                        format!("\nuse {key}::{{{derives}}}")
                    }
                } else {
                    format!("use {key}::{{{}}}", uses.join(", "))
                }
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    let derives = if derive_contents.len() >= 10 {
        let content = derive_contents.into_iter().collect::<Vec<_>>();
        // format long list of derives in a more nice manner
        let derives = content
            .chunks(5)
            .map(|derives| format!("    {},\n", derives.join(", ")))
            .collect::<String>();
        // remove trailing comma
        if let Some(derives) = derives.strip_suffix(",\n") {
            format!("\n{derives}\n")
        } else {
            format!("\n{derives}")
        }
    } else {
        derive_contents.into_iter().collect::<Vec<_>>().join(", ")
    };

    format!(
        "\
Derive alias `..{alias}` can be used like this:

```ignore
#[derive(..{alias})]
struct Example;
```

Which expands to the following:

```ignore
#[derive({derives})]
struct Example;{}{uses}
```",
        if uses.is_empty() { "" } else { "\n\n" },
    )
}
