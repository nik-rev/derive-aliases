//! Implementation detail of the `derive_aliases` crate

use crate::tokens::IntoTokens;
use crate::tokens::PathSeparator;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use proc_macro::{Literal, Spacing};
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};
use tokens::Path;
use tokens::Tokens;
use tokens::TokensIter;

mod tokens;

use tokens::CompileError;

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
//
// when doing `#[macro_use] extern crate proc` we will also globally import this macro
// and it will be suggested by rust_analyzer. but since this macro must be called just once,
// we don't want rust_analyzer to suggest it
#[cfg_attr(not(doc), doc(hidden))]
#[cfg_attr(not(feature = "show"), doc(hidden))]
#[allow(unused_assignments)]
#[proc_macro]
pub fn define(tts: TokenStream) -> TokenStream {
    // First, let's create a Nested Alias Map:
    //
    // Alias => Derive OR Alias
    //
    // It's nested because each alias expands to a bunch of aliases+derives,
    // but we want to "normalize" it, so each alias expands to a bunch of derives.
    // We'll do it later
    let mut nested_alias_map = HashMap::new();

    // Whole input to the macro
    let mut ts = TokensIter {
        stream: tts.into_iter().peekable(),
        span: Span::call_site(),
    };

    // Compile errors to report all at once
    let mut compile_errors = Vec::new();

    // We parse each alias declaration token-by-token. If the current alias
    // declaration has a syntax error we'll just report it and skip parsing the current alias.
    // This allows us to collect as many errors as possible before reporting them.
    macro_rules! skip_current_alias_declaration {
        () => {
            loop {
                match ts.tt() {
                    // reached end of the alias declaration
                    Some(TokenTree::Punct(punct)) if punct == ';' => break,
                    // reached end of the input
                    None => break,
                    // eat everything until the next alias declaration,
                    // that way we can report multiple errors
                    _ => (),
                }
            }
        };
    }

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

    // Visibility mode defines who can see the macro. It is a hack that is
    // necessary because there's no way to export macro from the crate without using `#[macro_export]` on it
    let visibility_mode = if export_derive_aliases.is_some() {
        // visibility `a`: can be accessed inside of crate, but not outside
        Ident::new("a", Span::call_site())
    } else {
        // visibility `b`: can be accessed outside of the crate
        Ident::new("b", Span::call_site())
    };

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
            compile_errors.push(CompileError::new(tt.span(), "expected identifier"));
            skip_current_alias_declaration!();
            continue 'parse_alias_declaration;
        };

        // Alias = ..Foo, Bar, baz::Baz;
        //       ^
        if ts.char('=').is_none() {
            skip_current_alias_declaration!();
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
                        skip_current_alias_declaration!();
                    }

                    let Some(alias) = ts.ident() else {
                        compile_errors.push(
                            ts.compile_error("expected identifier after `..` to form `..Alias`"),
                        );
                        skip_current_alias_declaration!();
                        continue 'parse_alias_declaration;
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
                            compile_errors.push(ts.compile_error(
                                "expected `;`, or `,` followed by a path like `::std::hash::Hash` or `..Alias`",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
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
                    skip_current_alias_declaration!();
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
                        skip_current_alias_declaration!();
                        continue 'parse_alias_declaration;
                    };

                    // ::std::hash::Hash
                    //   ^^^^^^^^^^^^^^^
                    let mut path = match ts.path() {
                        Ok(path) => path,
                        Err(err) => {
                            compile_errors.push(err);
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
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
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                    }
                }
                _ => {
                    compile_errors.push(ts.compile_error("expected absolute path like `::std::hash::Hash`, alias like `..Alias` or `;` signifying end of alias declaration"));
                    skip_current_alias_declaration!();
                    continue 'parse_alias_declaration;
                }
            }
        }

        nested_alias_map.insert(alias_name.to_string(), (alias_name.span(), entities));
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

    // This is a map `Alias => Derive`, so no more nested aliases
    let mut flat_alias_map = HashMap::new();

    // All of these are just `use ... as _` the only reason we have them
    // is to get access to the `Span` of whatever item they import, which we'll
    // use in documentation to get nice docs-on-hover and goto-definition
    let mut dummy_use_statements = TokenStream::new();

    // Build up the `flat_alias_map`
    for (alias_name, (alias_name_span, entities)) in &nested_alias_map {
        // A flat list of derives that the alias expands to
        let mut flat_derives = HashSet::new();

        // "external" aliases that were NOT defined by this macro,
        // we'll just nest the aliases then call the `__internal_derive_aliases_new_alias_with_externs` macro to de-duplicate
        // to de-duplicate
        let mut extern_aliases = Vec::new();

        // use crate::derive_alias::Foo as _;

        // We do this so we get documentation on hover
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
        flat_alias_map.insert(alias_name, (alias_name_span, flat_derives, extern_aliases));
    }

    // Finally let's expand all of this to a bunch of invocations of the `__internal_derive_aliases_new_alias!` macro
    flat_alias_map
        .into_iter()
        .flat_map(|(alias, (_alias_span, derives, mut extern_aliases))| {
            // The Input passed into the `new_alias!`
            //
            // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
            let input_into_new_alias_macro = [
                // Visibility mode `a` or `b`
                //
                // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                // ^
                TokenTree::Ident(visibility_mode.clone()),
                // Dollar token: The macro creates macros, so we can't use '$' in it. This becomes `$_:tt` so
                // we use `$_` inside the macro
                //
                // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //   ^
                TokenTree::Punct(Punct::new('$', proc_macro::Spacing::Alone)),
                // Real name of the alias
                //
                // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //     ^^^
                TokenTree::Ident(Ident::new(alias, Span::call_site())),
                // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //        ^
                TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
            ]
            .into_iter()
            // The input is then following by a bunch of paths to all the derives the macro expands to,
            // wrapped in `[...]`. so like `[::core::hash::Hash]`. That's because we want to compare 2 paths,
            // but `:path` specifiers can't be compared, so we compare `[$($tt:tt)*]` instead
            //
            // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
            //          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            .chain(derives.iter().flat_map(|derive| {
                [
                    // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                    // a        ^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Bracket,
                        // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                        // a         ^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^
                        TokenStream::from_iter(derive.clone().into_tokens()),
                    )),
                    // a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                    // a                            ^                     ^
                    TokenTree::Punct(Punct::new(',', proc_macro::Spacing::Joint)),
                ]
            }));

            // NOTE: Treat the last extern alias specially, because we'll actually invoke it.
            // The nested extern aliases will be invoked by this one, one-after-the-other
            if let Some(last_extern_alias) = extern_aliases.pop() {
                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenStream::from_iter([
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    // ^^^^^
                    TokenTree::Ident(Ident::new("crate", Span::call_site())),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //      ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //        ^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                    ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                      ^^^
                    TokenTree::Ident(last_extern_alias),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                         ^
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                    //                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter([
                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                            //                            ^
                            TokenTree::Punct(Punct::new('%', Spacing::Joint)),
                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                            //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            TokenTree::Group(Group::new(
                                Delimiter::Bracket,
                                extern_aliases.into_iter().fold(
                                    TokenStream::from_iter(input_into_new_alias_macro),
                                    |acc, alias| {
                                        TokenStream::from_iter([
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                               ^^^^^
                                            TokenTree::Ident(Ident::new(
                                                "crate",
                                                Span::call_site(),
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                    ^^
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                      ^^^^^^^^^^^^
                                            TokenTree::Ident(Ident::new(
                                                "derive_alias",
                                                Span::call_site(),
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                  ^^
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                    ^^
                                            TokenTree::Ident(alias),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                      ^
                                            TokenTree::Punct(Punct::new(
                                                ',',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                            TokenTree::Group(Group::new(Delimiter::Bracket, acc)),
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
                // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenStream::from_iter([
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    // ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //   ^^^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                 ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new(
                        "__internal_derive_aliases_new_alias",
                        Span::call_site(),
                    )),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                                                      ^
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter(
                            // The generated documentation. Which we can fully create from what we already know
                            //
                            // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                            //                                                          ^^^^^
                            [TokenTree::Literal(Literal::string(
                                &generate_documentation_for_alias(alias, &derives),
                            ))]
                            .into_iter()
                            // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                            //                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            .chain(input_into_new_alias_macro),
                        ),
                    )),
                ])
            }
        })
        // Compile errors, which we report al at once
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
            // instead of having to put it on every import. We don't have `mod _` which would be more semantically correct,
            // so this is the next-best-thing
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
    // Contains both regular derives and derive aliases
    //
    // ..Alias, Derive1, ..Alias2, std::Derive2
    let mut attr = TokensIter {
        stream: attr.into_iter().peekable(),
        span: Span::call_site(),
    };

    // This is just the raw TokenStream passed directly to `#[std::derive(..)]`
    let mut regular_derives = Tokens(TokenStream::new());

    let mut compile_errors = Vec::new();

    // A list of derive aliases, which we will expand at `crate::derive_alias::{alias}`
    let mut aliases = Vec::new();

    // panic!("{attr}");

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

            aliases.push(alias);

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
                    regular_derives.push(punct);
                }
                Some(_) => {
                    compile_errors.push(attr.compile_error("expected `,` or end of input"));
                    continue;
                }
                None => {
                    regular_derives.push(Punct::new(',', Spacing::Joint));
                }
            }
        }
    }

    let _ = attr;

    let regular_derives = regular_derives;

    // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])) [] }
    //
    // We treat the last alias specially
    if let Some(first_alias) = aliases.pop() {
        let innermost_ts = TokenStream::from_iter([
            TokenTree::Punct(Punct::new('@', Spacing::Joint)),
            TokenTree::Group(regular_derives.inside_of(Delimiter::Bracket)),
            TokenTree::Group(Group::new(Delimiter::Bracket, item)),
        ]);

        // Build up nesting
        //
        // @ [Debug,] [struct Foo;]
        // crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])
        // crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;]))
        let inner = aliases.into_iter().fold(innermost_ts, |acc, current| {
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new("crate", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                TokenTree::Ident(current),
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

        TokenStream::from_iter([
            TokenTree::Ident(Ident::new("crate", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Ident(first_alias),
            TokenTree::Punct(Punct::new('!', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Brace, stream)),
        ])
    } else {
        // No derive aliases used.
        // Just pass all derives to the standard library's
        TokenStream::from_iter(
            [
                TokenTree::Punct(Punct::new('#', Spacing::Joint)),
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter([
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(Ident::new("core", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(Ident::new("prelude", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(Ident::new("v1", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                        TokenTree::Ident(Ident::new("derive", Span::call_site())),
                        TokenTree::Group(regular_derives.inside_of(Delimiter::Parenthesis)),
                    ]),
                )),
            ]
            .into_iter()
            .chain(item),
        )
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
///     "..." a $ EqEq! [::core::cmp::Eq],[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::PartialEq],
/// }
/// ```
///
/// So instead of that, we nest the arguments inside of a call to this macro, which de-duplicates the derives we pass to `new_alias!`:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias_with_externs! {
///     a $ EqEq!
///     [::core::cmp::Eq],[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::PartialEq],
/// }
/// ```
///
/// This expands to this:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias! {
///     "..." a $ EqEq! [::core::cmp::Eq],[::core::cmp::PartialEq],
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

    // [::core::cmp::Eq],[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::PartialEq],
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
                    TokenStream::from_iter(path.into_tokens()),
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
