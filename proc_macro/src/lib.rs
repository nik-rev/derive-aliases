//! Implementation detail of the `derive_aliases` crate

use core::fmt;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use proc_macro::{Literal, Spacing};
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

#[cfg_attr(
    not(doc),
    doc = "\
Define derive aliases that can be used in [`#[derive]`](macro@derive)

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
    let mut ts = tts.into_iter().peekable();

    // Compile errors to report all at once
    let mut compile_errors = TokenStream::new();

    // Span of the last token, for better error messages
    let mut last_span = Span::call_site();

    /// Parses a single `$punct` character
    macro_rules! expect_punct {
        ($punct:literal) => {
            match ts.next() {
                Some(TokenTree::Punct(punct)) if punct == $punct => {
                    last_span = punct.span();
                    Some(punct)
                }
                Some(tt) => {
                    compile_errors.extend(CompileError::new(
                        tt.span(),
                        concat!("unexpected token; expected `", $punct, "`"),
                    ));
                    skip_current_alias_declaration!();
                    None
                }
                None => {
                    compile_errors.extend(CompileError::new(
                        last_span,
                        concat!("expected `", $punct, "` after this"),
                    ));
                    skip_current_alias_declaration!();
                    None
                }
            }
        };
    }

    // // eat everything until the next alias declaration,
    // // that way we can report multiple errors
    // Some(TokenTree::Punct(punct)) if punct == ';' => break,
    // // reached end of the input
    // None => break,
    // _ => ()

    // We parse each alias declaration token-by-token. If the current alias
    // declaration has a syntax error we'll just report it and skip parsing the current alias.
    // This allows us to collect as many errors as possible before reporting them.
    macro_rules! skip_current_alias_declaration {
        () => {
            loop {
                match ts.next() {
                    // eat everything until the next alias declaration,
                    // that way we can report multiple errors
                    Some(TokenTree::Punct(punct)) if punct == ';' => break,
                    // reached end of the input
                    None => break,
                    _ => (),
                }
            }
        };
    }

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
    'parse_alias_declaration: while let Some(tt) = ts.next() {
        // Alias = ..Foo, Bar, baz::Baz;
        // ^^^^^
        let TokenTree::Ident(alias_name) = tt else {
            compile_errors.extend(CompileError::new(tt.span(), "expected identifier"));

            skip_current_alias_declaration!();
            continue;
        };

        // Alias = ..Foo, Bar, baz::Baz;
        //       ^
        let Some(eq) = expect_punct!('=') else {
            skip_current_alias_declaration!();
            continue;
        };

        let mut entities = Vec::new();

        // Loop that parses the entire RHS
        //
        // Alias = ..Foo, Bar, baz::Baz;
        //         ^^^^^
        //                ^^^
        //                     ^^^^^^^^
        loop {
            match ts.next() {
                // Parsing Alias expansion
                //
                // Alias = std::hash::Hash, ..Foo;
                //                          ^^^^^
                Some(TokenTree::Punct(punct)) if punct == '.' => {
                    last_span = punct.span();

                    expect_punct!('.');

                    let alias = match ts.next() {
                        Some(TokenTree::Ident(punct)) => {
                            last_span = punct.span();
                            punct
                        }
                        Some(tt) => {
                            compile_errors.extend(CompileError::new(
                                tt.span(),
                                "unexpected token; expected identifier",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                        None => {
                            compile_errors.extend(CompileError::new(
                                last_span,
                                "expected identifier after this",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                    };

                    entities.push(Entity::Alias(alias));

                    match ts.next() {
                        Some(TokenTree::Punct(punct)) if punct == ';' => {
                            last_span = punct.span();
                            break;
                        }
                        Some(TokenTree::Punct(punct)) if punct == ',' => {
                            last_span = punct.span();
                            continue;
                        }
                        Some(tt) => {
                            compile_errors
                                .extend(CompileError::new(tt.span(), "expected `;` or `,`"));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                        None => {
                            compile_errors.extend(CompileError::new(
                                last_span,
                                "expected `;` or `,` after this",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                    }
                }
                // Parsing relative path to a derive
                //
                // Alias = std::hash::Hash, ..Foo;
                //         ^^^^^^^^^^^^^^^
                //
                // But this is disallowed to avoid surprises!
                Some(TokenTree::Ident(ident)) => {
                    compile_errors.extend(CompileError::new(
                        ident.span(),
                        concat!(
                            "to avoid surprises, path to derive in alias ",
                            "definition must be absolute - meaning it must start with `::`",
                            "\n\nfor example, use `::std::hash::Hash` instead ",
                            "of `std::hash::Hash` ",
                            "and use `::core::marker::Copy` instead",
                            " of just `Copy`",
                        ),
                    ));
                    skip_current_alias_declaration!();
                    continue 'parse_alias_declaration;
                }
                // Parsing absolute path to a derive
                //
                // Alias = ::std::hash::Hash, ..Foo;
                //         ^^^^^^^^^^^^^^^^^
                Some(TokenTree::Punct(colon)) if colon == ':' => {
                    last_span = colon.span();

                    let Some(colon_colon) = expect_punct!(':') else {
                        skip_current_alias_declaration!();
                        continue 'parse_alias_declaration;
                    };

                    let mut path = match Path::parse(last_span, &mut ts) {
                        Ok(path) => path,
                        Err(err) => {
                            compile_errors.extend(err);
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                    };

                    path.leading_colon = Some(PathSeparator {
                        first: colon.span(),
                        second: colon_colon.span(),
                    });

                    // Expect terminator.
                    match ts.next() {
                        Some(TokenTree::Punct(comma)) if comma == ',' => {
                            last_span = comma.span();
                            entities.push(Entity::Derive(path));
                            continue;
                        }
                        Some(TokenTree::Punct(semi)) if semi == ';' => {
                            last_span = semi.span();
                            entities.push(Entity::Derive(path));
                            break;
                        }
                        Some(tt) => {
                            compile_errors.extend(CompileError::new(
                                tt.span(),
                                "unexpected token; expected `:`, `;` or `,`",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                        None => {
                            compile_errors.extend(CompileError::new(
                                last_span,
                                "expected `:`, `;` or `,` after this",
                            ));
                            skip_current_alias_declaration!();
                            continue 'parse_alias_declaration;
                        }
                    }
                }
                Some(tt) => {
                    last_span = tt.span();
                    compile_errors.extend(CompileError::new(
                        tt.span(),
                        "unexpected token; expected `::`, `..` or an identifier",
                    ));
                    skip_current_alias_declaration!();
                    continue 'parse_alias_declaration;
                }
                None => {
                    compile_errors.extend(CompileError::new(
                        eq.span(),
                        "expected `::`, `..` or an identifier after this",
                    ));
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

    // The `pub use __derive_alias_Foo as Foo;`s
    let mut alias_use_stmts = TokenStream::new();

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

        // pub use __derive_alias_Foo as Foo;
        alias_use_stmts.extend([
            TokenTree::Ident(Ident::new("pub", Span::call_site())),
            TokenTree::Ident(Ident::new("use", Span::call_site())),
            TokenTree::Ident(Ident::new(
                &format!("__derive_alias_{alias_name}"),
                Span::call_site(),
            )),
            TokenTree::Ident(Ident::new("as", Span::call_site())),
            TokenTree::Ident(Ident::new(alias_name, *alias_name_span)),
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
            // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
            let input_into_new_alias_macro = [
                // Real name of the generated `macro_rules! __derive_alias_Foo`
                //
                // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                // ^^^^^^^^^^^^^^^^^^
                TokenTree::Ident(Ident::new(&format!("__derive_alias_{alias}"), *_alias_span)),
                // Dollar token: The macro creates macros, so we can't use '$' in it. This becomes `$_:tt` so
                // we use `$_` inside the macro
                //
                // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //                    ^
                TokenTree::Punct(Punct::new('$', proc_macro::Spacing::Alone)),
                // Real name of the alias
                //
                // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //                      ^^^
                TokenTree::Ident(Ident::new(alias, Span::call_site())),
                // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                //                         ^
                TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
            ]
            .into_iter()
            // The input is then following by a bunch of paths to all the derives the macro expands to,
            // wrapped in `[...]`. so like `[::core::hash::Hash]`. That's because we want to compare 2 paths,
            // but `:path` specifiers can't be compared, so we compare `[$($tt:tt)*]` instead
            //
            // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
            //                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            .chain(derives.iter().flat_map(|derive| {
                [
                    // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                    //                           ^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Bracket,
                        // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                        //                            ^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^
                        TokenStream::from_iter(derive.clone().into_tokens()),
                    )),
                    // __derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],
                    //                                               ^                     ^
                    TokenTree::Punct(Punct::new(',', proc_macro::Spacing::Joint)),
                ]
            }));

            // NOTE: Treat the last extern alias specially, because we'll actually invoke it.
            // The nested extern aliases will be invoked by this one, one-after-the-other
            if let Some(last_extern_alias) = extern_aliases.pop() {
                // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenStream::from_iter([
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    // ^^^^^
                    TokenTree::Ident(Ident::new("crate", Span::call_site())),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //      ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //        ^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                    ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                      ^^^
                    TokenTree::Ident(last_extern_alias),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                    //                         ^
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                    //                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter([
                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],]] }
                            //                            ^
                            TokenTree::Punct(Punct::new('%', Spacing::Joint)),
                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                            //                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            TokenTree::Group(Group::new(
                                Delimiter::Bracket,
                                extern_aliases.into_iter().fold(
                                    TokenStream::from_iter(input_into_new_alias_macro),
                                    |acc, alias| {
                                        TokenStream::from_iter([
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                               ^^^^^
                                            TokenTree::Ident(Ident::new(
                                                "crate",
                                                Span::call_site(),
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                    ^^
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                      ^^^^^^^^^^^^
                                            TokenTree::Ident(Ident::new(
                                                "derive_alias",
                                                Span::call_site(),
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                  ^^
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                    ^^
                                            TokenTree::Ident(alias),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                      ^
                                            TokenTree::Punct(Punct::new(
                                                ',',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[__derive_alias_Foo $ Foo! [::core::hash::Hash], [::core::fmt::Debug],] ] }
                                            //                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenStream::from_iter([
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    // ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //   ^^^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                 ^^
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Ident(Ident::new(
                        "__internal_derive_aliases_new_alias",
                        Span::call_site(),
                    )),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                                                      ^
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                    //                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter(
                            // The generated documentation. Which we can fully create from what we already know
                            //
                            // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                            //                                                          ^^^^^
                            [TokenTree::Literal(Literal::string(
                                &generate_documentation_for_alias(alias, &derives),
                            ))]
                            .into_iter()
                            // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq], }
                            //                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                            .chain(input_into_new_alias_macro),
                        ),
                    )),
                ])
            }
        })
        // Compile errors, which we report al at once
        .chain(compile_errors)
        // All the `pub use __derive_alias_Foo as Foo;` statements
        .chain(alias_use_stmts)
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
Like [`#[std::derive]`](https://doc.rust-lang.org/reference/attributes/derive.html), but with support for derive aliases generated by [`#[define]`](macro@define)

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
    let mut attr = attr.into_iter().peekable();

    // This is just the raw TokenStream passed directly to `#[std::derive(..)]`
    let mut regular_derives = TokenStream::new();

    // A list of derive aliases, which we will expand at `crate::derive_alias::{alias}`
    let mut aliases = Vec::new();

    while let Some(tt) = attr.next() {
        match tt {
            TokenTree::Punct(dot) if dot == '.' => {
                let _dot_dot = attr.next().expect("expected `..`");
                let Some(TokenTree::Ident(alias)) = attr.next() else {
                    panic!("expected `..alias`")
                };

                aliases.push(alias);

                match attr.peek() {
                    Some(TokenTree::Punct(punct)) if *punct == ',' => {
                        attr.next();
                    }
                    Some(_) => {
                        panic!("expected `,`");
                    }
                    None => (),
                }
            }
            tt => {
                let mut last_is_colon_1 = matches!(&tt, TokenTree::Punct(punct) if *punct == ':');
                let mut last_is_ident = matches!(&tt, TokenTree::Ident(_));

                regular_derives.extend([tt]);

                for next in attr.by_ref() {
                    match next {
                        TokenTree::Ident(ident) if !last_is_ident => {
                            last_is_colon_1 = false;
                            last_is_ident = true;
                            regular_derives.extend([TokenTree::Ident(ident)]);
                        }
                        TokenTree::Punct(punct) if punct == ',' => {
                            // skip adding comma because then we'd have to wrestle with
                            // figuring out whether to add trailing comma or no (we always want one)
                            break;
                        }
                        TokenTree::Punct(punct)
                            if punct == ':' && last_is_colon_1 || last_is_ident =>
                        {
                            #[allow(clippy::needless_bool_assign)]
                            if last_is_ident {
                                last_is_colon_1 = true;
                            } else {
                                last_is_colon_1 = false;
                            }
                            last_is_ident = false;

                            regular_derives.extend([TokenTree::Punct(punct)]);
                        }
                        _ => panic!("unexpected token"),
                    }
                }

                // always add a comma after the path
                regular_derives.extend([TokenTree::Punct(Punct::new(',', Spacing::Joint))]);
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
            TokenTree::Group(Group::new(Delimiter::Bracket, regular_derives)),
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
                        TokenTree::Group(Group::new(Delimiter::Parenthesis, regular_derives)),
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

/// A path to an item
///
/// Example: `::std::hash::Hash`
#[core::prelude::v1::derive(Clone, Debug)]
struct Path {
    /// `true` if the leading colon is present
    ///
    /// ```txt
    /// ::std::hash::Hash
    /// ^^
    /// ```
    leading_colon: Option<PathSeparator>,
    /// First component of the derive
    ///
    /// ```txt
    /// ::std::hash::Hash
    ///   ^^^
    /// ```
    first_component: Ident,
    /// Other components of the derive
    ///
    /// ```txt
    /// ::std::hash::Hash
    ///      ^^^^^^
    ///            ^^^^^^
    /// ```
    components: Vec<(PathSeparator, Ident)>,
}

impl Path {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        self.leading_colon
            .map(PathSeparator::into_tokens)
            .into_iter()
            .flatten()
            // first path segment
            //
            // ::core::hash::Hash
            //   ^^^
            .chain([TokenTree::Ident(self.first_component)])
            .chain(
                self.components
                    .into_iter()
                    .flat_map(|(separator, segment)| {
                        // path separator `::`
                        //
                        // ::std::hash::HashMap
                        //      ^^    ^^
                        separator
                            .into_tokens()
                            .into_iter()
                            // path segment
                            //
                            // ::std::hash::HashMap
                            //        ^^^^  ^^^^^^^
                            .chain(std::iter::once(TokenTree::Ident(segment)))
                    }),
            )
    }

    /// Requires the `ts` to start with `::segment` (absolute) or `segment` (relative), then
    /// expects 0 or more `::segment`s followed by whatever (once we hit "whatever", stops trying to parse further)
    ///
    /// `start_span` is the span of the thing directly before the `Path`
    pub fn parse(
        start_span: Span,
        ts: &mut std::iter::Peekable<proc_macro::token_stream::IntoIter>,
    ) -> Result<Self, CompileError> {
        let (leading_colon, first_component) = match ts.next() {
            Some(TokenTree::Ident(ident)) => (None, ident),
            Some(TokenTree::Punct(colon)) if colon == ':' => match ts.next() {
                Some(TokenTree::Punct(colon_colon)) if colon_colon == ':' => match ts.next() {
                    Some(TokenTree::Ident(ident)) => (
                        Some(PathSeparator {
                            first: colon.span(),
                            second: colon_colon.span(),
                        }),
                        ident,
                    ),
                    Some(tt) => return Err(CompileError::new(tt.span(), "expected identifier")),
                    None => {
                        return Err(CompileError::new(
                            colon_colon.span(),
                            "expected identifier after this",
                        ))
                    }
                },
                Some(tt) => return Err(CompileError::new(tt.span(), "expected `:`")),
                None => return Err(CompileError::new(colon.span(), "expected `:` after this")),
            },
            Some(tt) => return Err(CompileError::new(tt.span(), "expected `::` or identifier")),
            None => {
                return Err(CompileError::new(
                    start_span,
                    "expected `::` or identifier after this",
                ))
            }
        };

        let mut components = Vec::new();

        while let Some(colon) =
            ts.next_if(|tt| matches!(tt, TokenTree::Punct(colon) if *colon == ':'))
        {
            let colon_colon = match ts.next() {
                Some(TokenTree::Punct(colon_colon)) if colon_colon == ':' => colon_colon,
                Some(tt) => return Err(CompileError::new(tt.span(), "expected `::`")),
                None => return Err(CompileError::new(colon.span(), "expected `:` after this")),
            };

            let ident = match ts.next() {
                Some(TokenTree::Ident(ident)) => ident,
                Some(tt) => return Err(CompileError::new(tt.span(), "expected identifier")),
                None => {
                    return Err(CompileError::new(
                        colon_colon.span(),
                        "expected identifier after this",
                    ))
                }
            };

            components.push((
                PathSeparator {
                    first: colon.span(),
                    second: colon_colon.span(),
                },
                ident,
            ));
        }

        Ok(Path {
            leading_colon,
            first_component,
            components,
        })
    }
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
///     "..." __derive_alias_EqEq $ EqEq! [::core::cmp::Eq],[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::PartialEq],
/// }
/// ```
///
/// So instead of that, we nest the arguments inside of a call to this macro, which de-duplicates the derives we pass to `new_alias!`:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias_with_externs! {
///     __derive_alias_EqEq $ EqEq!
///     [::core::cmp::Eq],[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::PartialEq],
/// }
/// ```
///
/// This expands to this:
///
/// ```ignore
/// crate::__internal_derive_aliases_new_alias! {
///     "..." __derive_alias_EqEq $ EqEq! [::core::cmp::Eq],[::core::cmp::PartialEq],
/// }
/// ```
///
/// Originally, this macro was just a TT muncher. However, to accurately generate the documentation `"..."` I made it
/// into a proc macro, so it also generates the `"..."` based off the aliases it contains
#[doc(hidden)]
#[proc_macro]
pub fn __internal_derive_aliases_new_alias_with_externs(ts: TokenStream) -> TokenStream {
    let mut ts = ts.into_iter().peekable();

    // __derive_alias_EqEq
    let real_alias_name = match ts.next() {
        Some(TokenTree::Ident(ident)) => ident,
        _ => unreachable!("first token is the actual name of the `macro_rules!` alias"),
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
            TokenTree::Group(group) if group.delimiter() == Delimiter::Bracket => {
                group.stream().into_iter().peekable()
            }
            _ => unreachable!("every path is enclosed inside of `[...]`"),
        };

        paths.insert(
            Path::parse(Span::call_site(), &mut group)
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
            TokenTree::Ident(real_alias_name),
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

/// Separator in a path: `::`
///
/// ```ignore
/// ::std::hash::Hash
/// ^^   ^^    ^^
/// ```
#[core::prelude::v1::derive(Clone, Debug)]
struct PathSeparator {
    /// Span of the first `:`
    ///
    /// ```ignore
    /// ::std::hash::Hash
    /// ^    ^     ^
    /// ```
    first: Span,
    /// Span of the second `:`
    ///
    /// ```ignore
    /// ::std::hash::Hash
    ///  ^    ^     ^
    /// ```
    second: Span,
}

impl PathSeparator {
    fn into_tokens(self) -> [TokenTree; 2] {
        let mut first = Punct::new(':', Spacing::Joint);
        first.set_span(self.first);
        let mut second = Punct::new(':', Spacing::Joint);
        second.set_span(self.second);
        [TokenTree::Punct(first), TokenTree::Punct(second)]
    }
}

impl Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.leading_colon.is_some().hash(state);
        self.first_component.to_string().hash(state);
        for (_, component) in &self.components {
            component.to_string().hash(state);
        }
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.leading_colon.is_some() == other.leading_colon.is_some()
            && self.first_component.to_string() == other.first_component.to_string()
            && self
                .components
                .iter()
                .map(|(_, ident)| ident.to_string())
                .eq(other.components.iter().map(|(_, ident)| ident.to_string()))
    }
}

impl Eq for Path {}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::std::hash::Hash
        // ^^
        if self.leading_colon.is_some() {
            f.write_str("::")?;
        }
        // ::std::hash::Hash
        //   ^^^
        f.write_str(&self.first_component.to_string())?;
        for (_sep, component) in &self.components {
            // ::std::hash::Hash
            //      ^^
            //            ^^
            f.write_str("::")?;
            // ::std::hash::Hash
            //        ^^^^
            //              ^^^^
            f.write_str(&component.to_string())?;
        }

        Ok(())
    }
}

/// `.into_iter()` generates `compile_error!($message)` at `$span`
#[std::prelude::v1::derive(Debug)]
struct CompileError {
    /// Where the compile error is generates
    pub span: Span,
    /// Message of the compile error
    pub message: String,
}

impl CompileError {
    /// Create a new compile error
    pub fn new(span: Span, message: impl AsRef<str>) -> Self {
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
