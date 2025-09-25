//  `#[derive]` aliases for reducing code boilerplate
//
//  Aliases are defined in a special file `derive_aliases.rs`, located next to your **crate**'s `Cargo.toml`:
//
//  ```ignore
//  // Simple derive aliases
//  //
//  // `#[derive(..Copy, ..Eq)]` expands to `#[std::derive(Copy, Clone, PartialEq, Eq)]`
//
//  Copy = Copy, Clone;
//  Eq = PartialEq, Eq;
//
//  // You can nest them!
//  //
//  // `#[derive(..Ord, std::hash::Hash)]` expands to `#[std::derive(PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]`
//
//  Ord = PartialOrd, Ord, ..Eq;
//  ```
//
//  This file uses a tiny domain-specific language for defining the derive aliases (the parser is less than 20 lines of code!). `.rs` is used just for syntax highlighting.
//  These aliases can then be used in Rust like so:
//
//  ```ignore
//  // This globally overrides `std::derive` with `derive_aliases::derive` across the whole crate! Handy.
//  #[macro_use]
//  extern crate derive_aliases;
//
//  #[derive(..Copy, ..Ord, std::hash::Hash)]
//  struct HelloWorld;
//  ```
//
//  This expands to:
//
//  ```ignore
//  #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]
//  struct HelloWorld;
//  ```
//
//  # Documentation on hover
//
//  With `Ord` alias defines as follows:
//
//  ```ignore
//  Eq = PartialEq, Eq;
//  Ord = PartialOrd, Ord, ..Eq;
//  ```
//
//  Hovering over `..Ord` will show that it expands to `PartialOrd, Ord, PartialEq, Eq`:
//
//  ![hovering shows docs](https://github.com/nik-rev/derive-aliases/blob/main/docs.png?raw=true)
//
//  # `use` other alias files in `derive_aliases.rs`
//
//  `use` followed by a path will inline the derive aliases located in that file.
//
//  If `../my_other_aliases.rs` contains:
//
//  ```ignore
//  Ord = PartialOrd, Ord, ..Eq;
//  ```
//
//  And your `derive_aliases.rs` has:
//
//  ```ignore
//  use "../my_other_aliases.rs";
//
//  Eq = PartialEq, Eq;
//  ```
//
//  Then it will inline the aliases in the other file, expanding to:
//
//  ```ignore
//  Ord = PartialOrd, Ord, ..Eq;
//  Eq = PartialEq, Eq;
//  ```

use core::fmt;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use proc_macro::{Literal, Spacing};
use std::collections::HashSet;
use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;
use std::iter;

// when doing `#[macro_use] extern crate proc` we will also globally import this macro
// and it will be suggested by rust_analyzer. but since this macro must be called just once,
// we don't want rust_analyzer to suggest it
#[cfg_attr(not(doc), doc(hidden))]
#[allow(unused_assignments)]
#[proc_macro]
pub fn define(tts: TokenStream) -> TokenStream {
    // First we collect everything into a map which references itself
    //
    // Then, we'll "normalize" it by recursively expanding aliases
    // until there is no more left
    let mut nested_alias_map = HashMap::new();

    // Token trees
    let mut tts = tts.into_iter().peekable();

    // Compile errors to report all at once
    let mut compile_errors = TokenStream::new();

    // Span of the last token, or if there isn't one then it's where the macro
    // was invoked at
    let mut last_span = Span::call_site();

    /// Parses an identifier
    macro_rules! expect_ident {
        () => {
            match tts.next() {
                Some(TokenTree::Ident(punct)) => {
                    last_span = punct.span();
                    punct
                }
                Some(tt) => {
                    return CompileError::new(tt.span(), "unexpected token; expected identifier")
                        .into_iter()
                        .collect();
                }
                None => {
                    return CompileError::new(last_span, "expected identifier after this")
                        .into_iter()
                        .collect();
                }
            }
        };
    }

    /// Parses a single `$punct` character
    macro_rules! expect_punct {
        ($punct:literal) => {
            match tts.next() {
                Some(TokenTree::Punct(punct)) if punct == $punct => {
                    last_span = punct.span();
                    punct
                }
                Some(tt) => {
                    return CompileError::new(
                        tt.span(),
                        concat!("unexpected token; expected `", $punct, "`"),
                    )
                    .into_iter()
                    .collect();
                }
                None => {
                    return CompileError::new(
                        last_span,
                        concat!("expected `", $punct, "` after this"),
                    )
                    .into_iter()
                    .collect();
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
    while let Some(tt) = tts.next() {
        // Alias = ..Foo, Bar, baz::Baz;
        // ^^^^^
        let TokenTree::Ident(alias_name) = tt else {
            return CompileError::new(tt.span(), "expected identifier")
                .into_iter()
                .collect();
        };

        // Alias = ..Foo, Bar, baz::Baz;
        //       ^
        let eq = expect_punct!('=');

        let mut entities = Vec::new();

        // Loop that parses the entire RHS
        //
        // Alias = ..Foo, Bar, baz::Baz;
        //         ^^^^^
        //                ^^^
        //                     ^^^^^^^^
        loop {
            match tts.next() {
                // Parsing Alias expansion
                //
                // Alias = std::hash::Hash, ..Foo;
                //                          ^^^^^
                Some(TokenTree::Punct(punct)) if punct == '.' => {
                    last_span = punct.span();

                    expect_punct!('.');

                    let alias = expect_ident!();

                    entities.push(Entity::Alias(alias));

                    match tts.next() {
                        Some(TokenTree::Punct(punct)) if punct == ';' => {
                            last_span = punct.span();
                            break;
                        }
                        Some(TokenTree::Punct(punct)) if punct == ',' => {
                            last_span = punct.span();
                            continue;
                        }
                        Some(tt) => {
                            return CompileError::new(tt.span(), "expected `;` or `,`")
                                .into_iter()
                                .collect();
                        }
                        None => {
                            return CompileError::new(last_span, "expected `;` or `,` after this")
                                .into_iter()
                                .collect();
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
                    continue;
                }
                // Parsing absolute path to a derive
                //
                // Alias = ::std::hash::Hash, ..Foo;
                //         ^^^^^^^^^^^^^^^^^
                //
                // TODO: Make it an inherent method on `Path`
                Some(TokenTree::Punct(colon)) if colon == ':' => {
                    last_span = colon.span();

                    // compile_errors.extend(CompileError::new(last_span, "lol"));

                    let colon_colon = expect_punct!(':');

                    // compile_errors.extend(CompileError::new(colon_colon.span(), "lol"));

                    let mut path = match Path::parse(last_span, &mut tts) {
                        Ok(path) => path,
                        Err(err) => return err.into_iter().collect(),
                    };

                    // return CompileError::new(path.first_component.span(), &format!("{path}"))
                    //     .into_iter()
                    //     .collect();

                    path.leading_colon = Some(PathSeparator {
                        first: colon.span(),
                        second: colon_colon.span(),
                    });

                    // Expect terminator.
                    match tts.next() {
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
                            return CompileError::new(
                                tt.span(),
                                "unexpected token; expected `:`, `;` or `,`",
                            )
                            .into_iter()
                            .collect();
                        }
                        None => {
                            return CompileError::new(
                                last_span,
                                "expected `:`, `;` or `,` after this",
                            )
                            .into_iter()
                            .collect();
                        }
                    }
                }
                Some(tt) => {
                    last_span = tt.span();

                    return CompileError::new(
                        tt.span(),
                        "unexpected token; expected `::`, `..` or an identifier",
                    )
                    .into_iter()
                    .collect();
                }
                None => {
                    return CompileError::new(
                        eq.span(),
                        "expected `::`, `..` or an identifier after this",
                    )
                    .into_iter()
                    .collect();
                }
            }
        }

        nested_alias_map.insert(alias_name.to_string(), (alias_name.span(), entities));
    }

    fn resolve_alias(
        nested_alias_map: &HashMap<String, (Span, Vec<Entity>)>,
        entities: &[Entity],
        derives: &mut HashSet<Path>,
        alias_ref_use_stmts: &mut TokenStream,
        extern_aliases: &mut Vec<Ident>,
    ) {
        for entity in entities {
            match entity {
                Entity::Alias(ident) => {
                    let Some((_span, entities)) = nested_alias_map.get(&ident.to_string()) else {
                        // panic!("{ident:#?}");
                        // THIS macro invocation did not define this alias,
                        // but another one could have. Or it could have been imported from another
                        // module/crate
                        //
                        extern_aliases.push(ident.clone());
                        continue;
                    };

                    // IMPORTANT: This must only exist if it is NOT an extern alias

                    alias_ref_use_stmts.extend([
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
                        alias_ref_use_stmts,
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

    let mut flat_alias_map = HashMap::new();
    let mut alias_use_stmts = TokenStream::new();
    let mut alias_ref_use_stmts = TokenStream::new();

    for (alias_name, (alias_name_span, entities)) in &nested_alias_map {
        // A flat list of derives that the alias expands to
        let mut flat_derives = HashSet::new();

        // "external" aliases that were NOT defined by this macro,
        // we'll just call them appropriately
        let mut extern_aliases = Vec::new();

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

        resolve_alias(
            &nested_alias_map,
            entities,
            &mut flat_derives,
            &mut alias_ref_use_stmts,
            &mut extern_aliases,
        );

        for derive in &flat_derives {
            alias_ref_use_stmts.extend(
                [
                    TokenTree::Ident(Ident::new("use", Span::call_site())),
                    TokenTree::Punct({
                        let mut punct = Punct::new(':', Spacing::Joint);
                        punct.set_span(derive.leading_colon.clone().unwrap().first);
                        punct
                    }),
                    TokenTree::Punct({
                        let mut punct = Punct::new(':', Spacing::Joint);
                        punct.set_span(derive.leading_colon.clone().unwrap().second);
                        punct
                    }),
                    TokenTree::Ident(derive.first_component.clone()),
                ]
                .into_iter()
                .chain(derive.components.iter().flat_map(|(separator, ident)| {
                    [
                        TokenTree::Punct({
                            let mut punct = Punct::new(':', Spacing::Joint);
                            punct.set_span(separator.first);
                            punct
                        }),
                        TokenTree::Punct({
                            let mut punct = Punct::new(':', Spacing::Joint);
                            punct.set_span(separator.second);
                            punct
                        }),
                        TokenTree::Ident(ident.clone()),
                    ]
                }))
                .chain([
                    TokenTree::Ident(Ident::new("as", Span::call_site())),
                    TokenTree::Ident(Ident::new(
                        "_",
                        derive.components.last().map_or_else(
                            || derive.first_component.span(),
                            |(_, ident)| ident.span(),
                        ),
                    )),
                    TokenTree::Punct(Punct::new(';', Spacing::Joint)),
                ]),
            );
        }

        flat_alias_map.insert(alias_name, (alias_name_span, flat_derives, extern_aliases));
    }

    // Finally let's expand all of this to a bunch of invocations of the `__internal_new_alias!` macro
    flat_alias_map
        .into_iter()
        .flat_map(|(alias, (_alias_span, derives, mut extern_aliases))| {
            let input_tt = TokenStream::from_iter(
                [
                    TokenTree::Literal(Literal::string(&generate_documentation_for_alias(
                        alias, &derives,
                    ))),
                    TokenTree::Ident(Ident::new(&format!("__derive_alias_{alias}"), *_alias_span)),
                    TokenTree::Punct(Punct::new('$', proc_macro::Spacing::Alone)),
                    TokenTree::Ident(Ident::new(alias, Span::call_site())),
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                ]
                .into_iter()
                .chain(derives.iter().flat_map(|derive| {
                    [
                        TokenTree::Group(Group::new(
                            Delimiter::Bracket,
                            TokenStream::from_iter(
                                [
                                    // path separator `::`
                                    // since we know the path to derive is ALWAYS an absolute path
                                    //
                                    // ::std::hash::HashMap
                                    // ^^
                                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                                    // first path segment
                                    //
                                    // ::std::hash::HashMap
                                    //   ^^^
                                    TokenTree::Ident(derive.first_component.clone()),
                                ]
                                .into_iter()
                                .chain(
                                    derive.components.iter().flat_map(|(_colon, component)| {
                                        [
                                            // path separator `::`
                                            //
                                            // ::std::hash::HashMap
                                            //      ^^    ^^
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            TokenTree::Punct(Punct::new(
                                                ':',
                                                proc_macro::Spacing::Joint,
                                            )),
                                            // path segment
                                            //
                                            // ::std::hash::HashMap
                                            //        ^^^^  ^^^^^^^
                                            TokenTree::Ident(Ident::new(
                                                &component.to_string(),
                                                component.span(),
                                            )),
                                        ]
                                    }),
                                ),
                            ),
                        )),
                        TokenTree::Punct(Punct::new(',', proc_macro::Spacing::Joint)),
                    ]
                })),
            );

            let other = if let Some(last_alias) = extern_aliases.pop() {
                let inner_stream = extern_aliases.into_iter().fold(input_tt, |acc, alias| {
                    TokenStream::from_iter([
                        TokenTree::Ident(Ident::new("crate", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Ident(alias),
                        TokenTree::Punct(Punct::new(',', proc_macro::Spacing::Joint)),
                        TokenTree::Group(Group::new(Delimiter::Bracket, acc)),
                    ])
                });

                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("crate", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Ident(last_alias),
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter([
                            TokenTree::Punct(Punct::new('%', Spacing::Joint)),
                            TokenTree::Group(Group::new(Delimiter::Bracket, inner_stream)),
                        ]),
                    )),
                ])
            } else {
                TokenStream::from_iter([
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                    TokenTree::Ident(Ident::new("__internal_new_alias", Span::call_site())),
                    TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                    TokenTree::Group(Group::new(Delimiter::Brace, input_tt)),
                ])
            };

            TokenStream::from_iter(iter::empty().chain(other))
        })
        .chain(compile_errors)
        .chain(alias_use_stmts)
        .chain(TokenStream::from_iter([
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
            TokenTree::Ident(Ident::new("const", Span::call_site())),
            TokenTree::Ident(Ident::new("_", Span::call_site())),
            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
            TokenTree::Punct(Punct::new('=', Spacing::Joint)),
            TokenTree::Group(Group::new(Delimiter::Brace, alias_ref_use_stmts)),
            TokenTree::Punct(Punct::new(';', Spacing::Joint)),
        ]))
        .collect()
}

#[proc_macro_attribute]
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

/// Separator in a path: `::`
#[core::prelude::v1::derive(Clone, Debug)]
struct PathSeparator {
    /// Span of the first `:`
    first: Span,
    /// Span of the second `:`
    second: Span,
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
        if self.leading_colon.is_some() {
            f.write_str("::")?;
        }
        f.write_str(&self.first_component.to_string())?;
        for (_sep, component) in &self.components {
            f.write_str("::")?;
            f.write_str(&component.to_string())?;
        }

        Ok(())
    }
}

/// `.into_iter()` generates `compile_error!($message)` at `$span`
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
