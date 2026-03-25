use crate::tokens;
use crate::tokens::IntoTokens;
use crate::CompileError;
use proc_macro::Spacing;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use tokens::Path;
use tokens::TokensIter;

pub fn derive(attrs_before: TokenStream, attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut compile_errors = Vec::new();

    let ExtractedDerives {
        regular_derives,
        mut derive_aliases,
    } = extract_derives(attr, &mut compile_errors);

    let regular_derives: Vec<Vec<Path>> = vec![regular_derives];

    // This currently holds the entire item.
    //
    // All attributes `#[attr()]` at the start of the stream will be extracted,
    // and the non-processed ones will be placed into `other_attrs`
    //
    // It will be our job to put those attributes back when generating the code, before this stream
    let item_tokens = item.into_iter().peekable();

    // all the tokens for "compile_error!(...)" invocations, to be inserted
    // alongside all other input
    let compile_errors = compile_errors
        .into_iter()
        .flat_map(|compile_error| compile_error.into_tokens());

    // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[Debug,] [::core::clone::Clone]] [struct Foo;])) [] }
    //
    // We treat the last alias specially
    if let Some(first_alias) = derive_aliases.pop() {
        // Every regular derive and its `cfg` value
        //
        // [::core::marker::Copy] [::core::clone::Clone]
        // ^^^^^^^^^^^^^^^^^^^^^^
        //                        ^^^^^^^^^^^^^^^^^^^^^^
        let regular_derives = regular_derives.into_iter().flat_map(|derives| {
            derives.into_iter().map(move |derive| {
                // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[Debug,] [::core::clone::Clone]] [struct Foo;])) [] }
                //                                                                                    ^^^^^^^^
                //                                                                                             ^^^^^^^^^^^^^^^^^^^^^^
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    [TokenTree::Group(Group::new(
                        Delimiter::Brace,
                        TokenStream::from_iter([
                            TokenTree::Ident(Ident::new("all", Span::call_site())),
                            TokenTree::Group(Group::new(
                                Delimiter::Parenthesis,
                                TokenStream::new(),
                            )),
                        ]),
                    ))]
                    .into_iter()
                    .chain(derive.into_tokens())
                    .collect(),
                ))
            })
        });

        let innermost_ts = TokenStream::from_iter([
            // The '@' is a symbol that tells the macro that there are derive aliases. See docs on `::derive_aliases::__internal_derive_aliases_new_alias!`
            // for more info.
            //
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[Debug,] [Clone]] [struct Foo;])) [] }
            //                                                                                 ^
            TokenTree::Punct(Punct::new('@', Spacing::Joint)),
            // TODO: explain
            TokenTree::Group(Group::new(Delimiter::Parenthesis, attrs_before)),
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[Debug,] [Clone]] [struct Foo;])) [] }
            //                                                                                   ^^^^^^^^^^^^^^^^^^
            TokenTree::Group(Group::new(Delimiter::Bracket, regular_derives.collect())),
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[Debug,] [Clone]] [struct Foo;])) [] }
            //                                                                                                      ^^^^^^^^^^^^^
            TokenTree::Group(Group::new(Delimiter::Bracket, item_tokens.collect())),
        ]);

        // Every single alias exists as a `macro_rules!` item that knows how to inject itself into the invocation
        // of another alias. This architecture is required because a `derive` macro has no idea what derives
        // an alias expands into, a `derive` macro creates an empty list of `[]` aliases and then
        // calls each derive alias. Each derive alias populates this list. This list is then emitted as a `#[std::derive]`
        //
        // @ [[Debug,] [Clone]] [struct Foo;]
        //
        // [crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;])
        //
        // [crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;]))
        let inner = derive_aliases
            .into_iter()
            .fold(innermost_ts, |acc, current_alias| {
                TokenStream::from_iter([
                    TokenTree::Group(Group::new(
                        Delimiter::Bracket,
                        TokenStream::from_iter([
                            // [crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;]))
                            //  ^^^^^^^^^^^^^^^^^^^^^
                            TokenTree::Ident(Ident::new("crate", Span::call_site())),
                            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                            TokenTree::Ident(Ident::new("derive_alias", Span::call_site())),
                            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                            TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                            // [crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;]))
                            //                       ^^
                            TokenTree::Ident(current_alias),
                        ]),
                    )),
                    // Conceptually, these are the arguments to the alias.
                    //
                    // The macro processing these tokens will take contents inside the parentheses, and call the alias
                    //
                    // [crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;]))
                    //                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    //                          these arguments
                    //
                    // [crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;]))
                    //                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    //  ^^^^^^^^^^^^^^^^^^^^^^^ will be input into this macro (this is the alias, it is a `macro_rules!`)
                    //
                    // For that example, `Copy` is the alias that `Eq` will invoke next
                    TokenTree::Group(Group::new(Delimiter::Parenthesis, acc)),
                ])
            });

        // Wrap in a final invocation
        //
        // crate::derive_alias::Ord!([crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;])) [])
        //                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //
        // This represents the entry point of derive alias expansion.
        let stream = TokenStream::from_iter(inner.into_iter().chain([TokenTree::Group(
            Group::new(Delimiter::Bracket, TokenStream::new()),
        )]));

        // crate::derive_alias::Ord!([crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;])) [])
        // ^^^^^^^^^^^^^^^^^^^^^^^^^
        //
        // The final structure looks like this:
        //
        // crate::derive_alias::Ord!([crate::derive_alias::Eq]([crate::derive_alias::Copy](@ [[Debug,] [Clone]] [struct Foo;])) [])
        //                           ^^^^^^^^^^^^^^^^^^^^^^^^^ alias #1
        //                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ args to alias #1
        //                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^ alias #2
        //                                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ args to alias #2
        //                                                                                     ^^^^^^  regular derive #1
        //                                                                                              ^^^^^ regular derive #2
        //                                                                                                       ^^^^^^^^^^^^^ THE ITEM
        //                                                                                                                      ^^ the list where we every alias
        //                                                                                                                         injects its derives into
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
                TokenTree::Group(Group::new(Delimiter::Parenthesis, stream)),
                TokenTree::Punct(Punct::new(';', Spacing::Joint)),
            ]
            .into_iter()
            .chain(compile_errors),
        )
    } else {
        // No derive aliases used.
        // Just pass all derives to the standard library's

        // #[::core::prelude::v1::derive(Trait, Trait2,)]
        //                               ^^^^^^^^^^^^^^
        let derive_attr_input = regular_derives.into_iter().flat_map(|derives| {
            derives.into_iter().flat_map(|derive| {
                derive
                    .into_tokens()
                    .chain([TokenTree::Punct(Punct::new(',', Spacing::Joint))])
            })
        });
        // #[::core::prelude::v1::derive(Trait, Trait2,)]
        // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        let derive_attr = [
            // #[::core::prelude::v1::derive(Trait)]
            // ^
            TokenTree::Punct(Punct::new('#', Spacing::Joint)),
            // #[::core::prelude::v1::derive(Trait)]
            //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            TokenTree::Group(Group::new(
                Delimiter::Bracket,
                // #[::core::prelude::v1::derive(Trait)]
                //  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenStream::from_iter([
                    // #[::core::prelude::v1::derive(Trait)]
                    //   ^^
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    // #[::core::prelude::v1::derive(Trait)]
                    //     ^^^^
                    TokenTree::Ident(Ident::new("core", Span::call_site())),
                    // #[::core::prelude::v1::derive(Trait)]
                    //         ^^
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    // #[::core::prelude::v1::derive(Trait)]
                    //           ^^^^^^^
                    TokenTree::Ident(Ident::new("prelude", Span::call_site())),
                    // #[::core::prelude::v1::derive(Trait)]
                    //                  ^^
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    // #[::core::prelude::v1::derive(Trait)]
                    //                    ^^
                    TokenTree::Ident(Ident::new("v1", Span::call_site())),
                    // #[::core::prelude::v1::derive(Trait)]
                    //                      ^^
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    // #[::core::prelude::v1::derive(Trait)]
                    //                        ^^^^^^
                    TokenTree::Ident(Ident::new("derive", Span::call_site())),
                    // #[::core::prelude::v1::derive(Trait)]
                    //                              ^^^^^^^
                    TokenTree::Group(Group::new(
                        Delimiter::Parenthesis,
                        // #[::core::prelude::v1::derive(Trait)]
                        //                               ^^^^^
                        derive_attr_input.collect(),
                    )),
                ]),
            )),
        ];

        // The item with #[derive] attribute applied at the top
        let ts = TokenStream::from_iter(
            attrs_before
                .into_iter()
                .chain(derive_attr)
                .chain(item_tokens)
                .chain(compile_errors),
        );

        if option_env!("DERIVE_ALIASES_ANNOTATION_TEST").is_some() {
            // inside of our annotation tests, we use "trace_macros(true)" -
            // but that doesn't expand attribute macros, only declarative macros.
            //
            // And because when the `derive` macro receives no aliases as arguments it doesn't expand
            // to the invocation of a declarative macro; it expands to attribute macros (namely std::derive)
            //
            // To fix that, we wrap the entire output inside of a macro call that is just the
            // identity that returns all receives tokens. Now `trace_macros!(true)` will show
            // the expansion of this macro.
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new(
                    "required_for_annotation_tests",
                    Span::call_site(),
                )),
                TokenTree::Punct(Punct::new('!', Spacing::Joint)),
                TokenTree::Group(Group::new(Delimiter::Brace, ts)),
            ])
        } else {
            ts
        }
    }
}

/// Extracts derives and derive aliases from a `#[derive]` attribute.
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
