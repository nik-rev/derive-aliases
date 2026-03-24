use crate::cfg_true;
use crate::encode_cfg_predicate;
use crate::tokens;
use crate::tokens::IntoTokens;
use crate::CompileError;
use proc_macro::Spacing;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use tokens::Path;
use tokens::TokensIter;

pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
    // panic!("{}", item);

    let mut compile_errors = Vec::new();

    let ExtractedDerives {
        regular_derives,
        derive_aliases,
    } = extract_derives(attr, &mut compile_errors);

    let mut regular_derives: Vec<(TokenStream, Vec<Path>)> = vec![(cfg_true(), regular_derives)];

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

    // Attributes that we don't process, instead, they are kept as-is,
    // and we must re-emit them
    let attributes_to_re_emit = fun_name(
        &mut compile_errors,
        &mut regular_derives,
        &mut derive_aliases,
        &mut item_tokens,
    );

    // Tokens of the item, with all non-derive attributes inserted back
    let item_tokens = attributes_to_re_emit.into_iter().chain(item_tokens);

    // all the tokens for "compile_error!(...)" invocations, to be inserted
    // alongside all other input
    let compile_errors = compile_errors
        .into_iter()
        .flat_map(|compile_error| compile_error.into_tokens());

    // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } ::core::clone::Clone]] [struct Foo;])) [] }
    //
    // We treat the last alias specially
    if let Some((first_alias_cfg, first_alias)) = derive_aliases.pop() {
        // Every regular derive and its `cfg` value, which is just for now will be
        // always `true`, so basically #[cfg_attr(true, DERIVE)]
        //
        // [{ cfg } ::core::marker::Copy] [{ cfg } ::core::clone::Clone]
        // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        let regular_derives = regular_derives.into_iter().flat_map(|(cfg, derives)| {
            derives.into_iter().map(move |derive| {
                // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } ::core::clone::Clone]] [struct Foo;])) [] }
                //                                                                                    ^^^^^^^^^^^^^^^^
                //                                                                                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                TokenTree::Group(Group::new(
                    Delimiter::Bracket,
                    TokenStream::from_iter(
                        encode_cfg_predicate(cfg.clone())
                            .into_iter()
                            .chain(derive.into_tokens()),
                    ),
                ))
            })
        });

        let innermost_ts = TokenStream::from_iter([
            // The '@' is a symbol that tells the macro that there are derive aliases. See docs on `::derive_aliases::__internal_derive_aliases_new_alias!`
            // for more info.
            //
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])) [] }
            //                                                                                 ^
            TokenTree::Punct(Punct::new('@', Spacing::Joint)),
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])) [] }
            //                                                                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            TokenTree::Group(Group::new(Delimiter::Bracket, regular_derives.collect())),
            // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])) [] }
            //                                                                                                             Clone  ^^^^^^^^^^^^^
            TokenTree::Group(Group::new(Delimiter::Bracket, item_tokens.collect())),
        ]);

        // Build up nesting
        //
        // @ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;]
        // crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])
        // crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;]))
        let inner =
            derive_aliases
                .into_iter()
                .fold(innermost_ts, |acc, (current_cfg, current_alias)| {
                    // TODO: We want to GATE this alias behind `current_cfg`

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
        // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])) [] }
        //                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        let stream = TokenStream::from_iter(inner.into_iter().chain([TokenTree::Group(
            Group::new(Delimiter::Bracket, TokenStream::new()),
        )]));

        // crate::derive_alias::Ord! { crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [[{ cfg } Debug,] [{ cfg } Clone]] [struct Foo;])) [] }
        // ^^^^^^^^^^^^^^^^^^^^^^^^^
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

/// Loop through every attribute on the item
///
/// ```ignore
/// #[attr1]
/// #[attr2]
/// #[attr3]
/// #[attr4]
/// struct Item;
/// ```
///
/// Attributes that are `#[derive(..)]`-like attributes or `#[cfg_attr(predicate, derive(..))]`-like will
/// have their contents parsed and the derive aliases / derives will all be handled by a single proc macro call
///
/// Yes, this means that multiple `#[derive_aliases::derive]` calls will "merge" into a single one:
///
/// ```ignore
/// #[derive(..Eq, Clone)]
/// #[cfg_attr(feature = "serde", deny_unknown_fields)]
/// #[derive(..Ord, Copy)]
/// #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// struct Item;
/// ```
///
/// All of those attributes will actually act as if the user wrote this instead:
///
/// ```ignore
/// #[derive(..Eq, Clone, ..Ord, Copy)]
/// #[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// #[cfg_attr(feature = "serde", deny_unknown_fields)]
/// struct Item;
/// ```
///
/// Notably:
///
/// - What the user wrote should fail compilation. The `serde` helper attribute is only available
///   *after* the derive macro, but the user wrote them in reverse order. Because of our macro, it will compile
/// - The order in which the aliases/derives are actually evaluated is completely unspecified
///
/// Why do this?
///
/// Because we're mimicking what the `core::derive` macro actually does, we need to consider helper attributes.
/// Derive helper attributes work via name resolution. But in a macro context, we don't have access to the name resolution,
/// so we must try best-effort.
///
/// Earlier, we would have each `derive_aliases::derive` attribute handle its own logic and wrapping.
/// This didn't work because there were issues with helper attribute namespace being invalidated after the first
/// attribute was expanded, so instead all work is done by a single attribute.
///
/// This does mean that the user can't actually use a different name for the `derive` attribute,
/// only a hard-coded set of paths is read from, namely `derive`, `derive_aliases::derive`,
/// and `derive_aliases::derive`
fn fun_name(
    compile_errors: &mut Vec<CompileError>,
    regular_derives: &mut Vec<(TokenStream, Vec<Path>)>,
    derive_aliases: &mut Vec<(TokenStream, Ident)>,
    item_tokens: &mut std::iter::Peekable<proc_macro::token_stream::IntoIter>,
) -> TokenStream {
    let mut other_attrs = TokenStream::new();

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
                                let derives = extract_derives(derive_stream, compile_errors);
                                regular_derives
                                    .push((cfg_predicate.clone(), derives.regular_derives));
                                for alias in derives.derive_aliases {
                                    derive_aliases.push((cfg_predicate.clone(), alias));
                                }
                            }
                            // The attribute inside of #[cfg_attr] is NOT a derive macro., e.g. #[cfg_attr(predicate, doc(hidden))]
                            // or literally any other attribute
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
                                //
                                // #[cfg_attr(predicate, doc(hidden))]
                                other_attrs.extend([attr_hash, TokenTree::Group(group)]);
                                continue;
                            }
                        }
                    }
                    // A top-level `derive` attribute
                    is_derive!() => {
                        let Some(derive_stream) = attr_stream.group(Delimiter::Parenthesis) else {
                            // No error on purpose.
                            continue;
                        };

                        // Extract derives from this derive macro. It could also be aliases, too.
                        //
                        // #[derive(Clone, Copy)]
                        //          ^^^^^^^^^^^
                        let derives = extract_derives(derive_stream, compile_errors);

                        // this:
                        //
                        // #[derive(Clone, Copy)]
                        //
                        // is equivalent to this:
                        //
                        // #[cfg_attr(true, derive(Clone, Copy))]
                        let cfg_predicate = TokenStream::from_iter([cfg_true()]);
                        regular_derives.push((cfg_predicate.clone(), derives.regular_derives));
                        for alias in derives.derive_aliases {
                            derive_aliases.push((cfg_predicate.clone(), alias));
                        }
                    }
                    // This is not a #[derive(..)], nor is it a #[cfg_attr(predicate, derive(..))].
                    // It is any other attribute, e.g. `#[doc]`
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

                        // Add back the entire attribute
                        //
                        // #[doc = "whatever"]
                        other_attrs.extend([attr_hash, TokenTree::Group(group)]);
                        continue;
                    }
                }
            }
            // The item has been reached, all attributes were processed
            //
            // #[attrs]
            // struct Item;
            // ^^^^^^
            _ => break,
        }
    }

    other_attrs
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
