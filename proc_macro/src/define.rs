use crate::cfg_true;
use crate::generate_documentation_for_alias;
use crate::tokens;
use crate::tokens::IntoTokens;
use crate::CompileError;
use crate::PathSeparator;
use proc_macro::{Delimiter, Group, Ident, Punct, Span, TokenStream, TokenTree};
use proc_macro::{Literal, Spacing};
use std::collections::HashMap;
use std::collections::HashSet;
use tokens::Path;
use tokens::TokensIter;

pub fn define(tts: TokenStream) -> TokenStream {
    // Whole input to the macro
    let mut ts = TokensIter {
        stream: tts.into_iter().peekable(),
        span: Span::call_site(),
    };

    // Compile errors to report all at once
    let mut compile_errors = Vec::new();

    // Parse #![export_derive_aliases], if it exists
    let visibility_mode = VisibilityMode::parse(&mut ts, &mut compile_errors);

    // #[doc = "..."]
    // #[allow(non_camel_case_types)]
    // struct export_derive_aliases;
    let visibility_mode_tokens = visibility_mode.into_tokens();

    // Nested Alias Map:
    //
    // Alias => Derive OR Alias
    //
    // It's nested because each alias expands to a bunch of aliases+derives,
    // so it is recursive
    let nested_alias_map = NestedAliasMap::new(&mut ts, &mut compile_errors);

    // All of these are just `use ... as _` the only reason we have them
    // is to get access to the `Span` of whatever item they import, which we'll
    // use in documentation to get nice docs-on-hover and goto-definition
    //
    let mut dummy_use_stmts = DummyUseStmts(visibility_mode_tokens);

    // This is a map `Alias => ResolvedDerive`, with all aliases
    // knowing the exact set of derives they expand into (+ 'extern' aliases that
    // will be figured out by the Rust compiler)
    let mut resolved_aliases = Vec::new();

    // Resolve every single alias
    for nested_alias in &nested_alias_map {
        // A single alias. Done. We've resolved all of the actual aliases it points to
        resolved_aliases.push(ResolvedAlias::new(
            &nested_alias_map,
            &mut dummy_use_stmts,
            nested_alias,
        ));
    }

    // Finally let's expand all of this to a bunch of invocations of the `__internal_derive_aliases_new_alias!` macro
    resolved_aliases
        .into_iter()
        // Every alias is folded to become
        .flat_map(|resolved_alias| resolved_alias.into_tokens(visibility_mode))
        // Compile errors, which we report all at once
        .chain(compile_errors.into_iter().flat_map(IntoTokens::into_tokens))
        // Add all of the dummy use statements
        .chain(dummy_use_stmts.into_tokens())
        .collect()
}

struct NestedAlias<'a> {
    alias: Ident,
    entities: &'a [Entity],
}

struct NestedAliasMap(HashMap<String, (Span, Vec<Entity>)>);

impl<'a> IntoIterator for &'a NestedAliasMap {
    type Item = NestedAlias<'a>;

    type IntoIter = NestedAliasIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        NestedAliasIter(self.0.iter())
    }
}

struct NestedAliasIter<'a>(std::collections::hash_map::Iter<'a, String, (Span, Vec<Entity>)>);

impl<'a> Iterator for NestedAliasIter<'a> {
    type Item = NestedAlias<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(alias_name, (alias_span, entities))| NestedAlias {
                alias: Ident::new(alias_name, *alias_span),
                entities: entities.as_slice(),
            })
    }
}

impl NestedAliasMap {
    fn new(ts: &mut TokensIter, compile_errors: &mut Vec<CompileError>) -> Self {
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
                                ts.compile_error(
                                    "expected identifier after `..` to form `..Alias`",
                                ),
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
                            compile_errors.push(ts.compile_error(
                                "expected `:` to form a path like `::std::hash::Hash`",
                            ));
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

        Self(nested_alias_map)
    }
}

struct DummyUseStmts(TokenStream);

impl DummyUseStmts {
    /// Insert a dummy "use" statement, which only exists because we
    /// need a physical token of the ident's span, so we can associate
    /// the token's span to the user's input in `define!`
    ///
    /// use crate::derive_alias::Foo as _;
    fn insert_ident(&mut self, ident: Ident) {
        self.0.extend([
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
    }

    fn insert_path(&mut self, path: Path) {
        let underscore_span = path
            .components
            .last()
            .map_or_else(|| path.first_component.span(), |(_, ident)| ident.span());

        self.0.extend(
            [
                // use ::std::hash::Hash as _;
                // ^^^
                TokenTree::Ident(Ident::new("use", Span::call_site())),
            ]
            .into_iter()
            // use ::std::hash::Hash as _;
            //     ^^^^^^^^^^^^^^^^^
            .chain(path.into_tokens())
            // use ::std::hash::Hash as _;
            //                       ^^^^^
            .chain([
                // use ::std::hash::Hash as _;
                //                       ^^
                TokenTree::Ident(Ident::new("as", Span::call_site())),
                // use ::std::hash::Hash as _;
                //                          ^
                TokenTree::Ident(Ident::new("_", underscore_span)),
                // use ::std::hash::Hash as _;
                //                           ^
                TokenTree::Punct(Punct::new(';', Spacing::Joint)),
            ]),
        )
    }

    fn into_tokens(self) -> TokenStream {
        TokenStream::from_iter([
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
            TokenTree::Group(Group::new(Delimiter::Brace, self.0)),
            TokenTree::Punct(Punct::new(';', Spacing::Joint)),
        ])
    }
}

/// Resolves all entities of an alias into a flat list of derives.
///
/// # Example
///
/// Here we are resolving the `Ord` alias.
///
/// - The derives are inserted into `derives`. (in this example - `PartialOrd`, `Ord`)
/// - Extern aliases, aliases defined by other `define!` invocations (not this one) are
///   added into `extern_aliases`. (in this example - `Eq`)
///
/// ```ignore
/// derive_aliases::define! {
///     Copy = ::core::marker::Copy, ::core::clone::Clone;
///     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
///             ^^    ^^^^    ^^^^^^^^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^ entities
/// }
/// ```
fn resolve_alias(
    nested_alias_map: &NestedAliasMap,
    entities: &[Entity],
    derives: &mut HashSet<Path>,
    dummy_use_stmts: &mut DummyUseStmts,
    extern_aliases: &mut Vec<Ident>,
) {
    for entity in entities {
        match entity {
            Entity::Alias(ident) => {
                // Maybe this alias has been created inside of this `define!` call (it is NOT an extern alias)
                let Some((_span, entities)) = nested_alias_map.0.get(&ident.to_string()) else {
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
                dummy_use_stmts.insert_ident(ident.clone());

                // The entity is an alias, which expands to more entities.
                // Resolve the alias by getting the path to its derives recursively
                resolve_alias(
                    nested_alias_map,
                    entities,
                    derives,
                    dummy_use_stmts,
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

/// Visibility mode defines who can see derive aliases
///
/// It is a hack that is a necessary evil due to there being no way to export a `macro_rules!` macro
/// from the crate *without* using `#[macro_export]` on it
#[derive(Copy, Clone)]
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
    pub fn into_tokens(self) -> TokenStream {
        match self {
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
                                TokenTree::Ident(Ident::new(
                                    "non_camel_case_types",
                                    Span::call_site(),
                                ))
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
        }
    }

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

/// A single entity that appears on the RHS of an alias declaration
#[derive(Clone, Debug)]
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

/// Represents a **resolved** derive alias, where we know exactly what derives
/// this alias expands to.
///
/// ```ignore
/// derive_aliases::define! {
///     Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
/// }
/// derive_aliases::define! {
///     Copy = ::core::marker::Copy, ::core::clone::Clone;
///     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
/// }
/// ```
///
/// For the `Ord` alias above:
///
/// - flat_derives        =    ::core::marker::Copy, ::core::clone::Clone, ::core::cmp::PartialOrd, ::core::cmp::Ord
/// - extern_aliases      =    Eq
struct ResolvedAlias {
    /// The alias identifier itself.
    ///
    /// ```ignore
    /// derive_aliases::define! {
    ///     Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
    /// }
    /// derive_aliases::define! {
    ///     Copy = ::core::marker::Copy, ::core::clone::Clone;
    ///     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
    ///     ^^^
    /// }
    /// ```
    alias: Ident,
    /// A set of flat derives, that just expand as-is.
    ///
    /// In that example, this is `Copy`, `Clone`, `PartialOrd`, and `Ord`:
    ///
    /// ```ignore
    /// derive_aliases::define! {
    ///     Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
    /// }
    /// derive_aliases::define! {
    ///     Copy = ::core::marker::Copy, ::core::clone::Clone;
    ///            ^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^
    ///     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
    ///                         ^^^^^^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^
    /// }
    /// ```
    flat_derives: HashSet<Path>,
    /// All external aliases that were not defined in this invocation of `define!`.
    ///
    /// In that example, this is `Eq`:
    ///
    /// ```ignore
    /// derive_aliases::define! {
    ///     Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
    ///     ^^
    /// }
    /// derive_aliases::define! {
    ///     Copy = ::core::marker::Copy, ::core::clone::Clone;
    ///     Ord = ..Eq, ..Copy, ::core::cmp::PartialOrd, ::core::cmp::Ord;
    /// }
    /// ```
    extern_aliases: Vec<Ident>,
}

impl ResolvedAlias {
    fn new(
        nested_alias_map: &NestedAliasMap,
        dummy_use_stmts: &mut DummyUseStmts,
        nested_alias: NestedAlias,
    ) -> Self {
        // A flat list of derives that the alias expands to
        let mut flat_derives = HashSet::new();

        // "external" aliases that were NOT defined by this macro,
        // we'll just nest the aliases then call the `__internal_derive_aliases_new_alias_with_externs` macro to de-duplicate
        let mut extern_aliases = Vec::new();

        // use crate::derive_alias::Foo as _;
        //
        // We do this so we get documentation when hovering over the alias
        dummy_use_stmts.insert_ident(nested_alias.alias.clone());

        // Resolve the alias and all of its entities into a flat list of derives
        resolve_alias(
            nested_alias_map,
            nested_alias.entities,
            &mut flat_derives,
            dummy_use_stmts,
            &mut extern_aliases,
        );

        for derive in &flat_derives {
            // use ::std::hash::Hash as _;
            //     ^^^^^^^^^^^^^^^^^ then we take this span and associate it with
            //                       what the user wrote in `define!` call, to
            //                       get documentation on hover
            dummy_use_stmts.insert_path(derive.clone());
        }

        ResolvedAlias {
            alias: nested_alias.alias,
            flat_derives,
            extern_aliases,
        }
    }

    fn into_tokens(mut self, visibility_mode: VisibilityMode) -> TokenStream {
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
            TokenTree::Ident(self.alias.clone()),
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
        .chain(self.flat_derives.iter().flat_map(|derive| {
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
        if let Some(last_extern_alias) = self.extern_aliases.pop() {
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
                            self.extern_aliases.into_iter().fold(
                                TokenStream::from_iter(input_into_new_alias_macro),
                                |acc, alias| {
                                    TokenStream::from_iter([
                                        // crate::derive_alias::Ord! {% [crate::derive_alias::Eq,[a $ Foo! [ { cfg } ::core::hash::Hash], [ { cfg } ::core::fmt::Debug],] ] }
                                        //                               ^^^^^
                                        TokenTree::Ident(Ident::new("crate", Span::call_site())),
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
                            &generate_documentation_for_alias(
                                &self.alias.to_string(),
                                &self.flat_derives,
                            ),
                        ))]
                        .into_iter()
                        // ::derive_aliases::__internal_derive_aliases_new_alias! { "..." a $ Eq! [ { cfg } ::core::cmp::PartialEq], [ { cfg } ::core::cmp::Eq], }
                        //                                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                        .chain(input_into_new_alias_macro),
                    ),
                )),
            ])
        }
    }
}
