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

// mod alias_map;
// mod codegen;
// mod dsl;

/// Separator in a path: `::`
#[core::prelude::v1::derive(Clone, Debug)]
struct PathSeparator {
    /// Span of the first `:`
    first: Span,
    /// Span of the second `:`
    second: Span,
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

fn generate_use_statements_string(alias: &str, paths: &HashSet<Path>) -> String {
    // We want to group each path by where it is located,
    // if we import a bunch of derives from `num_traits` then we want to group those
    // into a single `use`
    //
    // To do this we hash the path to the parent module/crate that contains the alias
    let mut grouped_by_parent = HashMap::new();

    let mut derive_contents = BTreeSet::new();

    for path in paths {
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

        let mut aliaseds = Vec::new();

        // Loop that parses the entire RHS
        //
        // Alias = ..Foo, Bar, baz::Baz;
        //         ^^^^^
        //                ^^^
        //                     ^^^^^^^^
        'parse_entities: loop {
            match tts.next() {
                // Parsing Alias expansion
                //
                // Alias = std::hash::Hash, ..Foo;
                //                          ^^^^^
                Some(TokenTree::Punct(punct)) if punct == '.' => {
                    last_span = punct.span();

                    expect_punct!('.');

                    let alias = expect_ident!();

                    aliaseds.push(Entity::Alias(alias));

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
                Some(TokenTree::Punct(colon)) if colon == ':' => {
                    last_span = colon.span();

                    let colon_colon = expect_punct!(':');

                    let path_start = expect_ident!();
                    let mut components = Vec::new();

                    // Parse every component of the path
                    loop {
                        let colon = match tts.next() {
                            Some(TokenTree::Punct(colon)) if colon == ',' => {
                                last_span = colon.span();
                                aliaseds.push(Entity::Derive(Path {
                                    leading_colon: (Some(PathSeparator {
                                        first: colon.span(),
                                        second: colon_colon.span(),
                                    })),
                                    first_component: path_start,
                                    components,
                                }));
                                break;
                            }
                            Some(TokenTree::Punct(colon)) if colon == ';' => {
                                last_span = colon.span();
                                aliaseds.push(Entity::Derive(Path {
                                    leading_colon: (Some(PathSeparator {
                                        first: colon.span(),
                                        second: colon_colon.span(),
                                    })),
                                    first_component: path_start,
                                    components,
                                }));
                                break 'parse_entities;
                            }
                            Some(TokenTree::Punct(colon)) if colon == ':' => {
                                last_span = colon.span();
                                colon
                            }
                            Some(tt) => {
                                last_span = tt.span();
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
                        };
                        last_span = colon.span();
                        let colon_colon = expect_punct!(':');
                        last_span = colon_colon.span();
                        let segment = expect_ident!();
                        components.push((
                            PathSeparator {
                                first: colon.span(),
                                second: colon_colon.span(),
                            },
                            segment,
                        ));
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

        nested_alias_map.insert(alias_name.to_string(), (alias_name.span(), aliaseds));
    }

    fn resolve_alias(
        nested_alias_map: &HashMap<String, (Span, Vec<Entity>)>,
        entities: &[Entity],
        derives: &mut HashSet<Path>,
        compile_errors: &mut TokenStream,
        alias_ref_use_stmts: &mut TokenStream,
    ) {
        for entity in entities {
            match entity {
                Entity::Alias(ident) => {
                    alias_ref_use_stmts.extend([
                        TokenTree::Ident(Ident::new("use", Span::call_site())),
                        TokenTree::Ident(Ident::new(
                            &format!("__derive_alias_{ident}"),
                            ident.span(),
                        )),
                        TokenTree::Ident(Ident::new("as", Span::call_site())),
                        TokenTree::Ident(Ident::new("_", Span::call_site())),
                        TokenTree::Punct(Punct::new(';', Spacing::Joint)),
                    ]);
                    let Some((_span, entities)) = nested_alias_map.get(&ident.to_string()) else {
                        compile_errors.extend(CompileError::new(ident.span(), "unknown Alias"));
                        continue;
                    };

                    // The entity is an alias, which expands to more entities.
                    // Resolve the alias by getting the path to its derives recursively
                    resolve_alias(
                        nested_alias_map,
                        entities,
                        derives,
                        compile_errors,
                        alias_ref_use_stmts,
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

        alias_use_stmts.extend([
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
            &mut compile_errors,
            &mut alias_ref_use_stmts,
        );

        flat_alias_map.insert(alias_name, (alias_name_span, flat_derives));
    }

    // Finally let's expand all of this to a bunch of invocations of the `__internal_new_alias!` macro
    let a = flat_alias_map
        .into_iter()
        .flat_map(|(alias, (_alias_span, derives))| {
            TokenStream::from_iter(
                derives
                    .iter()
                    .flat_map(|derive| {
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
                        ])
                    })
                    .chain([
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Ident(Ident::new("derive_aliases", Span::call_site())),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Punct(Punct::new(':', proc_macro::Spacing::Joint)),
                        TokenTree::Ident(Ident::new("__internal_new_alias", Span::call_site())),
                        TokenTree::Punct(Punct::new('!', proc_macro::Spacing::Joint)),
                        TokenTree::Group(Group::new(
                            Delimiter::Brace,
                            TokenStream::from_iter(
                                [
                                    TokenTree::Punct(Punct::new('#', Spacing::Joint)),
                                    TokenTree::Group(Group::new(
                                        Delimiter::Bracket,
                                        TokenStream::from_iter([
                                            TokenTree::Ident(Ident::new("doc", Span::call_site())),
                                            TokenTree::Punct(Punct::new('=', Spacing::Joint)),
                                            TokenTree::Literal(Literal::string(
                                                &generate_use_statements_string(alias, &derives),
                                            )),
                                        ]),
                                    )),
                                    TokenTree::Ident(Ident::new(
                                        &format!("__derive_alias_{alias}"),
                                        *_alias_span,
                                    )),
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
                                                    TokenTree::Punct(Punct::new(
                                                        ':',
                                                        proc_macro::Spacing::Joint,
                                                    )),
                                                    TokenTree::Punct(Punct::new(
                                                        ':',
                                                        proc_macro::Spacing::Joint,
                                                    )),
                                                    // first path segment
                                                    //
                                                    // ::std::hash::HashMap
                                                    //   ^^^
                                                    TokenTree::Ident(
                                                        derive.first_component.clone(),
                                                    ),
                                                ]
                                                .into_iter()
                                                .chain(derive.components.iter().flat_map(
                                                    |(_colon, component)| {
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
                                                    },
                                                )),
                                            ),
                                        )),
                                        TokenTree::Punct(Punct::new(
                                            ',',
                                            proc_macro::Spacing::Joint,
                                        )),
                                    ]
                                })),
                            ),
                        )),
                    ]),
            )
        })
        .chain(compile_errors)
        .chain(alias_use_stmts)
        .chain(alias_ref_use_stmts)
        .collect();

    a
}

// /// A `#[derive]` that supports derive aliases
// ///
// /// For example, the following code, which uses `#[derive_aliases::derive]` instead of `#[std::derive]`:
// ///
// /// ```ignore
// /// use derive_aliases::derive;
// ///
// /// #[derive(..Ord)]
// /// ```
// ///
// /// Will expand to this:
// ///
// /// ```ignore
// /// use derive_aliases::derive;
// ///
// /// #[std::derive(Ord, PartialOrd, Eq, PartialEq)]
// /// ```
// ///
// /// If `derive_aliases.rs` contains the following:
// ///
// /// ```ignore
// /// // Defines alias `..Eq` that expands to `PartialEq, Eq`
// /// Eq = PartialEq, Eq;
// /// // Defines alias `..ord` that expands to `PartialOrd, Ord, PartialEq, Eq`
// /// Ord = PartialOrd, Ord, ..Eq;
// /// ```
// ///
// /// Derive aliases are defined in file `derive_aliases.rs` next to the crate's `Cargo.toml`
// ///
// /// For more info, see the [crate-level](crate) documentation
// #[proc_macro_attribute]
// pub fn derive(attr: TokenStream, item: TokenStream) -> TokenStream {
//     expand_aliases(attr).into_iter().chain(item).collect()
// }

// /// Expand a list of derives and derive aliases into a list of derives
// ///
// /// At a high-level, the `derive_aliases::derive` proc macro attribute:
// ///
// /// - Takes a list of aliases (e.g. `..MyAlias`) and normal derives (e.g. `std::hash::Hash`)
// /// - All aliases are expanded into a list of real Rust derives. These derives are then directly
// ///   passed to `std::derive`
// ///
// /// # Exact algorithm
// ///
// /// We do 99.9% of the work **once** per the entire compilation session, as all aliases are stored in a `static`.
// ///
// /// The exact algorithm is the following:
// ///
// /// 1. The `derive_aliases.rs` file is [parsed](alias_map::parse) into a map of `Alias => Vec<AliasDeclaration>`
// ///
// ///    - Each `Alias` is just a `String` which can be referenced as `..Alias`
// ///    - Each `AliasDeclaration` creates an `Alias`, which references a list of (`Alias` or `Derive`).
// ///
// ///    The map is self-referential, at this stage
// ///
// /// 2. We flatten this map into a map of `Alias => Vec<Derive>`, resolving any aliases recursively.
// ///
// ///    Each `Derive` is then just a path to the real `#[proc_macro_derive(..)]` contained in Rust, and it will be passed
// ///    directly to `std::derive`
// ///
// ///    The `#[cfg]` predicates we encounter along the way are cumulated together, so each `Derive` contains a `Vec<Cfg>`
// ///    These predicates are passed to `#[cfg_attr(..)]` and Rust must evaluate them to `true` for them to be invoked
// ///
// /// 3. If we emitted code with the current data, we would generate a `#[std::derive]` for every single derive alias, which would be inefficient.
// ///    We want our proc macro to be as fast
// ///
// ///    We therefore resolve this map yet again into a map of `Alias => { Vec<Cfg> => Vec<Derive> }`, where `Derive`s that have the
// ///    same `Vec<Cfg>` are placed together, and we will emit just a single `#[cfg_attr]` for each entry in the map.
// ///
// /// 4. We're now done with 99.9% of the work, and we store the result in [`ALIAS_MAP`].
// ///
// ///    From here on, every invocation of `derive` will skip all of those steps and directly read from the `ALIAS_MAP` in order to convert
// ///    it's list of aliases and derives into a list of derives, which are all passed to `#[std::derive]`,
// ///
// /// 5. When we receive an alias `..Alias`, we get a map of `Vec<Cfg> => Vec<Derive>` from `ALIAS_MAP` and iterate over
// ///    the entries, generating a single `#[std::derive]` (or `#[cfg_attr(.., std::derive(..))]` if there is at least 1 `Cfg` predicate)
// ///    for each entry.
// ///
// /// # Notes
// ///
// /// - If aliases expands to a derive that was already the result of an alias expansion, then it is ignored. This allows us to define aliases
// ///   that have overlapping derives and then use them together
// ///
// /// - When we encounter any errors, we collect them into a bunch of `compile_error!(..)` invocations then report them all at once
// fn expand_aliases(input: TokenStream) -> TokenStream {
//     // Derives that come from expansion of aliases which were already seen
//     //
//     // We'll just ignore them. We allow using several derive aliases that expand to the same derives,
//     // by removing all duplicates
//     let mut seen_expanded = HashSet::new();

//     // Stuff we receive inside our attribute macro's input, aka everything inside of `#[derive(<--here-->)]`
//     let mut input = input.into_iter().peekable();

//     // These are all of the `#[cfg_attr(.., std::derive(..))]` attributes
//     // that we will generate.
//     let mut cfg_attr_derives = TokenStream::new();

//     // These are all arguments to the plain `#[std::derive(<-- here -->)]`s that doesn't require any `cfg`
//     let mut no_cfg_derives = TokenStream::new();

//     // These imports exist only for documentation. They look like this:
//     //
//     // use crate::derive_aliases_doc::SomeAlias as _;
//     //                                ^^^^^^^^
//     //
//     // The `SomeAlias` will contain documentation about what the alias `..SomeAlias` expands to.
//     // When user hovers over `..SomeAlias`, they will see the docs about it
//     let mut dummy_imports_for_docs = TokenStream::new();

//     // We collect all errors in a single stream so we can report them all at once,
//     // and rust-analyzer allows to continue us to work with other derives - even though we may have syntax errors somewhere
//     let mut compile_errors = TokenStream::new();

//     let (alias_map, errors) = ALIAS_MAP.get_or_init(|| {
//         let content = std::fs::read_to_string(&*crate::DERIVE_ALIASES_FILE).unwrap();

//         alias_map::generate_alias_map(&content, std::sync::Arc::new(PathBuf::new()))
//     });

//     // Report all parse errors as compile errors
//     compile_errors.extend(
//         errors
//             .iter()
//             .flat_map(|err| CompileError::new(Span::call_site(), err)),
//     );

//     while let Some(tt) = input.next() {
//         // We manually have to handle all `..Alias`es, but regular derives will be passed to `std::derive` verbatim
//         let TokenTree::Punct(ref punct) = tt else {
//             no_cfg_derives.extend([tt]);
//             continue;
//         };

//         if *punct != '.' {
//             // Do not add leading commas.
//             //
//             // This can happen when there is an alias at the beginning, but it is removed because it has `cfg`s
//             //
//             // e.g. from this:
//             //
//             // #[derive(..Eq, PartialEq)]
//             //
//             // To this:
//             //
//             // #[cfg_attr(feature = "eq", Eq, PartialEq)]
//             // #[derive(, PartialOrd)]
//             //          ^ this comma still exists, must be removed
//             //
//             if *punct == ',' && no_cfg_derives.is_empty() {
//                 // skip
//             } else {
//                 no_cfg_derives.extend([tt]);
//             }
//             continue;
//         }

//         let Some(punct_next) = input.next_if(|tt| {
//             if let TokenTree::Punct(punct) = tt {
//                 *punct == '.'
//             } else {
//                 false
//             }
//         }) else {
//             compile_errors.extend(CompileError::new(
//                 punct.span(),
//                 "after `.` we expect `.` followed by a derive alias, for example: `..Alias`",
//             ));
//             continue;
//         };

//         // At this point, we have a `..` so we can absolutely certain that it is an alias

//         // consume the identifier `Alias`
//         let Some(TokenTree::Ident(alias)) = input.next() else {
//             compile_errors.extend(CompileError::new(
//                 punct_next.span(),
//                 "after `..` we expect a derive alias, for example: `..Alias`",
//             ));
//             continue;
//         };

//         // Parsed `..Alias`.

//         let alias_string = Alias::new(alias.to_string());

//         // All derives that this alias will expand to
//         let Some(derives) = alias_map.get(&alias_string) else {
//             let all_aliases = format_list(alias_map.keys().map(|key| &key.0.name));

//             let most_similar_alias = most_similar_alias(&alias_string.0.name)
//                 .map(|similar| {
//                     format!(
//                         "did you mean: `{similar}`?",
//                         // "did you mean: `{similar}`? it expands to: {}\n\n",
//                         // format_list(alias_map.get(&Alias::new(similar)).expect(
//                         //     "it is an existing alias, we got it by iterating over the alias names"
//                         // ))
//                     )
//                 })
//                 .unwrap_or_default();

//             compile_errors.extend(CompileError::new(
//                 alias.span(),
//                 format!(
//                     "The alias `{alias_string}` is undefined.\n\n{most_similar_alias}All available aliases: {all_aliases}",
//                 ),
//             ));

//             continue;
//         };

//         for (cfg, derives) in derives {
//             if cfg.is_empty() {
//                 // These are stuff that goes inside of  `#[std::derive(<-- here -->)]` requiring no cfg
//                 // Doing it this way is necessary as we'll append regular derives that don't come from an alias
//                 // expansion into the same TokenStream. Also, it reduces how many tokens we output which makes the macro faster
//                 no_cfg_derives.extend(codegen::into_std_derive_arguments(
//                     derives,
//                     &mut seen_expanded,
//                 ));
//             } else {
//                 // A list of attributes, each one is inside of `#[cfg_attr(.., std::derive(..))]`
//                 cfg_attr_derives.extend(codegen::cfg_std_derive_attr(
//                     cfg,
//                     derives,
//                     &mut seen_expanded,
//                 ));
//             }
//         }

//         // dummy `use` simply to reference the `alias` in the generated doc module, for documentation on hover
//         // TODO: This span only covers the identifier part:
//         //
//         // ..Alias
//         //   ^^^^^
//         //
//         // What we actually want is a span that also covers the 2 dots:
//         //
//         // ..Alias
//         // ^^^^^^^
//         //
//         // This would be possible with `Span::join`, but it is unstable
//         let span = alias.span();

//         dummy_imports_for_docs.extend(codegen::use_underscore(
//             iter::once(TokenTree::Ident(alias)),
//             span,
//         ));
//     }

//     // We split into 2 parts because we don't want to generate a `#[std::derive(..)]` if there are no non-cfg derives
//     if no_cfg_derives.is_empty() {
//         chain![dummy_imports_for_docs, cfg_attr_derives, compile_errors].collect()
//     } else {
//         chain![
//             dummy_imports_for_docs,
//             cfg_attr_derives,
//             codegen::attr(codegen::std_derive(no_cfg_derives).collect()),
//             compile_errors
//         ]
//         .collect()
//     }
// }

// /// Intersperse list with commas and show it as a string
// fn format_list<'a>(list: impl IntoIterator<Item = &'a String>) -> String {
//     list.into_iter()
//         .enumerate()
//         .flat_map(|(i, key)| {
//             // intersperse
//             if i == 0 {
//                 vec![key.as_str()]
//             } else {
//                 vec![", ", key.as_str()]
//             }
//         })
//         .collect::<String>()
// }

// /// Used in error messages to make good suggestions
// fn most_similar_alias(_alias: impl AsRef<str>) -> Option<String> {
//     None
//     // ALIAS_MAP
//     //     .0
//     //     .keys()
//     //     .map(|it| {
//     //         (
//     //             it.0.name.clone(),
//     //             strsim::normalized_damerau_levenshtein(&it.0.name, alias.as_ref()),
//     //         )
//     //     })
//     //     .max_by(|a, b| a.1.total_cmp(&b.1))
//     //     // if the 2 strings are not that similar, we don't have the most similar alias
//     //     //
//     //     // e.g. if we have "Foo", "Bar" and we input "Fooo" the most similar will be "Foo".
//     //     // If we remove "Foo", the most similar will be "Bar". But "Foo" and "Bar" are not similar at all, so
//     //     // we just ignore if we can't find above a certain similarity threshold
//     //     .filter(|it| it.1 >= 0.70)
//     //     .map(|it| it.0)
// }

// /// When the `derive_aliases::define!` macro is called, it processes the received
// /// `TokenStream` and then dumps the tokens into this file.
// ///
// /// The first call of the `derive_aliases::derive!` macro will then parse contents of this file
// /// into an in-memory data structure [`ALIAS_MAP`], which contains a mapping from aliases to derives.
// ///
// /// All other calls to `derive_aliases::derive!` will read directly from the efficient `ALIAS_MAP`
// /// and will not touch this static.
// ///
// /// Extra weird: `derive_aliases::define!` itself reads from the file to generate documentation for
// /// all of the derive aliases. This obviously must be optional - if this variable is not set, the
// /// `derive_aliases::derive!` macro must compile (in order to create it in the first place!)
// static DERIVE_ALIASES_FILE: LazyLock<PathBuf> = LazyLock::new(|| {
//     // HACK: Cargo does not expose `OUT_DIR` and hacky methods of retrieving it (e.g. via `std::env::args()`)
//     // don't always work. So, what we do is create a unique filename in the temporary directory
//     //
//     // Every crate which depends on `derive_aliases` MUST have a unique file name that doesn't clash with
//     // each other. If not, then bugs will happen: if 2 crates have the same `DERIVE_ALIASES_FILE` then
//     // this derive macro will read from the same file for both of them, and compilation will fail
//     //
//     // To guarantee that this can never happen, we use `CARGO_MANIFEST_DIR` because it is unique for each crate
//     std::env::temp_dir().join(format!(
//         "derive_aliases_{}",
//         std::env::var("CARGO_MANIFEST_DIR")
//             .expect("expected env variable `CARGO_MANIFEST_DIR` to be defined")
//             // replace all path separators so we don't refer to directories
//             .replacen(std::path::MAIN_SEPARATOR, "_", usize::MAX)
//     ))
// });

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
