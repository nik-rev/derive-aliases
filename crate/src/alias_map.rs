//! Expanding these derive aliases with such a definition:
//!
//! ```
//! Again = Hash, Eq;
//!
//! #[cfg(C)]
//! Bar = #[cfg(C)] Bar, #[cfg(D)] ..Again;
//!
//! Alias = Foo, #[cfg(D)] ..Bar;
//! ```
//!
//! Will give us an `FlatAliasMap` like this:
//!
//! ```rust
//! FlatAliasMap {
//!     Again => Hash, Eq;
//!     Bar => #[cfg(all(C, C))] Bar, #[cfg(all(C, D))] Hash, #[cfg(all(C, D))] Eq
//!                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!                                   expansion of ..Again
//!     Alias => Foo, #[cfg(all(D, C, C))] Bar, #[cfg(all(D, C, D))] Hash, #[cfg(all(D, C, D))] Eq
//!                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!                   expansion of ..Bar
//!                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//!                                          expansion of ..Again
//! }
//! ```
//!
//! Which we then turn into an `AliasMap` by sorting, de-duplicating and grouping all CFGs with the appropriate derives
//!
//! ```rust
//! AliasMap {
//!     Again => DerivesGroupedByCfgs {
//!         #[cfg(all())] => Hash, Eq
//!     };
//!     Bar => DerivesGroupedByCfgs {
//!         // removed duplicate 'C' in the same
//!         #[cfg(C)] => Bar
//!         // put 2 equivalent derives into the same bucket
//!         #[cfg(all(C, D))] => Hash, Eq
//!     }
//!     Alias =>. DerivesGroupedByCfgs {
//!         #[cfg(all())] => Foo
//!         // removed duplicate "C, C" => "C"
//!         #[cfg(all(D, C))] => Bar
//!         #[cfg(all(D, C, D))] => Hash, Eq
//!     }
//! }
//! ```
//!
//! Which then generates the following code, if we apply `#[derive(..Alias)]`:
//!
//! ```
//! #[std::derive(Foo)]
//! #[cfg_attr(all(D, C), std::derive(Bar))]
//! #[cfg_attr(all(D, C, D), std::derive(Hash, Eq))]
//! ```

use std::{collections::HashMap, iter, path::Path, sync::LazyLock};

use crate::{
    dsl::{self, parse_single_file, render_error, Alias, AliasDeclaration, ParseError, Stmt},
    format_list, most_similar_alias,
};

type DerivesGroupedByCfgs = HashMap<Vec<dsl::Cfg>, Vec<dsl::Derive>>;
type FlatAliasMap = HashMap<dsl::Alias, Vec<(Vec<dsl::Cfg>, dsl::Derive)>>;
type AliasMap = HashMap<dsl::Alias, DerivesGroupedByCfgs>;

/// Map from `Alias => Vec<Derive>`, where `Alias` expands to a bunch of derive macros (represented as plain strings).
///
/// We don't store the `TokenTree`s directly because they are `!Sync`
pub static ALIAS_MAP: LazyLock<(AliasMap, Vec<String>)> = LazyLock::new(|| {
    #[cfg(feature = "workspace")]
    let Ok(dir) = std::env::var("CARGO_WORKSPACE_DIR") else {
        return (HashMap::default(), vec![String::from(concat!(
            "\n\n`CARGO_WORKSPACE_DIR` environment variable must be set, which points to the directory containing the\n",
            "workspace `Cargo.toml`. Since cargo currently doesn't set this variable, in your workspace create `.cargo/config.toml` with contents:\n",
            "\n",
            "[env]\n",
            "CARGO_WORKSPACE_DIR = {{ value = \"\", relative = true }}\n",
        ))]);
    };

    let path = std::path::Path::new(&dir).join("derive_aliases.rs");

    let content = match std::fs::read_to_string(&path) {
        Ok(content) => content,
        Err(err) => {
            return (
                HashMap::default(),
                vec![format!(
                    concat!(
                        "expected {} to exist and contain derive aliases. error: {}.\nhere's an ",
                        "example of syntax in `derive_aliases.rs` file:\n\n{}\n"
                    ),
                    path.display(),
                    err,
                    dsl::EXAMPLE
                )],
            );
        }
    };

    // Recursive map of `Alias => (..Alias OR Derive)`, where `Derive` is a "leaf" but an `..Alias` can
    // be expanded into a list of (..Alias OR Derive)
    let (nested_alias_map, mut errors) = parse(&content, &path);

    // Let's resolve the map so each alias maps exactly to a list of derives

    // When we first parse all of the files, we have a deep nested, recursive tree of aliases to (aliases or derives)
    //
    // We want to resolve this tree recursively, so we essentially have a nice, flat map of
    //
    // Alias => Vec<Derive>
    //
    // Where none of the `Derive`s are aliases themselves.
    //
    // In order to do this, we iterate over all of the aliases and inline all RHS recursively until we can't
    // expand any of the `..Alias`ses anymore
    //
    // Additionally, we keep track of all `#[cfg]`s and add them to a `Vec<Cfg>`, so we end up storing
    // (Vec<Cfg>, Derive) as all of the `Cfg`s are additive
    let mut flat_alias_map = HashMap::<dsl::Alias, Vec<(Vec<dsl::Cfg>, dsl::Derive)>>::new();

    for (_, alias_decl) in &nested_alias_map {
        let cfgs = alias_decl.cfg.iter();
        let mut derives = vec![];

        // We keep a stack of aliases as we expand them, for better error messages
        let mut alias_stack = vec![];

        resolve_derives_for_alias(
            &alias_decl,
            &nested_alias_map,
            cfgs.cloned(),
            &mut derives,
            &mut errors,
            &content,
            &path.to_string_lossy(),
            &mut alias_stack,
        );

        flat_alias_map.insert(alias_decl.alias.clone(), derives);
    }

    // Now that we've got a flat map, it's not yet optimized for efficiency. Every `Derive` is associated with
    // multiple CFGs, and if we generated tokens as-is, we would be outputting an entire `#[cfg_attr]` for every single derive macro!
    //
    // Lets make a `Map<Cfgs => Derives>` that allows us to pick similar CFGs and put them all under a single `#[cfg_attr]`.
    // We want to try and group similar CFGs. E.g., group features `A, A, B` and `B, A, B` under a single `A, B` via sort-then-dedup
    //
    // This won't catch everything. For example, we can still have `#[cfg(all(feature = "serde", feature = "arbitrary"))]`
    // and `#[cfg(all(feature = "arbitrary", feature = "serde"))]` on the same item, which will put them into 2 different buckets.
    //
    // There's nothing we can do about this, because we don't parset the `cfg` expressions to find out what their meaning is semantically,
    // and I don't believe there would be any benefit at all in doing this, if not things getting even worse in terms of compile speeds

    let mut alias_to_grouped_derives = AliasMap::new();

    for (alias, derives) in flat_alias_map {
        // Group derives under similar CFGs
        let mut grouped_derives = DerivesGroupedByCfgs::new();

        for (mut cfgs, derive) in derives {
            // Let's sort all of our cfgs, then de-duplicating will be faster
            //
            // Also we know that `#[cfg(all(B, A))]` and `#[cfg(all(A, B))]` are the same, so we want to sort in order to
            // have both of those have the same hash
            cfgs.sort();

            // Now that we've sorted the above, we'll have this:
            //
            // `#[cfg(all(A, B))]` and `#[cfg(all(A, B))]`
            //
            // Let's remove duplicate CFGs to reduce how many tokens each invocation of `#[derive]` needs to output yet again
            cfgs.dedup();

            grouped_derives
                .entry(cfgs)
                .and_modify(|it| {
                    it.push(derive);
                })
                .or_default();
        }

        alias_to_grouped_derives.insert(alias, grouped_derives);
    }

    (alias_to_grouped_derives, errors)
});

fn resolve_derives_for_alias(
    // current alias
    alias_decl: &dsl::AliasDeclaration,
    // all aliases and their derives
    alias_map: &HashMap<dsl::Alias, dsl::AliasDeclaration>,
    mut current_cfg_stack: impl Iterator<Item = dsl::Cfg>,
    // derives for the current alias
    derives: &mut Vec<(Vec<dsl::Cfg>, dsl::Derive)>,
    errors: &mut Vec<String>,
    file: impl AsRef<str>,
    source: impl AsRef<str>,
    alias_stack: &mut Vec<dsl::Alias>,
) {
    // Iterate over all aliased items for this alias
    //
    // Alias = Foo, Bar, ..Baz  .next()
    //         ^^^              .next()
    //              ^^^         .next()
    //                   ^^^^^
    for aliased in iter::once(&alias_decl.aliased.first)
        .chain(alias_decl.aliased.items.iter().map(|(_, item)| item))
    {
        // We maintain a stack of `cfgs` which get progressively larger as we expand more and more aliases
        // This is necessay because all `cfg`s are additive
        let new_cfgs = current_cfg_stack.by_ref().chain(aliased.cfg.clone());

        match &aliased.item {
            // We insert a Derive. It is the "leaf". There is nothing else to expand.
            dsl::AliasedItem::Derive(derive) => {
                derives.push((new_cfgs.collect(), derive.clone()));
            }
            // An alias expansion. like `..Alias`. Expand by recursing 1 layer deeper, with a new layer of CFGs
            dsl::AliasedItem::AliasExpansion(alias_expansion) => {
                alias_stack.push(alias_expansion.alias.clone());
                if alias_expansion.alias == alias_decl.alias {
                    let alias_stack_formatted = alias_stack
                        .iter()
                        .rev()
                        .map(|alias| format!("- {}", alias.0.name))
                        .collect::<String>();

                    // ERROR: Alias recursion
                    errors.push(dsl::render_error(
                        &dsl::ParseError {
                            span: alias_decl.span.clone(),
                            message: format!(
                                "you cannot use an alias `..{}` inside of itself, that would lead to infinite recursion!\n\nstack of aliases:\n\n{alias_stack_formatted}",
                                alias_decl.alias.0.name
                            ),
                        },
                        &file,
                        &source,
                    ));
                    continue;
                }

                let Some(deeper_alias_decl) = alias_map.get(&alias_expansion.alias) else {
                    let alias_stack_formatted = alias_stack
                        .iter()
                        .rev()
                        .map(|alias| format!("- {}", alias.0.name))
                        .collect::<String>();

                    // ERROR: Alias not found.
                    errors.push(dsl::render_error(
                        &dsl::ParseError {
                            span: alias_decl.span.clone(),
                            message: format!(
                                "alias `{}` is undefined!\n\n{}available aliases: {}\n\nstack of aliases:\n\n{alias_stack_formatted}",
                                alias_expansion.alias.0.name,
                                most_similar_alias(&alias_expansion.alias.0.name)
                                    .map(|f| format!("help: did you mean: {f}\n\n"))
                                    .unwrap_or_default(),
                                format_list(alias_map.iter().map(|(a, _)| &a.0.name))
                            ),
                        },
                        &file,
                        &source,
                    ));
                    continue;
                };

                resolve_derives_for_alias(
                    deeper_alias_decl,
                    alias_map,
                    new_cfgs.into_iter(),
                    derives,
                    errors,
                    &file,
                    &source,
                    alias_stack,
                );
            }
        }
    }
}

/// Parse derive aliases file at `path` with `content`, and returns the result with all paths resolved.
/// The `Vec<String>` return is all parse errors encountered whilst parsing.
pub fn parse(
    content: impl AsRef<str>,
    path: impl AsRef<Path>,
) -> (HashMap<Alias, AliasDeclaration>, Vec<String>) {
    fn inner(
        content: &str,
        path: impl AsRef<Path>,
        alias_declarations: &mut HashMap<Alias, AliasDeclaration>,
        errors: &mut Vec<String>,
    ) {
        let file = parse_single_file(&content, path);

        for error in file.errors {
            render_error(&error, file.span.file.to_string_lossy(), content);
        }

        for stmt in file.stmts {
            match stmt {
                Stmt::AliasDeclaration(alias_declaration) => {
                    alias_declarations.insert(alias_declaration.alias.clone(), alias_declaration);
                }
                Stmt::Import(import) => {
                    match std::fs::read_to_string(&*import.path) {
                        Ok(file) => inner(&file, &*import.path, alias_declarations, errors),
                        Err(err) => {
                            errors.push(render_error(
                                &ParseError {
                                    span: import.span,
                                    message: format!(
                                        "failed to read file at {}: {err}",
                                        import.path.display()
                                    ),
                                },
                                file.span.file.to_string_lossy(),
                                content,
                            ));
                            continue;
                        }
                    };
                }
            }
        }
    }

    let mut errors = vec![];
    let mut alias_declarations = HashMap::new();

    // we need to return a MAP here

    inner(
        content.as_ref(),
        path.as_ref(),
        &mut alias_declarations,
        &mut errors,
    );

    (alias_declarations, errors)
}
