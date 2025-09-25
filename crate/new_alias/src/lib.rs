//! Implementation details for the `derive_aliases` crate
#![allow(clippy::crate_in_macro_def)]

#[doc(inline)]
pub use derive_aliases_impl::{define, derive};

/// # Glyphs
///
/// - `%`: Inject this alias's expansion into another `__internal_new_alias` call
///
/// With these aliases:
///
/// The following aliases:
///
/// ```
/// derive_aliases::define! {
///     Copy = Copy, Clone, Eq;
///     Eq = Eq, PartialEq, std::hash::Hash;
///     Ord = Ord, PartialOrd, ..Eq, std::hash::Hash;
/// }
/// ```
///
/// Expand to the following invocations of `__internal_new_alias!`:
///
/// ```ignore
///  __internal_new_alias!(
///      "a string which documents alias `Copy`" __derive_alias_Copy $ Copy! [Copy], [Clone], [Eq],
///  );
///  __internal_new_alias!(
///      "a string which documents alias `Eq`" __derive_alias_Eq $ Eq! [Eq], [PartialEq], [std::hash::Hash],
///  );
///  __internal_new_alias!(
///      "a string which documents alias `Ord`" __derive_alias_Ord $ Ord! [Ord], [PartialOrd], [Eq], [PartialEq], [std::hash::Hash],
///  );
/// ```
///
/// Each invocation expands to a macro after the `$`. This macro **is the real derive alias**.
///
/// Derive alias `macro_rules!` has the following properties:
///
/// - They can be composed with another derive alias
/// - Composing them means merging the **sets** of derives that they expand to.
///   What this means is 2 aliases that share derives they expand to will **merge** together.
///
/// ```
/// #[derive_aliases::derive(..Ord, ..Eq, ..Copy, Debug)]
/// struct Foo;
/// ```
///
/// Expands to this:
///
/// ```rs
/// crate::derive_alias::Ord! ( crate::derive_alias::Eq,(crate::derive_alias::Copy,(@ [Debug,] [struct Foo;])) [] );
/// ```
///
/// # Notes
///
/// - In order to **merge** sets of derives, we don't use the `:path` specifier since it cannot be compared to another `:path`.
///   Instead, we store paths to derives as `[$(:tt)*]` which we CAN compare with one another
///
/// Creating stacked aliases looks like this:
///
/// ```
/// Ord! (%
///     [Ord,
///         [Ord,
///             [
///                 "Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],
///             ]
///         ]
///     ]
/// );
/// ```
#[doc(hidden)]
#[macro_export]
macro_rules! __internal_new_alias {
    (
        $docs:literal
        $real_name:ident
        // This is simply a `$` to create the inner Alias `macro_rules!`
        $_:tt
        // Name of the Alias we are creating
        $NAME:ident!
        // Derives that this alias expands to
        //
        // Each derive is inside of `[...]` because we want to compare them for equality,
        // which cannot be done for meta-variables with the `path` specifier
        $(
            [$($derives:tt)*]
        ,)*
    ) => {
        #[doc = $docs]
        #[macro_export]
        #[doc(hidden)]
        macro_rules! $real_name {
            ///////////////////////////////////////////////////////////////

            // Ord! { #Eq, (Copy, (@ [Debug, ] [struct Foo;])), [] [] }
            // PROCESSING COMPLETED. DELEGATE TO INNER ALIAS.
            //
            // We have removed all existing derives that COULD
            // conflict with derives coming from the expansion of the
            // CURRENT alias.
            //
            // Now let's forward to the inner alias to process further
            (
                // Inner alias and all arguments to it
                # $_ Alias:path, ($_($_ pass:tt)*)
                // EMPTY = we have processed all derives
                []
                // list of paths
                [$_(
                    // a single path
                    [ $_ ($_ deduplicated:tt)* ],
                )*]
            ) => {
                // Expand the inner alias
                $_ Alias! {
                    // Call insides of the macro
                    $_($_ pass)*

                    // Add derives at the end of the list which was de-duplicated
                    [
                        // Derives that were already existing, but filtered to EXCLUDE
                        // any derives for THIS alias
                        $_(
                            // a single path to a derive
                            [$_($_ deduplicated)*],
                        )*

                        // All the derives for THIS alias
                        $(
                            // a single path to a derive
                            [$($derives)*],
                        )*
                    ]
                }
            };

            //
            // Remove each derive from the set
            $(
                // Remove a single CURRENT derive from the set
                (
                    // next alias and arguments
                    # $_ Alias:path, $_ pass:tt

                    [
                        // This STARTS WITH the derive currently being processed
                        [ $($derives)* ],
                        // rest of the paths
                        $_ (
                            // a single path
                            [ $_ ($_ rest:tt)* ],
                        )*
                    ]

                    // list of paths
                    [
                        $_ (
                            // a single path
                            [ $_ ($_ deduplicated:tt)* ],
                        )*
                    ]
                ) => {
                    crate::derive_alias::$NAME! {
                        // next alias and arguments
                        # $_ Alias, $_ pass

                        [$_ (
                            [$_($_ rest)*],
                        )*]

                        [$_ (
                            [ $_ ($_ deduplicated)* ],
                        )*]
                    }
                };
            )*

            // Everything else is just added as-is
            (
                # $_ Alias:path, $_ pass:tt
                [
                    // the first path
                    [ $_($_ first:tt)* ],
                    // rest of the paths
                    $_(
                        // a single path
                        [ $_ ($_ rest:tt)* ],
                    )*
                ]
                // list of paths
                [
                    $_ (
                        // a single path
                        [ $_ ($_ deduplicated:tt)* ],
                    )*
                ]
            ) => {
                crate::derive_alias::$NAME! {
                    # $_ Alias, $_ pass

                    // process rest of the list
                    [
                        $_ (
                            [$_($_ rest)*]
                        ,)*
                    ]

                    [
                        // existing de-duplicated paths
                        $_(
                            [$_($_ deduplicated)*],
                        )*

                        // add last path to the end, we know it can't be duplicated
                        [ $_($_ first)* ],
                    ]
                }
            };

            ///////////////////////////////////////////////////////////////

            // Now that we've removed the traits we want to add, Add them.
            // This guarantees there is NO duplicate of them here
            //
            // Copy! { ? [Debug,][struct Foo;] [] [[Ord], [PartialOrd], [PartialEq],] }
            (?
                [
                    $_($_ regular_derives:tt)*
                ]

                [
                    $_($_ item:tt)*
                ]

                // FINISHED = processed all derives, none left
                []

                // list of paths
                [$_ (
                    // a single path
                    [ $_ deduplicated:path ],
                )*]
            ) => {
                #[::core::prelude::v1::derive(
                    // All derives that did not come from an expansion
                    $_(
                        $_ regular_derives
                    )*
                    // Derives that were de-duplicated for THIS alias
                    $_(
                        $_ deduplicated,
                    )*
                    // Derives that come as a result of expansion of THIS alias
                    $(
                        $($derives)*,
                    )*
                )]

                // the item we are applying the derives to
                $_ ($_ item) *

                // compile_error!(stringify!($_($_ regular_derives )* $_($_ deduplicated)* $($derives)*));

                // const NAMES: &str = stringify!($_($_ regular_derives )* $_($_ deduplicated)* $($derives)*);
            };
            // Remove each derive from the set
            $(
                (?
                    $regular_derives:tt
                    $item:tt
                    [
                        // the first path
                        [ $($derives)* ],
                        // rest of the paths
                        $_ (
                            // a single path
                            [ $_ ($_ rest:tt)* ],
                        )*
                    ]
                    // list of paths
                    [$_(
                        // a single path
                        [ $_ ($_ deduplicated:tt)* ],
                    )*]
                ) => {
                    crate::derive_alias::$NAME! { ?
                        $_ regular_derives
                        $_ item

                        //
                        [$_ (
                            [$_($_ rest)*],
                        )*]

                        // list of paths we knew were not duplicated
                        [$_ (
                            // a single path
                            [$_($_ deduplicated)*],
                        )*]
                    }
                };
            )*
            // Everything else is just added as-is
            (?
                $_ regular_derives:tt
                $_ item:tt
                [
                    // the first path
                    [ $_($_ first:tt)* ],
                    // rest of the paths
                    $_(
                        // a single path
                        [ $_ ($_ rest:tt)* ],
                    )*
                ]
                // list of paths
                [
                    $_ (
                        // a single path
                        [ $_ ($_ deduplicated:tt)* ],
                    )*
                ]
            ) => {
                crate::derive_alias::$NAME! { ?
                    $_ regular_derives
                    $_ item
                    // a list of paths to process
                    [$_(
                        // a single path
                        [$_($_ rest)*],
                    )*]

                    // a list of paths we know is not duplicated
                    [
                        // a list of paths we already knew were not duplicated
                        $_(
                            // a single path
                            [$_($_ deduplicated)*],
                        )*

                        // the path we learned that is not duplicated right now
                        //
                        // push the path we know cannot be duplicated to the end
                        [$_($_ first)*],
                    ]
                }
            };

            // Reached the base case. No more nested aliases
            (@
                // list of derives that did not come from alias expansion
                $_ regular_derives:tt
                // the item that we will generate a `#[derive]` for
                $_ item:tt
                // list of paths
                [$_ (
                    // a single path
                    [ $_ ($_ derives:tt)* ],
                )*]
            ) => {
                // Add the existing derives but de-duplicate
                crate::derive_alias::$NAME! { ?
                    $_ regular_derives
                    $_ item

                    [$_(
                        [$_($_ derives)*]
                    ,)*]

                    // this will be populated with Derives that are NOT derives that
                    // could possibly come from this alias expansion,
                    // because we don't want to accidentally get duplicates
                    []
                }
            };

            ///////////////////////////////////////////////////////////////

            (
                $_ Alias:path,$_ tt:tt
                // list of paths
                [$_ (
                    // a single path
                    [ $_ ($_ derives:tt)* ],
                )*]
            ) => {
                // De-duplicate
                crate::derive_alias::$NAME! {
                    # $_ Alias,$_ tt
                    // All current derives
                    [$_ (
                        // a single derive
                        [$_($_ derives)*]
                    ,)*]
                    // De-duplicated derives will go in here
                    []
                }
            };

            ///////////////////////////////////////////////////////////////

            (%
                [
                    // The next alias to stack
                    $_ next_alias:path,

                    // arguments to create the alias
                    $_ args:tt
                ]

                // Our accumulator. We'll push aliases here
                $_ ( [ $_($_ accumulated:tt)* ], ) *
            ) => {
                $_ next_alias! { %
                    // arguments to create the next alias
                    $_ args

                    // the aliases we collected
                    $_ ( [ $_($_ accumulated)* ], ) *

                    // add our own aliases to top of the stack
                    $( [$($derives)*], )*
                }
            };

            // BASE CASE: Reached end of the alias accumulation, create
            (%
                [ $_ ($_ tt:tt)* ]

                $_ ( [ $_($_ accumulated:tt)* ], ) *
            ) => {
                // create the alias
                $crate::__internal_new_alias! {
                    // all existing arguments
                    $_ ( $_ tt )*

                    // the aliases we collected
                    $_ ( [ $_($_ accumulated)* ], ) *

                    // add our own aliases to top of the stack
                    $( [$($derives)*], )*
                }
            };
        }
    }
}
