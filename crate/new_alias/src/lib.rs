//! Implementation details for the `derive_aliases` crate
#![allow(clippy::crate_in_macro_def)]

#[doc(inline)]
pub use derive_aliases_impl::define;

#[doc(hidden)]
#[macro_export]
macro_rules! __internal_new_alias {
    (
        $(#[$attr:meta])*
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
        $(#[$attr])*
        #[macro_export]
        #[doc(hidden)]
        macro_rules! $real_name {
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
                $_ crate::derive_alias::$NAME!(?
                    $_ regular_derives
                    $_ item

                    [$_(
                        [$_($_ derives)*]
                    ,)*]

                    // this will be populated with Derives that are NOT derives that
                    // could possibly come from this alias expansion,
                    // because we don't want to accidentally get duplicates
                    []
                )
            };

            (
                $_ Alias:path,$_ tt:tt
                // list of paths
                [$_ (
                    // a single path
                    [ $_ ($_ derives:tt)* ],
                )*]
            ) => {
                // De-duplicate
                $_ crate::derive_alias::$NAME!(
                    # $_ Alias,$_ tt
                    // All current derives
                    [$_ (
                        // a single derive
                        [$_($_ derives)*]
                    ,)*]
                    // De-duplicated derives will go in here
                    []
                )
            };

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
                $_ Alias!(
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
                )
            };

            ///////////////////////////////////////////////////////////////

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
                    $_ crate::derive_alias::$NAME!(
                        // next alias and arguments
                        # $_ Alias, $_ pass

                        [$_ (
                            [$_($_ rest)*],
                        )*]

                        [$_ (
                            [ $_ ($_ deduplicated)* ],
                        )*]
                    )
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
                $_ crate::derive_alias::$NAME!(
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
                )
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
                    // compile_error!("y");
                    $_ crate::derive_alias::$NAME!(?
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
                    )
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
                $_ crate::derive_alias::$NAME!(?
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
                )
            };
        }
    }
}
