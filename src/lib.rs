//! [![crates.io](https://img.shields.io/crates/v/derive_aliases?style=flat-square&logo=rust)](https://crates.io/crates/derive_aliases)
//! [![docs.rs](https://img.shields.io/badge/docs.rs-derive_aliases-blue?style=flat-square&logo=docs.rs)](https://docs.rs/derive_aliases)
//! ![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue?style=flat-square)
//! ![msrv](https://img.shields.io/badge/msrv-1.56-blue?style=flat-square&logo=rust)
//! [![github](https://img.shields.io/github/stars/nik-rev/derive_aliases)](https://github.com/nik-rev/derive-aliases)
//!
//! This crate improves Rust's `derive` macro by supporting user-defined Derive aliases.
//!
//! ```toml
//! [dependencies]
//! derive_aliases = "0.3"
//! ```
//!
//! # Usage
//!
//! Define aliases uses `define!`, and use them with `#[derive]`:
//!
//! ```
//! use derive_aliases::derive;
//!
//! mod derive_alias {
//!     derive_aliases::define! {
//!         Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
//!         Ord = ..Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord;
//!         Copy = ::core::marker::Copy, ::core::clone::Clone;
//!     }
//! }
//!
//! #[derive(Debug, ..Ord, ..Copy)]
//! struct User;
//! ```
//!
//! The above expands to this:
//!
//! ```
//! #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
//! struct User;
//! ```
//!
//! # IDE Support
//!
//! One of my biggest goals with this crate was strong IDE Support and fast compile-times.
//! Hovering over an alias `#[..Alias]` shows *exactly* what it expands into, and even Goto Definition directly brings you where the alias is defined.
//!
//! # Tip
//!
//! To globally override `#[std::derive]` with `#[derive_aliases::derive]`, add the following:
//!
//! ```
//! #[macro_use]
//! extern crate derive_aliases;
//! ```
//!
//! The above lets you [`define!`] aliases and then use them anywhere in your crate!
//!
//! I have put a **ton** of effort into optimizing `derive_aliases` to be as zero-cost as possible in terms of compile-time over the standard library's `derive`,
//! so don't worry about any overhead of `#[derive_aliases::derive]` even when no aliases are used! `derive_aliases` has 0 dependencies (not even `quote` or `syn`!)
//!
//! # Details
//!
//! All derive aliases must exist at your `crate::derive_aliases`, so invoke the `derive_aliases::define!` macro there.
//!
//! You can define aliases in 1 crate, and use them from another. You can break `define!` apart into multiple definitions:
//!
//! ```
//! # use derive_aliases::derive;
//! #
//! mod derive_alias {
//!     mod foo {
//!         derive_aliases::define! {
//!             Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
//!             Ord = ::core::cmp::PartialOrd, ::core::cmp::Ord, ..Eq;
//!         }
//!     }
//!     mod bar {
//!         derive_aliases::define! {
//!             Copy = ::core::marker::Copy, ::core::clone::Clone;
//!             StdTraits = ..Eq, ..Ord, ..Copy, ::core::fmt::Debug, ::core::hash::Hash;
//!         }
//!     }
//!
//!     pub use foo::{Eq, Ord};
//!     pub use bar::{Copy, StdTraits};
//! }
//!
//! #[derive(..StdTraits)]
//! struct User;
//! ```
//!
//! The above Just Works. Most importantly, derive aliases need to available at `crate::derive_alias`.
#![allow(clippy::crate_in_macro_def)]

// The real user-interface of the crate.
//
// - `derive_aliases::define!` to define aliases
// - `#[derive_aliases::deive]` is like `#[std::derive]` but also supports using aliases
#[doc(inline)]
pub use derive_aliases_proc_macro::{define, derive};

/// This is the main macro that makes `derive_aliases` possible
///
/// At a high level:
///
/// - Invocation of `new_alias!` creates a new alias, which is a `macro_rules!`.
///   So an invocation of `derive_aliases::define!` expands to a bunch of invocations of `new_alias!` each of
///   which expand to `macro_rules!` alias
///
/// - The `#[derive_aliases::derive]` macro takes the item it is applied to `:item` and nests it inside of a
///   bunch of `macro_rules!` aliases located at `crate::derive_alias`
///
/// All of this must happen while keeping compile-times **extremely fast**.
///
/// # Example
///
/// Here's the simplest possible example. It defines an alias `..Eq` which expands to `Eq, PartialEq`. This macro will be invoked from `crate::derive_alias` module.
///
/// ```ignore
///  derive_aliases::define! {
///      Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
///  }
/// ```
///
/// The first expansion is this:
///
/// ```ignore
/// ::derive_aliases::__internal_derive_aliases_new_alias! {
///     "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq],
/// }
/// pub use __derive_alias_Eq as Eq;
/// ```
///
/// - `"..."` contains the documentation, stuff passed to `#[doc = "..."]` so user gets a nice overview of what happens on hover,
///   but it is pretty long and so we omit it
///
/// - `__derive_alias_Eq` is the real name of the generated `macro_rules!` alias. Because we must `#[macro_export]` it so it will
///   be at the crate root (but we want to pretend it isn't), we have to do this. The `pub use __derive_alias_Eq as Eq` makes it available
///   as `crate::derive_alias::Eq`, which is the actual path we reference when we later expand an alias like `#[derive_aliases::derive(..Eq)]`
///
/// - Because `new_alias!` itself creates a `macro_rules!` we need the `$` token inside of the `macro_rules!` definition. But we can't use `$`
///   since that refers to the meta-variables captured by `new_alias!` and now the macro created *inside* `new_alias!`. So we do the 2nd
///   best thing and have `$_:tt` which is always `$`. This allows us to use `$_` instead of `$` inside of the macro definition
///
/// - Next is `Eq!`, which is the real name of the macro that we'll `pub use ... as Eq`. The `macro_rules!` alias defined by `new_alias!` will
///   refer to *itself* at exactly that position, as it will do some recursive TT munching by calling itself
///
/// - Next we have a bunch of paths inside of `[]`: `[$($path:tt)*]`. These paths are the *fully resolved* paths that the alias itself expands to.
///   They do *not* reference another alias. So given this:
///
///   ```ignore
///    derive_aliases::define! {
///        Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
///        Ord = ::core::cmp::Ord, ::core::cmp::PartialOrd, ..Eq;
///    }
///   ```
///
///   The `Ord` alias will expand to an invocation of `new_alias!` like this:
///
///   ```ignore
///   ::derive_aliases::__internal_derive_aliases_new_alias! {
///       "..." __derive_alias_Ord $ Ord! [::core::cmp::PartialEq], [::core::cmp::Eq], [::core::cmp::Ord], [::core::cmp::PartialOrd],
///   }
///   pub use __derive_alias_Ord as Ord;
///   ```
///
///   Because the `define!` macro itself referenes alias `..Eq` which it itself defines, we are able to "inline" all the aliases
///   to make it more performant. However, we also allow to refer to aliases that were **not** defined inside of the same `define!` call.
///   I'll explain how this is possible later.
///
///   **Back to `[$($path:tt)*]`**. We essentally use that instead of `$path:path` because you cannot compare 2 `:path` meta-variables. it's
///   as simple as that. We **must** compare them, but we can't. But we CAN compare 1 `[$($path:tt)*]` with another `[$($path:tt)*]`
///
///   **Why we need to compare**: Our aliases have the important property that they act as *sets*. 2 aliases that expand to a bunch of derives,
///   both expansions contain e.g. `::core::clone::Clone` derive. **But they can be used together**. They are merged like `HashSet`s.
///
///   **How the merger happens**: It's a recursion. Essentially, if our CURRENT alias adds `A` and `B` but we have `A, B, C, D`. We recurse to remove
///   `A` and `B` from the set to get `C, D`. Then we add `A, B` again to get `A, B, C, D`. If any of `A` or `B` already existed, they won't be added
///   again. If they didn't, they will be added now
///
/// Let's look at what `new_alias!` expands to now:
///
/// ```ignore
/// ::derive_aliases::__internal_derive_aliases_new_alias! {
///     "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq],
/// }
/// pub use __derive_alias_Eq as Eq;
/// ```
///
/// The above expands into this:
///
/// ```ignore
/// macro_rules! __derive_alias_Eq { /* ... */ }
/// pub use __derive_alias_Eq as Eq;
/// ```
///
/// The insides of this `macro_rules!` alias are not shown, as it's pretty big. And it won't make much sense to try and
/// comprehend the generated `macro_rules!` alias. So instead let's see what an invocation of `#[derive]` expands to:
///
/// ```ignore
/// #[derive(Debug, ..Eq)]
/// struct Example;
/// ```
///
/// Expansion is this:
///
/// ```ignore
/// crate::derive_alias::Eq! { @[Debug,] [struct Example;] [] }
/// ```
///
/// 1. We collect all normal non-alias derives inside of `[...]`, so `Debug, Clone` will be collected as `Debug, Clone,`
/// 1. The actual `:item` that we apply this to is wrapped inside of `[]` entirely. This is incredibly efficient - we do **not**
///    parse the `:item` itself, so it is just `:tt`. We pass this `:tt` everywhere. It is a SINGLE `TokenTree`, so it is very, very
///    efficient to just pass it around. We only match on it as `[$($tt:tt)*]` to actually apply the alias at the END.
///
///    This is one of the KEY things that keeps compile-time for this macro extremely fast. We **do not** want to add any overhead **at all**.
/// 1. Finally we have the empty `[]`. this is the **accumulator**. As we resolve more and more derives, we'll add all of them to this `[]`
///    After we've resolved all the derives, we take all the things inside this `[]` and pass it to `#[std::deive]`.
/// 1. `crate::derive_alias::Eq!` is referencing the actual `macro_rules!` alias that we explained earlier. This macro knows that
///    it expands to derives `::core::cmp::PartialEq` and `::core::cmp::Eq` so it will add them to the `[]` accumulator mentioned previously
/// 1. The `@` is just a glyph to keep compile times very fast. The insides of `macro_rules!` alias `Eq` contain a lot of rules, but
///    since we start with `@` Rust's algorithm can discard all of the rules that do not match the first token, making compile-times very fast
///
/// Let's examine every stage of the expansion of the above:
///
/// ```ignore
/// crate::derive_alias::Eq! { @[Debug,] [struct Example;] [] }
///
/// crate::derive_alias::Eq! { ?[Debug,][struct Example;][][] }
///
/// #[::core::prelude::v1::derive(Debug, ::core::cmp::PartialEq, ::core::cmp::Eq)]
/// struct Example;
/// ```
///
/// # Example with derives that conflict
///
/// As you can see this expanion is **very** simple because it just directly expands to the `std::derive`.
/// However, it gets a lot more interesting when there are multiple aliases involved, and these aliases may contain
/// derives that are overlapping and should be merged.
///
/// Given these aliases:
///
/// ```ignore
/// derive_aliases::define! {
///     Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
///     Ord = ::core::cmp::Ord, ::core::cmp::PartialOrd, ..Eq;
/// }
/// ```
///
/// They'll expand to this:
///
/// ```ignore
/// ::derive_aliases::__internal_derive_aliases_new_alias! {
///     "..." __derive_alias_Ord $ Ord! [::core::cmp::PartialEq], [::core::cmp::Eq], [::core::cmp::Ord], [::core::cmp::PartialOrd],
/// }
/// pub use __derive_alias_Ord as Ord;
///
/// ::derive_aliases::__internal_derive_aliases_new_alias! {
///     "..." __derive_alias_Eq $ Eq! [::core::cmp::PartialEq], [::core::cmp::Eq],
/// }
/// pub use __derive_alias_Eq as Eq;
/// ```
///
/// Then expanding these aliases:
///
/// ```
/// #[derive(Debug, ..Eq, ..Ord)]
/// struct Example;
/// ```
///
/// Will expand to this:
///
/// ```
/// crate::derive_alias::Ord! { crate::derive_alias::Eq,(@[Debug,] [struct Example;]) [] }
/// ```
///
/// Now it's a little more interesting. We are nesting the aliases 1 inside the other. Let's examine
/// every state of their expansions (with `trace_macros!(true)`):
///
/// ```ignore
/// crate::derive_alias::Ord! { crate::derive_alias::Eq,(@[Debug,] [struct Example;]) [] }
/// crate::derive_alias::Ord! { #crate::derive_alias::Eq,(@[Debug,] [struct Example;]) [] [] }
///
/// // first, the Ord! simply inserts all of its expansions into our stack
/// crate::derive_alias::Eq! { @[Debug,] [struct Example;] [[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::Ord],[::core::cmp::PartialOrd],] }
///
/// // alias `Eq` expands to `::core::cmp::Eq` and `::core::cmp::PartialEq`. Because these can't just be added (they conflict with each-other)
/// // we go through all existing derives in our stack and remove all of those that are exactly `::core::cmp::PartialEq` and `::core::cmp::Eq`
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::Ord],[::core::cmp::PartialOrd],] [] }
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [[::core::cmp::PartialEq],[::core::cmp::Eq],[::core::cmp::Ord],[::core::cmp::PartialOrd],] [] }
///
/// // Removed `PartialEq`
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [[::core::cmp::Eq],[::core::cmp::Ord],[::core::cmp::PartialOrd],] [] }
///
/// // Removed `Eq`
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [[::core::cmp::Ord],[::core::cmp::PartialOrd],] [] }
///
/// // Did NOT remove `Ord`, added it to the "good" pile (no conflicts)
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [[::core::cmp::PartialOrd],] [[::core::cmp::Ord],] }
///
/// // Did NOT remove `PartialOrd`, added it to the "good" pile (no conflicts)
/// crate::derive_alias::Eq! { ?[Debug,] [struct Example;] [] [[::core::cmp::Ord],[::core::cmp::PartialOrd],] }
///
/// // Now that our pile is totally empty, and we reached end of the expansion (signified by the `?` glyph)
/// // let's generate the real `#[std::derive]`
/// #[::core::prelude::v1::derive(Debug,::core::cmp::Ord,::core::cmp::PartialOrd,::core::cmp::PartialEq,::core::cmp::Eq,)] struct Example;
/// ```
///
/// What we learned:
///
/// 1. Each additional alias adds an extra level of nesting
/// 1. We resolve aliases 1-at-a-time. Each alias knows what derives it expands to.
/// 1. Each alias REMOVES all the same aliases that exist from the list of aliases to expand to.
/// 1. Once done, it adds it aliases to the end of the list
/// 1. The process is repeated for each alias
#[doc(hidden)]
#[macro_export]
macro_rules! __internal_derive_aliases_new_alias {
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
        #[allow(clippy::crate_in_macro_def)]
        #[doc(hidden)]
        macro_rules! $real_name {
            ///////////////////////////////////////////////////////////////

            // Ord! { #Eq, (Copy, (@ [Debug, ] [struct Foo;])), [] [] }
            //
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
            // DONE
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
                $crate::__internal_derive_aliases_new_alias_with_externs! {
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

#[doc(hidden)]
pub use derive_aliases_proc_macro::__internal_derive_aliases_new_alias_with_externs;
