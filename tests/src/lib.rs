#![allow(warnings)]
// mod derive_aliases_doc;

mod derive_alias {
    mod foo {
        derive_aliases::define! {
            Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;

            AndMore = ..Ord, ::core::marker::Copy, ::core::clone::Clone, ::core::default::Default, ::std::hash::Hash;
        }
    }
    mod bar {
        derive_aliases::define! {
            Ord = ::core::cmp::PartialOrd, ::core::cmp::Ord, ..Eq;

            Everything = ::std::hash::Hash, ..Ord, ::core::marker::Copy, ::core::clone::Clone, ::core::default::Default;
        }
    }

    pub use bar::*;
    pub use foo::*;
}

use derive_alias::Everything;

// use ::core::cmp::Ord as _;
// use ::core::cmp::PartialOrd as _;
// crate::derive_alias::Eq! {
//     %["Derive alias `..Ord` can be used like this:\n\n```ignore\n#[derive(..Ord)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Ord, PartialOrd)]\nstruct Example;\n```" __derive_alias_Ord$ Ord![::core::cmp::PartialOrd],[::core::cmp::Ord],]
// }
// use ::core::clone::Clone as _;
// use ::core::cmp::Ord as _;
// use ::core::cmp::PartialOrd as _;
// use ::core::default::Default as _;
// use ::core::marker::Copy as _;
// use ::std::hash::Hash as _;
// crate::derive_alias::Eq! {
//     %["Derive alias `..Everything` can be used like this:\n\n```ignore\n#[derive(..Everything)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash, Ord, PartialOrd)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_Everything$Everything![::core::cmp::PartialOrd],[::std::hash::Hash],[::core::marker::Copy],[::core::default::Default],[::core::clone::Clone],[::core::cmp::Ord],]
// }

// // use crate::derive_alias::Eq as _;
// // use crate::derive_alias::Eq as _;
// // use crate::derive_alias::Everything;
// // use crate::derive_alias::Ord;
// // use crate::derive_alias::Ord as _;

// use ::core::clone::Clone as _;
// use ::core::default::Default as _;
// use ::core::marker::Copy as _;
// use ::std::hash::Hash as _;

// // crate::derive_alias::Ord! {
// //     %["Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::clone::Clone],[::core::default::Default],[::std::hash::Hash],[::core::marker::Copy],]
// // }
// // use ::core::cmp::Eq as _;
// // use ::core::cmp::PartialEq as _;
// ::derive_aliases::__internal_new_alias! {
//     "Derive alias `..Eq` can be used like this:\n\n```ignore\n#[derive(..Eq)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Eq, PartialEq)]\nstruct Example;\n```" __derive_alias_Eq$Eq![::core::cmp::Eq],[::core::cmp::PartialEq],
// }
// use __derive_alias_AndMore as AndMore;
// use __derive_alias_Eq as Eq;
// use __derive_alias_Ord as _;

// use derive_alias::Eq;

// use ::core::clone::Clone as _;
// use ::core::cmp::Ord as _;
// use ::core::cmp::PartialOrd as _;
// use ::core::default::Default as _;
// use ::core::marker::Copy as _;
// use ::std::hash::Hash as _;
// ::derive_aliases::__internal_new_alias! {
//     "Derive alias `..Everything` can be used like this:\n\n```ignore\n#[derive(..Everything)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash, Ord, PartialOrd)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_Everything$Everything![::core::default::Default],[::core::cmp::Ord],[::core::cmp::PartialOrd],[::core::clone::Clone],[::std::hash::Hash],[::core::marker::Copy],
// }
// use ::core::cmp::Ord as _;
// use ::core::cmp::PartialOrd as _;

// Eq! (%
//     ["Derive alias `..Ord` can be used like this:\n\n```ignore\n#[derive(..Ord)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Ord, PartialOrd)]\nstruct Example;\n```" __derive_alias_Ord$Ord![::core::cmp::PartialOrd],[::core::cmp::Ord],]
// );
// ::derive_aliases::__internal_new_alias! {
//     "Derive alias `..Ord` can be used like this:\n\n```ignore\n#[derive(..Ord)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Ord, PartialOrd)]\nstruct Example;\n```" __derive_alias_Ord$Ord![::core::cmp::PartialOrd],[::core::cmp::Ord],
// }
// compile_error! {
//     "unknown Alias"
// }
// use __derive_alias_Eq as _;
// use __derive_alias_Eq as _;
// use __derive_alias_Everything as Everything;
// use __derive_alias_Ord as Ord;
// use __derive_alias_Ord as _;

// use ::core::cmp::Eq as _;
// use ::core::cmp::PartialEq as _;
// ::derive_aliases::__internal_new_alias! {
//     "Derive alias `..Eq` can be used like this:\n\n```ignore\n#[derive(..Eq)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Eq, PartialEq)]\nstruct Example;\n```" __derive_alias_Eq$Eq![::core::cmp::PartialEq],[::core::cmp::Eq],
// }
// use ::core::clone::Clone as _;
// use ::core::default::Default as _;
// use ::core::marker::Copy as _;
// use ::std::hash::Hash as _;

// Ord! (% [
//         Ord,
//         [
//             "Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],
//         ]
//     ]
// );
// Ord! (%
//     [Ord,
//         [Ord,
//             [
//                 "Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],
//             ]
//         ]
//     ]
// );
// Ord! (%
//     [Ord,
//         [Ord,
//             [
//                 "Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],
//             ]
//         ]
//     ]
// );
// Ord! (% Ord,
//     ["Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],]
// );
// Ord! {
//     %["Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],][::core::cmp::PartialOrd],[::core::cmp::Ord],[::core::cmp::PartialEq],[::core::cmp::Eq],
// }

// Ord! (% Ord,
//     ["Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],]
// );

// ::derive_aliases::__internal_new_alias! {
//     "Derive alias `..AndMore` can be used like this:\n\n```ignore\n#[derive(..AndMore)]\nstruct Example;\n```\n\nWhich expands to the following:\n\n```ignore\n#[derive(Clone, Copy, Default, Hash)]\nstruct Example;\n\nuse core::hash::Hash\n```" __derive_alias_AndMore$AndMore![::core::default::Default],[::core::marker::Copy],[::core::clone::Clone],[::std::hash::Hash],
// }

// compile_error! {
//     "unknown Alias"
// }
// use __derive_alias_AndMore as AndMore;
// use __derive_alias_Eq as Eq;
// use __derive_alias_Ord as _;

// derive_aliases::define! {
//     #[cfg(feature = "arbitrary")]
//     Copy =
//         #[cfg(feature = "arbitrary")]
//         Copy,
//         #[cfg(feature = "arbitrary")]
//         Clone;

//     #[cfg(feature = "arbitrary")]
//     Eq = PartialEq, Eq;

//     #[cfg(all(feature = "smallvec", feature = "smallvec", all(feature = "smallvec"), feature = "smallvec"))]
//     Ord =
//         PartialOrd,
//         #[cfg(all(feature = "serde", feature = "smallvec"))]
//         Ord,
//         #[cfg(feature = "serde")]
//         ..Eq;

//     #[cfg(feature = "serde")]
//     Together = PartialOrd, std::hash::Hash;
//

// ::derive_aliases::__internal_new_alias! {
//     __derive_alias_Ord
//     $Ord![::core ::cmp
//     ::Eq],[::core ::cmp
//     ::Ord],[::core ::cmp ::PartialOrd],[::core ::cmp ::PartialEq],
// }
// ::derive_aliases::__internal_new_alias! {
//     __derive_alias_Eq $Eq![::core ::cmp ::PartialEq],[::core ::cmp ::Eq],
// }
// ::derive_aliases::__internal_new_alias! {
//     __derive_alias_AndMore
//     $AndMore![::core ::cmp
//     ::Eq],[::core ::default
//     ::Default],[::core ::cmp
//     ::PartialOrd],[::core ::cmp
//     ::PartialEq],[::core ::marker
//     ::Copy],[::core ::clone
//     ::Clone],[::core ::cmp ::Ord],[::std ::hash ::Hash],
// }
// ::derive_aliases::__internal_new_alias! {
//     __derive_alias_Everything
//     $Everything![::core ::default
//     ::Default],[::core ::marker
//     ::Clone],[::core ::marker
//     ::Copy],[::std ::hash
//     ::Hash],[::core ::cmp
//     ::PartialOrd],[::core ::cmp
//     ::Ord],[::core ::cmp ::PartialEq],[::core ::cmp ::Eq],
// }

macro_rules! expect_expansion {
    ($name:ident [$($input:tt)*] => $($($segment:ident)::*),*) => {
        mod $name {
            #[derive_aliases::derive($($input)*)]
            pub struct A(());
        }

        // HACK: since we can't use `+` as repetition separator
        #[allow(warnings, reason = "only for type check")]
        fn $name<A: $($($segment)::* +)* Sized>() {
            $name::<$name::A>();
        }
    };
}

// crate::derive_alias::Eq! { @[] [pub struct A(());] [] }

expect_expansion!(a [Clone] => Clone);
expect_expansion!(b [..Eq] => Eq, PartialEq);

// // Simple alias
// implements_traits!(a #[..Eq] => Eq, PartialEq);

// // Alias with custom type
// implements_traits!(b #[..Copy, std::hash::Hash] => Copy, Clone, std::hash::Hash);

// // 2 aliases, and custom derive in-between them
// implements_traits!(c #[..Eq, std::hash::Hash, ..Copy] => Eq, PartialEq, std::hash::Hash, Copy, Clone);

// // Removes duplicates
// //
// // Here, there will be 2 of `Hash` and 2 of `PartialOrd`
// implements_traits!(d #[..Ord, ..Together] => Ord, PartialOrd, Eq, PartialEq, std::hash::Hash);

// // Works with full path reference
// implements_traits!(e #[..Eq] => Eq, PartialEq);

#[cfg(test)]
#[test]
fn it_compiles() {}
