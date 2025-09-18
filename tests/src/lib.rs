// mod derive_aliases_doc;

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
// }

derive_aliases::define! {
    // extern use foo::bar;

    Eq = PartialEq, Eq;

    Ord = PartialOrd, Ord, ..Eq;

    Everything = ::std::hash::Hash, ..Ord, Copy, Clone, Default;
}

macro_rules! implements_traits {
    ($name:ident #[$($input:tt)*] => $($($segment:ident)::*),*) => {
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

// Simple alias
implements_traits!(a #[..Eq] => Eq, PartialEq);

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
