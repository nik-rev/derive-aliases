#![allow(warnings)]
#![cfg(test)]

#[macro_use]
extern crate derive_aliases;

macro_rules! implements_traits {
    ($name:ident #[$($derive:ident)::*($($input:tt)*)] => $($($segment:ident)::*),*) => {
        mod $name {
            #[$($derive)::*($($input)*)]
            pub struct A(());
        }

        // HACK: since we can't use `+` as repetition separator
        fn $name<A: $($($segment)::* +)* Sized>() {
            $name::<$name::A>();
        }
    };
}

// Simple alias
implements_traits!(a #[derive(..Eq)] => Eq, PartialEq);

// Alias with custom type
implements_traits!(b #[derive(..Copy, std::hash::Hash)] => Copy, Clone, std::hash::Hash);

// 2 aliases, and custom derive in-between them
implements_traits!(c #[derive(..Eq, std::hash::Hash, ..Copy)] => Eq, PartialEq, std::hash::Hash, Copy, Clone);

// Removes duplicates
//
// Here, there will be 2 of `Hash` and 2 of `PartialOrd`
implements_traits!(d #[derive(..Ord, ..Together)] => Ord, PartialOrd, Eq, PartialEq, std::hash::Hash);

// Works with full path reference
implements_traits!(e #[derive_aliases::derive(..Eq)] => Eq, PartialEq);

#[cfg(test)]
#[test]
fn it_compiles() {}
