macro_rules! assert_impls {
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

mod derive_alias {
    mod foo {
        derive_aliases::define! {
            Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
            AndMore = ::std::hash::Hash, ..Ord, ::core::clone::Clone, ::core::marker::Copy, ::core::default::Default;
        }
    }
    mod bar {
        derive_aliases::define! {
            Ord = ::core::cmp::PartialOrd, ::core::cmp::Ord, ..Eq;

            Everything = ::std::hash::Hash, ..Ord, ::core::marker::Copy, ::core::clone::Clone, ::core::default::Default;
        }
    }

    pub use bar::{Everything, Ord};
    pub use foo::*;
}

assert_impls!(a [Clone] => Clone);
assert_impls!(b [..Eq] => Eq, PartialEq);
assert_impls!(c [..AndMore] => PartialOrd, Ord, PartialEq, Eq, Copy, Clone, Default, std::hash::Hash);
assert_impls!(d [..Ord] => PartialOrd, Ord, PartialEq, Eq);
assert_impls!(e [..Everything] => std::hash::Hash, PartialOrd, Ord, Eq, PartialEq, Copy, Clone, Default);

assert_impls!(f [..Eq, Clone] => Eq, PartialEq, Clone);
assert_impls!(g [Clone, ..Eq] => Eq, PartialEq, Clone);

assert_impls!(h [Clone, ..Eq, Copy] => Eq, PartialEq, Clone, Copy);
assert_impls!(i [..Eq, Clone, Copy] => Eq, PartialEq, Clone, Copy);
assert_impls!(j [Clone, Copy, ..Eq] => Eq, PartialEq, Clone, Copy);

assert_impls!(m [..Eq, ..Eq] => Eq, PartialEq);

#[allow(unused_imports)]
use derive_alias::Everything;

#[cfg(test)]
#[test]
fn it_compiles() {}
