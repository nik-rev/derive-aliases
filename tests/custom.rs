#![allow(warnings)]
use derive_aliases::derive;

mod derive_alias {
    mod foo {
        derive_aliases::define! {
            Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
            Ord = ::core::cmp::PartialOrd, ::core::cmp::Ord, ..Eq;
        }
    }
    mod std_traits {
        derive_aliases::define! {
            Copy = ::core::marker::Copy, ::core::clone::Clone;
            StdTraits = ..Eq, ..Ord, ..Copy, ::core::fmt::Debug, ::core::hash::Hash;
        }
    }
    pub use foo::{Eq, Ord};
    pub use std_traits::{Copy, StdTraits};
}

#[derive(..StdTraits)]
struct User;
