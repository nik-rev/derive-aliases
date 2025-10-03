pub mod derive_alias {
    derive_aliases::define! {
        = ::core::cmp::Eq, ::core::cmp::PartialEq
    // ^ missing name
        AndMore = ::std::hash::Hash, ..Ord, ::core::clone::Clone, ::core::marker::Copy, ::core::default::Default;
    }
}

fn main() {}
