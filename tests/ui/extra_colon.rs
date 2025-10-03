pub mod derive_alias {
    derive_aliases::define! {
        Eq = ::core::cmp::Eq, ::core:::cmp::PartialEq;
        //                           ^ extra `:`
        AndMore = ::std::hash::Hash, ..Ord, ::core::clone::Clone, ::core::marker::Copy, ::core::default::Default;
    }
}

fn main() {}
