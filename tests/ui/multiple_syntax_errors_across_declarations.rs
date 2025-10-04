pub mod derive_alias {
    derive_aliases::define! {
        Eq = ::core:cmp::Eq, ::core::cmp::PartialEq;
        //         ^ syntax error #1
        AndMore = :std::hash::Hash, ..Ord, ::core::clone::Clone, ::core::marker::Copy, ::core::default::Default;
        //        ^ syntax error #2
    }
}

fn main() {}
