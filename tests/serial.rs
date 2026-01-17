//! Test that multiple `derive_aliases::derive` attributes
//! in a row won't mess up helper attribute name resolution
//!
//! https://github.com/nik-rev/derive-aliases/issues/4
#![allow(unused)]

mod derive_alias {
    derive_aliases::define! {
        Serialize = ::serde::Serialize;
        Clone = ::core::clone::Clone;
    }
}

#[derive_aliases::derive(..Serialize)]
#[derive_aliases::derive(..Clone)]
struct A {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}

#[derive_aliases::derive(..Clone)]
#[derive_aliases::derive(..Serialize)]
struct B {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}

#[serde_with::skip_serializing_none]
#[derive_aliases::derive(..Clone)]
#[derive_aliases::derive(..Serialize)]
struct C {
    field: Option<()>,
}
