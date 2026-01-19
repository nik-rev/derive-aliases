//! Test that multiple `derive_aliases::derive` attributes
//! in a row won't mess up helper attribute name resolution
//! on the field
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

#[derive(serde::Serialize)]
#[derive_aliases::derive(..Clone)]
struct A2 {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}

// Swap the order

#[derive_aliases::derive(..Clone)]
#[derive_aliases::derive(..Serialize)]
struct B {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}

#[derive(core::clone::Clone)]
#[derive_aliases::derive(..Serialize)]
struct B2 {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}
