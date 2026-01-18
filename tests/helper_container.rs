#![cfg(false)]
//! Test that multiple `derive_aliases::derive` attributes
//! in a row won't mess up helper attribute name resolution
//! on the container
#![allow(unused)]

mod derive_alias {
    derive_aliases::define! {
        Serialize = ::serde::Serialize;
        Clone = ::core::clone::Clone;
    }
}

#[derive_aliases::derive(..Serialize)]
#[derive_aliases::derive(..Clone)]
#[serde(deny_unknown_fields)]
struct A {}

#[derive_aliases::derive(..Clone)]
#[derive_aliases::derive(..Serialize)]
#[serde(deny_unknown_fields)]
struct B {}

// Clone can come afterwards because
#[derive_aliases::derive(..Serialize)]
#[serde(deny_unknown_fields)]
#[derive_aliases::derive(..Clone)]
struct C {}

#[derive_aliases::derive(::serde::Serialize)]
#[serde(deny_unknown_fields)]
#[derive_aliases::derive(..Clone)]
struct D {}

#[derive(::serde::Serialize)]
#[serde(deny_unknown_fields)]
#[derive_aliases::derive(..Clone)]
struct E {}

#[derive_aliases::derive(..Serialize)]
#[serde(deny_unknown_fields)]
#[derive_aliases::derive(::core::clone::Clone)]
struct F {}

#[derive_aliases::derive(..Serialize)]
#[serde(deny_unknown_fields)]
#[derive(::core::clone::Clone)]
struct G {}
