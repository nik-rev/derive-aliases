// //! Test that multiple `derive_aliases::derive` attributes
// //! in a row won't mess up helper attribute name resolution
// //! on the field
// //!
// //! https://github.com/nik-rev/derive-aliases/issues/4
// #![allow(unused)]

// mod derive_alias {
//     derive_aliases::define! {
//         Serialize = ::serde::Serialize;
//         Clone = ::core::clone::Clone;
//     }
// }

// // // This is a known issue.

// // #[derive(serde::Serialize)]
// // #[derive_aliases::derive(..Clone)]
// // struct A2 {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// // Swap the order

// // #[derive_aliases::derive(..Serialize)]
// // #[derive_aliases::derive(..Clone)]
// // /*^
// // @(all(), $(::core::clone::Clone))
// // @(all(), $(::serde::Serialize))
// // */
// // struct A {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// // #[cfg_attr(all(), ::core::prelude::v1::derive(::serde::Serialize))]
// // #[derive_aliases::derive(..Clone)]
// // struct A {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// // #[::core::prelude::v1::derive(::serde::Serialize)]
// // #[derive_aliases::derive(..Clone)]
// // struct A {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// // #[cfg_attr(all(), ::core::prelude::v1::derive(::core::clone::Clone))]
// // struct A {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// // #[derive_aliases::derive(..Clone)]
// // #[derive_aliases::derive(..Serialize)]
// // struct B {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }

// #[cfg_attr(all(), ::core::prelude::v1::derive(::serde::Serialize))]
// struct B {
//     #[serde(skip_serializing_if = "Option::is_none")]
//     field: Option<()>,
// }
// #[derive_aliases::derive(..Serialize)]
// #[derive_aliases::derive(..Clone)]
// struct A {
//     #[serde(skip_serializing_if = "Option::is_none")]
//     field: Option<()>,
// }

// //>

// #[cfg_attr(all(), ::core::prelude::v1::derive(::serde::Serialize))]
// #[derive_aliases::derive(..Clone)]
// struct X {
//     #[serde(skip_serializing_if = "Option::is_none")]
//     field: Option<()>,
// }

// // #[derive(core::clone::Clone)]
// // #[derive_aliases::derive(..Serialize)]
// // /*^
// // @(all(), $(::serde::Serialize))
// // */
// // struct B2 {
// //     #[serde(skip_serializing_if = "Option::is_none")]
// //     field: Option<()>,
// // }
