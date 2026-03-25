// //! Test that multiple `derive_aliases::derive` attributes
// //! in a row won't mess up helper attribute name resolution
// //! on the container
// #![allow(unused)]

// mod derive_alias {
//     derive_aliases::define! {
//         Serialize = ::serde::Serialize;
//         Clone = ::core::clone::Clone;
//     }
// }

// #[derive_aliases::derive(..Serialize)]
// #[derive_aliases::derive(..Clone)]
// #[serde(deny_unknown_fields)]
// /*^
// @(all(), $(::core::clone::Clone))
// @(all(), $(::serde::Serialize))
// serde(deny_unknown_fields)
// */
// struct A {}

// #[derive_aliases::derive(..Clone)]
// #[derive_aliases::derive(..Serialize)]
// #[serde(deny_unknown_fields)]
// /*^
// @(all(), $(::serde::Serialize))
// @(all(), $(::core::clone::Clone))
// serde(deny_unknown_fields)
// */
// struct B {}

// // Clone can come afterwards because
// #[derive_aliases::derive(..Serialize)]
// #[serde(deny_unknown_fields)]
// #[derive_aliases::derive(..Clone)]
// /*^
// @(all(), $(::core::clone::Clone))
// @(all(), $(::serde::Serialize))
// serde(deny_unknown_fields)
// */
// struct C {}

// #[derive_aliases::derive(::serde::Serialize)]
// #[serde(deny_unknown_fields)]
// #[derive_aliases::derive(..Clone)]
// /*^
// @(all(), $(::serde::Serialize))
// @(all(), $(::core::clone::Clone))
// serde(deny_unknown_fields)
// */
// struct D {}

// // This is a known, unfixable issue.
// //
// // The `derive_aliases::derive` attribute below will receive `serde(deny_unknown_fields)` as an input,
// // which causes it to emit this attribute in the input to inner macros. But the `derive(Deserialize)` won't be there,
// // so this will be a name resolution error.
// //
// // #[derive(::serde::Serialize)]
// // #[serde(deny_unknown_fields)]
// // #[derive_aliases::derive(..Clone)]
// // struct E {}

// #[derive_aliases::derive(..Serialize)]
// #[serde(deny_unknown_fields)]
// #[derive_aliases::derive(::core::clone::Clone)]
// /*^
// @(all(), $(::core::clone::Clone))
// @(all(), $(::serde::Serialize))
// serde(deny_unknown_fields)
// */
// struct F {}

// #[derive_aliases::derive(..Serialize)]
// #[serde(deny_unknown_fields)]
// #[derive(::core::clone::Clone)]
// /*^
// @(all(), $(::core::clone::Clone))
// @(all(), $(::serde::Serialize))
// serde(deny_unknown_fields)
// */
// struct G {}
