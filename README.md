# `derive_aliases`

<!-- cargo-rdme start -->

[![crates.io](https://img.shields.io/crates/v/derive_aliases?style=flat-square&logo=rust)](https://crates.io/crates/derive_aliases)
[![docs.rs](https://img.shields.io/badge/docs.rs-derive_aliases-blue?style=flat-square&logo=docs.rs)](https://docs.rs/derive_aliases)
![license](https://img.shields.io/badge/license-Apache--2.0_OR_MIT-blue?style=flat-square)
![msrv](https://img.shields.io/badge/msrv-1.60-blue?style=flat-square&logo=rust)
[![github](https://img.shields.io/github/stars/nik-rev/derive-aliases)](https://github.com/nik-rev/derive-aliases)

This crate improves Rust's `derive` macro by supporting user-defined Derive aliases.

```toml
[dependencies]
derive_aliases = "0.3"
```

## Usage

Define aliases using [`define!`](define), and use them with [`#[derive]`](derive):

```rust
mod derive_alias {
    derive_aliases::define! {
        Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
        Ord = ..Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord;
        Copy = ::core::marker::Copy, ::core::clone::Clone;
    }
}

use derive_aliases::derive;

// Use the aliases:
#[derive(Debug, ..Ord, ..Copy)]
struct User;
```

The above expands to this:

```rust
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
struct User;
```

- `#[derive(..Eq)]` expands to `#[derive(::core::cmp::PartialEq, ::core::cmp::Eq)]`
- `#[derive(..Ord)]` expands to `#[derive(..Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord)]`, which expands to `#[derive(::core::cmp::PartialEq, ::core::cmp::Eq, ::core::cmp::PartialOrd, ::core::cmp::Ord)]`

## IDE Support

Hovering over an alias `#[derive(..Alias)]` shows *exactly* what it expands into, and even Goto Definition directly brings you where the alias is defined.

![IDE Support](https://raw.githubusercontent.com/nik-rev/derive-aliases/main/ide_support.png)

## Tip

To globally override `#[std::derive]` with [`#[derive_aliases::derive]`](derive), add the following:

```rust
#[macro_use]
extern crate derive_aliases;
```

The above lets you [`define!`](macro@define) aliases and then use them anywhere in your crate!

I have put a **ton** of effort into optimizing `derive_aliases` to be as zero-cost as possible in terms of compile-time over the standard library's `derive`,
so don't worry about any overhead of `#[derive_aliases::derive]` even when no aliases are used! `derive_aliases` has 0 dependencies (not even `quote` or `syn`!)

## Derives are de-duplicated

Each derive alias expands into a bunch of derives, then de-duplicated. If there are 2 or more of the same derive, only 1 is kept.
This is useful when there are some "pre-requisite" derives needed, but if they already exist then don't add them (instead of compile error'ing).

```rust
extern crate zerocopy;

mod derive_alias {
    derive_aliases::define! {
        FastHash = ::zerocopy::ByteHash, ::zerocopy::Immutable, ::zerocopy::IntoBytes;
        FastEq = ::zerocopy::ByteEq, ::zerocopy::Immutable, ::zerocopy::IntoBytes;
    }
}

#[derive(..FastHash)]
struct Example;

// expands to:
#[derive(::zerocopy::ByteHash, ::zerocopy::Immutable, ::zerocopy::IntoBytes)]
struct Example;



#[derive(..FastEq)]
struct Example;

// expands to:
#[derive(::zerocopy::ByteEq, ::zerocopy::Immutable, ::zerocopy::IntoBytes)]
struct Example;



#[derive(..FastEq, ..FastHash)]
struct Example;

// expands to:
#[derive(::zerocopy::ByteEq, ::zerocopy::ByteHash, ::zerocopy::Immutable, ::zerocopy::IntoBytes)]
struct Example;

// note that the 2 `Immutable` and 2 `IntoBytes` derives were de-duplicated
```

## Splitting up derive aliases

All derive aliases must exist at your `crate::derive_alias`, so invoke the `derive_aliases::define!` macro there.

You can break `define!` apart into multiple definitions:

```rust
mod derive_alias {
    mod foo {
        derive_aliases::define! {
            Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
            Ord = ::core::cmp::PartialOrd, ::core::cmp::Ord, ..Eq;
        }
    }
    mod bar {
        derive_aliases::define! {
            Copy = ::core::marker::Copy, ::core::clone::Clone;
            StdTraits = ..Eq, ..Ord, ..Copy, ::core::fmt::Debug, ::core::hash::Hash;
        }
    }

    pub use foo::{Eq, Ord};
    pub use bar::{Copy, StdTraits};
}

#[derive(..StdTraits)]
struct User;
```

The above Just Works. Most importantly, derive aliases need to available at `crate::derive_alias`. This also allows you to share derive aliases across crates

<!-- cargo-rdme end -->
