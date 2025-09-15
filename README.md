# `derive_aliases`

[<img alt="crates.io" src="https://img.shields.io/crates/v/derive-aliases?style=for-the-badge" height="25">](https://crates.io/crates/derive-aliases)
[<img alt="github" src="https://img.shields.io/badge/github-derive--aliases-blue?style=for-the-badge" height="25">](https://github.com/nik-rev/derive-aliases)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/derive-aliases?style=for-the-badge" height="25">](https://docs.rs/derive-aliases)

This crate provides `#[derive]` aliases for reducing code boilerplate.

# Features

- Intuitive, simple syntax
  - Create alias: `Copy = Copy, Clone`
  - Use alias: `#[derive(Debug, ..Copy)]` expands to `#[std::derive(Debug, Copy, Clone)]`
- Very fast compile times, we don't pull *any* dependencies as our parsing logic is very simple
- Hovering on the aliases shows you documentation on what they expand into

# Usage

Aliases are defined in a special file `derive_aliases.rs`, located next to your **crate**'s `Cargo.toml`:

```rs
// Simple derive aliases
//
// `#[derive(..Copy, ..Eq)]` expands to `#[std::derive(Copy, Clone, PartialEq, Eq)]`

Copy = Copy, Clone;
Eq = PartialEq, Eq;

// You can nest them!
//
// `#[derive(..Ord, std::hash::Hash)]` expands to `#[std::derive(PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]`

Ord = PartialOrd, Ord, ..Eq;
```

This file uses a tiny domain-specific language for defining the derive aliases (the parser is less than 20 lines of code!). `.rs` is used just for syntax highlighting.
These aliases can then be used in Rust like so:

```rs
// This globally overrides `std::derive` with `derive_aliases::derive` across the whole crate! Handy.
#[macro_use]
extern crate derive_aliases;

#[derive(..Copy, ..Ord, std::hash::Hash)]
struct HelloWorld;
```

This expands to:

```rs
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]
struct HelloWorld;
```

# Single `derive_aliases.rs` in Cargo Workspaces

If you want to use the same `derive_aliases.rs` for all crates in your Cargo workspace, enable the `workspace` feature then define the `CARGO_WORKSPACE_DIR` env variable in `.cargo/config.toml`:

```toml
[env]
CARGO_WORKSPACE_DIR = { value = "", relative = true }
```

# `use` other alias files in `derive_aliases.rs`

`use` followed by a path will inline the derive aliases located in that file.

If `../my_other_aliases.rs` contains:

```rs
Ord = PartialOrd, Ord, ..Eq;
```

And your `derive_aliases.rs` has:

```rs
use "../my_other_aliases.rs";

Eq = PartialEq, Eq;
```

Then it will inline the aliases in the other file, expanding to:

```rs
Ord = PartialOrd, Ord, ..Eq;
Eq = PartialEq, Eq;
```

# Alternatives

- [`macro_rules_attribute`](https://docs.rs/macro_rules_attribute/latest/macro_rules_attribute/macro.derive_alias.html)
- [`derive_alias`](https://docs.rs/derive-alias/latest/derive_alias/)

