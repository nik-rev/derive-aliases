# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v0.4.8] - 2026-01-17

### Fixed

- Fixed an issue where applying more than 1 `#[derive_aliases::derive]` attribute in a row
would mess up name resolution for helper attributes ([#5](https://github.com/nik-rev/derive-aliases/pull/5))

## [v0.4.7] - 2025-10-13
## [v0.4.5] - 2025-10-13
## [v0.4.4] - 2025-10-13
## [v0.4.3] - 2025-10-13
## [v0.4.1] - 2025-10-04
## [v0.4.0] - 2025-10-04

By default, the derive aliases are private within your crate. To allow other crates to use
derive aliases defined in your crate, add the `#![export_derive_aliases]` attribute:

```rust
derive_aliases::define! {
    #![export_derive_aliases]

    Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
}
```

## [v0.3.1] - 2025-09-29
## [v0.3.0] - 2025-09-29

The way you define aliases has completely changed. Instead of a separate file that exists next to `Cargo.toml`, they
are defined at the invocation of the macro:

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

## [v0.2.2] - 2025-09-15
## [v0.2.0] - 2025-09-15
## [v0.1.1] - 2025-09-15
