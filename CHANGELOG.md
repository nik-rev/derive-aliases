# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

[Unreleased]: https://github.com/nik-rev/derive-aliases/compare/v0.4.9...HEAD

## [v0.4.9] - 2026-01-17 [YANKED]

[v0.4.9]: https://github.com/nik-rev/derive-aliases/compare/v0.4.8...v0.4.9

**YANKED**: This release contains a breaking change (https://github.com/nik-rev/derive-aliases/issues/6)

### Docs

- Various improvements

## [v0.4.8] - 2026-01-17 [YANKED]

[v0.4.8]: https://github.com/nik-rev/derive-aliases/compare/v0.4.7...v0.4.8

**YANKED**: This release contains a breaking change (https://github.com/nik-rev/derive-aliases/issues/6)

### Fixed

- Fixed an issue where applying more than 1 `#[derive_aliases::derive]` attribute in a row
would mess up name resolution for helper attributes ([#5](https://github.com/nik-rev/derive-aliases/pull/5))

## [v0.4.7] - 2025-10-13

[v0.4.7]: https://github.com/nik-rev/derive-aliases/compare/v0.4.6...v0.4.7

### Chore

- Updated `categories` and `keywords` sections in `Cargo.toml` metadata

## [v0.4.6] - 2025-10-13

[v0.4.6]: https://github.com/nik-rev/derive-aliases/compare/v0.4.5...v0.4.6

### Chore

- Removed unused file

## [v0.4.5] - 2025-10-13

[v0.4.5]: https://github.com/nik-rev/derive-aliases/compare/v0.4.4...v0.4.5

### Docs

- Various improvements

## [v0.4.4] - 2025-10-13

[v0.4.4]: https://github.com/nik-rev/derive-aliases/compare/v0.4.3...v0.4.4

### Changed

- Updated MSRV to 1.75

### Notes

- Future MSRV increases will be considered breaking, and only added in major versions of the crate

## [v0.4.3] - 2025-10-13

[v0.4.3]: https://github.com/nik-rev/derive-aliases/compare/v0.4.2...v0.4.3

### Changed

- Updated MSRV to 1.64

## [v0.4.2] - 2025-10-10

[v0.4.2]: https://github.com/nik-rev/derive-aliases/compare/v0.4.1...v0.4.2

### Docs

- Various improvements

## [v0.4.1] - 2025-10-04

[v0.4.1]: https://github.com/nik-rev/derive-aliases/compare/v0.4.0...v0.4.1

### Docs

- Various improvements

## [v0.4.0] - 2025-10-04

[v0.4.0]: https://github.com/nik-rev/derive-aliases/compare/v0.3.1...v0.4.0

### Changed

By default, the derive aliases are private within your crate. To allow other crates to use
derive aliases defined in your crate, add the `#![export_derive_aliases]` attribute:

```rust
derive_aliases::define! {
    #![export_derive_aliases]

    Eq = ::core::cmp::PartialEq, ::core::cmp::Eq;
}
```

## [v0.3.1] - 2025-09-29

[v0.3.1]: https://github.com/nik-rev/derive-aliases/compare/v0.3.0...v0.3.1

### Docs

- Various improvements

## [v0.3.0] - 2025-09-29

[v0.3.0]: https://github.com/nik-rev/derive-aliases/compare/v0.2.2...v0.3.0

### Changed

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

[v0.2.2]: https://github.com/nik-rev/derive-aliases/compare/v0.2.1...v0.2.2

### Docs

- Various improvements

## [v0.2.1] - 2025-09-15

[v0.2.1]: https://github.com/nik-rev/derive-aliases/compare/v0.2.0...v0.2.1

### Docs

- Added a showcase of IDE support

### Changed

- Improved compile errors

## [v0.2.0] - 2025-09-15

[v0.2.0]: https://github.com/nik-rev/derive-aliases/compare/v0.1.1...v0.2.0

### Added

- When hovering over derive aliases in Rust source code, show documentation on hover

## [v0.1.1] - 2025-09-15

[v0.1.1]: https://github.com/nik-rev/derive-aliases/compare/v0.1.0...v0.1.1

### Docs

- Various improvements

## [v0.1.0] - 2025-09-15

[v0.1.0]: https://github.com/nik-rev/derive-aliases/releases/v0.1.0

### Notes

This is the first release
