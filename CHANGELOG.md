# v0.3.0

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
