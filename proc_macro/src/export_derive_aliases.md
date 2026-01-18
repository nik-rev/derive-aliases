The `#![export_derive_aliases]` marker allows other crates to use these derive aliases.

# Details

The following invocation in `lib.rs`:

```ignore
pub mod derive_alias {
    derive_aliases::define! {
        Copy = ::core::marker::Copy, ::core::clone::Clone;
        Eq = ::core::cmp::Eq, ::core::cmp::PartialEq;
    }
}
```

Expands to the following code:

## Normally

```ignore
pub mod derive_alias {
    macro_rules! Copy { /* omitted */ }
    pub(crate) use Copy;

    macro_rules! Eq { /* omitted */ }
    pub(crate) use Eq;
}
```

Making `crate::derive_alias::Copy` and `crate::derive_alias::Eq` both resolve to the actual aliases.
The entire crate can access these aliases. However, another crate depending on the one where the `define!`
macro was invoked from won't be able to see any of the aliases.

## With `#![export_derive_aliases]`

```ignore
pub mod derive_alias {
    #[doc(hidden)]
    #[macro_export]
    macro_rules! Copy { /* omitted */ }
    #[doc(inline)]
    pub use Copy;

    #[doc(hidden)]
    #[macro_export]
    macro_rules! Eq { /* omitted */ }
    #[doc(inline)]
    pub use Eq;
}
```

If your crate is called `foo`, and you export these aliases - another crate depending on `foo` will be
able to access the aliases as `foo::derive_alias::Eq`.

**Note:** This also exports the macro at the crate root, so another crate can access the alias as `foo::Eq` instead of
`foo::derive_alias::Eq`.

Crates depending on your crate will only see the derive aliases in `derive_aliases` module,
not at the crate root because of the `#[doc(hidden)]`
