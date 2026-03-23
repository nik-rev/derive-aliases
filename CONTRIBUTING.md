# Annotation tests

Annotations are block comments (`/*^`) that describe how an attribute expands.

Conceptually, `derive_aliases` is a macro that expands to a bunch of `#[derive]` macros.
We can't use `cargo expand` because that will expand all macros recursively (including the `derive`), so in
`annotation.rs` there is custom infrastructure to essentially expand the `#[derive_aliases::derive]` macro,
but not expand the `#[::core::derive]` macros.

The `annotation.rs` runs every file under `tests.rs` and checks that its annotations are correct.

## Example annotation test

```rust
#[derive_aliases::derive(..Serialize)]
#[derive_aliases::derive(..Clone)]
/*^
$(::core::clone::Clone,)
@(feature = "serde", $(::serde::Serialize,))
*/
struct A {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}
```

The above requires that this:

```rust
#[derive_aliases::derive(..Serialize)]
#[cfg_attr(feature = "serde", derive_aliases::derive(..Clone))]
struct A {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}
```

Will expand into this:

```rust
#[::core::prelude::v1::derive(::serde::Serialize,)]
#[cfg_attr(feature = "serde", ::core::prelude::v1::derive(::core::clone::Clone,))]
struct A {
    #[serde(skip_serializing_if = "Option::is_none")]
    field: Option<()>,
}
```

## Annotation test syntax

Annotation test comment must start before the item definition, and after
all attribitues. The annotation test block comment must start with `/*^` instead
of just `/*`

Additionally:

- Each line in the annotation must be a separate attribute.
- The `#[` at the start and `]` at the end must not be specified
- Writing `$` is the same as writing `::core::prelude::v1::derive`
- Writing `@` is the same as writing `cfg_attr`
