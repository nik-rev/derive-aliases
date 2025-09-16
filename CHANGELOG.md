# v0.3.0

## Removed `feature = "workspace"`.

It used to allow you to put your `derive_aliases.rs` in the workspace root. Then in `.cargo/config.toml` you would define:

```toml
[env]
CARGO_WORKSPACE_DIR = { value = "", relative = true }
```

Then instead of reading from `${CARGO_MANIFEST_DIR}/derive_aliases.rs` we would read from `${CARGO_WORKSPACE_DIR}/derive_aliases.rs`, if `cfg(feature = "workspace")`

But this had problems.

1. While `CARGO_MANIFEST_DIR` is unique to each dependency, `CARGO_WORKSPACE_DIR` is global across
   the entire dependency tree. If your dependency uses crate `derive_alias`, you would override their derive aliases which would lead to problems
2. If your dependencies depend on `derive_aliases` and they do not expect to have `feature = "workspace"` activated, but you have it activated, because of Cargo's feature unification this will be enabled for them too. So their derive aliases will be read from *your* `derive_aliases.rs`, which would break things

You can still share a single `derive_aliases.rs` across your workspace, with the `use` statement

`workspace/derive_aliases.rs`:

```rs 
Copy = Copy, Clone;
```

`workspace/crates/foo/derive_aliases.rs`:

```rs 
use "../../derive_aliases.rs"

Eq = PartialEq, Eq;
```
