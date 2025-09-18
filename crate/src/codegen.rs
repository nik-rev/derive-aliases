//! Generating Rust code.
//!
//! All functions return `impl Iterator<Item = TokenTree>` even when they always return a single `TokenTree`.
//! This makes it easier to compose these functions together.

use crate::{chain, dsl};
use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::collections::HashSet;

/// Create a dummy import like this:
///
/// ```ignore
/// use crate::derive_aliases_doc::MyAlias as _;
///     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ import
/// ```
///
/// Then hovering over `MyAlias`:
///
/// ```ignore
/// #[derive(..MyAlias)]
///         // ^^^^^^^
/// ```
///
/// Will actually show documentation for the `derive_aliases_doc::MyAlias` because
/// we relate their spans together
pub fn use_underscore(
    import: impl Iterator<Item = TokenTree>,
    span: Span,
) -> impl Iterator<Item = TokenTree> {
    chain![
        ident("use", span),
        import.map(move |mut tt| {
            tt.set_span(span);
            tt
        }),
        ident("as", span),
        ident("_", span),
        punct(';')
    ]
}

/// Just like `use_underscore`, but keeps span of the import as-is
pub fn use_underscore_keep_span(
    import: impl Iterator<Item = TokenTree>,
    span: Span,
) -> impl Iterator<Item = TokenTree> {
    chain![
        ident("use", span),
        import,
        ident("as", span),
        ident("_", span),
        punct(';')
    ]
}

/// `.into_iter()` generates `compile_error!($message)` at `$span`
pub struct CompileError {
    /// Where the compile error is generates
    pub span: Span,
    /// Message of the compile error
    pub message: String,
}

impl CompileError {
    /// Create a new compile error
    pub fn new(span: Span, message: impl AsRef<str>) -> Self {
        Self {
            span,
            message: message.as_ref().to_string(),
        }
    }
}

impl IntoIterator for CompileError {
    type Item = TokenTree;
    type IntoIter = std::array::IntoIter<Self::Item, 3>;

    fn into_iter(self) -> Self::IntoIter {
        [
            TokenTree::Ident(Ident::new("compile_error", self.span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(self.span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(self.span);
                        string
                    })])
                });
                group.set_span(self.span);
                group
            }),
        ]
        .into_iter()
    }
}

/// For a single derive, generate `#[std::derive(..)]` with `#[cfg_attr(.., std::derive(..))]` if there is at least 1 configuration predicate
///
/// Generate one of:
///
/// - `#[std::derive($derives)]` if `cfg` is empty
/// - `#[cfg_attr($cfg, std::derive($derives))]` if there is just 1 `cfg` predicate
/// - `#[cfg_attr(all(...$cfg), std::derive($derives))]` if the are 2 or more `cfg` predicates
///
/// The goal is to minimize the amount of tokens created, leading to faster compile times
///
/// # Example
///
/// For example, in this `derive_aliases.rs` file:
///
/// ```rs
/// Eq = PartialEq, #[cfg(bar)] Eq;
/// Ord = PartialOrd, Ord, #[cfg(foo)] ..Eq;
/// ```
///
/// When expanding `..Ord`, we will generate:
///
/// ```ignore
/// #[std::derive(PartialOrd, Ord)]
/// #[cfg_attr(foo), std::derive(PartialEq)]
/// #[cfg_attr(all(foo, bar), std::derive(Eq))]
/// ```
pub fn cfg_std_derive_attr(
    cfgs: &[dsl::Cfg],
    derives: &[dsl::Derive],
    seen: &mut HashSet<dsl::Derive>,
) -> impl Iterator<Item = TokenTree> {
    // stuff #[<-- inside here ->]
    let input = match cfgs {
        [] => panic!(
            "caller handles empty `cfg` specially, by placing them into a single TokenStream"
        ),
        [cfg] => {
            chain![
                ident("cfg_attr", Span::call_site()),
                nested_in::<'(', ')'>(
                    chain![
                        cfg.cfg
                            .parse::<TokenStream>()
                            // TODO: CompilerError with exact span information instead of panicking
                            .expect("token stream inside of `#[cfg(...)]` is invalid"),
                        punct(','),
                        std_derive(into_std_derive_arguments(derives, seen).collect())
                    ]
                    .collect()
                ),
            ]
        }
        [..] => {
            // cfg_attr(all($cfg1, $cfg2,)), std::derive($derives)
            chain![
                ident("cfg_attr", Span::call_site()),
                nested_in::<'(', ')'>(
                    // all($cfg1, $cfg2,)
                    chain![
                        ident("all", Span::call_site()),
                        nested_in::<'(', ')'>(
                            cfgs.iter()
                                .flat_map(|cfg| {
                                    cfg.cfg
                                        .parse::<TokenStream>()
                                        // TODO: CompilerError with exact span information instead of panicking
                                        .expect("token stream inside of `#[cfg(...)]` is invalid")
                                        .into_iter()
                                        .chain(punct(','))
                                })
                                .collect()
                        ),
                        punct(','),
                        std_derive(into_std_derive_arguments(derives, seen).collect())
                    ]
                    .collect()
                ),
            ]
        }
    };

    attr(input.collect())
}

/// Wrap token stream in an attribute: `#[$tt]`
pub fn attr(tt: TokenStream) -> impl Iterator<Item = TokenTree> {
    punct('#').chain(nested_in::<'[', ']'>(tt))
}

/// A "function-like" attribute: `#[$f($arg)]`
pub fn attr_fn(f: &str, arg: &str) -> impl Iterator<Item = TokenTree> {
    attr(
        chain![
            ident(f, Span::call_site()),
            nested_in::<'(', ')'>(ident(arg, Span::call_site()).collect())
        ]
        .collect(),
    )
}

/// Resolve the derive arguments into something that can be passed into `std::derive(..)`
pub fn into_std_derive_arguments(
    derives: &[dsl::Derive],
    seen: &mut HashSet<dsl::Derive>,
) -> impl Iterator<Item = TokenTree> {
    // This is the token stream passed inside: #[std::derive(<-- all_derives ->)]`
    let mut all_derives = TokenStream::new();

    for (is_last, derive) in derives
        .iter()
        .enumerate()
        .map(|(i, derive)| (i + 1 == derives.len(), derive))
    {
        if seen.contains(derive) {
            // We've seen this derive before. Remember that we don't want to generate the same derive multiple times
            // and we allow using 2 aliases in the same derive even if they have overlapping aliases
            //
            // NOTE: This does not account for `#[cfg]` so it will be wrong sometimes, but there's nothing we can do about it
            continue;
        } else {
            // Mark as seen. If we then come across this exact at some point later, we'll ignore it
            seen.insert(derive.clone());
        }

        // build tokens for path of the derive, e.g. `std::hash::Hash`

        if derive.leading_colon.is_some() {
            all_derives.extend(path_sep());
        }

        all_derives.extend(ident(&derive.components.first.name, Span::call_site()));

        for (_, component) in &derive.components.items {
            all_derives.extend(path_sep());
            all_derives.extend(ident(&component.name, Span::call_site()));
        }

        // No trailing comma, this is especially important when we have no `cfg`s.
        //
        // e.g. we have:
        //
        // ..Alias, std::hash::Hash
        //
        // this might expand to:
        //
        // PartialEq, Eq, std::hash::Hash
        // ^^^^^^^^^^^^^ from expansion of `Alias`
        //
        //              ^ this comma is the same from the original input
        //
        // If we had actually inserted a trailing comma, we would have 2 commas in a row
        // which would be a syntax error
        if !is_last {
            all_derives.extend(punct(','));
        }
    }

    all_derives.into_iter()
}

/// Generates `::core::prelude::v1::derive($derives)`. We refer to it as `std::derive` everywhere for simplicity.
pub fn std_derive(derives: TokenStream) -> impl Iterator<Item = TokenTree> {
    chain![
        path_sep(),
        ident("core", Span::call_site()),
        path_sep(),
        ident("prelude", Span::call_site()),
        path_sep(),
        ident("v1", Span::call_site()),
        path_sep(),
        ident("derive", Span::call_site()),
        nested_in::<'(', ')'>(derives),
    ]
}

/// Wrap tokens inside of `[...]`, `{...}` or `(...)`
pub fn nested_in<const OPEN: char, const CLOSE: char>(
    stream: TokenStream,
) -> impl Iterator<Item = TokenTree> {
    [TokenTree::Group(Group::new(
        match (OPEN, CLOSE) {
            ('{', '}') => Delimiter::Brace,
            ('(', ')') => Delimiter::Parenthesis,
            ('[', ']') => Delimiter::Bracket,
            _ => panic!("invalid delimiters"),
        },
        stream,
    ))]
    .into_iter()
}

/// The token `::`
pub fn path_sep() -> impl Iterator<Item = TokenTree> {
    punct(':').chain(punct(':'))
}

/// Turn `ident` into a Rust identifier
pub fn ident(ident: impl AsRef<str>, span: Span) -> impl Iterator<Item = TokenTree> {
    [TokenTree::Ident(Ident::new(ident.as_ref(), span))].into_iter()
}

/// Turn `ch` into a Rust punctuation
pub fn punct(ch: char) -> impl Iterator<Item = TokenTree> {
    [TokenTree::Punct(Punct::new(ch, Spacing::Joint))].into_iter()
}

/// Generate a single line of a documentation comment, `#[doc = $comment]`
pub fn doc_comment(comment: &str) -> impl Iterator<Item = TokenTree> {
    attr(
        chain![
            ident("doc", Span::call_site()),
            punct('='),
            [TokenTree::Literal(Literal::string(comment))],
        ]
        .collect(),
    )
}
