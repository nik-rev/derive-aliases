use std::fmt::{self, Display};
use std::hash::Hash;
use std::iter;
use std::{borrow::Cow, iter::Peekable};

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

/// Types that can be turned into a bunch of `TokenTree`s
pub trait IntoTokens {
    /// Convert this type into a bunch of `TokenTree`s
    fn into_tokens(self) -> impl Iterator<Item = TokenTree>;
}

impl IntoTokens for Literal {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        iter::once(TokenTree::Literal(self))
    }
}

impl IntoTokens for Punct {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        iter::once(TokenTree::Punct(self))
    }
}

impl IntoTokens for Group {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        iter::once(TokenTree::Group(self))
    }
}

impl IntoTokens for Ident {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        iter::once(TokenTree::Ident(self))
    }
}

impl IntoTokens for TokenTree {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        iter::once(self)
    }
}

/// Peekable iterator over tokens
pub struct TokensIter {
    /// Stream of tokens
    pub stream: Peekable<proc_macro::token_stream::IntoIter>,
    /// Location of the current span. Used in error reporting
    pub span: Span,
}

impl Display for TokensIter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.stream.clone().collect::<TokenStream>().fmt(f)
    }
}

impl TokensIter {
    /// Create a `compile_error!("message")`
    pub fn compile_error(&self, message: impl Into<Cow<'static, str>>) -> CompileError {
        CompileError::new(self.span, message)
    }

    /// Eat all the tokens until `f` returns `true`. On punctuation tokens, it is passed the character. Use for error recovery
    pub fn eat_until_char(&mut self, f: impl Fn(char) -> bool) {
        loop {
            match self.tt() {
                // reached end of the alias declaration
                Some(TokenTree::Punct(punct)) if punct == ',' || punct == ';' => {
                    if f(punct.as_char()) {
                        break;
                    } else {
                        // predicate wasn't satisfied. continue
                    }
                }
                // reached end of the input
                None => break,
                // eat everything until the next alias declaration,
                // that way we can report multiple errors
                _ => (),
            }
        }
    }

    /// Peek the next token tree
    pub fn peek_tt(&mut self) -> Option<&TokenTree> {
        self.stream.peek()
    }

    /// Get the next token tree
    pub fn tt(&mut self) -> Option<TokenTree> {
        match self.stream.next() {
            Some(tt) => {
                self.span = tt.span();
                Some(tt)
            }
            None => None,
        }
    }

    /// Peek the next punctuation if it is a `char`
    pub fn peek_char(&mut self, char: char) -> Option<&Punct> {
        match self.stream.peek() {
            Some(TokenTree::Punct(punct)) if *punct == char => Some(punct),
            _ => None,
        }
    }

    /// Get the next token punctuation if it is `char`
    pub fn char(&mut self, char: char) -> Option<Punct> {
        match self.peek_tt() {
            Some(TokenTree::Punct(punct)) if *punct == char => {
                let Some(TokenTree::Punct(punct)) = self.stream.next() else {
                    unreachable!(".peek() returned `Some(TokenTree::Punct)`")
                };
                self.span = punct.span();

                Some(punct)
            }
            Some(tt) => {
                self.span = tt.span();
                None
            }
            None => None,
        }
    }

    /// Get insides of the next group with the given `delimiter`
    pub fn group(&mut self, delimiter: Delimiter) -> Option<TokenStream> {
        match self.peek_tt() {
            Some(TokenTree::Group(group)) if group.delimiter() == delimiter => {
                let Some(TokenTree::Group(group)) = self.stream.next() else {
                    unreachable!(".peek() returned `Some(TokenTree::Group)`")
                };
                self.span = group.span();

                Some(group.stream())
            }
            Some(tt) => {
                self.span = tt.span();
                None
            }
            None => None,
        }
    }

    /// Get the next token punctuation if it is an identifier
    pub fn ident(&mut self) -> Option<Ident> {
        match self.stream.peek() {
            Some(TokenTree::Ident(_ident)) => {
                let Some(TokenTree::Ident(ident)) = self.stream.next() else {
                    unreachable!(".peek() returned `Some(TokenTree::Ident)`")
                };
                self.span = ident.span();

                Some(ident)
            }
            Some(tt) => {
                self.span = tt.span();
                None
            }
            None => None,
        }
    }

    /// Expect `::`
    pub fn path_separator(&mut self) -> Option<PathSeparator> {
        match self.peek_tt() {
            Some(TokenTree::Punct(colon)) if *colon == ':' => {
                let span = colon.span();
                self.tt();

                match self.tt() {
                    Some(TokenTree::Punct(colon_colon)) if colon_colon == ':' => {
                        Some(PathSeparator {
                            first: span,
                            second: colon_colon.span(),
                        })
                    }
                    _ => None,
                }
            }
            Some(tt) => {
                self.span = tt.span();
                None
            }
            None => None,
        }
    }

    /// Expect a path.
    ///
    /// Requires the `ts` to start with `::segment` (absolute) or `segment` (relative), then
    /// expects 0 or more `::segment`s followed by whatever (once we hit "whatever", stops trying to parse further)
    ///
    /// `start_span` is the span of the thing directly before the `Path`
    pub fn path(&mut self) -> Result<Path, CompileError> {
        // Parse beginning of the path

        // ::std::hash::Hash
        // ^^
        let leading_colon = self.path_separator();

        // ::std::hash::Hash
        //   ^^^
        let first_component = self.ident().ok_or_else(|| {
            self.compile_error(if leading_colon.is_some() {
                "expected identifier to form a path like `::std::hash::Hash`"
            } else {
                "expected identifier or `::` to form a path like `::std::hash::Hash`"
            })
        })?;

        let mut components = Vec::new();

        // Parses rest of the path. Each segment is preceded by the path separator `::`
        //
        // ::std::hash::Hash
        //      ^^            iteration 1
        //        ^^^^
        //
        //            ^^      iteration 2
        //              ^^^^
        loop {
            if self.peek_char(':').is_none() {
                // finished parsing path
                break;
            }

            let separator = self.path_separator().ok_or_else(|| {
                self.compile_error("expected `::` to form a path like `::std::hash::Hash`")
            })?;

            let component = self.ident().ok_or_else(|| {
                self.compile_error("expected identifier to form a path like `::std::hash::Hash`")
            })?;

            components.push((separator, (component)));
        }

        Ok(Path {
            leading_colon,
            first_component,
            components,
        })
    }
}

/// A path to an item
///
/// Example: `::std::hash::Hash`
#[derive(Clone, Debug)]
pub struct Path {
    /// `true` if the leading colon is present
    ///
    /// ```txt
    /// ::std::hash::Hash
    /// ^^
    /// ```
    pub leading_colon: Option<PathSeparator>,
    /// First component of the derive
    ///
    /// ```txt
    /// ::std::hash::Hash
    ///   ^^^
    /// ```
    pub first_component: Ident,
    /// Other components of the derive
    ///
    /// ```txt
    /// ::std::hash::Hash
    ///      ^^^^^^
    ///            ^^^^^^
    /// ```
    pub components: Vec<(PathSeparator, Ident)>,
}

impl IntoTokens for Path {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        self.leading_colon
            .map(PathSeparator::into_tokens)
            .into_iter()
            .flatten()
            // first path segment
            //
            // ::core::hash::Hash
            //   ^^^
            .chain([TokenTree::Ident(self.first_component)])
            .chain(
                self.components
                    .into_iter()
                    .flat_map(|(separator, segment)| {
                        // path separator `::`
                        //
                        // ::std::hash::HashMap
                        //      ^^    ^^
                        separator
                            .into_tokens()
                            // path segment
                            //
                            // ::std::hash::HashMap
                            //        ^^^^  ^^^^^^^
                            .chain(std::iter::once(TokenTree::Ident(segment)))
                    }),
            )
    }
}

/// Separator in a path: `::`
///
/// ```ignore
/// ::std::hash::Hash
/// ^^   ^^    ^^
/// ```
#[derive(Clone, Debug)]
pub struct PathSeparator {
    /// Span of the first `:`
    ///
    /// ```ignore
    /// ::std::hash::Hash
    /// ^    ^     ^
    /// ```
    pub first: Span,
    /// Span of the second `:`
    ///
    /// ```ignore
    /// ::std::hash::Hash
    ///  ^    ^     ^
    /// ```
    pub second: Span,
}

impl IntoTokens for PathSeparator {
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        let mut first = Punct::new(':', Spacing::Joint);
        first.set_span(self.first);
        let mut second = Punct::new(':', Spacing::Joint);
        second.set_span(self.second);
        [TokenTree::Punct(first), TokenTree::Punct(second)].into_iter()
    }
}

impl Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.leading_colon.is_some().hash(state);
        self.first_component.to_string().hash(state);
        for (_, component) in &self.components {
            component.to_string().hash(state);
        }
    }
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.leading_colon.is_some() == other.leading_colon.is_some()
            && self.first_component.to_string() == other.first_component.to_string()
            && self
                .components
                .iter()
                .map(|(_, ident)| ident.to_string())
                .eq(other.components.iter().map(|(_, ident)| ident.to_string()))
    }
}

impl Eq for Path {}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // ::std::hash::Hash
        // ^^
        if self.leading_colon.is_some() {
            f.write_str("::")?;
        }
        // ::std::hash::Hash
        //   ^^^
        f.write_str(&self.first_component.to_string())?;
        for (_sep, component) in &self.components {
            // ::std::hash::Hash
            //      ^^
            //            ^^
            f.write_str("::")?;
            // ::std::hash::Hash
            //        ^^^^
            //              ^^^^
            f.write_str(&component.to_string())?;
        }

        Ok(())
    }
}

/// `.into_iter()` generates `compile_error!($message)` at `$span`
#[derive(Debug)]
pub struct CompileError {
    /// Where the compile error is generates
    pub span: Span,
    /// Message of the compile error
    pub message: Cow<'static, str>,
}

impl CompileError {
    /// Create a new `compile_error!`
    pub fn new(span: Span, message: impl Into<Cow<'static, str>>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

impl IntoTokens for CompileError {
    /// Generate the actual `compile_error!("message")`
    fn into_tokens(self) -> impl Iterator<Item = TokenTree> {
        [
            // compile_error!("message")
            // ^^^^^^^^^^^^^
            TokenTree::Ident(Ident::new("compile_error", self.span)),
            // compile_error!("message")
            //              ^
            TokenTree::Punct(Punct::new('!', Spacing::Alone)).with_span(self.span),
            // compile_error!("message")
            //               ^^^^^^^^^^^
            TokenTree::Group({
                Group::new(Delimiter::Brace, {
                    TokenStream::from_iter([
                        // compile_error!("message")
                        //                ^^^^^^^^^
                        TokenTree::Literal(Literal::string(&self.message)).with_span(self.span),
                    ])
                })
            })
            .with_span(self.span),
        ]
        .into_iter()
    }
}

/// Types that have a `Span`
trait Spanned {
    /// Set span of the item
    fn with_span(self, span: Span) -> Self;
}

impl Spanned for TokenTree {
    fn with_span(mut self, span: Span) -> Self {
        self.set_span(span);
        self
    }
}
