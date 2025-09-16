//! Parser for the Derive Alias DSL
//!
//! A single unit that can appear on the RHS of an alias definition
//!
//! # Grammar
//!
//! ```txt
//! alphabetic := 'a'..='z' | 'A'..='Z'
//! alphanumeric := alphabetic | '0'..='9'
//! string := '"' .* '"'
//!
//! Ident := '_' | alphabetic [ '_' | alphanumeric ]*
//! Derive := "::"? Ident [ "::" Ident ]*
//! Alias := Ident
//! AliasExpansion := '.' '.' Alias
//! AliasedItem := Derive | AliasExpansion
//! Cfg := "#[cfg(" .* ")]"
//! Aliased := Cfg? AliasedKind
//!
//! AliasDeclaration := Cfg? Alias '=' Aliased  [ ',' Aliased ]* ','?
//!
//! Import := "use" string  
//!
//! Stmt := AliasDeclaration | Import
//! Dsl := [ Stmt ';' ] *
//! ```

use std::hash::Hash;
use std::mem;
use std::path::Path;
use std::str::Chars;
use std::{ops::Range, path::PathBuf, sync::Arc};

/// Example file for the DSL
pub const EXAMPLE: &str = "\
// Simple derive aliases
//
// `#[derive(..Copy, ..Eq)]` expands to `#[std::derive(Copy, Clone, PartialEq, Eq)]`

Copy = Copy, Clone;
Eq = PartialEq, Eq;

// You can nest them!
//
// `#[derive(..Ord, std::hash::Hash)]` expands to `#[std::derive(PartialOrd, Ord, PartialEq, Eq, std::hash::Hash)]`

Ord = PartialOrd, Ord, ..Eq;";

pub mod token {
    use super::Span;

    /// `"cfg"`
    #[derive(Debug, Clone, Eq, Hash, PartialEq)]
    pub struct Cfg(pub Span);
    /// `"use"`
    #[derive(Debug, Clone)]
    pub struct Use(pub Span);
    /// `"::"`
    #[derive(Debug, Clone)]
    pub struct Colon(pub Span);
    /// `"="`
    #[derive(Debug, Clone)]
    pub struct Eq(pub Span);
    /// `","`
    #[derive(Debug, Clone)]
    pub struct Comma(pub Span);
    /// `"."`
    #[derive(Debug, Clone)]
    pub struct Dot(pub Span);
}

/// A list of `T`s separated by `Sep`.
#[derive(Debug, Clone)]
pub struct Separated<T, Sep> {
    /// The first `T` does not have any punctuation
    pub first: T,
    /// Each `T` with a punctuation before it
    pub items: Vec<(Sep, T)>,
}

/// Represents a location for use in error reporting
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Span {
    /// Byte offset in the source file
    pub location: Range<usize>,
    /// File that this span belongs to
    pub file: Arc<PathBuf>,
}

/// An identifier
#[derive(Debug, Clone)]
pub struct Ident {
    /// Name of this identifier
    pub name: String,
    /// Span of the identifier
    pub span: Span,
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name)
    }
}

impl Eq for Ident {}

/// Path to derive, like `std::hash::Hash`
#[derive(Debug, Clone)]
pub struct Derive {
    /// Span of the entire derive
    pub span: Span,
    /// `Some(span)` if we have the first `"::"`. It is optional
    pub leading_colon: Option<token::Colon>,
    /// Components, separated by `"::"`
    pub components: Separated<Ident, token::Colon>,
}

/// An alias, which expands to 1 or more `Aliased`s
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Alias(pub Ident);

/// An alias expansion, `..Alias`
#[derive(Debug, Clone)]
pub struct AliasExpansion {
    /// first `.`
    pub dot_1: token::Dot,
    /// second `.`
    pub dot_2: token::Dot,
    /// Alias e.g. `Alias`
    pub alias: Alias,
}

/// What an alias expands to
#[derive(Debug, Clone)]
pub enum AliasedItem {
    /// Path to derive, like `std::hash::Hash`
    Derive(Derive),
    /// An alias expansion, `..Alias`
    AliasExpansion(AliasExpansion),
}

/// Configuration predicate, passed verbatim to Rust
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Cfg {
    /// Span of the entire configuration predicate
    pub span: Span,
    /// The `"cfg"` keyword
    pub keyword: token::Cfg,
    /// Content inside `#[cfg(<-- here -->)]`
    pub cfg: String,
}

impl PartialOrd for Cfg {
    fn partial_cmp(&self, other: &Cfg) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Cfg {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.cfg.cmp(&other.cfg)
    }
}

/// A single aliased item
#[derive(Debug, Clone)]
pub struct Aliased {
    /// Span of the aliased contents
    pub span: Span,
    /// `#[cfg(...)]` that must be activated
    pub cfg: Option<Cfg>,
    /// The item that is aliased
    pub item: AliasedItem,
}

/// Declaration for a new alias
///
/// ```
/// MyAlias = #[cfg(feature = "foo")] Copy, Clone, ..Eq;
/// ^^^^^^^ alias
///           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ aliased
/// ```
#[derive(Debug, Clone)]
pub struct AliasDeclaration {
    /// `#[cfg(...)]` that must be activated
    pub cfg: Option<Cfg>,
    /// Span of the entire declaration
    pub span: Span,
    /// Name of the alias we are declaring
    pub alias: Alias,
    /// `"="` token
    pub eq_token: token::Eq,
    /// All aliased items
    pub aliased: Separated<Aliased, token::Comma>,
    /// Trailing `","` after the aliased
    pub trailing_comma: Option<token::Comma>,
}

/// `"use"` with a string to inline that file's contents
#[derive(Debug, Clone)]
pub struct Import {
    /// Location of the import statement
    pub span: Span,
    /// The `"use"`
    pub use_token: token::Use,
    /// Path that we are importing
    pub path: Arc<PathBuf>,
}

/// A single statemen
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Alias declaration
    AliasDeclaration(AliasDeclaration),
    /// Import statement to inline the contents of another derive alias file
    Import(Import),
}

/// The entire Derive Alias DSL syntax tree
#[derive(Debug, Clone)]
pub struct File {
    /// Span of the full file
    pub span: Span,
    /// List of statements in the file
    pub stmts: Vec<Stmt>,
    /// Encountered errors while parsing the file
    pub errors: Vec<ParseError>,
}

/// Error encountered while parsing
#[derive(Debug, Clone)]
pub struct ParseError {
    /// Location of the error
    pub span: Span,
    /// Error message
    pub message: String,
}

/// Functions return `Result<T, EncounteredParseError>` as we internally keep a list of errors, for better recovery
pub struct EncounteredParseError;

/// Parser of our Derive Aliases DSL
pub struct Parser<'a> {
    /// Original source code
    source: &'a str,
    /// Characters iterator over the source code
    ///
    /// This iterator is very cheap to clone, so we clone it quite a lot.
    chars: Chars<'a>,
    /// Path to the file that we are parsing
    file_path: Arc<PathBuf>,
    /// Current offset from the start of the file
    offset: usize,
    /// Internal list of errors that we keep track of **while parsing**.
    ///
    /// These errors are emptied into the `File`'s `errors`, so do not read from here once the parser has finished its job.
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    /// Parse `source` at `file_path`
    fn new(source: &'a str, file_path: Arc<PathBuf>) -> Self {
        Parser {
            source,
            chars: source.chars(),
            file_path,
            offset: 0,
            errors: Vec::new(),
        }
    }

    /// Peek the next character
    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Advance by 1 character
    fn eat(&mut self) -> Option<char> {
        let ch = self.chars.next();
        if let Some(c) = ch {
            self.offset += c.len_utf8();
        }
        ch
    }

    /// Like `eat`, but asserts the exact character we expect to see
    fn expect(&mut self, c: char) {
        let ch = self.eat().expect("non-empty");
        assert_eq!(
            ch, c,
            "BUG: expected {c} as offset {}, but found {ch}",
            self.offset
        );
    }

    /// Create a new span, for use in error reporting
    fn span(&self, start: usize, end: usize) -> Span {
        Span {
            location: start..end,
            file: Arc::clone(&self.file_path),
        }
    }

    /// Create a parse error
    fn error(&mut self, start: usize, end: usize, message: impl AsRef<str>) {
        debug_assert!(
            !message
                .as_ref()
                .chars()
                .next()
                .expect("error message is non-empty")
                .is_ascii_uppercase(),
            "error message must not start with an uppercase letter"
        );
        debug_assert!(
            !message.as_ref().ends_with("."),
            "error message must not end with a period"
        );
        self.errors.push(ParseError {
            span: self.span(start, end),
            message: message.as_ref().to_string(),
        });
    }

    /// Parse a single file
    fn parse_file(&mut self) -> File {
        let start_offset = self.offset;
        let mut stmts = Vec::new();

        self.skip_noise();

        while self.peek().is_some() {
            match self.parse_stmt() {
                Ok(stmt) => {
                    stmts.push(stmt);
                    self.skip_noise();
                }
                Err(EncounteredParseError) => {
                    // Error recovery: skip to the next potential statement separator
                    self.skip_to_recover();
                }
            }
        }

        // We've advanced all the way to the end
        let end_offset = self.offset;

        File {
            span: self.span(start_offset, end_offset),
            stmts,
            errors: mem::take(&mut self.errors),
        }
    }

    /// Noise = whitespace and comments, since they don't do anything and are just for the user
    fn skip_noise(&mut self) {
        loop {
            let _start_offset = self.offset;
            if let Some(c) = self.peek() {
                if c.is_whitespace() {
                    self.eat();
                    continue;
                }
                if c == '/' {
                    let mut chars = self.chars.clone();
                    chars.next();
                    if let Some(c2) = chars.next() {
                        match c2 {
                            '/' => {
                                // Line comment
                                self.expect('/');
                                self.expect('/');

                                // Eat all of the line comment's contents
                                while let Some(c) = self.peek() {
                                    // Eat until end of the line.
                                    if c == '\n' {
                                        break;
                                    }
                                    self.eat();
                                }

                                continue;
                            }
                            '*' => {
                                // First block comment token
                                self.expect('/');
                                self.expect('*');

                                // Eat all of the line comment's contents
                                let mut chars = self.chars.clone().peekable();
                                while let (Some(c), Some(c2)) = (chars.next(), chars.peek()) {
                                    // Eat until end of block comment.
                                    if c == '*' && *c2 == '/' {
                                        // Closing block comment token
                                        self.expect('*');
                                        self.expect('/');
                                        break;
                                    }
                                    self.eat();
                                }

                                continue;
                            }
                            _ => (),
                        }
                    }
                }
            }
            break;
        }
    }

    // When we failed to parse a statement, let's skip to the next one so we can report as many errors as possible
    fn skip_to_recover(&mut self) {
        while let Some(c) = self.peek() {
            if c == ';' {
                self.eat();
                return;
            }
            self.eat();
        }
    }

    /// Parse a single statement. Statements are delimited by semicolons
    ///
    /// ```txt
    /// Alias = Foo, ..Bar;
    /// ^^^^^^^^^^^^^^^^^^
    /// ```
    fn parse_stmt(&mut self) -> Result<Stmt, EncounteredParseError> {
        let start_offset = self.offset;
        let cfg = self.parse_cfg().transpose()?;
        self.skip_noise();

        let current_offset = self.offset;
        let ident_res = self.parse_ident();

        if let Ok(ident) = ident_res {
            let ident_name = ident.name.as_str();

            self.skip_noise();

            if self.peek() == Some('=') {
                return Ok(Stmt::AliasDeclaration(
                    self.parse_alias_declaration(cfg, ident)?,
                ));
            } else if ident_name == "use" {
                if cfg.is_some() {
                    let err_start = start_offset;
                    let err_end = self.offset;
                    self.error(
                        err_start,
                        err_end,
                        "import statements cannot have `#[cfg]` attributes",
                    );
                }
                return Ok(Stmt::Import(self.parse_import()?));
            } else {
                let err_start = current_offset;
                let err_end = self.offset;
                self.error(
                    err_start,
                    err_end,
                    format!("expected '=' or a keyword like 'use', but found '{ident_name}'"),
                );
                return Err(EncounteredParseError);
            }
        }

        let err_start = start_offset;
        let err_end = self.offset;
        self.error(
            err_start,
            err_end,
            "expected an identifier or `#[cfg]` at the start of a statement",
        );

        Err(EncounteredParseError)
    }

    /// Parse a `#[cfg]`:
    ///
    /// ```txt
    /// #[cfg(feature = "something")]
    /// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// Alias = Foo, ..Bar;
    /// ```
    ///
    /// We don't actually parse whatever is on the inside, as that is directly written to Rust, verbatim.
    fn parse_cfg(&mut self) -> Option<Result<Cfg, EncounteredParseError>> {
        let start_offset = self.offset;
        let mut temp_chars = self.chars.clone();

        if temp_chars.next() == Some('#') && temp_chars.next() == Some('[') {
            self.expect('#');
            self.expect('[');
            let _bracket_span = self.span(start_offset + 1, self.offset);

            self.skip_noise();

            let ident_start = self.offset;
            let ident = self.parse_ident();

            if let Ok(ident) = ident {
                if ident.name == "cfg" {
                    let keyword_span = self.span(ident_start, self.offset);
                    self.skip_noise();

                    if self.peek() == Some('(') {
                        self.expect('(');
                        let content_start = self.offset;

                        // The level of parenthee
                        let mut paren_level = 1;
                        let mut content_end = content_start;

                        while let Some(c) = self.peek() {
                            if c == '(' {
                                paren_level += 1;
                            } else if c == ')' {
                                paren_level -= 1;
                                if paren_level == 0 {
                                    break;
                                }
                            }
                            self.eat();
                            content_end = self.offset;
                        }

                        if self.peek() != Some(')') {
                            let err_start = start_offset;
                            let err_end = self.offset;
                            self.error(
                                err_start,
                                err_end,
                                "unbalanced parentheses in `#[cfg(...)]`. Expected ')'",
                            );
                            return Some(Err(EncounteredParseError));
                        }

                        let cfg_content = self.source[content_start..content_end].to_string();
                        self.eat(); // )

                        self.skip_noise();

                        if self.peek() == Some(']') {
                            self.eat(); // ]
                            let end_offset = self.offset;
                            let full_span = self.span(start_offset, end_offset);
                            return Some(Ok(Cfg {
                                span: full_span,
                                keyword: token::Cfg(keyword_span),
                                cfg: cfg_content,
                            }));
                        } else {
                            let err_start = content_end + 1;
                            let err_end = self.offset;
                            self.error(err_start, err_end, "expected ']' to close `#[cfg(...)`");
                            return Some(Err(EncounteredParseError));
                        }
                    } else {
                        let err_start = self.offset;
                        let err_end = self.offset + 1;
                        self.error(err_start, err_end, "expected '(' after `cfg`");
                        return Some(Err(EncounteredParseError));
                    }
                } else {
                    let err_start = ident_start;
                    let err_end = self.offset;
                    self.error(
                        err_start,
                        err_end,
                        format!("expected `cfg`, but found `{}`", ident.name),
                    );
                    return Some(Err(EncounteredParseError));
                }
            } else {
                let err_start = ident_start;
                let err_end = self.offset;
                self.error(err_start, err_end, "expected an identifier after `#[`");
                return Some(Err(EncounteredParseError));
            }
        }

        None
    }

    fn parse_import(&mut self) -> Result<Import, EncounteredParseError> {
        let start_offset = self.offset;
        let use_token_span = self.span(start_offset, start_offset + 3);

        self.skip_noise();

        let path_start = self.offset;
        if self.peek() != Some('"') {
            self.error(
                path_start,
                path_start,
                "expected a string literal for the path",
            );
            return Err(EncounteredParseError);
        }

        // Opening quote
        self.expect('"');

        let path_content_start = self.offset;
        let mut path_content_end = path_content_start;

        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            self.eat();
            path_content_end = self.offset;
        }

        if self.peek() != Some('"') {
            self.error(
                path_content_start,
                path_content_end,
                "unclosed string literal",
            );
            return Err(EncounteredParseError);
        }

        let path_str = &self.source[path_content_start..path_content_end];
        let path_buf = PathBuf::from(path_str);

        // Closing quote
        self.expect('"');

        self.skip_noise();

        if self.peek() != Some(';') {
            self.error(
                self.offset,
                self.offset,
                "expected ';' after `use` statement",
            );
            return Err(EncounteredParseError);
        }

        // Statement terminator
        self.expect(';');

        let end_offset = self.offset;
        let full_span = self.span(start_offset, end_offset);

        Ok(Import {
            span: full_span,
            use_token: token::Use(use_token_span),
            path: Arc::new(path_buf),
        })
    }

    fn parse_alias_declaration(
        &mut self,
        cfg: Option<Cfg>,
        alias: Ident,
    ) -> Result<AliasDeclaration, EncounteredParseError> {
        let start_offset = cfg
            .as_ref()
            .map(|c| c.span.location.start)
            .unwrap_or(alias.span.location.start);

        let eq_start = self.offset;
        if self.peek() != Some('=') {
            self.error(eq_start, eq_start, "expected '=' after alias name");
            return Err(EncounteredParseError);
        }
        self.eat(); // =
        let eq_token = token::Eq(self.span(eq_start, self.offset));

        self.skip_noise();

        let (first, items) = self.parse_separated_aliased()?;

        let aliased = Separated { first, items };

        let mut trailing_comma = None;
        self.skip_noise();
        if self.peek() == Some(',') {
            let comma_span = self.span(self.offset, self.offset + 1);
            trailing_comma = Some(token::Comma(comma_span));
            self.eat();
        }

        self.skip_noise();

        if self.peek() != Some(';') {
            self.error(
                self.offset,
                self.offset,
                "expected ';' after alias declaration",
            );
            return Err(EncounteredParseError);
        }
        self.eat(); // ;

        let end_offset = self.offset;
        let full_span = self.span(start_offset, end_offset);

        Ok(AliasDeclaration {
            cfg,
            span: full_span,
            alias: Alias(alias),
            eq_token,
            aliased,
            trailing_comma,
        })
    }

    fn parse_separated_aliased(
        &mut self,
    ) -> Result<(Aliased, Vec<(token::Comma, Aliased)>), EncounteredParseError> {
        let first = self.parse_aliased()?;
        let mut items = Vec::new();

        self.skip_noise();

        while self.peek() == Some(',') {
            let comma_start = self.offset;
            self.eat();
            let comma_token = token::Comma(self.span(comma_start, self.offset));

            self.skip_noise();

            let item = self.parse_aliased()?;
            items.push((comma_token, item));

            self.skip_noise();
        }

        Ok((first, items))
    }

    fn parse_aliased(&mut self) -> Result<Aliased, EncounteredParseError> {
        let start_offset = self.offset;
        let cfg = self.parse_cfg().transpose()?;

        self.skip_noise();

        let item = self.parse_aliased_item()?;

        let end_offset = self.offset;
        let full_span = self.span(start_offset, end_offset);

        Ok(Aliased {
            span: full_span,
            cfg,
            item,
        })
    }

    fn parse_aliased_item(&mut self) -> Result<AliasedItem, EncounteredParseError> {
        let start_offset = self.offset;
        let mut temp_chars = self.chars.clone();

        if temp_chars.next() == Some('.') && temp_chars.next() == Some('.') {
            // Alias expansion
            self.eat(); // .
            self.eat(); // .
            let _dot_dot_span = self.span(start_offset, self.offset);

            self.skip_noise();

            let alias_ident = self.parse_ident()?;

            Ok(AliasedItem::AliasExpansion(AliasExpansion {
                dot_1: token::Dot(self.span(start_offset, start_offset + 1)),
                dot_2: token::Dot(self.span(start_offset + 1, start_offset + 2)),
                alias: Alias(alias_ident),
            }))
        } else {
            // Derive
            Ok(AliasedItem::Derive(self.parse_derive()?))
        }
    }

    fn parse_derive(&mut self) -> Result<Derive, EncounteredParseError> {
        let start_offset = self.offset;
        let mut leading_colon = None;

        if self.peek() == Some(':') {
            let mut chars = self.chars.clone();
            chars.next();
            if chars.next() == Some(':') {
                let colon_start = self.offset;
                // eat ':'
                self.eat();
                // eat ':'
                self.eat();
                leading_colon = Some(token::Colon(self.span(colon_start, self.offset)));
            } else {
                self.error(self.offset, self.offset + 1, "expected `::`");
                return Err(EncounteredParseError);
            }
        }

        self.skip_noise();

        let _ident_start = self.offset;
        let first_ident = self.parse_ident()?;
        let mut components = vec![];

        self.skip_noise();

        while self.peek() == Some(':') {
            let mut temp_chars = self.chars.clone();
            temp_chars.next();
            if temp_chars.next() == Some(':') {
                let colon_start = self.offset;
                self.eat(); // :
                self.eat(); // :
                let colon_token = token::Colon(self.span(colon_start, self.offset));

                self.skip_noise();

                let next_ident = self.parse_ident()?;
                components.push((colon_token, next_ident));

                self.skip_noise();
            } else {
                let err_start = self.offset;
                let err_end = self.offset + 1;
                self.error(err_start, err_end, "expected `::`");
                return Err(EncounteredParseError);
            }
        }

        let end_offset = self.offset;
        let full_span = self.span(start_offset, end_offset);

        Ok(Derive {
            span: full_span,
            leading_colon,
            components: Separated {
                first: first_ident,
                items: components,
            },
        })
    }

    fn parse_ident(&mut self) -> Result<Ident, EncounteredParseError> {
        let start_offset = self.offset;
        let mut ident_name = String::new();

        let c = self.peek();
        if c.is_none() || (!c.unwrap().is_ascii_alphabetic() && c.unwrap() != '_') {
            let err_start = self.offset;
            let err_end = self.offset;
            self.error(
                err_start,
                err_end,
                "expected an identifier starting with an alphabet or `_`",
            );
            return Err(EncounteredParseError);
        }

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                ident_name.push(c);
                self.eat();
            } else {
                break;
            }
        }

        let end_offset = self.offset;
        let full_span = self.span(start_offset, end_offset);

        Ok(Ident {
            name: ident_name,
            span: full_span,
        })
    }
}

pub fn parse_single_file(source: impl AsRef<str>, file_path: impl AsRef<Path>) -> File {
    let file_arc = Arc::new(file_path.as_ref().to_path_buf());
    let mut parser = Parser::new(source.as_ref(), file_arc);
    parser.parse_file()
}

/// Render a single error, showing span information and location in the source file
pub fn render_error(error: &ParseError, file: &str, source: &str) -> String {
    let mut rendered_error = String::new();
    let span = &error.span;
    let start = span.location.start.max(0);
    let end = span.location.end.max(source.len());
    let start_line = source[..start].lines().count();
    let start_col = source[..start]
        .chars()
        .rev()
        .take_while(|c| *c != '\n')
        .count();

    rendered_error.push_str(&format!(
        "[{file}:{start_line}:{start_col}] Parse Error: {}:\n\n",
        error.message
    ));

    let line_content = source
        .lines()
        .nth(start_line.saturating_sub(1))
        .unwrap_or("");
    rendered_error.push_str(&format!("  {line_content}\n"));

    let indicator_len = source[start..end].len();
    let indicator = format!("{: >start_col$}{}", "", "^".repeat(indicator_len.max(1)));
    rendered_error.push_str(&format!("  {indicator}\n"));

    rendered_error
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn parse_and_check_errors(source: &str) -> File {
        let file_path = PathBuf::from("derive_alias.rs");
        let file = parse_single_file(source, file_path);
        if !file.errors.is_empty() {
            eprintln!(
                "{}",
                file.errors
                    .iter()
                    .map(|err| render_error(err, &file.span.file.to_string_lossy(), source))
                    .collect::<String>()
            );
        }
        file
    }

    #[test]
    fn alias_declaration() {
        let source = "Copy = Copy, Clone;";
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);

        let Stmt::AliasDeclaration(decl) = &file.stmts[0] else {
            panic!()
        };

        assert_eq!(decl.alias.0.name, "Copy");
        assert_eq!(decl.aliased.items.len(), 1);
        if let AliasedItem::Derive(derive) = &decl.aliased.first.item {
            assert_eq!(derive.components.first.name, "Copy");
        } else {
            panic!();
        }

        if let AliasedItem::Derive(derive) = &decl.aliased.items[0].1.item {
            assert_eq!(derive.components.first.name, "Clone");
        } else {
            panic!();
        }
    }

    #[test]
    fn alias_decl_with_cfgs() {
        let source = r#"#[cfg(feature = "serde")] Eq = PartialEq, #[cfg(feature = "bar")] ..Eq;"#;
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);

        let Stmt::AliasDeclaration(decl) = &file.stmts[0] else {
            panic!()
        };

        assert!(decl.cfg.is_some());
        assert_eq!(decl.cfg.as_ref().unwrap().cfg, r#"feature = "serde""#);

        assert_eq!(decl.alias.0.name, "Eq");

        let Aliased {
            cfg: Some(ref cfg),
            item: AliasedItem::AliasExpansion(AliasExpansion { ref alias, .. }),
            ..
        } = decl.aliased.items.first().unwrap().1
        else {
            panic!()
        };

        assert_eq!(cfg.cfg, "feature = \"bar\"");
        assert_eq!(alias.0.name, "Eq");
    }

    #[test]
    fn import_statement() {
        let source = r#"use "my_lib.da";"#;
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);

        if let Stmt::Import(import) = &file.stmts[0] {
            assert_eq!(*import.path, PathBuf::from("my_lib.da"));
        } else {
            panic!("Expected Import");
        }
    }

    #[test]
    fn derive_with_leading_colon() {
        let source = r#"DerivePath = ::std::path::PathBuf;"#;
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);

        if let Stmt::AliasDeclaration(decl) = &file.stmts[0] {
            if let AliasedItem::Derive(derive) = &decl.aliased.first.item {
                assert!(derive.leading_colon.is_some());
                assert_eq!(derive.components.first.name, "std");
                assert_eq!(derive.components.items[0].1.name, "path");
            } else {
                panic!("Expected Derive");
            }
        }
    }

    #[test]
    fn alias_expansion() {
        let source = "Ord = ..Eq, PartialOrd;";
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);

        if let Stmt::AliasDeclaration(decl) = &file.stmts[0] {
            if let AliasedItem::AliasExpansion(expansion) = &decl.aliased.first.item {
                assert_eq!(expansion.alias.0.name, "Eq");
            } else {
                panic!("Expected AliasExpansion");
            }
        }
    }

    #[test]
    fn multiple_statements() {
        let source = r#"
            A = B;
            #[cfg(debug_assertions)]
            C = D, E, F;
            use "path/to/another.da";
            G = ..H;
        "#;
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 4);
    }

    #[test]
    fn block_comments() {
        let source = r#"
            /* a */
            A  /* 1 */ = B, /* foo */ C, ..D /* bar */;
        "#;
        let file = parse_and_check_errors(source);
        assert!(file.errors.is_empty());
        assert_eq!(file.stmts.len(), 1);
    }

    #[test]
    fn invalid_alias_declaration_separator() {
        let source = "Invalid = ::Invalid.Derive::Path;";
        let file = parse_and_check_errors(source);
        assert!(!file.errors.is_empty());
        assert_eq!(file.stmts.len(), 0, "fails to parse the statement");
        assert!(file.errors[0].message.contains("expected ';'"));
    }

    #[test]
    fn unquoted_use_path() {
        let source = "use unquoted_path;";
        let file = parse_and_check_errors(source);
        assert!(!file.errors.is_empty());
        assert_eq!(file.stmts.len(), 0);

        assert!(file.errors[0]
            .message
            .contains("expected a string literal for the path"));
    }

    #[test]
    fn unclosed_cfg() {
        let source = "#[cfg(feature = \"serde\"]";
        let file = parse_and_check_errors(source);
        assert!(!file.errors.is_empty());
        assert!(file.errors[0].message.contains("Expected ')'"));
    }

    #[test]
    fn missing_equal_sign() {
        let source = "MissingEq Test, Test;";
        let file = parse_and_check_errors(source);
        assert!(!file.errors.is_empty());
        assert!(file.errors[0].message.contains("expected '='"));
        assert_eq!(file.stmts.len(), 0);
    }

    #[test]
    fn invalid_leading_char() {
        let source = "@Invalid = A;";
        let file = parse_and_check_errors(source);
        assert!(!file.errors.is_empty());
        assert!(file.errors[0].message.contains("expected an identifier"));
        assert_eq!(file.stmts.len(), 0);
    }
}
