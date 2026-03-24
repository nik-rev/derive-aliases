//! This tests annotation comments.
//!
//! For more information about annotation tests, see `CONTRIBUTING.md`

use std::{
    ops::Range,
    path::PathBuf,
    process::{Command, Stdio},
};

use fs_err as fs;
use itertools::Itertools;
use monostate::MustBe;
use serde_cursor::Cursor;

/// Tests with annotation comments
const TESTS: &[&str] = &["helper_container"];

/// A temporary file that exists only for a brief moment in time
///
/// We take a test file, add `trace_macros!()` at the top, then compile the file.
const ANNOTATION_TEMP_FILE: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/tmp-annotation.rs");

#[test]
fn annotation() -> evil::Result {
    let mut invalid_expansions = Vec::new();

    for test in TESTS {
        let test_file_path_relative = format!("tests/{test}.rs");
        let test_file_path = PathBuf::from(format!(
            "{}/{test_file_path_relative}",
            env!("CARGO_MANIFEST_DIR")
        ));

        // Original content of the test file
        let test_contents = fs::read_to_string(&test_file_path)?;

        // Same test file, with `trace_macros!(true)` embedded
        let test_contents = inject_trace_macros(&test_contents)?;

        // All the expansion information returned by `trace_macros!`
        let trace_macro_output = get_trace_macro_output(&test_contents)?;

        for expansion in trace_macro_output {
            // How many lines there are with attributes in them
            //
            // #[derive_aliases::derive(..Serialize)]  <<<<<< this is 2
            // #[derive_aliases::derive(..Clone)]      <<<<<<
            // /*^
            // $(::serde::Serialize,)
            // $(::core::clone::Clone,)
            // */
            // struct A {
            //
            // We start with 1 because the line that we started with
            // is itself an attribute
            //
            // #[derive_aliases::derive(..Serialize)]  <<<<<< we start here
            // #[derive_aliases::derive(..Clone)]
            // /*^
            // $(::serde::Serialize,)
            // $(::core::clone::Clone,)
            // */
            // struct A {
            let mut attribute_line_count = 1;

            let mut annotation = test_contents
                .lines()
                // skip to the start of the macro that is being expanded
                .skip(expansion.line_start)
                // skip all the attributes of the item
                //
                // #[derive_aliases::derive(..Serialize)]  <<<<<< skip
                // #[derive_aliases::derive(..Clone)]      <<<<<< skip
                // /*^
                // $(::serde::Serialize,)
                // $(::core::clone::Clone,)
                // */
                // struct A {
                .skip_while(|line| {
                    if line.starts_with('#') {
                        attribute_line_count += 1;
                        true
                    } else {
                        false
                    }
                })
                .peekable();

            // the annotation comment must be directly after all the attributes
            //
            // #[derive_aliases::derive(..Serialize)]
            // #[derive_aliases::derive(..Clone)]
            // /*^                                         <<<<<< must be exact match
            // $(::serde::Serialize,)
            // $(::core::clone::Clone,)
            // */
            // struct A {
            if annotation.peek().is_none_or(|line| *line != "/*^") {
                continue;
            }

            // #[derive_aliases::derive(..Serialize)]
            // #[derive_aliases::derive(..Clone)]
            // /*^                                         <<<<<< we are here now
            // $(::serde::Serialize,)
            // $(::core::clone::Clone,)
            // */
            // struct A {
            let _ = annotation.next().expect("we `continue` if `None`");

            // Contents of the annotation comments itself
            //
            // #[derive_aliases::derive(..Serialize)]
            // #[derive_aliases::derive(..Clone)]
            // /*^
            // $(::serde::Serialize,)                       <<<<<<
            // $(::core::clone::Clone,)                     <<<<<< all of this
            // */
            // struct A {
            let annotation_comment = annotation.take_while(|line| *line != "*/");

            // The expected expansion of the attributes, which is different
            // from the annotation comment - because we do some processing on the annotation comments
            let expected_expansion = annotation_comment
                .map(|line| {
                    format!("#[{line}]")
                        .replace("$", "::core::prelude::v1::derive")
                        .replace("@", "cfg_attr")
                        .split_whitespace()
                        .join("")
                })
                .join("\n");

            let expansion_byte_range = {
                // +1 because line_start is 1-indexed and we convert it into 0-indexed
                let expansion_start_line = expansion.line_start - 1;
                line_range_to_byte_range(
                    &test_contents,
                    expansion_start_line..expansion_start_line + attribute_line_count,
                )?
            };

            // Find the last message that starts with an attribute:
            //
            // expanding `Clone! .....
            // to `Clone! .....
            // expanding `Clone! .....
            // to `#
            //
            // Once we hit that '#', we have fully expanded all the derive aliases
            let actual_expansion = expansion
                .messages
                .iter()
                .rev()
                .find(|message| message.starts_with("to `#"))?
                .strip_prefix("to `")?
                // Remove whitespace, because it doesn't matter, and fluctutes a lot
                .split_whitespace()
                .join("");

            // We want each attribute to be on its own line
            let mut actual_expansion_with_newlines = String::new();

            let mut tmp = actual_expansion.chars().peekable();
            let mut started = false;

            while let Some(ch) = tmp.next() {
                if ch == '#' && tmp.peek().is_some_and(|it| it == &'[') {
                    if started {
                        actual_expansion_with_newlines.push_str("\n#");
                    } else {
                        actual_expansion_with_newlines.push('#');
                    }
                } else {
                    actual_expansion_with_newlines.push(ch);
                }
                started = true;
            }

            let actual_expansion = actual_expansion_with_newlines;

            // where all the attributes end
            //
            // #[derive_aliases::derive(..Serialize)]
            // #[derive_aliases::derive(..Clone)]
            // /*^
            // $(::serde::Serialize,)
            // $(::core::clone::Clone,)
            // */
            // struct A {
            // ^^^^^^ that would be here
            let end_of_attributes = actual_expansion
                .find("struct")
                .or_else(|| actual_expansion.find("pub"))
                .or_else(|| actual_expansion.find("union"))
                .or_else(|| actual_expansion.find("enum"))?;

            let actual_expansion = &actual_expansion[..end_of_attributes]
                .lines()
                .map(|line| line.split_whitespace().join(""))
                .join("\n");

            if *actual_expansion == expected_expansion {
                continue;
            }

            invalid_expansions.push(InvalidExpansion {
                span: expansion_byte_range,
                actual: actual_expansion.to_string(),
                file: test_contents.clone(),
                expected: expected_expansion,
                path: test_file_path_relative.clone().into(),
            });
        }
    }

    for invalid_expansion in &invalid_expansions {
        invalid_expansion.print();
    }

    if !invalid_expansions.is_empty() {
        panic!();
    }

    evil::Ok(())
}

/// Converts the given range of lines to a byte range in the given `file`
fn line_range_to_byte_range(file: &str, line_range: Range<usize>) -> evil::Result<Range<usize>> {
    let mut line_indices = file
        .char_indices()
        .flat_map(|(i, ch)| (ch == '\n').then_some(i))
        // skipping `line_range.start` lines will exclude the line that we are
        // actually interested in
        //
        // #[derive_aliases::derive(..Serialize)] <<<<<< this line is "line_start.start"
        // #[derive_aliases::derive(..Clone)]
        // /*^
        // $(::serde::Serialize,)
        // $(::core::clone::Clone,)
        // */
        //
        // We also take +1 more than how many attributes there are. If there are 2 lines with attributes,
        // we will need to take 3 line endings because:
        //
        // ^-------- we want this line ending
        // #[derive_aliases::derive(..Serialize)]
        //                                       ^------ and this line ending
        // #[derive_aliases::derive(..Clone)]
        //                                   ^------ and this line ending
        //
        .skip(line_range.start - 1)
        .take(line_range.len() + 1);

    let end_of_line_before_start_line = line_indices.next()?;
    let last_line_byte_start = line_indices.last()?;

    // +1 because we want the start to be directly after the very first line ending
    //
    // ^-------- 1st line ending
    // #[derive_aliases::derive(..Serialize)]
    // #[derive_aliases::derive(..Clone)]
    //                                   ^------ last line ending
    //
    // #[derive_aliases::derive(..Serialize)]
    // ^-------- but we actually want to start here (+1 byte)
    // #[derive_aliases::derive(..Clone)]
    //                                   ^------ last line ending
    evil::Ok(end_of_line_before_start_line + 1..last_line_byte_start)
}

/// Receives contents of a `.rs` file as an input, and compiles that
/// as a test, returning the compiler messages returned.
fn get_trace_macro_output(test_contents: &str) -> evil::Result<Vec<ExpansionMacroTrace>> {
    fs::write(ANNOTATION_TEMP_FILE, test_contents)?;

    let output = Command::new(env!("CARGO"))
        .arg("check")
        .arg("--test")
        .arg("tmp-annotation")
        .arg("--message-format")
        .arg("json")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    fs::remove_file(ANNOTATION_TEMP_FILE)?;

    let json = serde_json::Deserializer::from_slice(&output.stdout)
        .into_iter::<serde_json::Value>()
        .flatten();

    let expansions =
        json.flat_map(|value| ExpansionMacroTrace::from_json_slice(&serde_json::to_vec(&value)?));

    evil::Ok(expansions.collect())
}

/// Adds `trace_macros!(true)` at the start of the file, so we
/// can collect macro output
fn inject_trace_macros(contents: &str) -> evil::Result<String> {
    let lines: Vec<_> = contents.lines().collect();

    let start = lines.iter().enumerate().find_map(|(i, line)| {
        (!line.starts_with("#![") && !line.starts_with("//!")).then_some(i)
    })?;

    let (inner_attrs, source) = lines.split_at(start);
    let inner_attrs = inner_attrs.join("\n");
    let source = source.join("\n");

    // We very purposefully place trace_macros in such a way that no newlines are created,
    // to not mess up any line numbers when rendering the diagnostic messages

    evil::Ok(format!(
        "\
{inner_attrs} #![feature(trace_macros)] trace_macros!(true);
{source}"
    ))
}

struct ExpansionMacroTrace {
    /// Compiler messages
    messages: Vec<String>,
    /// Line start of the derive macro expansion
    line_start: usize,
}

impl ExpansionMacroTrace {
    fn from_json_slice(value: &[u8]) -> serde_json::Result<Self> {
        macro_rules! get {
            ($($cursor:tt)*) => {
                serde_json::from_slice::<Cursor!($($cursor)*)>(value).map(|it| it.0)
            };
        }

        get!(reason: MustBe!("compiler-message"))?;
        get!(message.message: MustBe!("trace_macro"))?;

        Ok(ExpansionMacroTrace {
            messages: get!(message.children.*.message)?,
            line_start: get!(message.spans.0.line_start)?,
        })
    }
}

struct InvalidExpansion {
    /// The expansion that we expected
    expected: String,
    /// The expansion that we actually got
    actual: String,
    /// Points to the first invocation of the derive macro
    /// leading up to the expansion
    span: Range<usize>,
    /// The file in which the error originated
    file: String,
    /// Path to the file in which the invalid expansion occurred
    path: PathBuf,
}
impl InvalidExpansion {
    /// Prints the formatted diagnostic about the invalid macro expansion
    fn print(&self) {
        let element = annotate_snippets::Snippet::source(&self.file)
            .line_start(1)
            .path(format!("{}", self.path.display()))
            .annotation(
                annotate_snippets::AnnotationKind::Primary
                    .span(self.span.clone())
                    .label("these attributes expand incorrectly"),
            );

        let report = &[
            annotate_snippets::Level::ERROR
                .primary_title("attributes expanded incorrectly")
                .element(element)
                .element(annotate_snippets::Padding)
                .element(annotate_snippets::Padding),
            annotate_snippets::Group::with_title(
                annotate_snippets::level::HELP
                    .primary_title(format!("expected expansion:\n\n{}\n", self.expected)),
            ),
            annotate_snippets::Group::with_title(
                annotate_snippets::level::HELP
                    .primary_title(format!("actual expansion:\n\n{}", self.actual)),
            ),
        ];

        let renderer = annotate_snippets::Renderer::styled()
            .decor_style(annotate_snippets::renderer::DecorStyle::Unicode);
        anstream::println!("{}", renderer.render(report));
        println!(
            "\n{}",
            pretty_assertions::StrComparison::new(&self.expected, &self.actual)
        );
    }
}
