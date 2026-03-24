#[test]
fn ui() {
    let harness = trybuild::TestCases::new();
    harness.compile_fail("tests/ui/*.rs");
}
