use crate::{end_capture, runner::Runner, start_capture};

/// Helper function to run Lox code and capture output
/// 
/// Returns a tuple of (result, stdout_lines, stderr_lines)
pub fn run_lox_code(code: &str) -> (Result<(), miette::Report>, Vec<String>, Vec<String>) {
    // Start capturing stdout
    start_capture();

    // Create and run the interpreter
    let runner = Runner::new(code);

    // Run the code
    let result = runner.run();

    // Get captured output
    let (stdout, stderr) = end_capture();

    (result, stdout, stderr)
}

/// Helper function to assert successful execution with expected output
pub fn assert_lox_output(code: &str, expected: Vec<&str>) {
    let (result, stdout, _stderr) = run_lox_code(code);
    match result {
        Ok(_) => assert_eq!(stdout, expected),
        Err(e) => panic!("Test failed with error: {}", e),
    }
}

/// Helper function to assert that code execution fails
pub fn assert_lox_error(code: &str) {
    let (result, _stdout, _stderr) = run_lox_code(code);
    assert!(result.is_err(), "Expected test to fail but it succeeded");
}

/// Helper function to assert execution with expected output and error
pub fn assert_lox_output_then_error(code: &str, expected: Vec<&str>) {
    let (result, stdout, _stderr) = run_lox_code(code);
    assert_eq!(stdout, expected);
    assert!(result.is_err(), "Expected test to fail but it succeeded");
}
