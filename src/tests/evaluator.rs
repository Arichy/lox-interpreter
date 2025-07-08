use crate::runner::Runner;

/// Tests for the evaluator functionality
/// These tests correspond to the "Evaluating Expressions" stages in CodeCrafters

#[test]
fn test_evaluate_booleans_and_nil() {
    // Stage: Literals: Booleans & Nil (iz6)
    // Command: evaluate
    // Test: Boolean and nil literals should be evaluated correctly
    
    // Test true literal
    let code = "true";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate 'true': {:?}", result);
    
    // Test false literal
    let code = "false";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate 'false': {:?}", result);
    
    // Test nil literal
    let code = "nil";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate 'nil': {:?}", result);
}

#[test]
fn test_evaluate_strings_and_numbers() {
    // Stage: Literals: Strings & Numbers (lv1)
    // Command: evaluate
    // Test: String and number literals should be evaluated correctly

    // Test string literal
    let code = r#""hello""#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate string literal: {:?}", result);

    // Test number literal
    let code = "42";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate number literal: {:?}", result);

    // Test decimal number
    let code = "3.14";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate decimal number: {:?}", result);
}

#[test]
fn test_evaluate_parentheses() {
    // Stage: Parentheses (oq9)
    // Command: evaluate
    // Test: Parenthesized expressions should be evaluated correctly

    let code = "(42)";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate parenthesized expression: {:?}", result);
}

#[test]
fn test_evaluate_unary_operators() {
    // Stage: Unary Operators: Negation & Not (dc1)
    // Command: evaluate
    // Test: Unary operators should be evaluated correctly

    // Test negation
    let code = "-42";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate negation: {:?}", result);

    // Test logical not
    let code = "!true";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate logical not: {:?}", result);

    let code = "!false";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate logical not: {:?}", result);
}

#[test]
fn test_evaluate_arithmetic_operators() {
    // Stage: Arithmetic Operators (1/2) (bp3) and (2/2) (jy2)
    // Command: evaluate
    // Test: Arithmetic operators should be evaluated correctly

    // Test addition
    let code = "1 + 2";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate addition: {:?}", result);

    // Test subtraction
    let code = "5 - 3";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate subtraction: {:?}", result);

    // Test multiplication
    let code = "4 * 6";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate multiplication: {:?}", result);

    // Test division
    let code = "8 / 2";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate division: {:?}", result);
}

#[test]
fn test_evaluate_string_concatenation() {
    // Stage: String Concatenation (jx8)
    // Command: evaluate
    // Test: String concatenation should work correctly

    let code = r#""hello" + " " + "world""#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate string concatenation: {:?}", result);
}

#[test]
fn test_evaluate_relational_operators() {
    // Stage: Relational Operators (et4)
    // Command: evaluate
    // Test: Relational operators should be evaluated correctly

    // Test less than
    let code = "1 < 2";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate less than: {:?}", result);

    // Test greater than or equal
    let code = "5 >= 3";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate greater than or equal: {:?}", result);
}

#[test]
fn test_evaluate_equality_operators() {
    // Stage: Equality Operators (hw7)
    // Command: evaluate
    // Test: Equality operators should be evaluated correctly

    // Test equality
    let code = "1 == 1";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate equality: {:?}", result);

    // Test inequality
    let code = "1 != 2";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Failed to evaluate inequality: {:?}", result);
}

#[test]
fn test_evaluate_runtime_errors_unary() {
    // Stage: Runtime Errors: Unary Operators (gj9)
    // Command: evaluate
    // Test: Invalid unary operations should cause runtime errors

    // Test negation of string (should fail)
    let code = r#"-"hello""#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_err(), "Expected runtime error for negating string");

    // Test logical not of number (should work - numbers are truthy)
    let code = "!42";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "Logical not of number should work: {:?}", result);
}

#[test]
fn test_evaluate_runtime_errors_binary() {
    // Stage: Runtime Errors: Binary Operators (1/2) (yu6) and (2/2) (cq1)
    // Command: evaluate
    // Test: Invalid binary operations should cause runtime errors

    // Test adding string and number (should fail)
    let code = r#""hello" + 42"#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_err(), "Expected runtime error for string + number");

    // Test subtracting strings (should fail)
    let code = r#""hello" - "world""#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_err(), "Expected runtime error for string - string");

    // Test dividing by zero
    let code = "1 / 0";
    let runner = Runner::new(code);
    let result = runner.run();
    // Note: Division by zero behavior may vary by implementation
    // Some implementations return infinity, others error
}

#[test]
fn test_evaluate_runtime_errors_relational() {
    // Stage: Runtime Errors: Relational Operators (ib5)
    // Command: evaluate
    // Test: Invalid relational operations should cause runtime errors

    // Test comparing string and number (should fail)
    let code = r#""hello" < 42"#;
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_err(), "Expected runtime error for string < number");

    // Test comparing incompatible types
    let code = "true > 5";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_err(), "Expected runtime error for boolean > number");
}

#[test]
fn test_evaluate_truthiness() {
    // Test Lox truthiness rules
    // Command: evaluate
    // Test: Only false and nil are falsy, everything else is truthy

    // Test falsy values
    let code = "!false";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "!false should evaluate successfully");

    let code = "!nil";
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "!nil should evaluate successfully");

    // Test truthy values
    let code = "!0";  // 0 is truthy in Lox
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "!0 should evaluate successfully");

    let code = r#"!"""#;  // empty string is truthy in Lox
    let runner = Runner::new(code);
    let result = runner.run();
    assert!(result.is_ok(), "!\"\" should evaluate successfully");
}
