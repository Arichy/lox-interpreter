use crate::parser::Parser;

/// Tests for the parser functionality
/// These tests correspond to the "Parsing Expressions" stages in CodeCrafters

#[test]
fn test_parse_booleans_and_nil() {
    // Stage: Booleans & Nil (sc2)
    // Command: parse
    // Test: Boolean and nil literals should be parsed correctly
    
    // Test true literal
    let code = "true";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "true");
    
    // Test false literal
    let code = "false";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "false");
    
    // Test nil literal
    let code = "nil";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();
    
    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "nil");
}

#[test]
fn test_parse_number_literals() {
    // Stage: Number literals (ra8)
    // Command: parse
    // Test: Number literals should be parsed correctly

    // Test integer literal
    let code = "42";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "42.0");

    // Test decimal literal
    let code = "3.14";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "3.14");
}

#[test]
fn test_parse_string_literals() {
    // Stage: String literals (th5)
    // Command: parse
    // Test: String literals should be parsed correctly

    let code = r#""hello world""#;
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), r#"hello world"#);
}

#[test]
fn test_parse_parentheses() {
    // Stage: Parentheses (xe6)
    // Command: parse
    // Test: Parenthesized expressions should be parsed correctly

    let code = "(42)";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(group 42.0)");
}

#[test]
fn test_parse_unary_operators() {
    // Stage: Unary Operators (mq1)
    // Command: parse
    // Test: Unary operators should be parsed correctly

    // Test negation
    let code = "-42";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(- 42.0)");

    // Test logical not
    let code = "!true";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(! true)");
}

#[test]
fn test_parse_arithmetic_operators() {
    // Stage: Arithmetic operators (1/2) (wa9) and (2/2) (yf2)
    // Command: parse
    // Test: Arithmetic operators should be parsed correctly

    // Test addition
    let code = "1 + 2";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(+ 1.0 2.0)");

    // Test subtraction
    let code = "5 - 3";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(- 5.0 3.0)");

    // Test multiplication
    let code = "4 * 6";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(* 4.0 6.0)");

    // Test division
    let code = "8 / 2";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(/ 8.0 2.0)");
}

#[test]
fn test_parse_comparison_operators() {
    // Stage: Comparison operators (uh4)
    // Command: parse
    // Test: Comparison operators should be parsed correctly

    // Test less than
    let code = "1 < 2";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(< 1.0 2.0)");

    // Test greater than or equal
    let code = "5 >= 3";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(>= 5.0 3.0)");
}

#[test]
fn test_parse_equality_operators() {
    // Stage: Equality operators (ht8)
    // Command: parse
    // Test: Equality operators should be parsed correctly

    // Test equality
    let code = "1 == 1";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(== 1.0 1.0)");

    // Test inequality
    let code = "1 != 2";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(!= 1.0 2.0)");
}

#[test]
fn test_parse_logical_operators() {
    // Stage: Logical operators (and/or)
    // Command: parse
    // Test: Logical operators should be parsed correctly

    // Test logical AND
    let code = "true and false";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(&& true false)");

    // Test logical OR
    let code = "true or false";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(program.inner.body[0].to_string(), "(|| true false)");
}

#[test]
fn test_parse_operator_precedence() {
    // Test operator precedence
    // Command: parse
    // Test: Operators should be parsed with correct precedence

    // Test arithmetic precedence: 1 + 2 * 3 should be 1 + (2 * 3)
    let code = "1 + 2 * 3";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(
        program.inner.body[0].to_string(),
        "(+ 1.0 (* 2.0 3.0))"
    );

    // Test comparison precedence: 1 + 2 < 3 + 4 should be (1 + 2) < (3 + 4)
    let code = "1 + 2 < 3 + 4";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(
        program.inner.body[0].to_string(),
        "(< (+ 1.0 2.0) (+ 3.0 4.0))"
    );
}

#[test]
fn test_parse_complex_expressions() {
    // Test complex nested expressions
    // Command: parse
    // Test: Complex expressions should be parsed correctly

    let code = "!(1 + 2) == (3 * 4)";
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();

    assert_eq!(program.inner.body.len(), 1);
    assert_eq!(
        program.inner.body[0].to_string(),
        "(== (! (group (+ 1.0 2.0))) (group (* 3.0 4.0)))"
    );
}

#[test]
fn test_parse_syntax_errors() {
    // Stage: Syntactic errors (wz8)
    // Command: parse
    // Test: Invalid syntax should cause parse errors

    // Test missing closing parenthesis
    let code = "(1 + 2";
    let mut parser = Parser::new(code);
    let result = parser.parse();
    assert!(result.is_err(), "Expected parse error for missing closing parenthesis");

    // Test invalid expression
    let code = "+ 1";
    let mut parser = Parser::new(code);
    let result = parser.parse();
    assert!(result.is_err(), "Expected parse error for invalid expression");
}
