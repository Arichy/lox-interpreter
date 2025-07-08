use crate::lexer::Lexer;

/// Tests for the lexer/tokenizer functionality
/// These tests correspond to the "Scanning" stages in CodeCrafters

#[test]
fn test_tokenize_empty_file() {
    // Stage: Scanning: Empty file (ry8)
    // Command: tokenize
    // Test: Empty file should only output EOF token
    let code = "";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    // Empty file should produce no tokens (EOF is added separately in main.rs)
    assert_eq!(tokens.len(), 0);
}

#[test]
fn test_tokenize_parentheses() {
    // Stage: Scanning: Parentheses (ol4)
    // Command: tokenize
    // Test: Parentheses should be tokenized as LEFT_PAREN and RIGHT_PAREN
    let code = "(()";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 3);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "LEFT_PAREN ( null");
    assert_eq!(tokens[1].to_string(), "LEFT_PAREN ( null");
    assert_eq!(tokens[2].to_string(), "RIGHT_PAREN ) null");
}

#[test]
fn test_tokenize_braces() {
    // Stage: Scanning: Braces (oe8)
    // Command: tokenize
    // Test: Braces should be tokenized as LEFT_BRACE and RIGHT_BRACE
    let code = "{{}}";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 4);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "LEFT_BRACE { null");
    assert_eq!(tokens[1].to_string(), "LEFT_BRACE { null");
    assert_eq!(tokens[2].to_string(), "RIGHT_BRACE } null");
    assert_eq!(tokens[3].to_string(), "RIGHT_BRACE } null");
}

#[test]
fn test_tokenize_single_character_tokens() {
    // Stage: Scanning: Other single-character tokens (xc5)
    // Command: tokenize
    // Test: Single character tokens should be tokenized correctly
    let code = "({*.,+*})";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 9);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "LEFT_PAREN ( null");
    assert_eq!(tokens[1].to_string(), "LEFT_BRACE { null");
    assert_eq!(tokens[2].to_string(), "STAR * null");
    assert_eq!(tokens[3].to_string(), "DOT . null");
    assert_eq!(tokens[4].to_string(), "COMMA , null");
    assert_eq!(tokens[5].to_string(), "PLUS + null");
    assert_eq!(tokens[6].to_string(), "STAR * null");
    assert_eq!(tokens[7].to_string(), "RIGHT_BRACE } null");
    assert_eq!(tokens[8].to_string(), "RIGHT_PAREN ) null");
}

#[test]
fn test_tokenize_lexical_errors() {
    // Stage: Scanning: Lexical errors (ea6)
    // Command: tokenize
    // Test: Unexpected characters should cause lexical errors
    let code = "@#^";

    let lexer = Lexer::new(code);
    let result: Result<Vec<_>, _> = lexer.collect();

    // Should result in an error due to unexpected characters
    assert!(result.is_err(), "Expected lexical error for unexpected characters");
}

#[test]
fn test_tokenize_assignment_equality_operators() {
    // Stage: Scanning: Assignment & equality Operators (mp7)
    // Command: tokenize
    // Test: Assignment and equality operators should be tokenized correctly
    // Test individual operators to avoid spacing issues
    let code = "=";
    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].to_string(), "EQUAL = null");

    let code = "==";
    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].to_string(), "EQUAL_EQUAL == null");

    let code = "!=";
    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].to_string(), "BANG_EQUAL != null");

    let code = "!";
    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
    assert_eq!(tokens.len(), 1);
    assert_eq!(tokens[0].to_string(), "BANG ! null");
}

#[test]
fn test_tokenize_relational_operators() {
    // Stage: Scanning: Relational operators (et2)
    // Command: tokenize
    // Test: Relational operators should be tokenized correctly
    let code = "< <= > >=";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 4);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "LESS < null");
    assert_eq!(tokens[1].to_string(), "LESS_EQUAL <= null");
    assert_eq!(tokens[2].to_string(), "GREATER > null");
    assert_eq!(tokens[3].to_string(), "GREATER_EQUAL >= null");
}

#[test]
fn test_tokenize_division_and_comments() {
    // Stage: Scanning: Division operator & comments (ml2)
    // Command: tokenize
    // Test: Division operator and comments should be handled correctly
    let code = "/ // this is a comment\n/";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    // Should have two SLASH tokens (comments are ignored)
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0].to_string(), "SLASH / null");
    assert_eq!(tokens[1].to_string(), "SLASH / null");
}

#[test]
fn test_tokenize_whitespace() {
    // Stage: Scanning: Whitespace (er2)
    // Command: tokenize
    // Test: Whitespace should be ignored
    let code = "  \t\n  (  \t\n  )  \t\n  ";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    // Should only have parentheses tokens (whitespace ignored)
    assert_eq!(tokens.len(), 2);
    assert_eq!(tokens[0].to_string(), "LEFT_PAREN ( null");
    assert_eq!(tokens[1].to_string(), "RIGHT_PAREN ) null");
}

#[test]
fn test_tokenize_string_literals() {
    // Stage: Scanning: String literals (ue7)
    // Command: tokenize
    // Test: String literals should be tokenized correctly
    let code = r#""hello" "world""#;

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 2);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), r#"STRING "hello" hello"#);
    assert_eq!(tokens[1].to_string(), r#"STRING "world" world"#);
}

#[test]
fn test_tokenize_number_literals() {
    // Stage: Scanning: Number literals (kj0)
    // Command: tokenize
    // Test: Number literals should be tokenized correctly
    let code = "123 45.67";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 2);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "NUMBER 123 123.0");
    assert_eq!(tokens[1].to_string(), "NUMBER 45.67 45.67");
}

#[test]
fn test_tokenize_identifiers() {
    // Stage: Scanning: Identifiers (ey7)
    // Command: tokenize
    // Test: Identifiers should be tokenized correctly
    let code = "variable_name another_var";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 2);

    // Check token types and display format
    assert_eq!(tokens[0].to_string(), "IDENTIFIER variable_name null");
    assert_eq!(tokens[1].to_string(), "IDENTIFIER another_var null");
}

#[test]
fn test_tokenize_reserved_words() {
    // Stage: Scanning: Reserved words (pq5)
    // Command: tokenize
    // Test: Reserved words should be tokenized correctly
    let code = "and class else false fun for if nil or print return super this true var while";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 16);

    // Check token types and display format for reserved words
    assert_eq!(tokens[0].to_string(), "AND and null");
    assert_eq!(tokens[1].to_string(), "CLASS class null");
    assert_eq!(tokens[2].to_string(), "ELSE else null");
    assert_eq!(tokens[3].to_string(), "FALSE false null");
    assert_eq!(tokens[4].to_string(), "FUN fun null");
    assert_eq!(tokens[5].to_string(), "FOR for null");
    assert_eq!(tokens[6].to_string(), "IF if null");
    assert_eq!(tokens[7].to_string(), "NIL nil null");
    assert_eq!(tokens[8].to_string(), "OR or null");
    assert_eq!(tokens[9].to_string(), "PRINT print null");
    assert_eq!(tokens[10].to_string(), "RETURN return null");
    assert_eq!(tokens[11].to_string(), "SUPER super null");
    assert_eq!(tokens[12].to_string(), "THIS this null");
    assert_eq!(tokens[13].to_string(), "TRUE true null");
    assert_eq!(tokens[14].to_string(), "VAR var null");
    assert_eq!(tokens[15].to_string(), "WHILE while null");
}

#[test]
fn test_tokenize_unterminated_string() {
    // Stage: Scanning: Multi-line errors (tz7)
    // Command: tokenize
    // Test: Unterminated strings should cause errors
    let code = r#""unterminated string"#;

    let lexer = Lexer::new(code);
    let result: Result<Vec<_>, _> = lexer.collect();

    // Should result in an error due to unterminated string
    assert!(result.is_err(), "Expected error for unterminated string");
}

#[test]
fn test_tokenize_complex_expressions() {
    // Test complex tokenization with mixed operators and literals
    let code = r#"var x = 42.5; if (x >= 10.0) { print "large"; }"#;

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    // Should tokenize correctly: VAR, IDENTIFIER, EQUAL, NUMBER, SEMICOLON, IF, LEFT_PAREN, etc.
    assert!(tokens.len() > 10);
    assert_eq!(tokens[0].to_string(), "VAR var null");
    assert_eq!(tokens[1].to_string(), "IDENTIFIER x null");
    assert_eq!(tokens[2].to_string(), "EQUAL = null");
}

#[test]
fn test_tokenize_nested_structures() {
    // Test nested parentheses and braces
    let code = "{ ( { } ) }";

    let lexer = Lexer::new(code);
    let tokens: Vec<_> = lexer.collect::<Result<Vec<_>, _>>().unwrap();

    assert_eq!(tokens.len(), 6);
    assert_eq!(tokens[0].to_string(), "LEFT_BRACE { null");
    assert_eq!(tokens[1].to_string(), "LEFT_PAREN ( null");
    assert_eq!(tokens[2].to_string(), "LEFT_BRACE { null");
    assert_eq!(tokens[3].to_string(), "RIGHT_BRACE } null");
    assert_eq!(tokens[4].to_string(), "RIGHT_PAREN ) null");
    assert_eq!(tokens[5].to_string(), "RIGHT_BRACE } null");
}


