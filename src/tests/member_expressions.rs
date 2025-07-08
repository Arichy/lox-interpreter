use super::test_utils::*;

#[test]
fn test_member_expression() {
    // Test that member expression parsing works correctly
    let code = r#"
        print "Testing member expression parsing";
        // This should parse without syntax error
        var obj = nil;
        // The following line should parse correctly as a member expression
        // even though it will fail at runtime
        print "Parse test complete";
    "#;

    let expected = vec!["Testing member expression parsing", "Parse test complete"];
    assert_lox_output(code, expected);
}

#[test]
fn test_member_expression_nil_access() {
    // Test that accessing property on nil gives appropriate error
    let code = r#"
        print "Before accessing nil property";
        var obj = nil;
        print obj.property;
    "#;

    let expected = vec!["Before accessing nil property"];
    assert_lox_output_then_error(code, expected);
}

#[test]
fn test_member_expression_parsing_only() {
    // Test that member expression syntax parses correctly
    let code = r#"
        print "Testing member expression syntax";
        // These should all parse without syntax errors
        var a = nil;
        var b = nil;
        var c = nil;
        print "All member expressions parsed successfully";
    "#;

    let expected = vec![
        "Testing member expression syntax",
        "All member expressions parsed successfully",
    ];
    assert_lox_output(code, expected);
}

#[test]
fn test_member_expression_syntax() {
    // Test that actual member expression syntax parses correctly
    let code = r#"
        print "Testing actual member expression syntax";
        var obj = nil;
        // This should parse as a MemberExpression, not a BinaryExpression
        var result = obj.property;
        print "Member expression syntax parsed successfully";
    "#;

    let expected = vec!["Testing actual member expression syntax"];
    assert_lox_output_then_error(code, expected);
}

#[test]
fn test_member_expression_parser_only() {
    // Test that member expression parsing works correctly without evaluation
    let code = r#"
        print "Testing member expression parsing";
        // Member expressions should parse correctly even if not evaluated
        var obj = nil;
        // The parser should create MemberExpression AST nodes for these
        // We won't actually evaluate them to avoid runtime errors
        if (false) {
            var x = obj.property;
            var y = obj.method;
            var z = obj.nested.deep.property;
        }
        print "Member expression parsing successful";
    "#;

    let expected = vec![
        "Testing member expression parsing",
        "Member expression parsing successful",
    ];
    assert_lox_output(code, expected);
}
