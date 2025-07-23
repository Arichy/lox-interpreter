use super::test_utils::*;

/// Tests for the runner functionality (run command)
/// These tests correspond to the "Statements & State" stages in CodeCrafters

#[test]
fn test_run_print_statements() {
    // Stage: Print: Generate output (xy1)
    // Command: run
    // Test: Print statements should generate output

    // Test print string literal
    let code = r#"print "Hello, World!";"#;
    let expected = vec!["Hello, World!"];
    assert_lox_output(code, expected);

    // Test print number literal
    let code = r#"print 42;"#;
    let expected = vec!["42"];
    assert_lox_output(code, expected);

    // Test print boolean literal
    let code = r#"print true;"#;
    let expected = vec!["true"];
    assert_lox_output(code, expected);

    // Test print expression
    let code = r#"print 12 + 24;"#;
    let expected = vec!["36"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_multiple_print_statements() {
    // Stage: Print: Multiple statements (oe4)
    // Command: run
    // Test: Multiple print statements should generate multiple lines of output

    let code = r#"
        print "first";
        print "second";
        print "third";
    "#;
    let expected = vec!["first", "second", "third"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_print_syntax_error() {
    // Test that invalid print statements cause syntax errors
    let code = r#"print;"#;
    assert_lox_error(code);
}

#[test]
fn test_run_expression_statements() {
    // Stage: Expression statements (fi3)
    // Command: run
    // Test: Expression statements should be executed correctly

    let code = r#"
        42;
        "hello";
        true;
    "#;
    // Expression statements don't produce output, but should execute without error
    let runner = crate::runner::Runner::new(code);
    let result = runner.run();
    assert!(
        result.is_ok(),
        "Failed to run expression statements: {:?}",
        result
    );
}

#[test]
fn test_run_variable_declarations() {
    // Stage: Variables: Declare variables (yg2)
    // Command: run
    // Test: Variable declarations should work correctly

    let code = r#"
        var a;
        var b = 42;
        print b;
    "#;
    let expected = vec!["42"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_variable_initialization() {
    // Stage: Variables: Initialize variables (bc1)
    // Command: run
    // Test: Variable initialization should work correctly

    let code = r#"
        var a = "hello";
        var b = 42;
        var c = true;
        print a;
        print b;
        print c;
    "#;
    let expected = vec!["hello", "42", "true"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_assignment_operation() {
    // Stage: Assignment operation (pl3)
    // Command: run
    // Test: Assignment operations should work correctly

    let code = r#"
        var a = 10;
        print a;
        a = 20;
        print a;
    "#;
    let expected = vec!["10", "20"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_block_syntax() {
    // Stage: Block syntax (vr5)
    // Command: run
    // Test: Block statements should work correctly

    let code = r#"
        {
            print "inside block";
        }
        print "outside block";
    "#;
    let expected = vec!["inside block", "outside block"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_scopes() {
    // Stage: Scopes (fb4)
    // Command: run
    // Test: Variable scoping should work correctly

    let code = r#"
        var a = "global";
        {
            var a = "local";
            print a;
        }
        print a;
    "#;
    let expected = vec!["local", "global"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_if_statements() {
    // Stage: If statements (ne3)
    // Command: run
    // Test: If statements should work correctly

    let code = r#"
        if (true) {
            print "true branch";
        }
        if (false) {
            print "false branch";
        }
    "#;
    let expected = vec!["true branch"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_else_statements() {
    // Stage: Else statements (st5)
    // Command: run
    // Test: Else statements should work correctly

    let code = r#"
        if (false) {
            print "if branch";
        } else {
            print "else branch";
        }
    "#;
    let expected = vec!["else branch"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_while_statements() {
    // Stage: While statements (qy3)
    // Command: run
    // Test: While statements should work correctly

    {
        let code = r#"
            var i = 0;
            while (i < 3) {
                print i;
                i = i + 1;
            }
        "#;
        let expected = vec!["0", "1", "2"];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
            fun f() {
            while (!false) return "ok";
            }

            print f();
        "#;
        let expected = vec!["ok"];
        assert_lox_output(code, expected);
    }
}

#[test]
fn test_run_for_statements() {
    // Stage: For statements (bw6)
    // Command: run
    // Test: For statements should work correctly

    let code = r#"
        for (var i = 0; i < 3; i = i + 1) {
            print i;
        }
    "#;
    let expected = vec!["0", "1", "2"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_variable_runtime_errors() {
    // Stage: Variables: Runtime Errors (sv7)
    // Command: run
    // Test: Undefined variables should cause runtime errors

    let code = r#"print undefined_variable;"#;
    assert_lox_error(code);
}

#[test]
fn test_run_variable_redeclaration() {
    // Stage: Variables: Redeclare variables (dw9)
    // Command: run
    // Test: Variable redeclaration should work correctly

    let code = r#"
        var a = 1;
        print a;
        var a = 2;
        print a;
    "#;
    let expected = vec!["1", "2"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_assignment_expressions() {
    // Test assignment as expressions
    // Command: run
    // Test: Assignment should return the assigned value

    let code = r#"
        var a;
        var b;
        print a = b = 5;
        print a;
        print b;
    "#;
    let expected = vec!["5", "5", "5"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_nested_scopes() {
    // Test more complex scoping scenarios
    // Command: run
    // Test: Nested scopes should work correctly

    let code = r#"
        var a = "global";
        {
            var a = "outer";
            {
                var a = "inner";
                print a;
            }
            print a;
        }
        print a;
    "#;
    let expected = vec!["inner", "outer", "global"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_variable_shadowing() {
    // Test variable shadowing in different scopes
    // Command: run
    // Test: Variables should shadow correctly

    let code = r#"
        var x = 1;
        {
            print x;  // Should print 1 (from outer scope)
            var x = 2;
            print x;  // Should print 2 (local variable)
        }
        print x;  // Should print 1 (outer scope again)
    "#;
    let expected = vec!["1", "2", "1"];
    assert_lox_output(code, expected);
}
