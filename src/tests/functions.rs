use super::test_utils::*;

/// Tests for function functionality
/// These tests correspond to the "Functions" stages in CodeCrafters

#[test]
fn test_run_native_functions() {
    // Stage: Native functions (av4)
    // Command: run
    // Test: Native functions like clock() should work
    
    let code = r#"print clock();"#;
    let runner = crate::runner::Runner::new(code);
    let result = runner.run();
    // Native functions should execute without error
    assert!(result.is_ok(), "Native function call should work: {:?}", result);
}

#[test]
fn test_run_functions_without_arguments() {
    // Stage: Functions without arguments (pg8)
    // Command: run
    // Test: Function declarations and calls without arguments
    
    let code = r#"
        fun greet() {
            print "Hello, World!";
        }
        greet();
    "#;
    let expected = vec!["Hello, World!"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_functions_with_arguments() {
    // Stage: Functions with arguments (lb6)
    // Command: run
    // Test: Function declarations and calls with arguments
    
    let code = r#"
        fun add(a, b) {
            return a + b;
        }
        print add(3, 4);
    "#;
    let expected = vec!["7"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_function_syntax_errors() {
    // Stage: Syntax errors (px4)
    // Command: run
    // Test: Invalid function syntax should cause errors
    
    // Missing function body
    let code = r#"fun test();"#;
    assert_lox_error(code);
    
    // Invalid parameter syntax
    let code = r#"fun test(,) { }"#;
    assert_lox_error(code);
}

#[test]
fn test_run_return_statements() {
    // Stage: Return statements (rd2)
    // Command: run
    // Test: Return statements should work correctly
    
    let code = r#"
        fun early_return() {
            print "before return";
            return "returned value";
            print "after return";  // Should not execute
        }
        print early_return();
    "#;
    let expected = vec!["before return", "returned value"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_higher_order_functions() {
    // Stage: Higher order functions (ey3)
    // Command: run
    // Test: Functions as first-class values
    
    let code = r#"
        fun makeAdder(n) {
            fun adder(x) {
                return x + n;
            }
            return adder;
        }
        var add5 = makeAdder(5);
        print add5(3);
    "#;
    let expected = vec!["8"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_function_runtime_errors() {
    // Stage: Runtime errors (fj7)
    // Command: run
    // Test: Function runtime errors
    
    // Calling undefined function
    let code = r#"undefined_function();"#;
    assert_lox_error(code);
    
    // Wrong number of arguments
    let code = r#"
        fun test(a, b) {
            return a + b;
        }
        test(1);  // Missing argument
    "#;
    assert_lox_error(code);
}

#[test]
fn test_run_function_scope() {
    // Stage: Function scope (bz4)
    // Command: run
    // Test: Function scoping rules
    
    let code = r#"
        var global = "global";
        fun test() {
            var local = "local";
            print global;  // Should access global
            print local;   // Should access local
        }
        test();
        // print local;  // Would cause error - local not accessible
    "#;
    let expected = vec!["global", "local"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_closures() {
    // Stage: Closures (gg6)
    // Command: run
    // Test: Closures should capture variables from enclosing scope
    
    let code = r#"
        fun outer() {
            var x = "captured";
            fun inner() {
                print x;
            }
            return inner;
        }
        var closure = outer();
        closure();
    "#;
    let expected = vec!["captured"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_recursive_functions() {
    // Test recursive function calls
    // Command: run
    // Test: Functions should be able to call themselves
    
    let code = r#"
        fun factorial(n) {
            if (n <= 1) {
                return 1;
            }
            return n * factorial(n - 1);
        }
        print factorial(5);
    "#;
    let expected = vec!["120"];
    assert_lox_output(code, expected);
}

#[test]
fn test_run_function_as_values() {
    // Test functions as first-class values
    // Command: run
    // Test: Functions should be assignable to variables
    
    let code = r#"
        fun sayHello() {
            print "Hello!";
        }
        var fn = sayHello;
        fn();
    "#;
    let expected = vec!["Hello!"];
    assert_lox_output(code, expected);
}
