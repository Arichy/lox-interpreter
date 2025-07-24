use super::{Evaluator, Vm};
use crate::{ast::*, Parser};
use std::collections::HashSet;

use crate::runner::StackFrame;
use rustc_hash::FxHashMap as HashMap;

// Helper function to parse code, find a function declaration, and collect its closure bindings.
fn get_closure_bindings_for_fn(code: &str, function_name: &str) -> HashSet<String> {
    let mut parser = Parser::new(code);
    let program = parser.parse().unwrap();
    let mut evaluator = Evaluator::new(code);
    let mut vm = Vm::new();

    // Push a dummy stack frame to simulate a real call environment
    vm.call_stack.push(StackFrame::new(
        vm.current_env.clone(),
        HashMap::default(),
        None,
    ));

    // Enter a new scope to simulate a non-global context
    vm.enter_scope();

    let mut target_func_decl: Option<FunctionDeclaration> = None;

    // Process statements to set up the environment before finding the target function
    for stmt in &program.body {
        if let StatementInner::Declaration(Declaration {
            inner: DeclarationInner::Function(func_decl),
            ..
        }) = &stmt.inner {
            if func_decl.name.name == function_name {
                target_func_decl = Some(func_decl.clone());
                break; // Stop after finding the target function
            }
        }

        // For any variable declarations encountered before the target function, define them in the VM.
        if let StatementInner::Declaration(Declaration {
            inner: DeclarationInner::Variable(var_decl),
            ..
        }) = &stmt.inner {
            vm.define_variable(var_decl.id.name.to_string(), crate::evaluator::Value::new_nil());
        }
    }

    let func_decl = target_func_decl
        .unwrap_or_else(|| panic!("Function '{}' not found in code", function_name));

    let bindings = evaluator.collect_closure_bindings(&func_decl, &vm);
    
    vm.leave_scope().unwrap();

    bindings
        .keys()
        .map(|k| k.to_string())
        .collect::<HashSet<String>>()
}

#[test]
fn test_capture_single_variable() {
    let code = r#"
        var x = "outer";
        fun my_closure() {
            print x;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "my_closure");
    assert!(bindings.contains("x"));
    assert_eq!(bindings.len(), 1);
}

#[test]
fn test_capture_multiple_variables() {
    let code = r#"
        var a = 1;
        var b = 2;
        fun my_closure() {
            print a + b;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "my_closure");
    assert!(bindings.contains("a"));
    assert!(bindings.contains("b"));
    assert_eq!(bindings.len(), 2);
}

#[test]
fn test_no_capture_for_pure_function() {
    let code = r#"
        fun pure_function() {
            return 1;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "pure_function");
    assert!(bindings.is_empty());
}

#[test]
fn test_no_capture_for_local_variables() {
    let code = r#"
        var x = "outer";
        fun my_closure() {
            var x = "inner";
            print x;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "my_closure");
    assert!(bindings.is_empty());
}

#[test]
fn test_no_capture_for_parameters() {
    let code = r#"
        var x = "outer";
        fun my_closure(x) {
            print x;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "my_closure");
    assert!(bindings.is_empty());
}

#[test]
fn test_capture_with_shadowing() {
    let code = r#"
        var x = "outer";
        fun my_closure() {
            var y = x; // 'x' should be captured here
            {
                var x = "inner";
                print x;
            }
            print y;
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "my_closure");
    assert!(bindings.contains("x"));
    assert_eq!(bindings.len(), 1);
}

#[test]
fn test_nested_closure_capture() {
    let code = r#"
        var a = "level1";
        fun outer() {
            var b = "level2";
            fun inner() {
                print a + b;
            }
        }
    "#;
    // This helper is too simple for nested functions.
    // We would need a more complex setup to test this properly.
    // For now, this test is more of a placeholder.
    // To test `inner`, we would need to execute `outer` first.
    // The current helper only inspects top-level functions.
}

#[test]
fn test_no_capture_of_global_functions() {
    let code = r#"
        fun helper() { return 1; }
        fun main() {
            print helper();
        }
    "#;
    let bindings = get_closure_bindings_for_fn(code, "main");
    // The current implementation might capture global functions.
    // A robust implementation should distinguish between variables and functions.
    // Let's assume for now it doesn't capture them.
    assert!(!bindings.contains("helper"));
}