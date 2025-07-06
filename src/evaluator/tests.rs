use super::*;
use crate::{
    ast::{
        BlockStatement, BlockStatementInner, Expression, ExpressionInner, FunctionDeclaration,
        FunctionDeclarationInner, Identifier, IdentifierInner, Statement, StatementInner,
    },
    runner::{Environment, Vm},
    Parser,
};

#[test]
fn test_collect_closure_binding_env_basic() {
    // Create a simple function that references an external variable
    let external_var_name = "external_var";
    let param_name = "param";

    // Create identifier for external variable
    let external_var_identifier = Identifier {
        inner: IdentifierInner {
            name: Cow::Borrowed(external_var_name),
        },
        range: (0, 0),
    };

    // Create expression that references external variable
    let external_var_expr = Expression {
        inner: ExpressionInner::Identifier(external_var_identifier),
        range: (0, 0),
    };

    // Create statement that uses the external variable
    let stmt = Statement {
        inner: StatementInner::Expression(external_var_expr),
        range: (0, 0),
    };

    // Create function body with the statement
    let body = BlockStatement {
        inner: BlockStatementInner {
            statements: vec![stmt],
        },
        range: (0, 0),
    };

    // Create function parameter
    let param = Identifier {
        inner: IdentifierInner {
            name: Cow::Borrowed(param_name),
        },
        range: (0, 0),
    };

    // Create function declaration
    let func_decl = FunctionDeclaration {
        inner: FunctionDeclarationInner {
            name: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed("test_func"),
                },
                range: (0, 0),
            },
            parameters: vec![param],
            body: Box::new(body),
        },
        range: (0, 0),
    };

    // Create VM with environment containing external variable
    let env = Environment::new_global();
    let external_value = Value::new_string("external_value".into());
    env.define(external_var_name.to_string(), external_value.clone());

    let mut vm = Vm::new();
    vm.current_env = env.clone();

    // Create evaluator
    let evaluator = Evaluator::new("");

    // Test collect_closure_binding_env
    let binding_env = evaluator.collect_closure_binding_env(&func_decl, &vm);

    // Verify that external variable environment is captured
    assert_eq!(binding_env.len(), 1);
    assert!(binding_env.contains_key(external_var_name));

    // Verify the captured environment contains the variable
    let captured_env = binding_env.get(external_var_name).unwrap();
    let captured_value = captured_env.get(external_var_name).unwrap();
    match (&*captured_value.inner, &*external_value.inner) {
        (ValueInner::String(s1), ValueInner::String(s2)) => assert_eq!(s1, s2),
        _ => panic!("Expected string values"),
    }
}

#[test]
fn test_collect_closure_binding_env_excludes_params() {
    // Create a function that references a parameter (should not be captured)
    let param_name = "param";

    // Create identifier for parameter
    let param_identifier = Identifier {
        inner: IdentifierInner {
            name: Cow::Borrowed(param_name),
        },
        range: (0, 0),
    };

    // Create expression that references parameter
    let param_expr = Expression {
        inner: ExpressionInner::Identifier(param_identifier.clone()),
        range: (0, 0),
    };

    // Create statement that uses the parameter
    let stmt = Statement {
        inner: StatementInner::Expression(param_expr),
        range: (0, 0),
    };

    // Create function body with the statement
    let body = BlockStatement {
        inner: BlockStatementInner {
            statements: vec![stmt],
        },
        range: (0, 0),
    };

    // Create function declaration
    let func_decl = FunctionDeclaration {
        inner: FunctionDeclarationInner {
            name: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed("test_func"),
                },
                range: (0, 0),
            },
            parameters: vec![param_identifier],
            body: Box::new(body),
        },
        range: (0, 0),
    };

    // Create VM with environment containing variable with same name as parameter
    let env = Environment::new_global();
    let param_value = Value::new_string("param_value".into());
    env.define(param_name.to_string(), param_value);

    let mut vm = Vm::new();
    vm.current_env = env.clone();

    // Create evaluator
    let evaluator = Evaluator::new("");

    // Test collect_closure_binding_env
    let binding_env = evaluator.collect_closure_binding_env(&func_decl, &vm);

    // Verify that parameter is NOT captured (since it's a local variable)
    assert_eq!(binding_env.len(), 0);
}

#[test]
fn test_closure_binding_env_integration() {
    // Test the full integration of closure binding environment with function declarations
    let outer_var_name = "outer_var";
    let inner_var_name = "inner_var";

    // Create VM with environment containing outer variable
    let env = Environment::new_global();
    let outer_value = Value::new_string("outer_value".into());
    env.define(outer_var_name.to_string(), outer_value.clone());

    let mut vm = Vm::new();
    vm.current_env = env.clone();

    // Create evaluator
    let evaluator = Evaluator::new("");

    // Create a function declaration that references the outer variable
    let outer_var_identifier = Identifier {
        inner: IdentifierInner {
            name: Cow::Borrowed(outer_var_name),
        },
        range: (0, 0),
    };

    // Create variable declaration inside function (should not be captured)
    let inner_var_decl = VariableDeclaration {
        inner: VariableDeclarationInner {
            id: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(inner_var_name),
                },
                range: (0, 0),
            },
            init: Some(Expression {
                inner: ExpressionInner::Identifier(outer_var_identifier.clone()),
                range: (0, 0),
            }),
        },
        range: (0, 0),
    };

    // Create statement that declares the inner variable
    let var_decl_stmt = Statement {
        inner: StatementInner::Declaration(Declaration {
            inner: DeclarationInner::Variable(inner_var_decl),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Create statement that uses the inner variable
    let inner_var_use = Statement {
        inner: StatementInner::Expression(Expression {
            inner: ExpressionInner::Identifier(Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(inner_var_name),
                },
                range: (0, 0),
            }),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Create function body with both statements
    let body = BlockStatement {
        inner: BlockStatementInner {
            statements: vec![var_decl_stmt, inner_var_use],
        },
        range: (0, 0),
    };

    // Create function declaration
    let func_decl = FunctionDeclaration {
        inner: FunctionDeclarationInner {
            name: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed("test_func"),
                },
                range: (0, 0),
            },
            parameters: vec![],
            body: Box::new(body),
        },
        range: (0, 0),
    };

    // Test collect_closure_binding_env directly
    let binding_env = evaluator.collect_closure_binding_env(&func_decl, &vm);

    // Verify that only the outer variable environment is captured
    assert_eq!(binding_env.len(), 1);
    assert!(binding_env.contains_key(outer_var_name));
    assert!(!binding_env.contains_key(inner_var_name));

    // Verify the captured environment contains the correct value
    let captured_env = binding_env.get(outer_var_name).unwrap();
    let captured_value = captured_env.get(outer_var_name).unwrap();
    match (&*captured_value.inner, &*outer_value.inner) {
        (ValueInner::String(s1), ValueInner::String(s2)) => assert_eq!(s1, s2),
        _ => panic!("Expected string values"),
    }
}

#[test]
fn test_closure_binding_env_comprehensive() {
    // Test comprehensive closure binding environment functionality
    let global_var = "global_var";
    let outer_var = "outer_var";
    let param_var = "param_var";
    let local_var = "local_var";

    // Create VM with environment containing variables
    let env = Environment::new_global();
    let global_value = Value::new_string("global_value".into());
    let outer_value = Value::new_string("outer_value".into());
    env.define(global_var.to_string(), global_value.clone());
    env.define(outer_var.to_string(), outer_value.clone());

    let mut vm = Vm::new();
    vm.current_env = env.clone();

    // Create evaluator
    let evaluator = Evaluator::new("");

    // Create complex function that:
    // 1. Takes a parameter (should not be captured)
    // 2. Declares a local variable (should not be captured)
    // 3. Uses outer variable (should be captured)
    // 4. Uses global variable (should be captured)

    // Parameter
    let param_identifier = Identifier {
        inner: IdentifierInner {
            name: Cow::Borrowed(param_var),
        },
        range: (0, 0),
    };

    // Local variable declaration
    let local_var_decl = VariableDeclaration {
        inner: VariableDeclarationInner {
            id: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(local_var),
                },
                range: (0, 0),
            },
            init: Some(Expression {
                inner: ExpressionInner::Identifier(Identifier {
                    inner: IdentifierInner {
                        name: Cow::Borrowed(param_var),
                    },
                    range: (0, 0),
                }),
                range: (0, 0),
            }),
        },
        range: (0, 0),
    };

    // Statement declaring local variable
    let local_var_stmt = Statement {
        inner: StatementInner::Declaration(Declaration {
            inner: DeclarationInner::Variable(local_var_decl),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Statement using outer variable
    let outer_var_stmt = Statement {
        inner: StatementInner::Expression(Expression {
            inner: ExpressionInner::Identifier(Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(outer_var),
                },
                range: (0, 0),
            }),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Statement using global variable
    let global_var_stmt = Statement {
        inner: StatementInner::Expression(Expression {
            inner: ExpressionInner::Identifier(Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(global_var),
                },
                range: (0, 0),
            }),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Statement using local variable
    let local_var_use_stmt = Statement {
        inner: StatementInner::Expression(Expression {
            inner: ExpressionInner::Identifier(Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed(local_var),
                },
                range: (0, 0),
            }),
            range: (0, 0),
        }),
        range: (0, 0),
    };

    // Create function body with all statements
    let body = BlockStatement {
        inner: BlockStatementInner {
            statements: vec![
                local_var_stmt,
                outer_var_stmt,
                global_var_stmt,
                local_var_use_stmt,
            ],
        },
        range: (0, 0),
    };

    // Create function declaration
    let func_decl = FunctionDeclaration {
        inner: FunctionDeclarationInner {
            name: Identifier {
                inner: IdentifierInner {
                    name: Cow::Borrowed("comprehensive_func"),
                },
                range: (0, 0),
            },
            parameters: vec![param_identifier],
            body: Box::new(body),
        },
        range: (0, 0),
    };

    // Test collect_closure_binding_env
    let binding_env = evaluator.collect_closure_binding_env(&func_decl, &vm);

    // Verify captured variables
    assert_eq!(
        binding_env.len(),
        2,
        "Should capture outer_var and global_var"
    );
    assert!(
        binding_env.contains_key(outer_var),
        "Should capture outer_var"
    );
    assert!(
        binding_env.contains_key(global_var),
        "Should capture global_var"
    );

    // Verify NOT captured variables
    assert!(
        !binding_env.contains_key(param_var),
        "Should NOT capture parameter"
    );
    assert!(
        !binding_env.contains_key(local_var),
        "Should NOT capture local variable"
    );

    // Verify captured environments contain correct values
    let outer_env = binding_env.get(outer_var).unwrap();
    let outer_captured_value = outer_env.get(outer_var).unwrap();
    match (&*outer_captured_value.inner, &*outer_value.inner) {
        (ValueInner::String(s1), ValueInner::String(s2)) => assert_eq!(s1, s2),
        _ => panic!("Expected string values for outer_var"),
    }

    let global_env = binding_env.get(global_var).unwrap();
    let global_captured_value = global_env.get(global_var).unwrap();
    match (&*global_captured_value.inner, &*global_value.inner) {
        (ValueInner::String(s1), ValueInner::String(s2)) => assert_eq!(s1, s2),
        _ => panic!("Expected string values for global_var"),
    }
}
