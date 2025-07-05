#[cfg(test)]
mod integration_tests {
    use crate::{
        runner::Runner,
        start_capture, end_capture,
    };

    // Helper function to run Lox code and capture output
    fn run_lox_code(code: &str) -> Result<Vec<String>, String> {
        // Start capturing stdout
        start_capture();
        
        // Create and run the interpreter
        let runner = Runner::new(code);
        
        // Run the code
        let result = runner.run();
        
        // Get captured output
        let (stdout, _stderr) = end_capture();
        
        match result {
            Ok(_) => Ok(stdout),
            Err(e) => Err(e.to_string()),
        }
    }

    #[test]
    fn test_closure_counter() {
        let code = r#"fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }

  return count;
}

var counter = makeCounter();
counter();
counter();"#;
        
        let expected = vec!["1", "2"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_scope_shadowing() {
        let code = r#"var counter = 89;

fun incrementCounter(amount) {
  counter = counter + amount;
  print counter;
}

{
  counter = 83;
  incrementCounter(6);
  print counter;
}
print counter;"#;
        
        let expected = vec!["89", "89", "89"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_nested_closure_scope() {
        let code = r#"var count = 0;

{
  fun makeCounter() {
    fun counter() {
      count = count + 1;
      print count;
    }
    return counter;
  }

  var counter1 = makeCounter();
  counter1();
  counter1();

  var count = 0;

  counter1();
}"#;
        
        let expected = vec!["1", "2", "3"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_simple_print() {
        let code = r#"print "hello world";"#;
        let expected = vec!["hello world"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_variable_assignment() {
        let code = r#"var x = 5;
print x;
x = 10;
print x;"#;
        let expected = vec!["5", "10"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_with_parameters() {
        let code = r#"fun makeAdder(x) {
  fun add(y) {
    return x + y;
  }
  return add;
}

var add5 = makeAdder(5);
print add5(3);
print add5(7);"#;
        
        let expected = vec!["8", "12"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_multiple_closures_same_env() {
        let code = r#"fun makeCounters() {
  var count = 0;
  
  fun increment() {
    count = count + 1;
    print count;
  }
  
  return increment;
}

var inc = makeCounters();
inc();
inc();"#;
        
        let expected = vec!["1", "2"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_simple_nested_closure() {
        let code = r#"fun outer(x) {
  fun inner(y) {
    return x + y;
  }
  return inner;
}

var f = outer(10);
print f(5);"#;
        
        let expected = vec!["15"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_scope_isolation() {
        let code = r#"fun makeCounter(initial) {
  var count = initial;
  fun counter() {
    count = count + 1;
    return count;
  }
  return counter;
}

var counter1 = makeCounter(0);
var counter2 = makeCounter(10);

print counter1();
print counter2();
print counter1();
print counter2();"#;
        
        let expected = vec!["1", "11", "2", "12"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_with_global_vars() {
        let code = r#"var global = 100;

fun makeModifier(amount) {
  fun modify() {
    global = global + amount;
    return global;
  }
  return modify;
}

var add10 = makeModifier(10);
var add20 = makeModifier(20);

print add10();
print add20();
print add10();
print global;"#;
        
        let expected = vec!["110", "130", "140", "140"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_complex_scoping() {
        let code = r#"var a = "global";

{
  fun showA() {
    print a;
  }
  
  showA();
  var a = "block";
  showA();
}"#;
        
        let expected = vec!["global", "global"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_with_local_function() {
        let code = r#"fun makeCalculator() {
  var result = 0;
  
  fun add(n) {
    result = result + n;
    print result;
  }
  
  return add;
}

var calc = makeCalculator();
calc(5);
calc(3);
calc(2);"#;
        
        let expected = vec!["5", "8", "10"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_variable_mutation() {
        let code = r#"fun makeAccumulator() {
  var sum = 0;
  
  fun add(value) {
    sum = sum + value;
    print sum;
  }
  
  return add;
}

var acc = makeAccumulator();
acc(10);
acc(20);
acc(30);"#;
        
        let expected = vec!["10", "30", "60"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_parameter_shadowing() {
        let code = r#"fun outer(x) {
  fun inner(x) {
    return x * 2;
  }
  return inner(x) + x;
}

print outer(5);"#;
        
        let expected = vec!["15"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_captures_after_reassignment() {
        let code = r#"var x = 10;

fun makeClosure() {
  fun inner() {
    print x;
  }
  return inner;
}

var closure = makeClosure();
x = 20;
closure();"#;
        
        let expected = vec!["20"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_variable_lifetime() {
        let code = r#"var closure;

{
  var localVar = 42;
  fun captureLocal() {
    print localVar;
  }
  closure = captureLocal;
}

closure();"#;
        
        let expected = vec!["42"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_multiple_closures_same_variable() {
        let code = r#"var shared = 0;

fun makeClosure1() {
  fun closure1() {
    shared = shared + 1;
    print shared;
  }
  return closure1;
}

fun makeClosure2() {
  fun closure2() {
    shared = shared + 10;
    print shared;
  }
  return closure2;
}

var c1 = makeClosure1();
var c2 = makeClosure2();

c1();
c2();
c1();"#;
        
        let expected = vec!["1", "11", "12"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_captures_from_multiple_scopes() {
        let code = r#"var global = 100;

fun outer() {
  var outer_var = 200;
  
  fun inner() {
    print global;
    print outer_var;
  }
  
  return inner;
}

var f = outer();
f();"#;
        
        let expected = vec!["100", "200"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_with_conditional_capture() {
        let code = r#"var condition = true;
var value = 10;

fun makeConditionalClosure() {
  fun conditional() {
    if (condition) {
      print value;
    } else {
      print "false branch";
    }
  }
  return conditional;
}

var closure = makeConditionalClosure();
closure();

condition = false;
closure();"#;
        
        let expected = vec!["10", "false branch"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_modifies_captured_after_scope_ends() {
        let code = r#"var modifyClosure;

{
  var capturedVar = 5;
  
  fun modify() {
    capturedVar = capturedVar * 2;
    print capturedVar;
  }
  
  modifyClosure = modify;
  print capturedVar;
}

modifyClosure();
modifyClosure();"#;
        
        let expected = vec!["5", "10", "20"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_chain_variable_access() {
        let code = r#"fun level1() {
  var a = 1;
  
  fun level2() {
    var b = 2;
    print a;
    print b;
    print a + b;
  }
  
  return level2;
}

var f = level1();
f();"#;
        
        let expected = vec!["1", "2", "3"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_parameter_vs_captured_variable() {
        let code = r#"var x = 100;

fun makeClosureWithParam(x) {
  fun inner() {
    print x;
  }
  return inner;
}

var closure = makeClosureWithParam(42);
closure();
print x;"#;
        
        let expected = vec!["42", "100"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_returning_closure() {
        let code = r#"fun outer(x) {
  fun middle(y) {
    fun inner(z) {
      return x + y + z;
    }
    return inner;
  }
  return middle;
}

var f = outer(1);
var g = f(2);
print g(3);"#;
        
        let expected = vec!["6"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_closure_with_recursion() {
        let code = r#"fun makeFactorial() {
  fun factorial(n) {
    if (n <= 1) {
      return 1;
    }
    return n * factorial(n - 1);
  }
  return factorial;
}

var fact = makeFactorial();
print fact(5);
print fact(3);"#;
        
        let expected = vec!["120", "6"];
        
        match run_lox_code(code) {
            Ok(result) => assert_eq!(result, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }


}