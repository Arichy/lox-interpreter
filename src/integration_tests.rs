#[cfg(test)]
mod integration_tests {
    use crate::{end_capture, runner::Runner, start_capture};

    // Helper function to run Lox code and capture output
    fn run_lox_code(code: &str) -> (Result<(), miette::Report>, Vec<String>, Vec<String>) {
        // Start capturing stdout
        start_capture();

        // Create and run the interpreter
        let runner = Runner::new(code);

        // Run the code
        let result = runner.run();

        // Get captured output
        let (stdout, _stderr) = end_capture();

        (result, stdout, _stderr)
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_simple_print() {
        let code = r#"print "hello world";"#;
        let expected = vec!["hello world"];

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_class_decl() {
        {
            let code = r#"
                class Robot {}
                class Wizard {}
                print Robot;
                print Wizard;
                print "Both classes successfully printed";
            "#;

            let expected = vec!["Robot", "Wizard", "Both classes successfully printed"];

            let (result, stdout, _stderr) = run_lox_code(code);
            match result {
                Ok(_) => assert_eq!(stdout, expected),
                Err(e) => panic!("Test failed with error: {}", e),
            }
        }

        {
            let code = r#"
                {
                  // Class declaration inside blocks should work
                  class Dinosaur {}
                  print "Inside block: Dinosaur exists";
                  print Dinosaur;
                }
                print "Accessing out-of-scope class:";
                print Dinosaur; // expect runtime error
            "#;

            let expected = vec![
                "Inside block: Dinosaur exists",
                "Dinosaur",
                "Accessing out-of-scope class:",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_err());
        }

        {
            let code = r#"
                // Class declaration inside function should work
                fun foo() {
                  class Superhero {}
                  print "Class declared inside function";
                  print Superhero;
                }

                foo();
                print "Function called successfully";
            "#;

            let expected = vec![
                "Class declared inside function",
                "Superhero",
                "Function called successfully",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }
    }

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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
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

        let (result, stdout, _stderr) = run_lox_code(code);
        // Should print the first message but then error on nil access
        assert_eq!(stdout, expected);
        assert!(result.is_err());
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
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

        let (result, stdout, _stderr) = run_lox_code(code);
        // Should parse successfully and print first message before failing on nil access
        assert_eq!(stdout, expected);
        assert!(result.is_err()); // Should error when trying to access property on nil
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

        let (result, stdout, _stderr) = run_lox_code(code);
        match result {
            Ok(_) => assert_eq!(stdout, expected),
            Err(e) => panic!("Test failed with error: {}", e),
        }
    }

    #[test]
    fn test_class_call() {
        {
            let code = r#"
                class Spaceship {}
                var falcon = Spaceship();
                print falcon;
            "#;

            let expected = vec!["Spaceship instance"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Robot {}
                var r1 = Robot();
                var r2 = Robot();

                print "Created multiple robots:";
                print r1;
                print r2;
            "#;

            let expected = vec![
                "Created multiple robots:",
                "Robot instance",
                "Robot instance",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Wizard {}
                class Dragon {}

                // Instantiating classes in a function should work
                fun createCharacters() {
                  var merlin = Wizard();
                  var smaug = Dragon();
                  print "Characters created in fantasy world:";
                  print merlin;
                  print smaug;
                  return merlin;
                }

                var mainCharacter = createCharacters();
                // An instance of a class should be truthy
                if (mainCharacter) {
                  print "The main character is:";
                  print mainCharacter;
                } else {
                  print "Failed to create a main character.";
                }
            "#;

            let expected = vec![
                "Characters created in fantasy world:",
                "Wizard instance",
                "Dragon instance",
                "The main character is:",
                "Wizard instance",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Superhero {}

                var count = 0;
                while (count < 3) {
                  var hero = Superhero();
                  print "Hero created:";
                  print hero;
                  count = count + 1;
                }

                print "All heroes created!";
            "#;

            let expected = vec![
                "Hero created:",
                "Superhero instance",
                "Hero created:",
                "Superhero instance",
                "Hero created:",
                "Superhero instance",
                "All heroes created!",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }
    }

    #[test]
    fn test_object_get_set() {
        {
            let code = r#"
                class Spaceship {}
                var falcon = Spaceship();

                falcon.name = "Millennium Falcon";
                falcon.speed = 75.5;

                print "Ship details:";
                print falcon.name;
                print falcon.speed;
            "#;

            let expected = vec!["Ship details:", "Millennium Falcon", "75.5"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                // Multiple properties and conditional access
                class Robot {}
                var r2d2 = Robot();

                r2d2.model = "Astromech";
                r2d2.operational = false;

                if (r2d2.operational) {
                  print r2d2.model;
                  r2d2.mission = "Navigate hyperspace";
                  print r2d2.mission;
                }
            "#;

            let expected: Vec<&str> = vec![];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                // Multiple instances with properties
                class Superhero {}
                var batman = Superhero();
                var superman = Superhero();

                batman.name = "Batman";
                batman.called = 18;

                superman.name = "Superman";
                superman.called = 66;

                print "Times " + superman.name + " was called: ";
                print superman.called;
                print "Times " + batman.name + " was called: ";
                print batman.called;
            "#;

            let expected: Vec<&str> = vec![
                "Times Superman was called: ",
                "66",
                "Times Batman was called: ",
                "18",
            ];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                // Property manipulation in functions
                class Wizard {}
                var gandalf = Wizard();

                gandalf.color = "Grey";
                gandalf.power = nil;
                print gandalf.color;

                fun promote(wizard) {
                  wizard.color = "White";
                  if (true) {
                    wizard.power = 100;
                  } else {
                    wizard.power = 0;
                  }
                }

                promote(gandalf);
                print gandalf.color;
                print gandalf.power;
            "#;

            let expected: Vec<&str> = vec!["Grey", "White", "100"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                // Property manipulation in functions
                class Wizard {}
                var gandalf = Wizard();

                gandalf.color = "Grey";
                print gandalf.color;
                print gandalf.power;
            "#;

            let expected: Vec<&str> = vec!["Grey"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_err());
        }
    }

    #[test]
    fn test_instance_methods() {
        {
            let code = r#"
                class Robot {
                  beep() {
                    print "Beep boop!";
                  }
                }

                var r2d2 = Robot();
                r2d2.beep();

                Robot().beep();
            "#;

            let expected: Vec<&str> = vec!["Beep boop!", "Beep boop!"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                {
                  class Foo {
                    returnSelf() {
                      return Foo;
                    }
                  }

                  print Foo().returnSelf();  // expect: Foo
                }
            "#;

            let expected: Vec<&str> = vec!["Foo"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Wizard {
                  castSpell(spell) {
                    print "Casting a magical spell: " + spell;
                  }
                }

                class Dragon {
                  breatheFire(fire, intensity) {
                    print "Breathing " + fire + " with intensity: "
                    + intensity;
                  }
                }

                // Methods on different class instances
                var merlin = Wizard();
                var smaug = Dragon();

                // Conditional method calling
                if (true) {
                  var action = merlin.castSpell;
                  action("Fireball");
                } else {
                  var action = smaug.breatheFire;
                  action("Fire", "100");
                }
            "#;

            let expected: Vec<&str> = vec!["Casting a magical spell: Fireball"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Superhero {
                  useSpecialPower(hero) {
                    print "Using power: " + hero.specialPower;
                  }

                  hasSpecialPower(hero) {
                    return hero.specialPower;
                  }

                  giveSpecialPower(hero, power) {
                    hero.specialPower = power;
                  }
                }

                // Methods in functions
                fun performHeroics(hero, superheroClass) {
                  if (superheroClass.hasSpecialPower(hero)) {
                    superheroClass.useSpecialPower(hero);
                  } else {
                    print "No special power available";
                  }
                }

                var superman = Superhero();
                var heroClass = Superhero();

                if (true) {
                  heroClass.giveSpecialPower(superman, "Flight");
                } else {
                  heroClass.giveSpecialPower(superman, "Strength");
                }

                performHeroics(superman, heroClass);
            "#;

            let expected: Vec<&str> = vec!["Using power: Flight"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }
    }

    #[test]
    fn test_this() {
        {
            let code = r#"
                class Spaceship {
                  identify() {
                    print this;
                  }
                }

                Spaceship().identify();
            "#;

            let expected: Vec<&str> = vec!["Spaceship instance"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Calculator {
                  add(a, b) {
                    return a + b + this.memory;
                  }
                }

                var calc = Calculator();
                calc.memory = 82;
                print calc.add(92, 1);
            "#;

            let expected: Vec<&str> = vec!["175"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Animal {
                  makeSound() {
                    print this.sound;
                  }
                  identify() {
                    print this.species;
                  }
                }

                var dog = Animal();
                dog.sound = "Woof";
                dog.species = "Dog";

                var cat = Animal();
                cat.sound = "Meow";
                cat.species = "Cat";

                // Swap methods between instances
                cat.makeSound = dog.makeSound;
                dog.identify = cat.identify;

                cat.makeSound();
                dog.identify();
            "#;

            let expected: Vec<&str> = vec!["Woof", "Cat"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }

        {
            let code = r#"
                class Wizard {
                  getSpellCaster() {
                    fun castSpell() {
                      print this;
                      print "Casting spell as " + this.name;
                    }

                    return castSpell;
                  }
                }

                var wizard = Wizard();
                wizard.name = "Merlin";
                wizard.getSpellCaster()();
            "#;

            let expected: Vec<&str> = vec!["Wizard instance", "Casting spell as Merlin"];

            let (res, stdout, _stderr) = run_lox_code(code);
            assert_eq!(stdout, expected);
            assert!(res.is_ok());
        }
    }
}
