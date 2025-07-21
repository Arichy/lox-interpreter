use super::*;

#[test]
fn test_self_initialization() {
    {
        let code = r#"
                var a = "value";
                var a = a;      // This is allowed in global scope
                print a;        // Should print "value"
            "#;

        let mut parser = Parser::new(code);

        let res = parser.parse();

        assert!(res.is_ok());
    }

    {
        let code = r#"
                var a = "outer";
                {
                    var a = a;  // Error: Can't read local variable in its own initializer
                }
            "#;

        let mut parser = Parser::new(code);

        let res = parser.parse();

        assert!(res.is_err());
    }

    {
        let code = r#"
                fun returnArg(arg) {
                    return arg;
                }

                var b = "global";
                {
                    var a = "first";
                    var b = returnArg(b);    // Error: Can't read local variable in its own initializer
                    print b;
                }

                var b = b + " updated";
                print b;
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err());
    }
}

#[test]
fn variable_redeclaration() {
    {
        let code = r#"
                {
                  var a = "value";
                  var a = "other";
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err());
    }

    {
        let code = r#"
                fun foo(a) {
                  var a;
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err())
    }

    {
        let code = r#"
                fun foo(arg, arg) {
                  "body";
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err())
    }

    {
        let code = r#"
                var a = "1";
                print a;

                var a;
                print a;

                var a = "2";
                print a;

                {
                  var a = "1";
                  var a = "2";
                  print a;
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err())
    }
}

#[test]
fn invalid_return() {
    {
        let code = r#"
               fun foo() {
                    return "at function scope is ok";
                }

                return;
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        println!("{res:?}");
        assert!(res.is_err())
    }

    {
        let code = r#"
                fun foo() {
                    if (true) {
                        return "early return";
                    }

                    for (var i = 0; i < 10; i = i + 1) {
                        return "loop return";
                    }
                }

                if (true) {
                    return "conditional return";
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        println!("{res:?}");
        assert!(res.is_err())
    }

    {
        let code = r#"
                {
                    return "not allowed in a block either";
                }

                fun allowed() {
                    if (true) {
                        return "this is fine";
                    }
                    return;
                }
            "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        println!("{res:?}");
        assert!(res.is_err())
    }
}

#[test]
fn parse_class() {
    {
        let code = r#"
                class Spaceship {}
                print Spaceship;
            "#;

        let mut parser = Parser::new(code);
        let tts = parser.parse().unwrap();
        let tt = tts.inner.body.get(0).unwrap();
        assert_eq!(tt.to_string(), "(class Spaceship)");
    }

    {
        let code = r#"
                class Robot {
                  beep() {
                    print "Beep boop!";
                  }
                }
            "#;

        let mut parser = Parser::new(code);
        let tts = parser.parse().unwrap();
        let tt = tts.inner.body.get(0).unwrap();
        assert_eq!(tt.to_string(), "(class Robot method beep)");
    }
}

#[test]
fn test_call_expr() {
    {
        let code = r#"a.b.method();"#;

        let mut parser = Parser::new(code);
        let tts = parser.parse().unwrap();
        let tt = tts.inner.body.get(0).unwrap();
        println!("{tt:#?}");
    }
}

#[test]
fn test_invalid_this() {
    {
        let code = r#"
            fun notAMethod() {
              print this;
            }
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();
        assert!(res.is_err());
    }

    {
        let code = r#"
            class Person {
            sayName() {
                print this();
            }
            }
            Person().sayName();
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        println!("{res:?}");

        assert!(res.is_err());
    }

    // {
    //     let code = r#"
    //         class Confused {
    //             method() {
    //                 fun inner(instance) {
    //                 var feeling = "confused";
    //                 print this.feeling;
    //                 }
    //                 return inner;
    //             }
    //         }

    //         var instance = Confused();
    //         var m = instance.method();
    //         m(instance);
    //     "#;

    //     let mut parser = Parser::new(code);
    //     let res = parser.parse();

    //     println!("{res:?}");

    //     assert!(res.is_err());
    // }
}

#[test]
fn test_return_within_init() {
    {
        let code = r#"
            class ThingDefault {
              init() {
                this.x = "foo";
                this.y = 42;
                return this;
              }
            }
            var out = ThingDefault();
            print out;
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }

    {
        let code = r#"
            class Foo {
              init() {
                return "something else";
              }
            }

            Foo();

        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }

    {
        let code = r#"
               class Foo {
                 init() {
                   return this.callback();
                 }

                 callback() {
                   return "callback";
                 }
               }

               Foo();
           "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }
}

#[test]
fn inheritance_errors() {
    {
        let code = r#"
               class Foo < Foo {}
           "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }
}

#[test]
fn test_super() {
    {
        let code = r#"
            class Doughnut {
              cook() {
                print "Fry until golden brown.";
              }
            }

            // Super can be used to call the overridden method
            // of the parent class
            class BostonCream < Doughnut {
              cook() {
                super.cook();
              }
            }
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_ok());
    }

    {
        let code = r#"
            class Foo {
              cook() {
                // Foo is not a subclass
                super.cook(); // expect compile error
              }
            }
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }

    {
        let code = r#"
            class A {}

            class B < A {
              method() {
                // super must be followed by `.`
                // and an expression
                super; // expect compile error
              }
            }
        "#;

        let mut parser = Parser::new(code);
        let res = parser.parse();

        assert!(res.is_err());
    }
}
