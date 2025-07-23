use super::test_utils::*;

#[test]
fn basic_super() {
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

        BostonCream().cook();
    "#;
    let expected = vec!["Fry until golden brown."];
    assert_lox_output(code, expected);
}

#[test]
fn cascading_super() {
    let code = r#"
        class Base {
          method() {
            print "Base.method()";
          }
        }

        class Parent < Base {
          method() {
            super.method();
          }
        }

        class Child < Parent {
          method() {
            super.method();
          }
        }

        var parent = Parent();
        parent.method();
        var child = Child();
        child.method();

    "#;
    let expected = vec!["Base.method()", "Base.method()"];
    assert_lox_output(code, expected);
}

#[test]
fn super_closure() {
    let code = r#"
        class A {
          say() {
            print "A";
          }
        }

        class B < A {
          getClosure() {
            fun closure() {
              super.say();
            }
            return closure;
          }

          say() {
            print "B";
          }
        }

        class C < B {
          say() {
            print "C";
          }
        }

        C().getClosure()();

    "#;
    let expected = vec!["A"];
    assert_lox_output(code, expected);
}
