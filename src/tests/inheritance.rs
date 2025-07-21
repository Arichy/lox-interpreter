use crate::tests::test_utils::assert_lox_output;

#[test]
fn test_basic_syntax() {
    {
        let code = r#"
            class Doughnut {}

            class BostonCream < Doughnut {}

            print Doughnut();
            print BostonCream();
        "#;

        let expected: Vec<&str> = vec!["Doughnut instance", "BostonCream instance"];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
            {
              class A {}

              // B is a subclass of A
              class B < A {}

              // C is also a subclass of A
              class C < A {}

              print A();
              print B();
              print C();
            }
        "#;

        let expected: Vec<&str> = vec!["A instance", "B instance", "C instance"];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
            class Vehicle {}

            // Car is a subclass of Vehicle
            class Car < Vehicle {}

            // Sedan is a subclass of Car
            class Sedan < Car {}

            print Vehicle();
            print Car();
            print Sedan();

            {
              // Truck is a subclass of Vehicle
              class Truck < Vehicle {}
              print Truck();
            }
        "#;

        let expected: Vec<&str> = vec![
            "Vehicle instance",
            "Car instance",
            "Sedan instance",
            "Truck instance",
        ];
        assert_lox_output(code, expected);
    }
}

#[test]
fn test_inheriting_methods() {
    {
        let code = r#"
            class Doughnut {
              cook() {
                print "Fry until golden brown.";
              }
            }

            // BostonCream is a subclass of Doughnut
            class BostonCream < Doughnut {}

            // BostonCream class should inherit the cook
            // method from Doughnut class
            BostonCream().cook();
        "#;

        let expected: Vec<&str> = vec!["Fry until golden brown."];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
            class Root {
              getName() {
                print "Root class";
              }
            }

            class Parent < Root {
              parentMethod() {
                print "Method defined in Parent";
              }
            }

            class Child < Parent {
              childMethod() {
                print "Method defined in Child";
              }
            }

            var root = Root();
            var parent = Parent();
            var child = Child();

            // Root methods are available to all
            root.getName();
            parent.getName();
            child.getName();

            // Parent methods are available to Parent and Child
            parent.parentMethod();
            child.parentMethod();

            // Child methods are only available to Child
            child.childMethod();

        "#;

        let expected: Vec<&str> = vec![
            "Root class",
            "Root class",
            "Root class",
            "Method defined in Parent",
            "Method defined in Parent",
            "Method defined in Child",
        ];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
               class A {
                 method() {
                   print "A method";
                 }
               }

               // B inherits method `method` from A
               // and overrides it with a new implementation
               class B < A {
                 method() {
                   print "B method";
                 }
               }

               var b = B();
               b.method();  // expect: B method
           "#;

        let expected: Vec<&str> = vec!["B method"];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
               class Base {
                 init(a) {
                   this.a = a;
                 }
               }

               // Constructors can also be overridden
               class Derived < Base {
                 init(a, b) {
                   this.a = a;
                   this.b = b;
                 }
               }

               var derived = Derived(89, 32);
               print derived.a;
               print derived.b;

        "#;

        let expected: Vec<&str> = vec!["89", "32"];
        assert_lox_output(code, expected);
    }

    {
        let code = r#"
            class Animal {
              speak() {
                return "Animal speaks";
              }

              makeSound() {
                return "Generic sound";
              }

              communicate() {
                return this.speak() + " : " + this.makeSound();
              }
            }

            class Dog < Animal {
              speak() {
                return "Dog speaks";
              }

              makeSound() {
                return "Woof";
              }
            }

            class Puppy < Dog {
              speak() {
                return "Puppy speaks";
              }
            }

            var animal = Animal();
            var dog = Dog();
            var puppy = Puppy();

            print animal.communicate();
            print dog.communicate();
            print puppy.communicate();
        "#;

        let expected: Vec<&str> = vec![
            "Animal speaks : Generic sound",
            "Dog speaks : Woof",
            "Puppy speaks : Woof",
        ];
        assert_lox_output(code, expected);
    }
}
