use super::test_utils::*;

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
        assert_lox_output(code, expected);
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
fn test_class_call() {
    {
        let code = r#"
            class Spaceship {}
            var falcon = Spaceship();
            print falcon;
        "#;

        let expected = vec!["Spaceship instance"];
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output_then_error(code, expected);
    }
}
