use super::test_utils::*;

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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
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
        assert_lox_output(code, expected);
    }
}
