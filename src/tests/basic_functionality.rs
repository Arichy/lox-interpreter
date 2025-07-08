use super::test_utils::*;

#[test]
fn test_simple_print() {
    let code = r#"print "hello world";"#;
    let expected = vec!["hello world"];
    assert_lox_output(code, expected);
}

#[test]
fn test_variable_assignment() {
    let code = r#"var x = 5;
print x;
x = 10;
print x;"#;
    let expected = vec!["5", "10"];
    assert_lox_output(code, expected);
}
