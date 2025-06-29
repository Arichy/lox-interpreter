use core::fmt;
use std::borrow::Cow;

use miette::{Error, LabeledSpan};

use crate::{
    parse::{Atom, Op, TokenTree, TokenTreeInner},
    Parser,
};

pub struct Evaluator<'de> {
    whole: &'de str,
    pub parser: Parser<'de>,
}

pub enum EvaluateResult<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
}

impl fmt::Display for EvaluateResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvaluateResult::Bool(boolean) => write!(f, "{}", boolean),
            EvaluateResult::Nil => write!(f, "nil"),
            EvaluateResult::Number(num) => write!(f, "{}", num),
            EvaluateResult::String(string) => write!(f, "{}", string),
        }
    }
}

impl<'de> Evaluator<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            parser: Parser::new(input),
        }
    }

    pub fn evaluate_expression(mut self) -> Result<EvaluateResult<'de>, Error> {
        let parser = std::mem::take(&mut self.parser);
        let token_tree = parser.parse_expression()?;
        self.evaluate_token_tree(&token_tree)
    }

    pub fn evaluate_token_tree(
        &self,
        token_tree: &TokenTree<'de>,
    ) -> Result<EvaluateResult<'de>, Error> {
        let evaluate_result = match &token_tree.inner {
            TokenTreeInner::Atom(atom) => match atom {
                Atom::String(string) => EvaluateResult::String(string.clone()),
                Atom::Number(num) => EvaluateResult::Number(*num),
                Atom::Nil => EvaluateResult::Nil,
                Atom::Bool(boolean) => EvaluateResult::Bool(*boolean),
                Atom::Ident(ident) => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            token_tree.range.0..token_tree.range.1,
                            "here"
                        )],
                        help = format!("Unexpected {ident}"),
                        "Unexpected Ident",
                    )
                    .with_source_code(self.whole.to_string()))
                }
                Atom::Super => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            token_tree.range.0..token_tree.range.1,
                            "here"
                        )],
                        "Unexpected this",
                    )
                    .with_source_code(self.whole.to_string()))
                }
                Atom::This => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            token_tree.range.0..token_tree.range.1,
                            "here"
                        )],
                        "Unexpected this",
                    )
                    .with_source_code(self.whole.to_string()))
                }
            },

            TokenTreeInner::Cons(op, operands) => match op {
                Op::Group => {
                    self.evaluate_token_tree(operands.first().expect("Group must not be empty."))?
                }

                Op::Minus if operands.len() == 1 => {
                    let rest = self.evaluate_token_tree(
                        operands.first().expect("- must be followed by a number."),
                    )?;
                    if let EvaluateResult::Number(num) = rest {
                        EvaluateResult::Number(-num)
                    } else {
                        return Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                token_tree.range.0..token_tree.range.1,
                                "here"
                            )],
                            "Operand must be a number",
                        )
                        .with_source_code(self.whole.to_string()));
                    }
                }

                Op::Bang => {
                    let rest = self.evaluate_token_tree(
                        operands.first().expect("! must be followed by a bool."),
                    )?;

                    match rest {
                        EvaluateResult::Bool(boolean) => EvaluateResult::Bool(!boolean),
                        EvaluateResult::Nil => EvaluateResult::Bool(!false),
                        _ => EvaluateResult::Bool(!true),
                    }
                }
                Op::Plus => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs)?;
                    let rhs_value = self.evaluate_token_tree(rhs)?;

                    match (lhs_value, rhs_value) {
                        (EvaluateResult::Number(left_num), EvaluateResult::Number(right_num)) => {
                            EvaluateResult::Number(left_num + right_num)
                        }

                        (
                            EvaluateResult::String(left_string),
                            EvaluateResult::String(right_string),
                        ) => EvaluateResult::String(left_string + right_string),

                        _ => {
                            return Err(miette::miette!(
                                labels = vec![
                                    LabeledSpan::at(lhs.range.0..lhs.range.1, "lhs"),
                                    LabeledSpan::at(rhs.range.0..rhs.range.1, "rhs")
                                ],
                                "{op} can only be used with two numbers or two strings",
                            )
                            .with_source_code(self.whole.to_string()));
                        }
                    }
                }

                Op::Minus | Op::Star | Op::Slash => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs)?;
                    let rhs_value = self.evaluate_token_tree(rhs)?;

                    if let EvaluateResult::Number(left_num) = lhs_value {
                        if let EvaluateResult::Number(right_num) = rhs_value {
                            let result_num = match op {
                                Op::Plus => left_num + right_num,
                                Op::Minus => left_num - right_num,
                                Op::Star => left_num * right_num,
                                Op::Slash => left_num / right_num,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::Number(result_num)
                        } else {
                            return Err(miette::miette!(
                                labels = vec![LabeledSpan::at(rhs.range.0..rhs.range.1, "here")],
                                "{op} can only be used with numbers",
                            )
                            .with_source_code(self.whole.to_string()));
                        }
                    } else {
                        return Err(miette::miette!(
                            labels = vec![LabeledSpan::at(lhs.range.0..lhs.range.1, "here")],
                            "{op} can only be used with numbers",
                        )
                        .with_source_code(self.whole.to_string()));
                    }
                }

                Op::Greater | Op::GreaterEqual | Op::Less | Op::LessEqual => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs)?;
                    let rhs_value = self.evaluate_token_tree(rhs)?;

                    match (lhs_value, rhs_value) {
                        (EvaluateResult::Number(l), EvaluateResult::Number(r)) => {
                            let result_value = match op {
                                Op::Greater => l > r,
                                Op::GreaterEqual => l >= r,
                                Op::Less => l < r,
                                Op::LessEqual => l <= r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::Bool(result_value)
                        }
                        _ => {
                            return Err(miette::miette!(
                                labels = vec![LabeledSpan::at(lhs.range.0..rhs.range.1, "here")],
                                "{op} can only be used with numbers",
                            )
                            .with_source_code(self.whole.to_string()));
                        }
                    }
                }

                Op::EqualEqual | Op::BangEqual => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs)?;
                    let rhs_value = self.evaluate_token_tree(rhs)?;

                    match (lhs_value, rhs_value) {
                        (EvaluateResult::Number(l), EvaluateResult::Number(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::Bool(result_value)
                        }
                        (EvaluateResult::String(l), EvaluateResult::String(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::Bool(result_value)
                        }

                        (EvaluateResult::Bool(l), EvaluateResult::Bool(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::Bool(result_value)
                        }

                        _ => EvaluateResult::Bool(false),
                    }
                }

                _ => {
                    todo!()
                }
            },

            _ => {
                todo!()
            }
        };

        Ok(evaluate_result)
    }
}
