use core::fmt;
use std::borrow::Cow;

use miette::{Error, LabeledSpan};

use crate::{
    parse::{Atom, Op, TokenTree, TokenTreeInner},
    Parser,
};

pub struct Evaluator<'de> {
    whole: &'de str,
    parser: Parser<'de>,
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

    fn evaluate_token_tree(
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

                Op::Minus => {
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
                            "- must be followed by a number",
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
