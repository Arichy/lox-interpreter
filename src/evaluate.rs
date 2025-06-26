use core::fmt;
use std::borrow::Cow;

use miette::{Error, LabeledSpan};

use crate::{
    parse::{Atom, TokenTree, TokenTreeInner},
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
        let token_tree = self.parser.parse_expression()?;

        let evaluate_result = match token_tree.inner {
            TokenTreeInner::Atom(atom) => match atom {
                Atom::String(string) => EvaluateResult::String(string),
                Atom::Number(num) => EvaluateResult::Number(num),
                Atom::Nil => EvaluateResult::Nil,
                Atom::Bool(boolean) => EvaluateResult::Bool(boolean),
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
            _ => {
                todo!()
            }
        };

        Ok(evaluate_result)
    }
}
