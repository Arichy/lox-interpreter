use miette::{miette, Error, LabeledSpan};

use crate::{
    evaluate::{EvaluateResult, Evaluator},
    parse::{Op, TokenTree, TokenTreeInner},
};

pub struct Runner<'de> {
    evaluator: Evaluator<'de>,
}

impl<'de> Runner<'de> {
    pub fn new(input: &'de str) -> Self {
        let evaluator = Evaluator::new(input);
        Self { evaluator }
    }

    pub fn run(mut self) -> Result<(), Error> {
        match self.evaluator.parser.parse() {
            Ok(statements) => {
                for statement in statements {
                    self.run_statement(statement)?;
                }

                Ok(())
            }
            Err(err) => return Err(err),
        }
    }

    pub fn run_statement(&mut self, statement: TokenTree<'de>) -> Result<(), Error> {
        match statement {
            TokenTree { inner, range } => match inner {
                TokenTreeInner::Atom(atom) => {}
                TokenTreeInner::Cons(cons, operands) => match cons {
                    Op::Print => match operands.first() {
                        Some(operand) => {
                            let value_to_print = self.evaluator.evaluate_token_tree(operand);
                            match value_to_print {
                                Ok(result) => match result {
                                    EvaluateResult::Bool(b) => {
                                        println!("{}", b);
                                    }
                                    EvaluateResult::Number(n) => {
                                        println!("{}", n);
                                    }
                                    EvaluateResult::String(s) => {
                                        println!("{}", s);
                                    }
                                    EvaluateResult::Nil => {
                                        println!("nil");
                                    }
                                },
                                Err(e) => {
                                    return Err(e);
                                }
                            }
                        }
                        None => {}
                    },
                    Op::Return => {}
                    _ => {}
                },

                TokenTreeInner::Call { callee, arguments } => {}

                TokenTreeInner::Fun {
                    name,
                    parameters,
                    body,
                } => {}

                TokenTreeInner::If { condition, yes, no } => {}
            },
            _ => {}
        }
        Ok(())
    }
}
