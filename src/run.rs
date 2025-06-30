use std::collections::HashMap;

use miette::{miette, Error, LabeledSpan};

use crate::{
    evaluate::{EvaluateResult, EvaluateResultInner, Evaluator},
    parse::{Atom, Op, TokenTree, TokenTreeInner},
};
#[derive(Debug, Default)]
pub struct StackFrame<'de> {
    variables: HashMap<String, EvaluateResult<'de>>,
}

#[derive(Debug, Default)]
pub struct RuntimeState<'de> {
    stack: Vec<StackFrame<'de>>,
}

impl<'de> RuntimeState<'de> {
    fn new() -> Self {
        Self {
            stack: vec![StackFrame::default()],
        }
    }

    pub fn current_stack_frame(&self) -> &StackFrame<'de> {
        self.stack.last().expect("No stack frame")
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame<'de> {
        self.stack.last_mut().expect("No stack frame")
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&EvaluateResult<'de>> {
        self.current_stack_frame().variables.get(variable)
    }

    pub fn set_variable_value(&mut self, variable: &str, value: EvaluateResult<'de>) {
        self.current_stack_frame_mut()
            .variables
            .insert(variable.to_string(), value);
    }
}

pub struct Runner<'de> {
    evaluator: Evaluator<'de>,
    state: RuntimeState<'de>,
}

impl<'de> Runner<'de> {
    pub fn new(input: &'de str) -> Self {
        let evaluator = Evaluator::new(input);
        let state = RuntimeState::new();
        Self { evaluator, state }
    }

    pub fn run(mut self) -> Result<(), Error> {
        loop {
            let statement = self.evaluator.parser.next();
            match statement {
                Some(Ok(statement)) => {
                    self.run_statement(statement)?;
                }
                Some(Err(err)) => {
                    crate::log_stderr!("{err:?}");
                    std::process::exit(65);
                }
                None => break,
            }
        }

        Ok(())
    }

    pub fn run_statement(&mut self, statement: TokenTree<'de>) -> Result<(), Error> {
        match &statement {
            TokenTree { inner, range } => match inner {
                TokenTreeInner::Atom(atom) => {}
                TokenTreeInner::Cons(cons, operands) => match cons {
                    Op::Group
                    | Op::Minus
                    | Op::Bang
                    | Op::Plus
                    | Op::Star
                    | Op::Slash
                    | Op::Greater
                    | Op::GreaterEqual
                    | Op::Less
                    | Op::LessEqual
                    | Op::EqualEqual
                    | Op::BangEqual => {
                        // expression statement, just ignore
                        self.evaluator
                            .evaluate_token_tree(&statement, Some(&self.state))?;
                    }

                    Op::Print => {
                        let print_value = |value: &EvaluateResult<'de>| match &**value {
                            EvaluateResultInner::Bool(b) => {
                                crate::log_stdout!("{}", b);
                            }
                            EvaluateResultInner::Number(n) => {
                                crate::log_stdout!("{}", n);
                            }
                            EvaluateResultInner::String(s) => {
                                crate::log_stdout!("{}", s);
                            }
                            EvaluateResultInner::Nil => {
                                crate::log_stdout!("nil");
                            }
                        };

                        match operands.first() {
                            Some(operand) => match operand {
                                TokenTree {
                                    inner: TokenTreeInner::Atom(Atom::Ident(ident)),
                                    range,
                                } => match self.state.get_variable_value(ident) {
                                    Some(value) => {
                                        print_value(value);
                                    }
                                    None => {
                                        return Err(miette::miette!(
                                            labels =
                                                vec![LabeledSpan::at(range.0..range.1, "here")],
                                            help = format!("{ident} is not defined"),
                                            "Runtime error: variable not found"
                                        )
                                        .with_source_code(self.evaluator.whole.to_string()));
                                    }
                                },

                                TokenTree {
                                    inner: TokenTreeInner::Atom(Atom::Super),
                                    range,
                                } => {
                                    return Err(miette::miette!(
                                        labels = vec![LabeledSpan::at(range.0..range.1, "here")],
                                        help = format!("Unexpected {operand:?}"),
                                        "Runtime error"
                                    )
                                    .with_source_code(self.evaluator.whole.to_string()));
                                }

                                TokenTree {
                                    inner: TokenTreeInner::Atom(Atom::This),
                                    range,
                                } => {}

                                _ => {
                                    let value_to_print = self
                                        .evaluator
                                        .evaluate_token_tree(operand, Some(&self.state));

                                    match value_to_print {
                                        Ok(result) => {
                                            print_value(&result);
                                        }
                                        Err(e) => {
                                            return Err(e);
                                        }
                                    }
                                }
                            },
                            None => {}
                        }
                    }

                    Op::Return => {}

                    Op::Var => {
                        let variable_name = &operands[0];
                        let init = &operands[1];

                        match variable_name {
                            TokenTree {
                                inner: TokenTreeInner::Atom(Atom::Ident(ident)),
                                ..
                            } => {
                                let init = self
                                    .evaluator
                                    .evaluate_token_tree(init, Some(&self.state))?;

                                self.state.set_variable_value(&ident, init);
                            }

                            _ => unreachable!(),
                        }
                    }
                    _ => {}
                },

                TokenTreeInner::Call { callee, arguments } => {}

                TokenTreeInner::Fun {
                    name,
                    parameters,
                    body,
                } => {}

                TokenTreeInner::If { condition, yes, no } => {}

                TokenTreeInner::Eof => {}
            },
            _ => {}
        }
        Ok(())
    }
}
