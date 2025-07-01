use std::collections::HashMap;

use miette::{miette, Error, LabeledSpan};

use crate::{
    error,
    evaluate::{EvaluateResult, EvaluateResultInner, Evaluator},
    parse::{Atom, Op, TokenTree, TokenTreeInner},
};

#[derive(Debug, Default)]
pub struct Scope<'de> {
    variables: HashMap<String, EvaluateResult<'de>>,
}

impl<'de> Scope<'de> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn is_variable_defined(&self, variable: &str) -> bool {
        self.variables.contains_key(variable)
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&EvaluateResult<'de>> {
        self.variables.get(variable)
    }

    pub fn set_variable_value(&mut self, variable: String, value: EvaluateResult<'de>) {
        self.variables.insert(variable, value);
    }
}

#[derive(Debug, Default)]
pub struct StackFrame<'de> {
    scopes: Vec<Scope<'de>>,
}

impl<'de> StackFrame<'de> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    pub fn current_scope(&self) -> &Scope<'de> {
        self.scopes.last().expect("No scope in stack frame")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope<'de> {
        self.scopes.last_mut().expect("No scope in stack frame")
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Debug, Default)]
pub struct RuntimeState<'de> {
    stack: Vec<StackFrame<'de>>,
}

impl<'de> RuntimeState<'de> {
    fn new() -> Self {
        Self {
            stack: vec![StackFrame::new()],
        }
    }

    pub fn current_stack_frame(&self) -> &StackFrame<'de> {
        self.stack.last().expect("No stack frame")
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame<'de> {
        self.stack.last_mut().expect("No stack frame")
    }

    pub fn is_variable_defined(&self, variable: &str) -> bool {
        self.current_stack_frame()
            .current_scope()
            .is_variable_defined(variable)
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&EvaluateResult<'de>> {
        self.current_stack_frame()
            .current_scope()
            .get_variable_value(variable)
    }

    pub fn set_variable_value(&mut self, variable: &str, value: EvaluateResult<'de>) {
        self.current_stack_frame_mut()
            .current_scope_mut()
            .set_variable_value(variable.to_string(), value);
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

    fn whole(&self) -> &str {
        self.evaluator.whole
    }

    pub fn run(mut self) -> Result<(), Error> {
        loop {
            let statement = self.evaluator.parser.next();
            match statement {
                Some(Ok(statement)) => {
                    self.run_statement(&statement)?;
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

    pub fn run_statement(&mut self, statement: &TokenTree<'de>) -> Result<(), Error> {
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
                    | Op::BangEqual
                    | Op::Equal => {
                        // expression statement, just ignore
                        self.evaluator
                            .evaluate_token_tree(&statement, Some(&mut self.state))?;
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
                                        return Err(error::RuntimeError::ReferenceError {
                                            src: self.whole().to_string(),
                                            ident: ident.to_string(),
                                            err_span: (range.0..range.1).into(),
                                        }
                                        .into());
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
                                    .with_source_code(self.whole().to_string()));
                                }

                                TokenTree {
                                    inner: TokenTreeInner::Atom(Atom::This),
                                    range,
                                } => {}

                                _ => {
                                    let value_to_print = self
                                        .evaluator
                                        .evaluate_token_tree(operand, Some(&mut self.state));

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
                        let init = operands.get(1);

                        match variable_name {
                            TokenTree {
                                inner: TokenTreeInner::Atom(Atom::Ident(ident)),
                                ..
                            } => {
                                let init = if let Some(init) = init {
                                    self.evaluator
                                        .evaluate_token_tree(init, Some(&mut self.state))?
                                } else {
                                    EvaluateResult::new_nil()
                                };

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

                TokenTreeInner::Block { statements } => {
                    // Create a new scope for the block
                    self.state.current_stack_frame_mut().push_scope();

                    // Run each statement in the block
                    for statement in statements {
                        self.run_statement(statement)?;
                    }

                    // Pop the scope after executing the block
                    self.state.current_stack_frame_mut().pop_scope();
                }
                TokenTreeInner::Eof => {}
            },
            _ => {}
        }
        Ok(())
    }
}
