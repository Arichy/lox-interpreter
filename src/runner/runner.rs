use std::{borrow::Cow, collections::HashMap};

use chrono::Utc;
use miette::{miette, Error, LabeledSpan};

use crate::{
    ast::{
        Declaration, DeclarationInner, Expression, ForInit, ForInitInner, Statement, StatementInner,
    },
    error,
    evaluator::{Evaluator, Value, ValueInner},
    log_stdout,
    runner::global::{self, console},
};

#[derive(Debug, Default)]
pub struct Scope<'de> {
    pub is_loop: bool,
    pub should_break: bool,
    pub should_continue: bool,
    variables: HashMap<String, Value<'de>>,
}

impl<'de> Scope<'de> {
    pub fn new(is_loop: bool) -> Self {
        Self {
            is_loop,
            should_break: false,
            should_continue: false,
            variables: HashMap::new(),
        }
    }

    pub fn is_variable_defined(&self, variable: &str) -> bool {
        self.variables.contains_key(variable)
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&Value<'de>> {
        self.variables.get(variable)
    }

    pub fn set_variable_value(&mut self, variable: String, value: Value<'de>) {
        self.variables.insert(variable, value);
    }

    pub fn get_variable_value_mut(&mut self, variable: &str) -> Option<&mut Value<'de>> {
        self.variables.get_mut(variable)
    }
}

#[derive(Debug, Default)]
pub struct StackFrame<'de> {
    scopes: Vec<Scope<'de>>,
}

impl<'de> StackFrame<'de> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(false)],
        }
    }

    pub fn scopes(&self) -> &[Scope<'de>] {
        &self.scopes
    }

    pub fn scopes_mut(&mut self) -> &mut Vec<Scope<'de>> {
        &mut self.scopes
    }

    pub fn current_scope(&self) -> &Scope<'de> {
        self.scopes.last().expect("No scope in stack frame")
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope<'de> {
        self.scopes.last_mut().expect("No scope in stack frame")
    }

    pub fn push_scope(&mut self, is_loop: bool) {
        self.scopes.push(Scope::new(is_loop));
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn nearest_enclosing_loop_scope(&self) -> Option<&Scope<'de>> {
        self.scopes.iter().rev().find(|scope| scope.is_loop)
    }
}

#[derive(Debug, Default)]
pub struct RuntimeState<'de> {
    stack: Vec<StackFrame<'de>>,
}

impl<'de> RuntimeState<'de> {
    pub fn new() -> Self {
        Self {
            stack: vec![StackFrame::new()],
        }
    }

    pub fn push_stack_frame(&mut self) {
        self.stack.push(StackFrame::new());
    }

    pub fn pop_stack_frame(&mut self) {
        if self.stack.len() > 1 {
            self.stack.pop();
        } else {
            panic!("Cannot pop the last stack frame");
        }
    }

    pub fn current_stack_frame(&self) -> &StackFrame<'de> {
        self.stack.last().expect("No stack frame")
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame<'de> {
        self.stack.last_mut().expect("No stack frame")
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&Value<'de>> {
        let current_stack_frame = self.current_stack_frame();
        for scope in current_stack_frame.scopes().iter().rev() {
            if let Some(value) = scope.get_variable_value(variable) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_variable_value_mut(&mut self, variable: &str) -> Option<&mut Value<'de>> {
        let current_stack_frame = self.current_stack_frame_mut();
        for scope in current_stack_frame.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_variable_value_mut(variable) {
                return Some(value);
            }
        }
        None
    }

    pub fn new_variable(&mut self, variable: String, value: Value<'de>) {
        self.current_stack_frame_mut()
            .current_scope_mut()
            .set_variable_value(variable, value);
    }

    fn set_global_variable(&mut self, variable: String, value: Value<'de>) {
        if let Some(scope) = self.stack.first_mut() {
            scope
                .current_scope_mut()
                .set_variable_value(variable, value);
        } else {
            // If no stack frame exists, create one
            let mut new_frame = StackFrame::new();
            new_frame
                .current_scope_mut()
                .set_variable_value(variable, value);
            self.stack.push(new_frame);
        }
    }

    fn init_global(&mut self) {
        self.set_global_variable(
            "hi".to_string(),
            Value::new_string(Cow::Owned("Hello, World!".to_string())),
        );

        self.set_global_variable(
            "clock".to_string(),
            Value::new_native_function("clock".to_string(), global::clock),
        );

        let console = global::Console::new();
        self.set_global_variable("console".to_string(), console);
    }
}

pub struct Runner<'de> {
    evaluator: Evaluator<'de>,
    state: RuntimeState<'de>,
}

impl<'de> Runner<'de> {
    pub fn new(input: &'de str) -> Self {
        let evaluator = Evaluator::new(input);
        let mut state = RuntimeState::new();
        state.init_global();
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

    pub fn run_statement(&mut self, statement: &Statement<'de>) -> Result<(), Error> {
        self.evaluator.run_statement(statement, &mut self.state)
    }

    fn run_expression(&mut self, expression: &Expression<'de>) -> Result<Value<'de>, Error> {
        self.evaluator
            .evaluate_expression(expression, &mut self.state)
    }
}
