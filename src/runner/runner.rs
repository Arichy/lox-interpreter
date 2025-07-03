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
    is_loop: bool,
    should_break: bool,
    should_continue: bool,
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
        if self.check_should_continue() || self.check_should_break() {
            return Ok(());
        }

        // println!(
        //     "Running token tree: {:#?}. scopes: {:#?}",
        //     statement.inner,
        //     self.state.current_stack_frame().scopes()
        // );

        match &statement.inner {
            StatementInner::Block(block) => {
                self.state.current_stack_frame_mut().push_scope(false);
                for stmt in &block.statements {
                    self.run_statement(stmt)?;
                }
                self.state.current_stack_frame_mut().pop_scope();
            }
            StatementInner::Break => {
                let scopes = self.state.current_stack_frame_mut().scopes_mut();

                for scope in scopes.iter_mut().rev() {
                    if scope.is_loop {
                        // println!("set break, scopes: {:?}", scope);
                        // println!("before set: scopes: {:?}", );
                        // Set the should_break flag to true for the innermost loop scope
                        scope.should_break = true;
                        // println!("after set break, scopes: {:?}", scopes);

                        return Ok(());
                    }
                }

                return Err(error::RuntimeError::BreakOrContinueOutsideLoop {
                    src: self.whole().to_string(),
                    cause: "break".to_string(),
                    err_span: (statement.range.0..statement.range.1).into(),
                }
                .into());
            }
            StatementInner::Continue => {
                let scopes = self.state.current_stack_frame().scopes();

                for scope in scopes.iter().rev() {
                    if scope.is_loop {
                        return Ok(());
                    }
                }

                return Err(error::RuntimeError::BreakOrContinueOutsideLoop {
                    src: self.whole().to_string(),
                    cause: "continue".to_string(),
                    err_span: (statement.range.0..statement.range.1).into(),
                }
                .into());
            }
            StatementInner::Declaration(decl) => match &decl.inner {
                DeclarationInner::Variable(variable_declaration) => {
                    let variable_name = variable_declaration.id.to_string();
                    let init = match &variable_declaration.init {
                        Some(init) => self.evaluator.evaluate_expression(init, &mut self.state)?,
                        None => {
                            // If no initializer is provided, we set the variable to nil
                            Value::new_nil()
                        }
                    };

                    self.state.new_variable(variable_name, init);
                }
                DeclarationInner::Function(func_declaration) => {
                    let function_name = func_declaration.name.to_string();
                    let function_value = Value::new_function(
                        function_name.clone(),
                        func_declaration.parameters.clone(),
                        *func_declaration.body.clone(),
                    );

                    // Register the function in the current scope
                    self.state
                        .current_stack_frame_mut()
                        .current_scope_mut()
                        .set_variable_value(function_name, function_value);
                }

                _ => {
                    todo!("function and class declarations are not yet implemented");
                }
            },

            StatementInner::Expression(expr) => {
                self.evaluator
                    .evaluate_expression(expr, &mut self.state)
                    .map_err(|e| error::RuntimeError::BadOperandError {
                        src: self.whole().to_string(),
                        operator: "expression".to_string(),
                        reason: e.to_string(),
                        err_span: (statement.range.0..statement.range.1).into(),
                    })?;
            }

            StatementInner::For(for_statement) => {
                self.state.current_stack_frame_mut().push_scope(true);

                if let Some(init) = &for_statement.init {
                    self.run_for_init(&init)?;
                }

                match &for_statement.body.inner {
                    StatementInner::Block(block) => {
                        while for_statement
                            .test
                            .as_ref()
                            .map(|c| self.check_loop_condition(c))
                            .unwrap_or(Ok(true))?
                        {
                            /// @XXX: a new scope is created for each iteration
                            self.state.current_stack_frame_mut().push_scope(false);
                            for statement in &block.statements {
                                self.run_statement(statement)?;
                            }
                            self.state.current_stack_frame_mut().pop_scope();

                            if let Some(update) = &for_statement.update {
                                self.run_expression(update)?;
                            }
                        }
                    }

                    single_statement => {
                        while for_statement
                            .test
                            .as_ref()
                            .map(|c| self.check_loop_condition(c))
                            .unwrap_or(Ok(true))?
                        {
                            self.run_statement(&for_statement.body)?;

                            if let Some(update) = &for_statement.update {
                                self.run_expression(update)?;
                            }
                        }
                    }
                }

                self.state.current_stack_frame_mut().pop_scope();
            }

            StatementInner::If(if_statement) => {
                let condition_boolean = self
                    .evaluator
                    .evaluate_expression(&if_statement.test, &mut self.state)?
                    .boolean();

                if condition_boolean {
                    self.run_statement(&if_statement.consequent)?;
                } else {
                    // Run the 'no' branch if it exists
                    if let Some(alternate) = &if_statement.alternate {
                        self.run_statement(alternate)?;
                    }
                }
            }

            StatementInner::Print(expr) => {
                let value = self.run_expression(expr)?;
                log_stdout!("{value}");
            }

            StatementInner::While(while_statement) => {
                self.state.current_stack_frame_mut().push_scope(true);

                match &while_statement.body.inner {
                    StatementInner::Block(block) => {
                        while self.check_loop_condition(&while_statement.test)? {
                            /// @XXX: a new scope is created for each iteration
                            self.state.current_stack_frame_mut().push_scope(false);
                            for statement in &block.statements {
                                self.run_statement(statement)?;
                            }
                            self.state.current_stack_frame_mut().pop_scope();
                        }
                    }

                    single_statement => {
                        while self.check_loop_condition(&while_statement.test)? {
                            self.run_statement(&while_statement.body)?;
                        }
                    }
                }

                self.state.current_stack_frame_mut().pop_scope();
            }

            _ => {
                todo!()
            }
        }

        Ok(())
    }

    fn run_expression(&mut self, expression: &Expression<'de>) -> Result<Value<'de>, Error> {
        Ok(self
            .evaluator
            .evaluate_expression(expression, &mut self.state)
            .map_err(|e| error::RuntimeError::BadOperandError {
                src: self.whole().to_string(),
                operator: "expression".to_string(),
                reason: e.to_string(),
                err_span: (expression.range.0..expression.range.1).into(),
            })?)
    }

    fn run_for_init(&mut self, init: &ForInit<'de>) -> Result<(), Error> {
        match &init.inner {
            ForInitInner::VariableDeclaration(variable_declaration) => {
                let variable_name = variable_declaration.id.to_string();
                let init_value = match &variable_declaration.init {
                    Some(init) => self.evaluator.evaluate_expression(init, &mut self.state)?,
                    None => Value::new_nil(),
                };

                self.state.new_variable(variable_name, init_value);
            }
            ForInitInner::Expression(expr) => {
                self.evaluator
                    .evaluate_expression(expr, &mut self.state)
                    .map_err(|e| error::RuntimeError::BadOperandError {
                        src: self.whole().to_string(),
                        operator: "for init expression".to_string(),
                        reason: e.to_string(),
                        err_span: (init.range.0..init.range.1).into(),
                    })?;
            }
        }

        Ok(())
    }

    fn check_should_continue(&self) -> bool {
        self.state
            .current_stack_frame()
            .nearest_enclosing_loop_scope()
            .map_or(false, |scope| scope.should_continue)
    }

    fn check_should_break(&self) -> bool {
        self.state
            .current_stack_frame()
            .nearest_enclosing_loop_scope()
            .map_or(false, |scope| scope.should_break)
    }

    fn check_loop_condition(&mut self, cond: &Expression<'de>) -> Result<bool, Error> {
        Ok(self
            .evaluator
            .evaluate_expression(cond, &mut self.state)?
            .boolean()
            && !self.check_should_break())
    }
}
