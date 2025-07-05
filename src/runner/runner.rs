use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    env,
    fmt::format,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::evaluator::ClosureBindingEnv;

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

#[derive(Debug)]
pub struct EnvironmentInner<'de> {
    pub bindings: RefCell<HashMap<String, Value<'de>>>,
    pub parent: Option<Environment<'de>>,
}

impl<'de> EnvironmentInner<'de> {
    pub fn define(&self, name: String, value: Value<'de>) {
        self.bindings.borrow_mut().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value<'de>> {
        // println!(
        //     "Looking for variable: {}, current env bindings: {}",
        //     name,
        //     self.bindings
        //         .borrow()
        //         .keys()
        //         .cloned()
        //         .collect::<Vec<_>>()
        //         .join(", ")
        // );
        if let Some(value) = self.bindings.borrow().get(name) {
            return Some(value.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get(name);
        }

        None
    }

    pub fn assign(&self, name: &str, value: Value<'de>) -> bool {
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.assign(name, value);
        }

        false
    }
}

#[derive(Debug, Clone)]
pub struct Environment<'de>(pub Rc<EnvironmentInner<'de>>);

impl<'de> Environment<'de> {
    pub fn new_global() -> Self {
        Self(Rc::new(EnvironmentInner {
            bindings: RefCell::new(HashMap::new()),
            parent: None,
        }))
    }

    pub fn new_enclosed(&self) -> Self {
        Self(Rc::new(EnvironmentInner {
            bindings: RefCell::new(HashMap::new()),
            parent: Some(self.clone()),
        }))
    }
}

impl<'de> Deref for Environment<'de> {
    type Target = EnvironmentInner<'de>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Default)]
pub struct LoopContext {
    pub should_break: bool,
    pub should_continue: bool,
}

#[derive(Debug)]
pub struct StackFrame<'de> {
    pub return_value: Option<Value<'de>>,
    pub env_before_call: Environment<'de>,
    pub closure_binding_env: ClosureBindingEnv<'de>,
    pub closure_env: Option<Environment<'de>>,
}

impl<'de> StackFrame<'de> {
    pub fn new(
        env_before_call: Environment<'de>,
        closure_binding_env: ClosureBindingEnv<'de>,
        closure_env: Option<Environment<'de>>,
    ) -> Self {
        Self {
            env_before_call,
            return_value: None,
            closure_binding_env,
            closure_env,
        }
    }
}

#[derive(Debug)]
pub struct Vm<'de> {
    pub global: Environment<'de>,
    pub current_env: Environment<'de>,
    call_stack: Vec<StackFrame<'de>>,
    loop_context_stack: Vec<LoopContext>,
}

impl<'de> Vm<'de> {
    pub fn new() -> Self {
        let global = Environment::new_global();
        let current_env = global.clone();

        let mut vm = Self {
            global,
            current_env,
            call_stack: vec![],
            loop_context_stack: vec![],
        };

        vm.init_global();

        vm
    }

    fn init_global(&mut self) {
        self.global.define(
            "hi".to_string(),
            Value::new_string(Cow::Owned("Hello, World!".to_string())),
        );

        self.global.define(
            "clock".to_string(),
            Value::new_native_function("clock".to_string(), global::clock),
        );

        let console = global::Console::new();
        self.global.define("console".to_string(), console);
    }

    pub fn enter_scope(&mut self) {
        let new_env = self.current_env.new_enclosed();
        self.current_env = new_env;
    }

    pub fn leave_scope(&mut self) {
        if let Some(parent) = &self.current_env.parent {
            self.current_env = parent.clone();
        } else {
            panic!("Cannot leave global scope");
        }
    }

    pub fn enter_function(
        &mut self,
        closure_env: Option<Environment<'de>>,
        closure_binding_env: ClosureBindingEnv<'de>,
    ) {
        // 1. push a new stack frame with storing current environment
        let env_before_call = self.current_env.clone();
        let new_stack_frame =
            StackFrame::new(env_before_call, closure_binding_env, closure_env.clone());
        self.call_stack.push(new_stack_frame);

        // 2. create a new environment for the function call
        let call_env = if let Some(closure_env) = closure_env {
            closure_env.new_enclosed()
        } else {
            self.current_env.new_enclosed()
        };

        self.current_env = call_env;
    }

    pub fn leave_function(&mut self) -> Result<(), Error> {
        if let Some(current_stack_frame) = self.current_stack_frame() {
            // Restore the environment before the function call
            self.current_env = current_stack_frame.env_before_call.clone();
            // Pop the stack frame
            self.call_stack.pop();
            Ok(())
        } else {
            Err(error::RuntimeError::InternalError {
                message: format!("Cannot leave function: no current stack frame found"),
            }
            .into())
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<Value<'de>> {
        let current_stack_frame = self.current_stack_frame()?;

        // println!(
        //     "Looking for variable: {}, current_stack_frame closure bindings: {:?}",
        //     name,
        //     current_stack_frame.closure_binding_env.keys()
        // );
        if let Some(closure_env) = current_stack_frame.closure_binding_env.get(name) {
            // if the variable is found in the closure bindings, try to get it in the parent environment
            closure_env.get(name)
        } else {
            self.current_env.get(name)
        }
    }

    pub fn define_variable(&mut self, name: String, value: Value<'de>) {
        self.current_env.define(name, value);
    }

    pub fn assign_variable(&mut self, name: &str, value: Value<'de>) -> bool {
        // Check the current stack frame's closure bindings first
        let Some(current_stack_frame) = self.current_stack_frame() else {
            return self.current_env.assign(name, value);
        };

        if let Some(closure_env) = current_stack_frame.closure_binding_env.get(name) {
            return closure_env.assign(name, value);
        }

        self.current_env.assign(name, value)
    }

    pub fn current_stack_frame(&self) -> Option<&StackFrame<'de>> {
        self.call_stack.last()
    }

    pub fn current_stack_frame_mut(&mut self) -> Option<&mut StackFrame<'de>> {
        self.call_stack.last_mut()
    }

    pub fn enter_loop(&mut self) {
        self.loop_context_stack.push(LoopContext::default());
    }

    pub fn leave_loop(&mut self) {
        self.loop_context_stack
            .pop()
            .expect("Not in a loop context.");
    }

    pub fn current_loop_context_mut(&mut self) -> Option<&mut LoopContext> {
        self.loop_context_stack.last_mut()
    }

    pub fn signal_break(&mut self) -> bool {
        if let Some(loop_ctx) = self.current_loop_context_mut() {
            loop_ctx.should_break = true;
            true
        } else {
            false
        }
    }

    pub fn signal_continue(&mut self) -> bool {
        if let Some(loop_ctx) = self.current_loop_context_mut() {
            loop_ctx.should_continue = true;
            true
        } else {
            false
        }
    }

    pub fn current_loop_context(&self) -> Option<&LoopContext> {
        self.loop_context_stack.last()
    }
}

pub struct Runner<'de> {
    evaluator: Evaluator<'de>,
    vm: Vm<'de>,
}

impl<'de> Runner<'de> {
    pub fn new(input: &'de str) -> Self {
        let evaluator = Evaluator::new(input);
        let mut vm = Vm::new();

        Self { evaluator, vm }
    }

    fn whole(&self) -> &str {
        self.evaluator.whole
    }

    pub fn run(mut self) -> Result<(), Error> {
        self.vm.call_stack.push(StackFrame::new(
            self.vm.current_env.clone(),
            HashMap::new(),
            None,
        ));

        self.vm.enter_scope();
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
        self.vm.leave_scope();

        self.vm.call_stack.pop().ok_or_else(|| {
            miette!(error::RuntimeError::InternalError {
                message: "Cannot pop from an empty call stack".to_string()
            })
        })?;

        Ok(())
    }

    pub fn run_statement(&mut self, statement: &Statement<'de>) -> Result<(), Error> {
        self.evaluator.run_statement(statement, &mut self.vm)
    }

    fn run_expression(&mut self, expression: &Expression<'de>) -> Result<Value<'de>, Error> {
        self.evaluator.evaluate_expression(expression, &mut self.vm)
    }
}
