use std::{
    borrow::Cow,
    cell::RefCell,
    env,
    fmt::format,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use rustc_hash::FxHashMap as HashMap;

use crate::{
    evaluator::ClosureBindingEnv,
    log_stderr,
    runner::cache::{CacheKey, CallCache},
};

use chrono::Utc;
use miette::{miette, Error, LabeledSpan, WrapErr};

use crate::{
    ast::{
        Declaration, DeclarationInner, Expression, ForInit, ForInitInner, Statement, StatementInner,
    },
    error,
    evaluator::{Evaluator, Value, ValueInner},
    log_stdout,
    runner::{
        cache,
        global::{self, console},
    },
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EnvironmentType {
    Global,
    Module,
    Enclosed,
}

pub type Bindings<'de> = HashMap<String, Rc<RefCell<Value<'de>>>>;

#[derive(Debug)]
pub struct EnvironmentInner<'de> {
    env_type: EnvironmentType, // Unique identifier for the environment
    pub bindings: RefCell<Bindings<'de>>,
    pub parent: Option<Environment<'de>>,
}

impl<'de> EnvironmentInner<'de> {
    pub fn define(&self, name: String, value: Value<'de>) {
        self.bindings
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
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
            return Some((*value.borrow()).clone());
        }

        if let Some(parent) = &self.parent {
            return parent.get(name);
        }

        None
    }

    pub fn assign(&self, name: &str, value: Value<'de>) -> bool {
        // if self.bindings.borrow().contains_key(name) {
        //     // self.bindings.borrow_mut().insert(name.to_string(), value);
        //     let var = self.bindings.borrow_mut().get(name);
        //     return true;
        // }

        if let Some(binding_value_ptr) = self.bindings.borrow().get(name) {
            *binding_value_ptr.borrow_mut() = value;
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
            env_type: EnvironmentType::Global,
            bindings: RefCell::new(HashMap::default()),
            parent: None,
        }))
    }

    pub fn new_module(global: Environment<'de>) -> Self {
        Self(Rc::new(EnvironmentInner {
            env_type: EnvironmentType::Module,
            bindings: RefCell::new(HashMap::default()),
            parent: Some(global),
        }))
    }

    pub fn new_enclosed(&self) -> Self {
        Self(Rc::new(EnvironmentInner {
            env_type: EnvironmentType::Enclosed,
            bindings: RefCell::new(HashMap::default()),
            parent: Some(self.clone()),
        }))
    }

    pub fn is_global(&self) -> bool {
        self.0.env_type == EnvironmentType::Global
    }

    pub fn is_module(&self) -> bool {
        self.0.env_type == EnvironmentType::Module
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
    // pub closure_binding_env: ClosureBindingEnv<'de>,
    pub closure_bindings: Bindings<'de>,

    // When js_this is enabled, we will store `this` on stack frame to dynamically bind `this` to callee object.
    // We also need a `captured_this_value` to handle closure which captures `this` in a method.
    // In default behavior when the feature is disabled, we could capture `this` in local bindings because `this` could be considered as a special variable.
    // But in this feature, we cannot do it because `this` cannot be considered as a variable, so we need this ad-hoc field to store `this`.
    #[cfg(feature = "js_this")]
    pub this_value: Option<Value<'de>>,
    #[cfg(feature = "js_this")]
    pub captured_this_value: Option<Value<'de>>,

    pub function_value: Option<Value<'de>>,
}

impl<'de> StackFrame<'de> {
    pub fn new(
        env_before_call: Environment<'de>,
        // closure_binding_env: ClosureBindingEnv<'de>,
        closure_bindings: Bindings<'de>,
        function_value: Option<Value<'de>>,
    ) -> Self {
        Self {
            env_before_call,
            return_value: None,
            // closure_binding_env,
            closure_bindings,
            function_value,
            #[cfg(feature = "js_this")]
            this_value: None,
            #[cfg(feature = "js_this")]
            captured_this_value: None,
        }
    }
}

#[derive(Debug)]
pub struct Vm<'de> {
    pub global: Environment<'de>,
    pub current_env: Environment<'de>,
    call_stack: Vec<StackFrame<'de>>,
    loop_context_stack: Vec<LoopContext>,
    // cache for pure functions
    pub pure_function_call_cache: CallCache<'de>,
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
            pure_function_call_cache: HashMap::default(),
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

    pub fn enter_module(&mut self) {
        // Create a new module environment
        let new_env = Environment::new_module(self.global.clone());
        // Set the current environment to the new module environment
        self.current_env = new_env;
    }

    pub fn leave_module(&mut self) -> Result<(), Error> {
        // Check if we are in a module environment
        if self.current_env.env_type == EnvironmentType::Module {
            // If so, revert to the global environment
            self.current_env = self.global.clone();
            Ok(())
        } else {
            Err(error::RuntimeError::InternalError {
                message: "Cannot leave non-module environment".to_string(),
            }
            .into())
        }
    }

    pub fn enter_scope(&mut self) {
        let new_env = self.current_env.new_enclosed();
        self.current_env = new_env;
    }

    pub fn leave_scope(&mut self) -> Result<(), Error> {
        if let Some(parent) = &self.current_env.parent {
            self.current_env = parent.clone();
            Ok(())
        } else {
            Err(error::RuntimeError::InternalError {
                message: "Cannot leave scope, no parent environment found".to_string(),
            }
            .into())
        }
    }

    pub fn enter_function(&mut self, closure_bindings: Bindings<'de>, function_value: Value<'de>) {
        // 1. push a new stack frame with storing current environment
        let env_before_call = self.current_env.clone();
        let new_stack_frame =
            StackFrame::new(env_before_call, closure_bindings, Some(function_value));
        self.call_stack.push(new_stack_frame);

        // 2. create a new environment for the function call
        // let call_env = if let Some(closure_env) = closure_env {
        //     closure_env.new_enclosed()
        // } else {
        //     self.current_env.new_enclosed()
        // };
        let call_env = self.current_env.new_enclosed();

        self.current_env = call_env;
    }

    pub fn leave_function(&mut self) -> Result<(), Error> {
        let current_stack_frame = self.current_stack_frame().wrap_err("leaving function")?;
        // Restore the environment before the function call
        self.current_env = current_stack_frame.env_before_call.clone();
        self.call_stack.pop();

        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Option<Value<'de>> {
        let Ok(current_stack_frame) = self.current_stack_frame() else {
            return self.current_env.get(name);
        };

        // println!(
        //     "Looking for variable: {}, current_stack_frame closure bindings: {:?}",
        //     name,
        //     current_stack_frame.closure_binding_env.keys()
        // );

        // 1. check on current closure bindings
        if let Some(closure_binding_ptr) = current_stack_frame.closure_bindings.get(name) {
            // if the variable is found in the closure bindings, try to get it in the parent environment
            return Some((*closure_binding_ptr.borrow()).clone());
        }

        // 2. check if it's recursive closure function itself
        for stack_frame in self.call_stack.iter().rev() {
            if let Some(func_value) = &stack_frame.function_value {
                let ValueInner::Function(func) = &*func_value.inner else {
                    unreachable!()
                };
                if func.name == name {
                    return Some(func_value.clone());
                }
            }
        }

        self.current_env.get(name)
    }

    pub fn define_variable(&mut self, name: String, value: Value<'de>) {
        self.current_env.define(name, value);
    }

    pub fn assign_variable(&mut self, name: &str, value: Value<'de>) -> bool {
        // Check the current stack frame's closure bindings first
        let Ok(current_stack_frame) = self.current_stack_frame() else {
            return self.current_env.assign(name, value);
        };

        if let Some(closure_binding_ptr) = current_stack_frame.closure_bindings.get(name) {
            // return closure_env.assign(name, value);
            (*closure_binding_ptr.borrow_mut()) = value;
            return true;
        }

        self.current_env.assign(name, value)
    }

    pub fn current_stack_frame(&self) -> Result<&StackFrame<'de>, Error> {
        self.call_stack.last().ok_or_else(|| {
            miette!(error::RuntimeError::InternalError {
                message: "No current stack frame found".to_string()
            })
        })
    }

    pub fn current_stack_frame_mut(&mut self) -> Result<&mut StackFrame<'de>, Error> {
        self.call_stack.last_mut().ok_or_else(|| {
            miette!(error::RuntimeError::InternalError {
                message: "No current stack frame found".to_string()
            })
        })
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
        let program = match self.evaluator.parser.parse() {
            Ok(program) => program,
            Err(e) => {
                log_stderr!("{e:?}");
                std::process::exit(65);
            }
        };

        self.vm.call_stack.push(StackFrame::new(
            self.vm.current_env.clone(),
            HashMap::default(),
            None,
        ));

        self.vm.enter_module();

        for statement in &program.body {
            self.run_statement(&statement)?;
        }

        self.vm.leave_module();

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
