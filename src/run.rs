use std::collections::HashMap;

use miette::{miette, Error, LabeledSpan};

use crate::{
    error,
    evaluate::{EvaluateResult, EvaluateResultInner, Evaluator},
    parse::{Atom, Op, TokenTree, TokenTreeInner},
};

#[derive(Debug, Default)]
pub struct Scope<'de> {
    is_loop: bool,
    should_break: bool,
    should_continue: bool,
    variables: HashMap<String, EvaluateResult<'de>>,
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

    pub fn get_variable_value(&self, variable: &str) -> Option<&EvaluateResult<'de>> {
        self.variables.get(variable)
    }

    pub fn set_variable_value(&mut self, variable: String, value: EvaluateResult<'de>) {
        self.variables.insert(variable, value);
    }

    pub fn get_variable_value_mut(&mut self, variable: &str) -> Option<&mut EvaluateResult<'de>> {
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

    pub fn current_stack_frame(&self) -> &StackFrame<'de> {
        self.stack.last().expect("No stack frame")
    }

    pub fn current_stack_frame_mut(&mut self) -> &mut StackFrame<'de> {
        self.stack.last_mut().expect("No stack frame")
    }

    pub fn get_variable_value(&self, variable: &str) -> Option<&EvaluateResult<'de>> {
        let current_stack_frame = self.current_stack_frame();
        for scope in current_stack_frame.scopes().iter().rev() {
            if let Some(value) = scope.get_variable_value(variable) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_variable_value_mut(&mut self, variable: &str) -> Option<&mut EvaluateResult<'de>> {
        let current_stack_frame = self.current_stack_frame_mut();
        for scope in current_stack_frame.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_variable_value_mut(variable) {
                return Some(value);
            }
        }
        None
    }

    pub fn new_variable(&mut self, variable: String, value: EvaluateResult<'de>) {
        self.current_stack_frame_mut()
            .current_scope_mut()
            .set_variable_value(variable, value);
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
            let token_tree = self.evaluator.parser.next();
            match token_tree {
                Some(Ok(token_tree)) => {
                    self.run_token_tree(&token_tree)?;
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

    pub fn run_token_tree(&mut self, statement: &TokenTree<'de>) -> Result<(), Error> {
        if self.check_should_continue() || self.check_should_break() {
            return Ok(());
        }

        // println!(
        //     "Running token tree: {:#?}. scopes: {:#?}",
        //     statement.inner,
        //     self.state.current_stack_frame().scopes()
        // );

        match &statement.inner {
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
                | Op::And
                | Op::Or
                | Op::Equal => {
                    // expression statement, just ignore
                    self.evaluator
                        .evaluate_token_tree(&statement, &mut self.state)?;
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
                                let value_to_print =
                                    self.evaluator.evaluate_token_tree(operand, &mut self.state);

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
                                self.evaluator.evaluate_token_tree(init, &mut self.state)?
                            } else {
                                EvaluateResult::new_nil()
                            };

                            self.state.new_variable(ident.to_string(), init);
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

            TokenTreeInner::If { condition, yes, no } => {
                let condition_boolean = self
                    .evaluator
                    .evaluate_token_tree(&condition, &mut self.state)?
                    .boolean();

                if condition_boolean {
                    // Run the 'yes' branch

                    self.run_token_tree(yes)?;
                } else {
                    // Run the 'no' branch if it exists
                    if let Some(no) = no {
                        self.run_token_tree(no)?;
                    }
                }
            }

            TokenTreeInner::While { condition, body } => {
                self.state.current_stack_frame_mut().push_scope(true);
                match &body.inner {
                    TokenTreeInner::Block { statements } => {
                        while self.check_loop_condition(condition)? {
                            // self.run_token_tree(body)?;
                            for statement in statements {
                                self.run_token_tree(statement)?;
                                // match statement.inner {
                                //     // break would modify the `shoud_break` flag of the nearest out loop scope
                                //     TokenTreeInner::Break | TokenTreeInner::Continue => break,
                                //     _ => {}
                                // }
                            }
                        }
                    }

                    single_statement => {
                        while self.check_loop_condition(condition)? {
                            self.run_token_tree(body)?;
                        }
                    }
                }
                self.state.current_stack_frame_mut().pop_scope();
            }

            TokenTreeInner::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.state.current_stack_frame_mut().push_scope(true);

                if let Some(init) = init {
                    self.run_token_tree(init)?;
                }

                match &body.inner {
                    TokenTreeInner::Block { statements } => {
                        while condition
                            .as_ref()
                            .map(|c| self.check_loop_condition(c))
                            .unwrap_or(Ok(true))?
                        {
                            /// @XXX: a new scope is created for each iteration
                            self.state.current_stack_frame_mut().push_scope(false);
                            for statement in statements {
                                self.run_token_tree(statement)?;
                            }
                            self.state.current_stack_frame_mut().pop_scope();

                            if let Some(increment) = increment {
                                self.run_token_tree(increment)?;
                            }
                        }
                    }

                    single_statement => {
                        while condition
                            .as_ref()
                            .map(|c| self.check_loop_condition(c))
                            .unwrap_or(Ok(true))?
                        {
                            self.run_token_tree(body)?;

                            if let Some(increment) = increment {
                                self.run_token_tree(increment)?;
                            }
                        }
                    }
                }

                self.state.current_stack_frame_mut().pop_scope();
            }

            TokenTreeInner::Block { statements } => {
                // Create a new scope for the block
                self.state.current_stack_frame_mut().push_scope(false);

                // Run each statement in the block
                for statement in statements {
                    self.run_token_tree(statement)?;
                    // match statement.inner {
                    //     // break would modify the `shoud_break` flag of the nearest out loop scope
                    //     TokenTreeInner::Break | TokenTreeInner::Continue => break,
                    //     _ => {}
                    // }
                }

                // Pop the scope after executing the block
                self.state.current_stack_frame_mut().pop_scope();
            }

            TokenTreeInner::Break => {
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

            TokenTreeInner::Continue => {
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

            TokenTreeInner::Eof => {}
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

    fn check_loop_condition(&mut self, cond: &TokenTree<'de>) -> Result<bool, Error> {
        Ok(self
            .evaluator
            .evaluate_token_tree(cond, &mut self.state)?
            .boolean()
            && !self.check_should_break())
    }
}
