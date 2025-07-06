use core::fmt;
use std::{
    borrow::Cow,
    cell::RefCell,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use rustc_hash::FxHashMap as HashMap;

use miette::{Error, LabeledSpan, SourceSpan};

use crate::{
    ast::{
        AssignmentExpression, AssignmentExpressionInner, BinaryExpression, BinaryExpressionInner,
        BlockStatement, BlockStatementInner, BoolLiteral, BoolLiteralInner, CallExpression,
        CallExpressionInner, ClassBodyItem, ClassBodyItemInner, ClassDeclaration,
        ClassDeclarationInner, Declaration, DeclarationInner, Expression, ExpressionInner, ForInit,
        ForInitInner, ForStatement, ForStatementInner, FunctionDeclaration,
        FunctionDeclarationInner, GroupExpression, GroupExpressionInner, Identifier,
        IdentifierInner, IfStatement, IfStatementInner, Literal, LiteralInner, MemberExpression,
        MemberExpressionInner, NilLiteral, NilLiteralInner, NumberLiteral, NumberLiteralInner, Op,
        Spanned, Statement, StatementInner, StringLiteral, StringLiteralInner, TokenTree,
        TokenTreeInner, UnaryExpression, UnaryExpressionInner, VariableDeclaration,
        VariableDeclarationInner, Visitor, WhileStatement, WhileStatementInner,
    },
    error, log_stdout,
    runner::{cache::build_cache_key, global::console, Environment, Vm},
    Parser,
};

// Type alias for closure binding environment
pub type ClosureBindingEnv<'de> = HashMap<String, Environment<'de>>;

unsafe impl Send for Value<'_> {}
unsafe impl Sync for Value<'_> {}

/// Represents the result on stack or expression evaluation (temporary value).
/// Using `Rc` allows us to share the result without copying the data.
/// So it's cheap to clone
#[derive(Debug, Clone)]
pub struct Value<'de> {
    pub(crate) inner: Rc<ValueInner<'de>>,
}

#[derive(Debug)]
pub struct NativeFunction<'de> {
    pub name: String,
    pub fn_ptr: fn(&[Value<'de>]) -> Result<Value<'de>, Error>,
}

impl fmt::Display for NativeFunction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "function {}() {{
            [native code]
        }}",
            self.name
        )
    }
}

#[derive(Debug)]
pub enum ValueInner<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Function(Function<'de>),
    Object(RefCell<Object<'de>>),
    NativeFunction(NativeFunction<'de>),
}

#[derive(Debug)]
pub struct Function<'de> {
    pub name: String,
    pub parameters: Vec<Identifier<'de>>,
    pub body: BlockStatement<'de>,
    pub closure_env: Option<Environment<'de>>,
    pub closure_binding_env: ClosureBindingEnv<'de>,
}

impl Function<'_> {
    fn is_pure(&self) -> bool {
        // pure function: 1. no closure bindings 2. no side effects
        self.closure_binding_env.is_empty()
    }
}

#[derive(Debug)]
pub struct Object<'de> {
    pub properties: HashMap<String, Value<'de>>,
}

impl<'de> ValueInner<'de> {
    pub fn boolean(&self) -> bool {
        match self {
            ValueInner::Bool(boolean) => *boolean,
            ValueInner::Nil => false,
            _ => true,
        }
    }
}

impl<'de> Value<'de> {
    pub fn new_string(string: Cow<'de, str>) -> Self {
        Self {
            inner: Rc::new(ValueInner::String(string)),
        }
    }

    pub fn new_number(num: f64) -> Self {
        Self {
            inner: Rc::new(ValueInner::Number(num)),
        }
    }

    pub fn new_nil() -> Self {
        Self {
            inner: Rc::new(ValueInner::Nil),
        }
    }

    pub fn new_bool(boolean: bool) -> Self {
        Self {
            inner: Rc::new(ValueInner::Bool(boolean)),
        }
    }

    pub fn new_function(
        name: String,
        parameters: Vec<Identifier<'de>>,
        body: BlockStatement<'de>,
        closure_env: Option<Environment<'de>>,
        closure_binding_env: ClosureBindingEnv<'de>,
    ) -> Self {
        Self {
            inner: Rc::new(ValueInner::Function(Function {
                name,
                parameters,
                body,
                closure_env,
                closure_binding_env,
            })),
        }
    }

    pub fn new_object(properties: HashMap<String, Value<'de>>) -> Self {
        Self {
            inner: Rc::new(ValueInner::Object(RefCell::new(Object { properties }))),
        }
    }

    pub fn new_native_function(
        name: String,
        fn_ptr: fn(&[Value<'de>]) -> Result<Value<'de>, Error>,
    ) -> Self {
        Self {
            inner: Rc::new(ValueInner::NativeFunction(NativeFunction { name, fn_ptr })),
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &**self {
            ValueInner::Bool(boolean) => write!(f, "{}", boolean),
            ValueInner::Nil => write!(f, "nil"),
            ValueInner::Number(num) => write!(f, "{}", num),
            ValueInner::String(string) => write!(f, "{}", string),
            ValueInner::Function(func) => {
                write!(
                    f,
                    "Function(name: {}, parameters: {:?})",
                    func.name, func.parameters
                )
            }
            ValueInner::Object(object) => {
                let properties: Vec<String> = object
                    .borrow()
                    .properties
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect();
                write!(f, "Object({})", properties.join(", "))
            }
            ValueInner::NativeFunction(native_func) => {
                write!(f, "{}", native_func)
            }
        }
    }
}

impl<'de> Deref for Value<'de> {
    type Target = ValueInner<'de>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub struct Evaluator<'de> {
    pub whole: &'de str,
    pub parser: Parser<'de>,
}

impl<'de> Evaluator<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            parser: Parser::new(input),
        }
    }

    // only used in evaluate command
    pub fn evaluate_command(&mut self, vm: &mut Vm<'de>) -> Result<Value<'de>, Error> {
        let expr = self.parser.parse_expression()?;
        match expr {
            Some(expr) => {
                // self.evaluate_token_tree(&tt, state)
                todo!()
            }
            None => {
                return Err(error::SyntaxError {
                    src: self.whole.to_string(),
                    message: "expected an expression".to_string(),
                    err_span: (0..0).into(),
                }
                .into());
            } // self.evaluate_token_tree(&tt, state);
        }
    }

    fn evaluate_identifier(
        &self,
        ident: &Identifier<'de>,
        vm: &mut Vm<'de>,
        err_span: SourceSpan,
    ) -> Result<Value<'de>, Error> {
        if let Some(ident_value) = vm.get_variable(&ident.name) {
            Ok(ident_value.clone())
        } else {
            return Err(error::RuntimeError::ReferenceError {
                src: self.whole.to_string(),
                ident: ident.to_string(),
                err_span,
            }
            .into());
        }
    }

    pub fn evaluate_expression(
        &mut self,
        expr: &Expression<'de>,
        vm: &mut Vm<'de>,
    ) -> Result<Value<'de>, Error> {
        match &expr.inner {
            ExpressionInner::Binary(binary_expr) => {
                match binary_expr.operator {
                    Op::Plus => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        match (&*left_value, &*right_value) {
                            (ValueInner::Number(left_num), ValueInner::Number(right_num)) => {
                                // I don't know why it cannot infer the Ok type here.
                                Ok::<Value<'de>, Error>(Value::new_number(left_num + right_num))
                            }
                            (ValueInner::String(left_str), ValueInner::String(right_str)) => {
                                Ok(Value::new_string(left_str.clone() + right_str.clone()))
                            }

                            _ => {
                                return Err(error::RuntimeError::BadOperandError {
                                    src: self.whole.to_string(),
                                    operator: binary_expr.operator.to_string(),
                                    reason: "operands must be both numbers or strings".to_string(),
                                    err_span: binary_expr.range.into(),
                                }
                                .into());
                            }
                        }
                    }

                    Op::Minus | Op::Star | Op::Slash => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        if let (ValueInner::Number(left_num), ValueInner::Number(right_num)) =
                            (&*left_value, &*right_value)
                        {
                            match binary_expr.operator {
                                Op::Minus => Ok(Value::new_number(left_num - right_num)),
                                Op::Star => Ok(Value::new_number(left_num * right_num)),
                                Op::Slash => {
                                    if *right_num == 0.0 {
                                        return Err(error::RuntimeError::BadOperandError {
                                            src: self.whole.to_string(),
                                            operator: binary_expr.operator.to_string(),
                                            reason: "Cannot divide by zero".to_string(),
                                            err_span: expr.range.into(),
                                        }
                                        .into());
                                    }
                                    Ok(Value::new_number(left_num / right_num))
                                }
                                _ => unreachable!("by out arm"),
                            }
                        } else {
                            Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: binary_expr.operator.to_string(),
                                reason: "Operands must be numbers".to_string(),
                                err_span: expr.range.into(),
                            }
                            .into())
                        }
                    }

                    Op::Greater | Op::GreaterEqual | Op::Less | Op::LessEqual => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        if let (ValueInner::Number(left_num), ValueInner::Number(right_num)) =
                            (&*left_value, &*right_value)
                        {
                            let result = match binary_expr.operator {
                                Op::Greater => left_num > right_num,
                                Op::GreaterEqual => left_num >= right_num,
                                Op::Less => left_num < right_num,
                                Op::LessEqual => left_num <= right_num,
                                _ => unreachable!("by out arm"),
                            };
                            Ok(Value::new_bool(result))
                        } else {
                            Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: binary_expr.operator.to_string(),
                                reason: "Operands must be numbers".to_string(),
                                err_span: expr.range.into(),
                            }
                            .into())
                        }
                    }

                    Op::EqualEqual | Op::BangEqual => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        let result = match (&*left_value, &*right_value) {
                            (ValueInner::Number(left_num), ValueInner::Number(right_num)) => {
                                match binary_expr.operator {
                                    Op::EqualEqual => left_num == right_num,
                                    Op::BangEqual => left_num != right_num,
                                    _ => unreachable!("by out arm"),
                                }
                            }
                            (ValueInner::String(left_str), ValueInner::String(right_str)) => {
                                match binary_expr.operator {
                                    Op::EqualEqual => left_str == right_str,
                                    Op::BangEqual => left_str != right_str,
                                    _ => unreachable!("by out arm"),
                                }
                            }
                            (ValueInner::Bool(left_bool), ValueInner::Bool(right_bool)) => {
                                match binary_expr.operator {
                                    Op::EqualEqual => left_bool == right_bool,
                                    Op::BangEqual => left_bool != right_bool,
                                    _ => unreachable!("by out arm"),
                                }
                            }
                            (ValueInner::Nil, ValueInner::Nil) => true,
                            _ => false,
                        };

                        Ok(Value::new_bool(result))
                    }

                    Op::And => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        if !left_value.boolean() {
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        Ok(right_value)
                    }
                    Op::Or => {
                        let left_value = self.evaluate_expression(&binary_expr.left, vm)?;
                        if left_value.boolean() {
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate_expression(&binary_expr.right, vm)?;
                        Ok(right_value)
                    }

                    Op::Equal => {
                        if let ExpressionInner::Identifier(ident) = &binary_expr.left.inner {
                            let value = self.evaluate_expression(&binary_expr.right, vm)?;

                            // if let Some(variable) = vm.get_variable(&ident.name) {
                            //     *variable = value.clone();
                            // } else {
                            //     return Err(error::RuntimeError::ReferenceError {
                            //         src: self.whole.to_string(),
                            //         ident: ident.name.to_string(),
                            //         err_span: ident.range.into(),
                            //     }
                            //     .into());
                            // }

                            if !vm.assign_variable(&ident.name, value.clone()) {
                                return Err(error::RuntimeError::ReferenceError {
                                    src: self.whole.to_string(),
                                    ident: ident.name.to_string(),
                                    err_span: ident.range.into(),
                                }
                                .into());
                            }

                            Ok(value)
                        } else {
                            return Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: binary_expr.operator.to_string(),
                                reason: "Left operand must be an identifier".to_string(),
                                err_span: expr.range.into(),
                            }
                            .into());
                        }
                    }

                    Op::Field => {
                        let object_value = self.evaluate_expression(&binary_expr.left, vm)?;

                        match &*object_value {
                            ValueInner::Object(object) => {
                                let object = object.borrow();
                                // let property = self
                                //     .evaluate_expression(&binary_expr.right, state)?
                                //     .to_string();

                                let property_name = match &binary_expr.right.inner {
                                    ExpressionInner::Identifier(ident) => {
                                        // If the property is an identifier, we can use its name directly
                                        if ident.name.is_empty() {
                                            return Err(error::RuntimeError::BadOperandError {
                                                src: self.whole.to_string(),
                                                operator: "member access".to_string(),
                                                reason: "Property name cannot be empty".to_string(),
                                                err_span: expr.range.into(),
                                            }
                                            .into());
                                        }
                                        ident.name.to_string()
                                    }

                                    expr => {
                                        let expr = match expr {
                                            ExpressionInner::Assignment(a) => a.to_string(),
                                            ExpressionInner::Binary(b) => b.to_string(),
                                            ExpressionInner::Call(c) => c.to_string(),
                                            ExpressionInner::Literal(l) => l.to_string(),
                                            ExpressionInner::Unary(u) => u.to_string(),
                                            ExpressionInner::Group(g) => g.to_string(),
                                            ExpressionInner::Member(m) => m.to_string(),
                                            ExpressionInner::Identifier(i) => i.to_string(),
                                        };

                                        return Err(error::RuntimeError::BadOperandError {
                                            src: self.whole.to_string(),
                                            operator: "member access".to_string(),
                                            reason: format!("{} is not a valid property", expr),
                                            err_span: binary_expr.right.range.into(),
                                        }
                                        .into());
                                    }
                                };

                                if let Some(property_value) = object.properties.get(&property_name)
                                {
                                    Ok(property_value.clone())
                                } else {
                                    Ok(Value::new_nil())
                                }
                            }

                            _ => {
                                return Err(error::RuntimeError::BadOperandError {
                                    src: self.whole.to_string(),
                                    operator: "member access".to_string(),
                                    reason: format!("{} is not an object", object_value),
                                    err_span: expr.range.into(),
                                }
                                .into());
                            }
                        }
                    }
                    _ => {
                        todo!()
                    }
                }
            }

            ExpressionInner::Identifier(ident) => {
                self.evaluate_identifier(ident, vm, (expr.range.0..expr.range.1).into())
            }

            ExpressionInner::Group(group_expr) => {
                let inner_value = self.evaluate_expression(&group_expr.expression, vm)?;
                Ok(inner_value)
            }

            ExpressionInner::Literal(literal) => match &literal.inner {
                LiteralInner::String(string) => Ok(Value::new_string(string.0.clone())),
                LiteralInner::Number(num) => Ok(Value::new_number(num.0)),
                LiteralInner::Nil(_) => Ok(Value::new_nil()),
                LiteralInner::Bool(boolean) => Ok(Value::new_bool(boolean.0)),
            },

            ExpressionInner::Unary(unary_expr) => {
                let inner_value = self.evaluate_expression(&unary_expr.right, vm)?;

                match unary_expr.operator {
                    Op::Minus => {
                        if let ValueInner::Number(num) = &*inner_value {
                            Ok(Value::new_number(-num))
                        } else {
                            Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: unary_expr.operator.to_string(),
                                reason: "Operand must be a number".to_string(),
                                err_span: expr.range.into(),
                            }
                            .into())
                        }
                    }
                    Op::Bang => Ok(Value::new_bool(!inner_value.boolean())),
                    _ => unreachable!(
                        "Only `-` and `!` operators are supported for unary expressions"
                    ),
                }
            }

            ExpressionInner::Call(call_expr) => {
                let mut callee_value = self.evaluate_expression(&call_expr.callee, vm)?;
                // arguments evaluation must happen before closure environment is set
                let mut arguments = Vec::new();
                for arg in &call_expr.arguments {
                    let arg_value = self.evaluate_expression(arg, vm)?;
                    arguments.push(arg_value);
                }

                match &*callee_value {
                    ValueInner::Function(func) => {
                        if func.parameters.len() != call_expr.arguments.len() {
                            return Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: "call".to_string(),
                                reason: format!(
                                    "Expected {} arguments, but got {}",
                                    func.parameters.len(),
                                    call_expr.arguments.len()
                                ),
                                err_span: expr.range.into(),
                            }
                            .into());
                        }

                        let is_pure_function = func.closure_binding_env.is_empty();

                        let mut cache_key = None;
                        if is_pure_function {
                            // Check the cache for the pure function call
                            cache_key = Some(build_cache_key(&callee_value, &arguments));

                            if let Some(cached_value) =
                                vm.pure_function_call_cache.get(cache_key.as_ref().unwrap())
                            {
                                return Ok(cached_value.clone());
                            }
                        }

                        vm.enter_function(
                            func.closure_env.clone(),
                            func.closure_binding_env.clone(),
                        );

                        // let mut arguments = Vec::new();
                        for (param, arg_value) in func.parameters.iter().zip(arguments) {
                            vm.define_variable(param.to_string(), arg_value);
                        }

                        for stmt in &func.body.statements {
                            self.run_statement(stmt, vm)?;
                        }

                        let return_value = match vm.current_stack_frame() {
                            Ok(frame) => frame
                                .return_value
                                .clone()
                                .unwrap_or_else(|| Value::new_nil()),
                            Err(e) => {
                                return Err(error::RuntimeError::InternalError {
                                    message: e.to_string(),
                                }
                                .into());
                            }
                        };

                        vm.leave_function()?;

                        // set cache
                        if is_pure_function {
                            vm.pure_function_call_cache
                                .insert(cache_key.unwrap(), return_value.clone());
                        }

                        return Ok(return_value);
                    }

                    ValueInner::NativeFunction(native_func) => {
                        let mut arguments = Vec::new();
                        for arg in &call_expr.arguments {
                            let arg_value = self.evaluate_expression(arg, vm)?;
                            arguments.push(arg_value);
                        }

                        // Call the native function
                        let ret = (native_func.fn_ptr)(&arguments);
                        return ret;
                    }
                    _ => {
                        return Err(error::RuntimeError::BadOperandError {
                            src: self.whole.to_string(),
                            operator: "call".to_string(),
                            reason: "Callee must be a function".to_string(),
                            err_span: expr.range.into(),
                        }
                        .into());
                    }
                }
            }

            ExpressionInner::Member(member) => {
                let object_value = self.evaluate_expression(&member.object, vm)?;

                match &*object_value {
                    ValueInner::Object(object) => {
                        let object = object.borrow();
                        let property = self.evaluate_expression(&member.object, vm)?.to_string();

                        if let Some(property_value) = object.properties.get(&property) {
                            Ok(property_value.clone())
                        } else {
                            Ok(Value::new_nil())
                        }
                    }

                    _ => {
                        return Err(error::RuntimeError::BadOperandError {
                            src: self.whole.to_string(),
                            operator: "member access".to_string(),
                            reason: format!("{} is not an object", object_value),
                            err_span: expr.range.into(),
                        }
                        .into());
                    }
                }
            }

            _ => {
                todo!()
            }
        }
    }

    pub fn run_statement(
        &mut self,
        statement: &Statement<'de>,
        vm: &mut Vm<'de>,
    ) -> Result<(), Error> {
        if self.check_should_continue(vm)
            || self.check_should_break(vm)
            || self.check_should_return(vm)
        {
            return Ok(());
        }

        match &statement.inner {
            StatementInner::Block(block) => {
                vm.enter_scope();
                for stmt in &block.statements {
                    self.run_statement(stmt, vm)?;
                }
                vm.leave_scope();
            }
            StatementInner::Break => {
                if !vm.signal_break() {
                    return Err(error::RuntimeError::BreakOrContinueOutsideLoop {
                        src: self.whole.to_string(),
                        cause: "break".to_string(),
                        err_span: (statement.range.0..statement.range.1).into(),
                    }
                    .into());
                }
            }
            StatementInner::Continue => {
                if !vm.signal_continue() {
                    return Err(error::RuntimeError::BreakOrContinueOutsideLoop {
                        src: self.whole.to_string(),
                        cause: "continue".to_string(),
                        err_span: (statement.range.0..statement.range.1).into(),
                    }
                    .into());
                }
            }
            StatementInner::Declaration(decl) => match &decl.inner {
                DeclarationInner::Variable(variable_declaration) => {
                    let variable_name = variable_declaration.id.to_string();
                    let init = match &variable_declaration.init {
                        Some(init) => self.evaluate_expression(init, vm)?,
                        None => {
                            // If no initializer is provided, we set the variable to nil
                            Value::new_nil()
                        }
                    };

                    if let Ok(current_stack_frame) = vm.current_stack_frame_mut() {
                        // If the variable was a closure binding, we remove it because it's being redefined
                        current_stack_frame
                            .closure_binding_env
                            .remove(&variable_name);
                    }

                    vm.define_variable(variable_name, init);
                }

                DeclarationInner::Function(func_declaration) => {
                    let function_name = func_declaration.name.to_string();

                    let closure_binding_env =
                        self.collect_closure_binding_env(func_declaration, vm);

                    let function_value = Value::new_function(
                        function_name.clone(),
                        func_declaration.parameters.clone(),
                        *func_declaration.body.clone(),
                        Some(vm.current_env.clone()),
                        closure_binding_env,
                    );

                    if let Ok(current_stack_frame) = vm.current_stack_frame_mut() {
                        // If the variable was a closure binding, we remove it because it's being redefined
                        current_stack_frame
                            .closure_binding_env
                            .remove(&function_name);
                    }

                    // Register the function in the current scope
                    vm.define_variable(function_name, function_value);
                }

                DeclarationInner::Class(class_decl) => {}
            },

            StatementInner::Expression(expr) => {
                self.evaluate_expression(expr, vm).map_err(|e| {
                    error::RuntimeError::BadOperandError {
                        src: self.whole.to_string(),
                        operator: "expression".to_string(),
                        reason: e.to_string(),
                        err_span: (statement.range.0..statement.range.1).into(),
                    }
                })?;
            }

            StatementInner::For(for_statement) => {
                // vm.current_stack_frame_mut().push_scope(true);
                // vm.enter_loop();
                vm.enter_loop();
                vm.enter_scope();

                if let Some(init) = &for_statement.init {
                    self.run_for_init(&init, vm)?;
                }

                match &for_statement.body.inner {
                    StatementInner::Block(block) => {
                        while for_statement
                            .test
                            .as_ref()
                            .map(|c| self.check_loop_condition(c, vm))
                            .unwrap_or(Ok(true))?
                        {
                            /// @XXX: a new scope is created for each iteration
                            // vm.current_stack_frame_mut().push_scope(false);
                            vm.enter_scope();
                            for statement in &block.statements {
                                self.run_statement(statement, vm)?;
                            }
                            // vm.current_stack_frame_mut().pop_scope();
                            vm.leave_scope();

                            if let Some(update) = &for_statement.update {
                                self.evaluate_expression(update, vm)?;
                            }
                        }
                    }

                    single_statement => {
                        while for_statement
                            .test
                            .as_ref()
                            .map(|c| self.check_loop_condition(c, vm))
                            .unwrap_or(Ok(true))?
                        {
                            self.run_statement(&for_statement.body, vm)?;

                            if let Some(update) = &for_statement.update {
                                self.evaluate_expression(update, vm)?;
                            }
                        }
                    }
                }

                // vm.current_stack_frame_mut().pop_scope();
                vm.leave_scope();
                vm.leave_loop();
            }

            StatementInner::If(if_statement) => {
                let condition_boolean = self.evaluate_expression(&if_statement.test, vm)?.boolean();

                if condition_boolean {
                    self.run_statement(&if_statement.consequent, vm)?;
                } else {
                    // Run the 'no' branch if it exists
                    if let Some(alternate) = &if_statement.alternate {
                        self.run_statement(alternate, vm)?;
                    }
                }
            }

            StatementInner::Print(expr) => {
                let value = self.evaluate_expression(expr, vm)?;
                log_stdout!("{value}");
            }

            StatementInner::Return(return_statement) => {
                let return_value = if let Some(expr) = &return_statement {
                    self.evaluate_expression(expr, vm)?
                } else {
                    Value::new_nil()
                };

                // vm.current_stack_frame_mut().return_value = Some(return_value);
                vm.current_stack_frame_mut()
                    .expect("Stack frame should always be present")
                    .return_value = Some(return_value);
            }

            StatementInner::While(while_statement) => {
                // vm.current_stack_frame_mut().push_scope(true);
                vm.enter_loop();
                vm.enter_scope();

                match &while_statement.body.inner {
                    StatementInner::Block(block) => {
                        while self.check_loop_condition(&while_statement.test, vm)? {
                            /// @XXX: a new scope is created for each iteration
                            // vm.current_stack_frame_mut().push_scope(false);
                            vm.enter_scope();
                            for statement in &block.statements {
                                self.run_statement(statement, vm)?;
                            }
                            // vm.current_stack_frame_mut().pop_scope();
                            vm.leave_scope();
                        }
                    }

                    single_statement => {
                        while self.check_loop_condition(&while_statement.test, vm)? {
                            self.run_statement(&while_statement.body, vm)?;
                        }
                    }
                }

                // vm.current_stack_frame_mut().pop_scope();
                vm.leave_scope();
                vm.leave_loop();
            }

            _ => {
                todo!()
            }
        }

        Ok(())
    }

    fn run_for_init(&mut self, init: &ForInit<'de>, vm: &mut Vm<'de>) -> Result<(), Error> {
        match &init.inner {
            ForInitInner::VariableDeclaration(variable_declaration) => {
                let variable_name = variable_declaration.id.to_string();
                let init_value = match &variable_declaration.init {
                    Some(init) => self.evaluate_expression(init, vm)?,
                    None => Value::new_nil(),
                };

                // vm.new_variable(variable_name, init_value);
                vm.define_variable(variable_name, init_value);
            }
            ForInitInner::Expression(expr) => {
                self.evaluate_expression(expr, vm).map_err(|e| {
                    error::RuntimeError::BadOperandError {
                        src: self.whole.to_string(),
                        operator: "for init expression".to_string(),
                        reason: e.to_string(),
                        err_span: (init.range.0..init.range.1).into(),
                    }
                })?;
            }
        }

        Ok(())
    }

    fn check_should_continue(&self, vm: &Vm<'de>) -> bool {
        vm.current_loop_context()
            .map_or(false, |loop_ctx| loop_ctx.should_continue)
    }

    fn check_should_break(&self, vm: &Vm<'de>) -> bool {
        vm.current_loop_context()
            .map_or(false, |loop_ctx| loop_ctx.should_break)
    }

    fn check_should_return(&self, vm: &Vm<'de>) -> bool {
        vm.current_stack_frame()
            .map_or(false, |frame| frame.return_value.is_some())
    }

    fn check_loop_condition(
        &mut self,
        cond: &Expression<'de>,
        vm: &mut Vm<'de>,
    ) -> Result<bool, Error> {
        // Ok(self.evaluate_expression(cond, state)?.boolean() && !self.check_should_break(state))
        Ok(self.evaluate_expression(cond, vm)?.boolean() && !self.check_should_break(vm))
    }

    // for simplicity, we try to collect all variables used in the function body
    fn collect_closure_binding_env<'ast, 'vm>(
        &self,
        func_decl: &'ast FunctionDeclaration<'de>,
        vm: &'vm Vm<'de>,
    ) -> ClosureBindingEnv<'de> {
        let mut binding_env = HashMap::default();

        struct AstVisitor<'ast, 'vm, 'de, 'local> {
            func_decl: &'ast FunctionDeclaration<'de>,
            vm: &'vm Vm<'de>,
            binding_env: &'local mut ClosureBindingEnv<'de>,
            local_vars: std::collections::HashSet<String>,
        }

        impl<'ast, 'vm, 'de, 'local> Visitor<'ast, 'de> for AstVisitor<'ast, 'vm, 'de, 'local> {
            type Output = ();

            fn visit_identifier(&mut self, identifier: &'ast Identifier<'de>) -> Self::Output {
                let name = identifier.inner.name.as_ref();

                // @FIXME: maybe we need to remove `&& !self.vm.global.bindings.borrow().contains_key(name)`
                // because closure_binding_env is used to tell if a function is pure.
                // But if a global non-pure function is called, the function is not pure, but we'll regard it as pure, which is not correct.

                // If it's not a local variable, check if it's in the environment
                if !self.local_vars.contains(name)
                    && !self.vm.global.bindings.borrow().contains_key(name)
                {
                    // if let Some(value) = self.vm.current_env.get(name) {
                    //     self.bindings.insert(name.to_string(), value);
                    // }
                    let mut env_option = Some(self.vm.current_env.clone());
                    while let Some(ref env) = env_option {
                        if env.bindings.borrow().contains_key(name) {
                            self.binding_env.insert(name.to_string(), env.clone());
                            break;
                        }
                        env_option = env.parent.clone();
                    }
                }
            }

            fn visit_assignment_expression(
                &mut self,
                expr: &'ast AssignmentExpression<'de>,
            ) -> Self::Output {
                self.visit_expression(&expr.inner.right);
                // Note: assignment target is handled separately as it may introduce new local variables
            }

            fn visit_variable_declaration(
                &mut self,
                decl: &'ast VariableDeclaration<'de>,
            ) -> Self::Output {
                // Add the variable to local variables
                self.local_vars
                    .insert(decl.inner.id.inner.name.as_ref().to_string());

                // Visit the initializer if present
                if let Some(init) = &decl.inner.init {
                    self.visit_expression(init);
                }
            }

            fn visit_function_declaration(
                &mut self,
                decl: &'ast FunctionDeclaration<'de>,
            ) -> Self::Output {
                // Add the function name to local variables
                self.local_vars
                    .insert(decl.inner.name.inner.name.as_ref().to_string());

                // Note: We don't visit the function body as it has its own scope
            }

            fn visit_class_declaration(&mut self, decl: &ClassDeclaration<'de>) -> Self::Output {
                // Add the class name to local variables
                self.local_vars
                    .insert(decl.inner.id.inner.name.as_ref().to_string());
            }
        }

        // Initialize local variables with function parameters
        let mut local_vars = std::collections::HashSet::new();
        for param in &func_decl.inner.parameters {
            local_vars.insert(param.inner.name.as_ref().to_string());
        }

        let mut visitor = AstVisitor {
            func_decl,
            vm,
            binding_env: &mut binding_env,
            local_vars,
        };

        // Visit the function body to collect closure binding environment
        visitor.visit_block_statement(&func_decl.inner.body);

        binding_env
    }
}

#[cfg(test)]
mod tests;
