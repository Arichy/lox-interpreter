use core::fmt;
use std::{borrow::Cow, ops::Deref, rc::Rc};

use miette::{Error, LabeledSpan, SourceSpan};

use crate::{
    ast::{
        AssignmentExpression, Expression, ExpressionInner, Identifier, LiteralInner, Op, TokenTree,
    },
    error,
    runner::RuntimeState,
    Parser,
};

/// Represents the result on stack or expression evaluation (temporary value).
/// Using `Rc` allows us to share the result without copying the data.
/// So it's cheap to clone
#[derive(Debug, Clone)]
pub struct Value<'de> {
    inner: Rc<ValueInner<'de>>,
}

#[derive(Debug, Clone)]
pub enum ValueInner<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
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
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &**self {
            ValueInner::Bool(boolean) => write!(f, "{}", boolean),
            ValueInner::Nil => write!(f, "nil"),
            ValueInner::Number(num) => write!(f, "{}", num),
            ValueInner::String(string) => write!(f, "{}", string),
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
    pub fn evaluate_command(&mut self, state: &mut RuntimeState<'de>) -> Result<Value<'de>, Error> {
        let expr: Option<crate::ast::Spanned<crate::ast::ExpressionInner<'de>>> =
            self.parser.parse_expression()?;
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
        state: &mut RuntimeState<'de>,
        err_span: SourceSpan,
    ) -> Result<Value<'de>, Error> {
        if let Some(ident_value) = state.get_variable_value(&ident.name) {
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
        state: &mut RuntimeState<'de>,
    ) -> Result<Value<'de>, Error> {
        match &expr.inner {
            // assigment expression is a kind of binary expression
            // ExpressionInner::Assignment(assignment_expr) => match assignment_expr.operator {
            //     Op::Equal => {
            //     let variable = &assignment_expr.left.name;
            //     let value = self.evaluate_expression(&assignment_expr.right, state)?;

            //     if let Some(variable) = state.get_variable_value_mut(variable) {
            //         *variable = value.clone();
            //     } else {
            //         return Err(error::RuntimeError::ReferenceError {
            //             src: self.whole.to_string(),
            //             ident: variable.to_string(),
            //             err_span: assignment_expr.left.range.into(),
            //         }
            //         .into());
            //     };

            //     Ok(value)
            // }
            //     _ => unreachable!("Only `=` operator is supported for assignment currently"),
            // },
            ExpressionInner::Binary(binary_expr) => {
                let left_value = self.evaluate_expression(&binary_expr.left, state)?;
                let right_value = self.evaluate_expression(&binary_expr.right, state)?;

                match binary_expr.operator {
                    Op::Plus => match (&*left_value, &*right_value) {
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
                    },

                    Op::Minus | Op::Star | Op::Slash => {
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
                        let left_value = self.evaluate_expression(&binary_expr.left, state)?;
                        if !left_value.boolean() {
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate_expression(&binary_expr.right, state)?;
                        Ok(right_value)
                    }
                    Op::Or => {
                        let left_value = self.evaluate_expression(&binary_expr.left, state)?;
                        if left_value.boolean() {
                            return Ok(left_value);
                        }
                        let right_value = self.evaluate_expression(&binary_expr.right, state)?;
                        Ok(right_value)
                    }

                    Op::Equal => {
                        if let ExpressionInner::Identifier(ident) = &binary_expr.left.inner {
                            let value = self.evaluate_expression(&binary_expr.right, state)?;

                            if let Some(variable) = state.get_variable_value_mut(&ident.name) {
                                *variable = value.clone();
                            } else {
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

                    _ => {
                        todo!()
                    }
                }
            }

            ExpressionInner::Identifier(ident) => {
                self.evaluate_identifier(ident, state, expr.range.into())
            }

            ExpressionInner::Group(group_expr) => {
                let inner_value = self.evaluate_expression(&group_expr.expression, state)?;
                Ok(inner_value)
            }

            ExpressionInner::Literal(literal) => match &literal.inner {
                LiteralInner::String(string) => Ok(Value::new_string(string.0.clone())),
                LiteralInner::Number(num) => Ok(Value::new_number(num.0)),
                LiteralInner::Nil(_) => Ok(Value::new_nil()),
                LiteralInner::Bool(boolean) => Ok(Value::new_bool(boolean.0)),
            },

            ExpressionInner::Unary(unary_expr) => {
                let inner_value = self.evaluate_expression(&unary_expr.right, state)?;

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

            _ => {
                todo!()
            }
        }
    }
}
