use core::fmt;
use std::{borrow::Cow, ops::Deref, rc::Rc};

use miette::{Error, LabeledSpan, SourceSpan};

use crate::{
    error,
    parse::{Atom, Op, TokenTree, TokenTreeInner},
    run::RuntimeState,
    Parser,
};

#[derive(Debug, Clone)]
pub struct EvaluateResult<'de> {
    inner: Rc<EvaluateResultInner<'de>>,
}

#[derive(Debug, Clone)]
pub enum EvaluateResultInner<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
}

impl<'de> EvaluateResult<'de> {
    pub fn new_string(string: Cow<'de, str>) -> Self {
        Self {
            inner: Rc::new(EvaluateResultInner::String(string)),
        }
    }

    pub fn new_number(num: f64) -> Self {
        Self {
            inner: Rc::new(EvaluateResultInner::Number(num)),
        }
    }

    pub fn new_nil() -> Self {
        Self {
            inner: Rc::new(EvaluateResultInner::Nil),
        }
    }

    pub fn new_bool(boolean: bool) -> Self {
        Self {
            inner: Rc::new(EvaluateResultInner::Bool(boolean)),
        }
    }
}

impl fmt::Display for EvaluateResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &**self {
            EvaluateResultInner::Bool(boolean) => write!(f, "{}", boolean),
            EvaluateResultInner::Nil => write!(f, "nil"),
            EvaluateResultInner::Number(num) => write!(f, "{}", num),
            EvaluateResultInner::String(string) => write!(f, "{}", string),
        }
    }
}

impl<'de> Deref for EvaluateResult<'de> {
    type Target = EvaluateResultInner<'de>;

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

    pub fn evaluate_expression(mut self) -> Result<EvaluateResult<'de>, Error> {
        let parser = std::mem::take(&mut self.parser);
        let token_tree = parser.parse_expression()?;
        self.evaluate_token_tree(&token_tree, None)
    }

    pub fn evaluate_token_tree<'rt>(
        &self,
        token_tree: &TokenTree<'de>,
        state: Option<&RuntimeState<'de>>,
    ) -> Result<EvaluateResult<'de>, Error> {
        let evaluate_result: EvaluateResult<'de> = match &token_tree.inner {
            TokenTreeInner::Atom(atom) => match atom {
                Atom::String(string) => EvaluateResult::new_string(string.clone()),

                Atom::Number(num) => EvaluateResult::new_number(*num),

                Atom::Nil => EvaluateResult::new_nil(),

                Atom::Bool(boolean) => EvaluateResult::new_bool(*boolean),

                Atom::Ident(ident) => {
                    if let Some(state) = state {
                        if let Some(ident_value) = state.get_variable_value(ident) {
                            ident_value.clone()
                        } else {
                            return Err(error::RuntimeError::ReferenceError {
                                src: self.whole.to_string(),
                                ident: ident.to_string(),
                                err_span: (token_tree.range.0..token_tree.range.1).into(),
                            }
                            .into());
                        }
                    } else {
                        return Err(error::UnexpectedTokenTree {
                            src: self.whole.to_string(),
                            token_tree: token_tree.clone().into(),
                            err_span: (token_tree.range.0..token_tree.range.1).into(),
                        }
                        .into());
                    }
                }
                Atom::Super => {
                    return Err(error::SyntaxError {
                        src: self.whole.to_string(),
                        message: format!("invalid use of keyword super"),
                        err_span: (token_tree.range.0..token_tree.range.1).into(),
                    }
                    .into());
                }
                Atom::This => {
                    todo!()
                }
            },

            TokenTreeInner::Cons(op, operands) => match op {
                Op::Group => self.evaluate_token_tree(
                    operands.first().expect("Group must not be empty."),
                    state,
                )?,

                Op::Minus if operands.len() == 1 => {
                    let rest = self.evaluate_token_tree(
                        operands.first().expect("- must be followed by a number."),
                        state,
                    )?;

                    if let EvaluateResultInner::Number(num) = &*rest {
                        EvaluateResult::new_number(-num)
                    } else {
                        return Err(error::RuntimeError::BadOperandError {
                            src: self.whole.to_string(),
                            operator: op.to_string(),
                            reason: "operands must be numbers".to_string(),
                            err_span: (token_tree.range.0..token_tree.range.1).into(),
                        }
                        .into());
                    }
                }

                Op::Bang => {
                    let rest = self.evaluate_token_tree(
                        operands.first().expect("! must be followed by a bool."),
                        state,
                    )?;

                    match &*rest {
                        EvaluateResultInner::Bool(boolean) => EvaluateResult::new_bool(!boolean),
                        EvaluateResultInner::Nil => EvaluateResult::new_bool(true),
                        _ => EvaluateResult::new_bool(!true),
                    }
                }
                Op::Plus => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs, state)?;
                    let rhs_value = self.evaluate_token_tree(rhs, state)?;

                    match (&*lhs_value.inner, &*rhs_value) {
                        (
                            EvaluateResultInner::Number(left_num),
                            EvaluateResultInner::Number(right_num),
                        ) => EvaluateResult::new_number(left_num + right_num),

                        (
                            EvaluateResultInner::String(left_string),
                            EvaluateResultInner::String(right_string),
                        ) => EvaluateResult::new_string(left_string.clone() + right_string.clone()),

                        _ => {
                            return Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: op.to_string(),
                                reason: "operands must be both numbers or strings".to_string(),
                                err_span: (lhs.range.0..rhs.range.1).into(),
                            }
                            .into());
                        }
                    }
                }

                Op::Minus | Op::Star | Op::Slash => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs, state)?;
                    let rhs_value = self.evaluate_token_tree(rhs, state)?;

                    if let EvaluateResultInner::Number(left_num) = &*lhs_value {
                        if let EvaluateResultInner::Number(right_num) = &*rhs_value {
                            let result_num = match op {
                                Op::Plus => left_num + right_num,
                                Op::Minus => left_num - right_num,
                                Op::Star => left_num * right_num,
                                Op::Slash => left_num / right_num,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::new_number(result_num)
                        } else {
                            return Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: op.to_string(),
                                reason: "can only be used with numbers".to_string(),
                                err_span: (rhs.range.0..rhs.range.1).into(),
                            }
                            .into());
                        }
                    } else {
                        return Err(error::RuntimeError::BadOperandError {
                            src: self.whole.to_string(),
                            operator: op.to_string(),
                            reason: "can only be used with numbers".to_string(),
                            err_span: (rhs.range.0..rhs.range.1).into(),
                        }
                        .into());
                    }
                }

                Op::Greater | Op::GreaterEqual | Op::Less | Op::LessEqual => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs, state)?;
                    let rhs_value = self.evaluate_token_tree(rhs, state)?;

                    match (&*lhs_value, &*rhs_value) {
                        (EvaluateResultInner::Number(l), EvaluateResultInner::Number(r)) => {
                            let result_value = match op {
                                Op::Greater => l > r,
                                Op::GreaterEqual => l >= r,
                                Op::Less => l < r,
                                Op::LessEqual => l <= r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::new_bool(result_value)
                        }
                        _ => {
                            return Err(error::RuntimeError::BadOperandError {
                                src: self.whole.to_string(),
                                operator: op.to_string(),
                                reason: "can only be used with numbers".to_string(),
                                err_span: (rhs.range.0..rhs.range.1).into(),
                            }
                            .into());
                        }
                    }
                }

                Op::EqualEqual | Op::BangEqual => {
                    let lhs = &operands[0];
                    let rhs = &operands[1];
                    let lhs_value = self.evaluate_token_tree(lhs, state)?;
                    let rhs_value = self.evaluate_token_tree(rhs, state)?;

                    match (&*lhs_value, &*rhs_value) {
                        (EvaluateResultInner::Number(l), EvaluateResultInner::Number(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::new_bool(result_value)
                        }
                        (EvaluateResultInner::String(l), EvaluateResultInner::String(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::new_bool(result_value)
                        }

                        (EvaluateResultInner::Bool(l), EvaluateResultInner::Bool(r)) => {
                            let result_value = match op {
                                Op::EqualEqual => l == r,
                                Op::BangEqual => l != r,
                                _ => unreachable!("by the outer match arm pattern"),
                            };

                            EvaluateResult::new_bool(result_value)
                        }

                        _ => EvaluateResult::new_bool(false),
                    }
                }

                _ => {
                    todo!()
                }
            },

            _ => {
                todo!()
            }
        };

        Ok(evaluate_result)
    }
}
