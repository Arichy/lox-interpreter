use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

use crate::{
    error::owned::OwnedTokenTree,
    parse::{Op, TokenTree, TokenTreeInner},
};

pub mod owned;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token: '{token}' in input")]
pub struct SingleTokenError {
    #[source_code]
    pub src: String,

    pub token: char,

    #[label = "this input character"]
    pub err_span: SourceSpan,
}
impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_recognized_ = &self.src[..=self.err_span.offset()];
        until_recognized_.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringTerminationError {
    #[source_code]
    pub src: String,

    #[label = "this string literal"]
    pub err_span: SourceSpan,
}
impl StringTerminationError {
    pub fn line(&self) -> usize {
        let until_recognized_ = &self.src[..=self.err_span.offset()];
        until_recognized_.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("SyntaxError: {message}")]
pub struct SyntaxError {
    #[source_code]
    pub src: String,

    pub message: String,

    #[label]
    pub err_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct UnexpectedTokenTree {
    #[source_code]
    pub src: String,

    pub token_tree: OwnedTokenTree,

    #[label = "this string literal"]
    pub err_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error)]
pub enum RuntimeError {
    #[error("ReferenceError: {ident} is not defined")]
    ReferenceError {
        #[source_code]
        src: String,
        ident: String,
        #[label = "here"]
        err_span: SourceSpan,
    },

    #[error("Bad Operand: for {operator}, {reason}")]
    BadOperandError {
        #[source_code]
        src: String,
        operator: String,
        reason: String,
        #[label = "here"]
        err_span: SourceSpan,
    },
}
