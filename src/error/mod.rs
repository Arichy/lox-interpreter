use miette::{Diagnostic, LabeledSpan, SourceSpan};
use thiserror::Error;

// use crate::error::owned::OwnedTokenTree;

pub mod owned;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof {
    #[source_code]
    pub src: String,

    #[label = "unexpected EOF"]
    pub err_span: SourceSpan,
}
impl Eof {
    pub fn new(src: &str) -> Self {
        Self {
            src: src.to_string(),
            err_span: SourceSpan::from((src.len() - 1, src.len() - 1)),
        }
    }
}

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
#[error("RedeclarationError: Already a variable with name `{name}` in this scope.")]
pub struct RedeclarationError {
    #[source_code]
    pub src: String,

    pub name: String,

    #[label = "`{name}` already exists"]
    pub existing_span: SourceSpan,

    #[label]
    pub err_span: SourceSpan,
}

#[derive(Diagnostic, Debug, Error)]
#[error("ParseInternalError: {message}")]
pub struct ParseInternalError {
    pub message: String,
}

// #[derive(Diagnostic, Debug, Error)]
// #[error("Unexpected token tree")]
// pub struct UnexpectedTokenTree {
//     #[source_code]
//     pub src: String,

//     pub token_tree: OwnedTokenTree,

//     #[label]
//     pub err_span: SourceSpan,
// }

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

    #[error("SyntaxError: {cause} must be inside a loop")]
    BreakOrContinueOutsideLoop {
        #[source_code]
        src: String,
        cause: String,
        #[label = "here"]
        err_span: SourceSpan,
    },

    #[error("InternalError: {message}")]
    InternalError { message: String },
}
