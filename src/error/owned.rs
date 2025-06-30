use crate::{
    lex::{Token, TokenKind},
    parse::{Atom, Op, TokenTree, TokenTreeInner},
};

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedToken {
    pub kind: TokenKind,
    pub offset: usize,
    pub origin: String,
}

impl From<Token<'_>> for OwnedToken {
    fn from(value: Token<'_>) -> Self {
        Self {
            kind: value.kind,
            offset: value.offset,
            origin: value.origin.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OwnedAtom {
    String(String),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(String),
    Super,
    This,
}

impl From<Atom<'_>> for OwnedAtom {
    fn from(value: Atom<'_>) -> Self {
        match value {
            Atom::Bool(b) => Self::Bool(b),
            Atom::Nil => Self::Nil,
            Atom::Ident(i) => Self::Ident(i.to_string()),
            Atom::Number(n) => Self::Number(n),
            Atom::String(s) => Self::String(s.to_string()),
            Atom::Super => Self::Super,
            Atom::This => Self::This,
        }
    }
}

#[derive(Debug, Clone)]
enum OwnedTokenTreeInner {
    Atom(OwnedAtom),
    Cons(Op, Vec<OwnedTokenTree>),
    Fun {
        name: OwnedAtom,
        parameters: Vec<OwnedToken>,
        body: Box<OwnedTokenTree>,
    },
    Call {
        callee: Box<OwnedTokenTree>,
        arguments: Vec<OwnedTokenTree>,
    },
    If {
        condition: Box<OwnedTokenTree>,
        yes: Box<OwnedTokenTree>,
        no: Option<Box<OwnedTokenTree>>,
    },
    Eof,
}

fn to_owned_vec<R, O>(vec: Vec<R>) -> Vec<O>
where
    R: Clone + Into<O>,
{
    vec.iter().cloned().map(|item| item.into()).collect()
}

impl From<TokenTree<'_>> for OwnedTokenTree {
    fn from(value: TokenTree<'_>) -> Self {
        let owned_inner = match value.inner {
            TokenTreeInner::Atom(atom) => OwnedTokenTreeInner::Atom(atom.into()),

            TokenTreeInner::Call { callee, arguments } => OwnedTokenTreeInner::Call {
                callee: Box::new((*callee).into()),
                arguments: to_owned_vec(arguments),
            },

            TokenTreeInner::Cons(op, operands) => {
                OwnedTokenTreeInner::Cons(op, to_owned_vec(operands))
            }

            TokenTreeInner::Fun {
                name,
                parameters,
                body,
            } => OwnedTokenTreeInner::Fun {
                name: name.into(),
                parameters: to_owned_vec(parameters),
                body: todo!(),
            },

            _ => {
                todo!()
            }
        };

        Self {
            inner: owned_inner,
            range: value.range,
        }
    }
}

// only used in error message
#[derive(Debug, Clone)]
pub struct OwnedTokenTree {
    pub inner: OwnedTokenTreeInner,
    pub range: (usize, usize),
}
