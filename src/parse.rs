use miette::{Context, Error, LabeledSpan};
use std::{borrow::Cow, collections::HashMap, fmt};

use crate::{
    error,
    lex::{Token, TokenKind},
    Lexer,
};

/// Defines the precedence levels for operators in the parser
/// Higher values indicate higher precedence
///
/// This enum replaces the raw integer binding power values with named constants,
/// making it easier to:
/// 1. Understand the precedence hierarchy
/// 2. Add new operators without having to manually assign precedence numbers
/// 3. Maintain the precedence relationships between operators
///
/// Operators with "Right" suffix are used for the right side of binary expressions
/// and are typically one level higher to enforce left-associativity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingPower {
    None = 0, // Base level (no binding power)

    SpecialCall = 1, // return and print statements

    // assignment should be expression
    // since it's right-associative, the right side should have higher precedence
    AssignmentLeft = 3,
    AssignmentRight = 2,

    LogicalOr = 4, // Logical OR operator (||)
    LogicalOrRight = 5,

    Comparison = 6, // Comparison operators (==, !=, <, >, <=, >=)
    ComparisonRight = 7,

    Term = 8, // Additive operators (+, -)
    TermRight = 9,

    Factor = 10, // Multiplicative operators (*, /)
    FactorRight = 11,

    Unary = 12, // Unary operators (-, !)

    Call = 14, // Function calls

    MemberAccess = 16, // Member/field access (.)
    MemberAccessRight = 17,
}

impl BindingPower {
    /// Returns the next higher precedence level
    /// Useful for incrementing binding power when needed, such as for right-associative operators
    /// or when creating custom precedence rules
    pub fn next_higher(&self) -> Self {
        match self {
            BindingPower::None => BindingPower::SpecialCall,
            BindingPower::SpecialCall => BindingPower::LogicalOr, // Special calls have low precedence
            BindingPower::AssignmentLeft => BindingPower::LogicalOr,
            BindingPower::AssignmentRight => BindingPower::AssignmentRight,
            BindingPower::LogicalOr => BindingPower::LogicalOrRight,
            BindingPower::LogicalOrRight => BindingPower::Comparison,
            BindingPower::Comparison => BindingPower::ComparisonRight,
            BindingPower::ComparisonRight => BindingPower::Term,
            BindingPower::Term => BindingPower::TermRight,
            BindingPower::TermRight => BindingPower::Factor,
            BindingPower::Factor => BindingPower::FactorRight,
            BindingPower::FactorRight => BindingPower::Unary,
            BindingPower::Unary => BindingPower::Call,
            BindingPower::Call => BindingPower::MemberAccess,
            BindingPower::MemberAccess => BindingPower::MemberAccessRight,
            BindingPower::MemberAccessRight => BindingPower::MemberAccessRight, // Highest level
        }
    }
}

#[derive(Debug, Default)]
pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}

pub struct Ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(Cow<'de, str>),
    Super,
    This,
}

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // NOTE: this feels more correct
            // Atom::String(s) => write!(f, "\"{s}\""),
            Atom::String(s) => write!(f, "{s}"),
            Atom::Number(n) => {
                if *n == n.trunc() {
                    // tests require that integers are printed as x.0
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Atom::Nil => write!(f, "nil"),
            Atom::Bool(b) => write!(f, "{b:?}"),
            Atom::Ident(i) => write!(f, "{i}"),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    And,
    Or,
    Equal,
    Call,
    For,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::LessEqual => "<=",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::And => "and",
                Op::Or => "or",
                Op::Call => "call",
                Op::Equal => "=",
                Op::For => "for",
                Op::Class => "class",
                Op::Print => "print",
                Op::Return => "return",
                Op::Field => ".",
                Op::Var => "var",
                Op::While => "while",
                Op::Group => "group",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenTree<'de> {
    pub inner: TokenTreeInner<'de>,
    pub range: (usize, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTreeInner<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Fun {
        name: Atom<'de>,
        parameters: Vec<Token<'de>>,
        body: Box<TokenTree<'de>>,
    },
    Call {
        callee: Box<TokenTree<'de>>,
        arguments: Vec<TokenTree<'de>>,
    },
    If {
        condition: Box<TokenTree<'de>>,
        yes: Box<TokenTree<'de>>,
        no: Option<Box<TokenTree<'de>>>,
    },
    Eof,
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl fmt::Display for TokenTreeInner<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTreeInner::Atom(i) => write!(f, "{}", i),
            TokenTreeInner::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {s}")?;
                }
                write!(f, ")")
            }
            TokenTreeInner::Fun {
                name,
                parameters,
                body,
            } => {
                write!(f, "(def {name}")?;
                for p in parameters {
                    write!(f, " {p}")?
                }
                write!(f, " {body})")
            }
            TokenTreeInner::Call { callee, arguments } => {
                write!(f, "(({callee})")?;
                for a in arguments {
                    write!(f, " {a}")?;
                }
                write!(f, ")")
            }
            TokenTreeInner::If { condition, yes, no } => {
                write!(f, "(if {condition} {yes}")?;
                if let Some(no) = no {
                    write!(f, " {no}")?;
                }
                write!(f, ")")
            }
            TokenTreeInner::Eof => {
                write!(f, "")
            }
        }
    }
}

fn prefix_binding_power(op: Op) -> ((), BindingPower) {
    match op {
        Op::Print | Op::Return => ((), BindingPower::SpecialCall), // Low precedence for statements
        Op::Bang | Op::Minus => ((), BindingPower::Unary),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Op) -> Option<(BindingPower, ())> {
    let res = match op {
        Op::Call => (BindingPower::Call, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: Op) -> Option<(BindingPower, BindingPower)> {
    let res = match op {
        // Logical operators (lowest precedence)
        Op::And | Op::Or => (BindingPower::LogicalOr, BindingPower::LogicalOrRight),

        // Comparison operators
        Op::BangEqual
        | Op::EqualEqual
        | Op::Less
        | Op::LessEqual
        | Op::Greater
        | Op::GreaterEqual => (BindingPower::Comparison, BindingPower::ComparisonRight),

        // Additive operators
        Op::Plus | Op::Minus => (BindingPower::Term, BindingPower::TermRight),

        // Multiplicative operators
        Op::Star | Op::Slash => (BindingPower::Factor, BindingPower::FactorRight),

        // Member access (highest precedence)
        Op::Field => (BindingPower::MemberAccessRight, BindingPower::MemberAccess),

        Op::Equal => {
            // Assignment operator
            (BindingPower::AssignmentLeft, BindingPower::AssignmentRight)
        }

        _ => return None,
    };
    Some(res)
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse_expression(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_expression_within(BindingPower::None)
    }

    pub fn parse(&mut self) -> Result<Vec<TokenTree<'de>>, Error> {
        let mut token_trees = vec![];
        loop {
            match self.parse_statement_within(BindingPower::None) {
                Ok(token) => {
                    if matches!(
                        &token,
                        TokenTree {
                            inner: TokenTreeInner::Eof,
                            ..
                        }
                    ) {
                        break;
                        // eof
                    } else {
                        token_trees.push(token);
                    }
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(token_trees)
    }

    /// The usizes in Result is the range
    pub fn parse_block(&mut self) -> Result<(TokenTree<'de>, usize, usize), Error> {
        let left_brace_token = self.lexer.expect(TokenKind::LeftBrace, "missing {")?;

        // TODO: in a loop with semicolons? depends on class vs body
        let block = self.parse_statement_within(BindingPower::None)?;
        let right_brace_token = self.lexer.expect(TokenKind::RightBrace, "missing }")?;

        Ok((block, left_brace_token.offset, right_brace_token.offset + 1))
    }

    /// The usize in Result is the offset of right paren, the end position of the call expression
    pub fn parse_fn_call_arguments(&mut self) -> Result<(Vec<TokenTree<'de>>, usize), Error> {
        let mut arguments = Vec::new();

        match self.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                offset,
                ..
            })) => Ok((arguments, offset + 1)),

            _ => {
                let mut end;

                loop {
                    let argument = self
                        .parse_expression_within(BindingPower::None)
                        .wrap_err_with(|| {
                            format!("in argument #{} of function call", arguments.len() + 1)
                        })?;

                    arguments.push(argument);

                    let token = self
                        .lexer
                        .expect_where(
                            |token| matches!(token.kind, TokenKind::RightParen | TokenKind::Comma),
                            "continuing argument list",
                        )
                        .wrap_err("in argument list of function call")?;
                    end = token.offset;

                    if token.kind == TokenKind::RightParen {
                        break;
                    }
                }

                Ok((arguments, end + 1))
            }
        }
    }

    fn maybe_semicolon(&mut self) {
        match self.lexer.peek() {
            Some(Ok(peek)) if peek.kind == TokenKind::Semicolon => {
                let _ = self.lexer.next();
            }
            _ => {}
        }
    }

    pub fn parse_statement_within(
        &mut self,
        // looking_for: Option<(Op, usize)>,
        min_bp: BindingPower,
    ) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => {
                return Ok(TokenTree {
                    inner: TokenTreeInner::Eof,
                    range: (self.lexer.offset(), self.lexer.offset()),
                })
            }
            Some(Err(e)) => {
                // let msg = looking_for_msg();

                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            // atoms
            Token {
                kind: TokenKind::String,
                origin,
                offset,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::String(Token::unescape(origin))),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Number(n),
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Number(n)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::True,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Bool(true)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::False,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Bool(false)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Nil,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Nil),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Ident,
                origin,
                offset,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Ident(Cow::Borrowed(origin))),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Super,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Super),
                range: (offset, offset + origin.len()),
            },

            Token {
                kind: TokenKind::This,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::This),
                range: (offset, offset + origin.len()),
            },

            // Token {
            //     kind: TokenKind::Ident,
            //     origin,
            //     offset,
            // } => TokenTree {
            //     inner: TokenTreeInner::Atom(Atom::Ident(origin)),
            //     range: (offset, offset + origin.len()),
            // },

            // Token {
            //     kind: TokenKind::Super,
            //     origin,
            //     offset,
            // } => TokenTree {
            //     inner: TokenTreeInner::Atom(Atom::Super),
            //     range: (offset, offset + origin.len()),
            // },

            // Token {
            //     kind: TokenKind::This,
            //     origin,
            //     offset,
            // } => TokenTree {
            //     inner: TokenTreeInner::Atom(Atom::This),
            //     range: (offset, offset + origin.len()),
            // },
            Token {
                kind: TokenKind::LeftParen,
                origin,
                offset,
            } => {
                let lhs = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in bracketed expression")?;

                let right_paren_token = self
                    .lexer
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression",
                    )
                    .wrap_err("after bracketed expression")?;

                // lhs
                // TokenTree::Cons(Op::Group, vec![lhs])
                TokenTree {
                    inner: TokenTreeInner::Cons(Op::Group, vec![lhs]),
                    range: (offset, right_paren_token.offset + 1),
                }
            }

            // unary prefix expression
            Token {
                kind: TokenKind::Print | TokenKind::Return,
                offset,
                origin,
            } => {
                let op = match lhs.kind {
                    TokenKind::Print => Op::Print,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {op:?}"))?;

                self.maybe_semicolon();

                // return Ok(TokenTree::Cons(op, vec![rhs]));
                return Ok(TokenTree {
                    range: (offset, rhs.range.1),
                    inner: TokenTreeInner::Cons(op, vec![rhs]),
                });
            }

            Token {
                kind: TokenKind::Bang | TokenKind::Minus,
                offset,
                origin,
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err("in right-hand side")?;
                // TokenTree::Cons(op, vec![rhs])
                TokenTree {
                    range: (offset, rhs.range.1),
                    inner: TokenTreeInner::Cons(op, vec![rhs]),
                }
            }

            Token {
                kind: TokenKind::For,
                offset,
                origin,
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in for loop condition")?;

                let init = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in init condition of for loop")?;
                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let cond = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in loop condition of for loop")?;
                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let inc = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in incremental condition of for loop")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop condition")?;

                self.lexer
                    .expect(TokenKind::LeftBrace, "missing {")
                    .wrap_err("in for loop")?;

                let (block, block_start, block_end) =
                    self.parse_block().wrap_err("in body of for loop")?;

                // return Ok(TokenTree::Cons(Op::For, vec![init, cond, inc, block]));
                return Ok(TokenTree {
                    inner: TokenTreeInner::Cons(Op::For, vec![init, cond, inc, block]),
                    range: (offset, block_end),
                });
            }

            Token {
                kind: TokenKind::While,
                offset,
                origin,
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in while loop condition")?;

                let cond = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err_with(|| format!("in condition of while loop"))?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in while loop condition")?;

                let (block, block_start, block_end) =
                    self.parse_block().wrap_err("in body of while loop")?;

                // return Ok(TokenTree::Cons(Op::While, vec![cond, block]));
                return Ok(TokenTree {
                    inner: TokenTreeInner::Cons(Op::While, vec![cond, block]),
                    range: (offset, block_end),
                });
            }

            Token {
                kind: TokenKind::Var,
                offset,
                origin,
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in variable assignment")?;
                assert_eq!(token.kind, TokenKind::Ident);

                // let ident = TokenTree::Atom(Atom::Ident(token.origin));
                let ident: TokenTree<'de> = TokenTree {
                    inner: TokenTreeInner::Atom(Atom::Ident(Cow::Borrowed(token.origin))),
                    range: (token.offset, token.offset + token.origin.len()),
                };

                // Prepare variable declaration result
                let result = if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Equal,
                        ..
                    }))
                ) {
                    // Found equals sign, parse initialization expression
                    self.lexer
                        .expect(TokenKind::Equal, "missing =")
                        .wrap_err("in variable assignment")?;

                    let second = self
                        .parse_expression_within(BindingPower::None)
                        .wrap_err("in variable assignment expression")?;

                    // Variable declaration with initialization expression
                    TokenTree {
                        range: (offset, second.range.1),
                        inner: TokenTreeInner::Cons(Op::Var, vec![ident, second]),
                    }
                } else {
                    // Variable declaration without initialization expression
                    TokenTree {
                        range: (offset, ident.range.1),
                        inner: TokenTreeInner::Cons(Op::Var, vec![ident]),
                    }
                };

                // Handle semicolon in a unified way
                self.maybe_semicolon();

                return Ok(result);
            }

            Token {
                kind: TokenKind::Class,
                offset,
                origin,
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in class name")?;
                assert_eq!(token.kind, TokenKind::Ident);

                // let ident = TokenTree::Atom(Atom::Ident(token.origin));
                let ident = TokenTree {
                    inner: TokenTreeInner::Atom(Atom::Ident(Cow::Borrowed(token.origin))),
                    range: (token.offset, token.offset + token.origin.len()),
                };

                let (block, block_start, block_end) =
                    self.parse_block().wrap_err("in class definition")?;

                // return Ok(TokenTree::Cons(Op::Class, vec![ident, block]));

                return Ok(TokenTree {
                    inner: TokenTreeInner::Cons(Op::Class, vec![ident, block]),
                    range: (offset, block_end),
                });
            }

            Token {
                kind: TokenKind::Fun,
                offset,
                origin,
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in function name declaration")?;
                assert_eq!(token.kind, TokenKind::Ident);
                let name = token.origin;
                let ident = Atom::Ident(Cow::Borrowed(token.origin));

                let mut parameters = Vec::new();

                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                if matches!(
                    self.lexer.next(),
                    Some(Ok(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }))
                ) {
                    // immediate parameter list end
                } else {
                    loop {
                        let parameter = self
                            .lexer
                            .expect(TokenKind::Ident, "unexpected token")
                            .wrap_err_with(|| {
                                format!("in parameter #{} of function {name}", parameters.len() + 1)
                            })?;

                        parameters.push(parameter);

                        let token = self
                            .lexer
                            .expect_where(
                                |token| {
                                    matches!(token.kind, TokenKind::RightParen | TokenKind::Comma)
                                },
                                "continuing parameter list",
                            )
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenKind::RightParen {
                            break;
                        }
                    }
                }

                let (block, block_start, block_end) = self
                    .parse_block()
                    .wrap_err_with(|| format!("in body of function {name}"))?;

                // return Ok(TokenTree::Fun {
                //     name: ident,
                //     parameters,
                //     body: Box::new(block),
                // });

                return Ok(TokenTree {
                    inner: TokenTreeInner::Fun {
                        name: ident,
                        parameters,
                        body: Box::new(block),
                    },
                    range: (offset, block_end),
                });
            }

            Token {
                kind: TokenKind::If,
                offset,
                origin,
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err("in if condition")?;

                let cond = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in if condition")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in if condition")?;

                // self.lexer
                //     .expect(TokenKind::LeftBrace, "missing {")
                //     .wrap_err("in if condition")?;

                // let block = self.parse_within(0).wrap_err("in block of if")?;
                // self.lexer
                //     .expect(TokenKind::RightBrace, "missing }")
                //     .wrap_err("in if condition")?;
                let (block, block_start, block_end) =
                    self.parse_block().wrap_err("in body of if")?;

                let mut otherwise = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        offset: _,
                        origin: _
                    }))
                ) {
                    self.lexer.next();

                    otherwise = Some(self.parse_block().wrap_err("in body of else")?);
                }

                // return Ok(TokenTree::If {
                //     condition: Box::new(cond),
                //     yes: Box::new(block),
                //     no: otherwise.map(Box::new),
                // });

                let end = otherwise
                    .as_ref()
                    .map_or(block_end, |otherwise| otherwise.2);

                return Ok(TokenTree {
                    inner: TokenTreeInner::If {
                        condition: Box::new(cond),
                        yes: Box::new(block),
                        no: otherwise.map(|(otherwise_block, _, _)| Box::new(otherwise_block)),
                    },
                    range: (offset, end),
                });
            }

            token => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        token.offset..token.offset + token.origin.len(),
                        "here"
                    )],
                    help = format!("Unexpected {token:?}"),
                    "Expected a statement"
                )
                .with_source_code(self.whole.to_string()))
            }
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in place of expected operator");
            }

            let (op, &op_start) = match op.map(|res| res.as_ref().expect("handled Err above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::RightParen
                        | TokenKind::Comma
                        | TokenKind::Semicolon
                        | TokenKind::RightBrace,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    offset,
                    ..
                }) => (Op::Call, offset),
                Some(Token {
                    kind: TokenKind::Dot,
                    offset,
                    ..
                }) => (Op::Field, offset),
                Some(Token {
                    kind: TokenKind::Minus,
                    offset,
                    ..
                }) => (Op::Minus, offset),
                Some(Token {
                    kind: TokenKind::Plus,
                    offset,
                    ..
                }) => (Op::Plus, offset),
                Some(Token {
                    kind: TokenKind::Star,
                    offset,
                    ..
                }) => (Op::Star, offset),
                Some(Token {
                    kind: TokenKind::BangEqual,
                    offset,
                    ..
                }) => (Op::BangEqual, offset),

                Some(Token {
                    kind: TokenKind::EqualEqual,
                    offset,
                    ..
                }) => (Op::EqualEqual, offset),
                Some(Token {
                    kind: TokenKind::LessEqual,
                    offset,
                    ..
                }) => (Op::LessEqual, offset),
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    offset,
                    ..
                }) => (Op::GreaterEqual, offset),
                Some(Token {
                    kind: TokenKind::Less,
                    offset,
                    ..
                }) => (Op::Less, offset),
                Some(Token {
                    kind: TokenKind::Greater,
                    offset,
                    ..
                }) => (Op::Greater, offset),
                Some(Token {
                    kind: TokenKind::Slash,
                    offset,
                    ..
                }) => (Op::Slash, offset),
                Some(Token {
                    kind: TokenKind::And,
                    offset,
                    ..
                }) => (Op::And, offset),
                Some(Token {
                    kind: TokenKind::Or,
                    offset,
                    ..
                }) => (Op::Or, offset),
                Some(Token {
                    kind: TokenKind::Equal,
                    offset,
                    ..
                }) => (Op::Equal, offset),

                Some(token) => {
                    return Err(error::SyntaxError {
                        src: self.whole.to_string(),
                        message: format!("Expected an infix operator, found {}", token.origin),
                        err_span: (token.offset..token.offset + token.origin.len()).into(),
                    }
                    .into());
                }
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = if op == Op::Call {
                    // TokenTree::Call {
                    //     callee: Box::new(lhs),
                    //     arguments: self
                    //         .parse_fn_call_arguments()
                    //         .wrap_err("in function call arguments")?,
                    // }
                    let (arguments, end) = self
                        .parse_fn_call_arguments()
                        .wrap_err("in function call arguments")?;

                    TokenTree {
                        inner: TokenTreeInner::Call {
                            callee: Box::new(lhs),
                            arguments,
                        },
                        range: (op_start, end),
                    }
                } else {
                    // TokenTree::Cons(op, vec![lhs])

                    TokenTree {
                        range: (op_start, lhs.range.1),
                        inner: TokenTreeInner::Cons(op, vec![lhs]),
                    }
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;
                // lhs = TokenTree::Cons(op, vec![lhs, rhs]);

                if matches!(rhs.inner, TokenTreeInner::Eof) {
                    return Err(error::Eof {
                        src: self.whole.to_string(),
                        err_span: (rhs.range.0 - 1..rhs.range.0 - 1).into(),
                    }
                    .into());
                }

                lhs = TokenTree {
                    range: (lhs.range.0, rhs.range.1),
                    inner: TokenTreeInner::Cons(op, vec![lhs, rhs]),
                };

                continue;
            }
            break;
        }

        self.maybe_semicolon();

        Ok(lhs)
    }

    pub fn parse_expression_within(
        &mut self,
        min_bp: BindingPower,
    ) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => {
                return Ok(TokenTree {
                    inner: TokenTreeInner::Eof,
                    range: (self.lexer.offset(), self.lexer.offset()),
                })
            }
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            // atoms
            Token {
                kind: TokenKind::String,
                origin,
                offset,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::String(Token::unescape(origin))),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Number(n),
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Number(n)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::True,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Bool(true)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::False,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Bool(false)),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Nil,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Nil),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Ident,
                origin,
                offset,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Ident(Cow::Borrowed(origin))),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Super,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::Super),
                range: (offset, offset + origin.len()),
            },

            Token {
                kind: TokenKind::This,
                offset,
                origin,
            } => TokenTree {
                inner: TokenTreeInner::Atom(Atom::This),
                range: (offset, offset + origin.len()),
            },

            // group
            Token {
                kind: TokenKind::LeftParen,
                offset,
                origin,
            } => {
                let lhs = self
                    .parse_expression_within(BindingPower::None)
                    .wrap_err("in bracketed expression")?;

                let right_paren_token = self
                    .lexer
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression terminator",
                    )
                    .wrap_err("after bracketed expression")?;

                TokenTree {
                    inner: TokenTreeInner::Cons(Op::Group, vec![lhs]),
                    range: (offset, right_paren_token.offset + 1),
                }
            }

            // unary prefix expression
            Token {
                kind: TokenKind::Bang | TokenKind::Minus,
                offset,
                origin,
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err("in right-hand side")?;
                // TokenTree::Cons(op, vec![rhs])
                TokenTree {
                    range: (offset, rhs.range.1),
                    inner: TokenTreeInner::Cons(op, vec![rhs]),
                }
            }

            token => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        token.offset..token.offset + token.origin.len(),
                        "here"
                    )],
                    help = format!("Unexpected {token:?}"),
                    "Expected an expression"
                )
                .with_source_code(self.whole.to_string()));
            }
        };

        loop {
            let op = self.lexer.peek();

            if op.map_or(false, |op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("checked Some above")
                    .expect_err("checked Err above"))
                .wrap_err("in place of expected operator");
            }
            let (op, &op_start) = match op.map(|res| res.as_ref().expect("handled Err above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::RightParen
                        | TokenKind::Comma
                        | TokenKind::Semicolon
                        | TokenKind::RightBrace,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    offset,
                    ..
                }) => (Op::Call, offset),
                Some(Token {
                    kind: TokenKind::Dot,
                    offset,
                    ..
                }) => (Op::Field, offset),
                Some(Token {
                    kind: TokenKind::Minus,
                    offset,
                    ..
                }) => (Op::Minus, offset),
                Some(Token {
                    kind: TokenKind::Plus,
                    offset,
                    ..
                }) => (Op::Plus, offset),
                Some(Token {
                    kind: TokenKind::Star,
                    offset,
                    ..
                }) => (Op::Star, offset),
                Some(Token {
                    kind: TokenKind::BangEqual,
                    offset,
                    ..
                }) => (Op::BangEqual, offset),

                Some(Token {
                    kind: TokenKind::EqualEqual,
                    offset,
                    ..
                }) => (Op::EqualEqual, offset),
                Some(Token {
                    kind: TokenKind::LessEqual,
                    offset,
                    ..
                }) => (Op::LessEqual, offset),
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    offset,
                    ..
                }) => (Op::GreaterEqual, offset),
                Some(Token {
                    kind: TokenKind::Less,
                    offset,
                    ..
                }) => (Op::Less, offset),
                Some(Token {
                    kind: TokenKind::Greater,
                    offset,
                    ..
                }) => (Op::Greater, offset),
                Some(Token {
                    kind: TokenKind::Slash,
                    offset,
                    ..
                }) => (Op::Slash, offset),
                Some(Token {
                    kind: TokenKind::And,
                    offset,
                    ..
                }) => (Op::And, offset),
                Some(Token {
                    kind: TokenKind::Or,
                    offset,
                    ..
                }) => (Op::Or, offset),
                Some(Token {
                    kind: TokenKind::Equal,
                    offset,
                    ..
                }) => (Op::Equal, offset),
                Some(token) => {
                    return Err(error::SyntaxError {
                        src: self.whole.to_string(),
                        message: format!("Expected an infix operator, found {}", token.origin),
                        err_span: (token.offset..token.offset + token.origin.len()).into(),
                    }
                    .into());
                }
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => {
                        let (arguments, end) = self
                            .parse_fn_call_arguments()
                            .wrap_err("in function call arguments")?;

                        TokenTree {
                            inner: TokenTreeInner::Call {
                                callee: Box::new(lhs),
                                arguments,
                            },
                            range: (op_start, end),
                        }
                    }
                    _ => TokenTree {
                        range: (op_start, lhs.range.1),
                        inner: TokenTreeInner::Cons(op, vec![lhs]),
                    },
                };

                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    // TODO: ternary
                    //     let mhs = self.parse_within(0);
                    //     assert_eq!(lexer.next(), Token::Op(':'));
                    //     let rhs = self.parse_within(r_bp);
                    //     S::Cons(op, vec![lhs, mhs, rhs])
                    _ => {
                        let rhs = self
                            .parse_expression_within(r_bp)
                            .wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;

                        if matches!(rhs.inner, TokenTreeInner::Eof) {
                            return Err(miette::miette!(
                                labels = vec![LabeledSpan::at(rhs.range.0..rhs.range.1, "here")],
                                help = format!("Unexpected end of input after {lhs} {op}"),
                                "Expected an expression"
                            )
                            .with_source_code(self.whole.to_string()));
                        }

                        // TokenTree::Cons(op, vec![lhs, rhs])
                        TokenTree {
                            range: (lhs.range.0, rhs.range.1),
                            inner: TokenTreeInner::Cons(op, vec![lhs, rhs]),
                        }
                    }
                };

                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

impl<'de> Iterator for Parser<'de> {
    type Item = Result<TokenTree<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.parse_statement_within(BindingPower::None) {
            Ok(statement) => {
                if matches!(
                    &statement,
                    TokenTree {
                        inner: TokenTreeInner::Eof,
                        ..
                    }
                ) {
                    return None;
                }
                Some(Ok(statement))
            }
            Err(err) => Some(Err(err)),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn basic_parser() {
//         let input = r#"
//         96 + "baz";
//         "#;

//         let mut parser = Parser::new(input);

//         let res = parser.parse();
//         println!("{res:?}");
//     }
// }
