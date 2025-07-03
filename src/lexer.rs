use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use std::{borrow::Cow, fmt};
use thiserror::Error;

use crate::error::{Eof, SingleTokenError, StringTerminationError};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub kind: TokenKind,
    pub offset: usize,
    pub origin: &'de str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Break,
    Continue,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let origin = self.origin;
        match self.kind {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {origin} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {origin} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {origin} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {origin} null"),
            TokenKind::Comma => write!(f, "COMMA {origin} null"),
            TokenKind::Dot => write!(f, "DOT {origin} null"),
            TokenKind::Minus => write!(f, "MINUS {origin} null"),
            TokenKind::Plus => write!(f, "PLUS {origin} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {origin} null"),
            TokenKind::Star => write!(f, "STAR {origin} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {origin} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {origin} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {origin} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {origin} null"),
            TokenKind::Less => write!(f, "LESS {origin} null"),
            TokenKind::Greater => write!(f, "GREATER {origin} null"),
            TokenKind::Slash => write!(f, "SLASH {origin} null"),
            TokenKind::Bang => write!(f, "BANG {origin} null"),
            TokenKind::Equal => write!(f, "EQUAL {origin} null"),
            TokenKind::String => write!(f, "STRING {origin} {}", Token::unescape(origin)),
            TokenKind::Ident => write!(f, "IDENTIFIER {origin} null"),
            TokenKind::Number(n) => {
                if n == n.trunc() {
                    // tests require that integers are printed as x.0
                    write!(f, "NUMBER {origin} {n}.0")
                } else {
                    write!(f, "NUMBER {origin} {n}")
                }
            }
            TokenKind::And => write!(f, "AND {origin} null"),
            TokenKind::Class => write!(f, "CLASS {origin} null"),
            TokenKind::Else => write!(f, "ELSE {origin} null"),
            TokenKind::False => write!(f, "FALSE {origin} null"),
            TokenKind::For => write!(f, "FOR {origin} null"),
            TokenKind::Fun => write!(f, "FUN {origin} null"),
            TokenKind::If => write!(f, "IF {origin} null"),
            TokenKind::Nil => write!(f, "NIL {origin} null"),
            TokenKind::Or => write!(f, "OR {origin} null"),
            TokenKind::Print => write!(f, "PRINT {origin} null"),
            TokenKind::Return => write!(f, "RETURN {origin} null"),
            TokenKind::Super => write!(f, "SUPER {origin} null"),
            TokenKind::This => write!(f, "THIS {origin} null"),
            TokenKind::True => write!(f, "TRUE {origin} null"),
            TokenKind::Var => write!(f, "VAR {origin} null"),
            TokenKind::While => write!(f, "WHILE {origin} null"),
            TokenKind::Break => write!(f, "BREAK {origin} null"),
            TokenKind::Continue => write!(f, "CONTINUE {origin} null"),
        }
    }
}

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> Cow<'de, str> {
        // lox has no escaping, so just remove the "
        // Since it has no escaping, strings cannot contain " characters.
        Cow::Borrowed(s.trim_matches('"'))
    }
}

#[derive(Debug, Default)]
pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, miette::Error>>,
}
impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }
}

impl<'de> Lexer<'de> {
    pub fn offset(&self) -> usize {
        self.byte
    }

    pub fn expect(
        &mut self,
        expected: TokenKind,
        unexpected: &str,
    ) -> Result<Token<'de>, miette::Error> {
        self.expect_where(|next| next.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'de>, miette::Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.origin.len(),
                    "here"
                )],
                help = format!("Expected {token:?}"),
                "{unexpected}",
            )
            .with_source_code(self.whole.to_string())),
            // .wrap_err_with(looking_for_msg)
            Some(Err(e)) => Err(e),
            None => Err(Eof {
                src: self.whole.to_string(),
                err_span: SourceSpan::from(self.byte..self.byte),
            }
            .into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, miette::Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    /// Once the iterator returns `Err`, it will only return `None`.
    fn next(&mut self) -> Option<Self::Item> {
        // self.peeked = None;
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let c_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.byte += c.len_utf8();
            self.rest = chars.as_str();

            enum Started {
                Slash,
                String,
                Number,
                Ident,
                IfEqualElse(TokenKind, TokenKind),
            }

            let just = move |token: TokenKind| {
                Some(Ok(Token {
                    kind: token,
                    offset: c_at,
                    origin: c_str,
                }))
            };

            let started = match c {
                '(' => return just(TokenKind::LeftParen),
                ')' => return just(TokenKind::RightParen),
                '{' => return just(TokenKind::LeftBrace),
                '}' => return just(TokenKind::RightBrace),
                ',' => return just(TokenKind::Comma),
                '.' => return just(TokenKind::Dot),
                '-' => return just(TokenKind::Minus),
                '+' => return just(TokenKind::Plus),
                ';' => return just(TokenKind::Semicolon),
                '*' => return just(TokenKind::Star),
                '/' => Started::Slash,
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()));
                }
            };

            match started {
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        // this is a comment!

                        let line_end = self.rest.find('\n').unwrap_or_else(|| self.rest.len());
                        self.byte += line_end;
                        self.rest = &self.rest[line_end..];

                        continue;
                    } else {
                        return Some(Ok(Token {
                            kind: TokenKind::Slash,
                            offset: c_at,
                            origin: c_str,
                        }));
                    }
                }
                Started::String => {
                    // println!("string {c_onwards}");
                    if let Some(end) = self.rest.find('"') {
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        return Some(Ok(Token {
                            kind: TokenKind::String,
                            offset: c_at,
                            origin: literal,
                        }));
                    } else {
                        let err = StringTerminationError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        }
                        .into();

                        // println!("no end found {c_onwards} {:?}", self.rest);
                        self.byte += self.rest.len();
                        self.rest = &self.rest[self.rest.len()..];
                        return Some(Err(err));
                    }
                }
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let mut literal = &c_onwards[..first_non_digit];

                    let mut dotted = literal.splitn(3, '.');

                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            literal = &literal[..one.len()];
                        }
                        _ => {}
                    }

                    let extra_bytes = literal.len() - c.len_utf8();

                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette!(
                                labels = vec![LabeledSpan::at(
                                    self.byte - literal.len()..self.byte,
                                    "this numeric literal"
                                )],
                                "{e}"
                            )
                            .with_source_code(self.whole.to_string())));
                        }
                    };
                    return Some(Ok(Token {
                        kind: TokenKind::Number(n),
                        offset: c_at,
                        origin: literal,
                    }));
                }

                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());

                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "print" => TokenKind::Print,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        "break" => TokenKind::Break,
                        "continue" => TokenKind::Continue,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token {
                        kind,
                        offset: c_at,
                        origin: literal,
                    }));
                }
                Started::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;

                    if self.rest.starts_with('=') {
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        return Some(Ok(Token {
                            kind: yes,
                            offset: c_at,
                            origin: span,
                        }));
                    }

                    return Some(Ok(Token {
                        kind: no,
                        offset: c_at,
                        origin: c_str,
                    }));
                }
            };
        }
    }
}
