use miette::{Context, Error, LabeledSpan, WrapErr};
use rustc_hash::FxHashSet as HashSet;
use std::{borrow::Cow, collections::HashMap, fmt};

use crate::{
    ast::{
        self, BinaryExpression, BinaryExpressionInner, BlockStatement, BlockStatementInner,
        BoolLiteral, BoolLiteralInner, CallExpression, CallExpressionInner, ClassBody,
        ClassBodyInner, ClassBodyItem, ClassBodyItemInner, ClassDeclaration, ClassDeclarationInner,
        Declaration, DeclarationInner, Expression, ExpressionInner, ForInit, ForInitInner,
        ForStatement, ForStatementInner, FunctionDeclaration, FunctionDeclarationInner,
        GroupExpression, GroupExpressionInner, Identifier, IdentifierInner, IfStatement,
        IfStatementInner, Literal, LiteralInner, MemberExpression, MemberExpressionInner,
        NilLiteral, NilLiteralInner, NumberLiteral, NumberLiteralInner, Op, PrintStatementInner, Program, ProgramInner, Spanned, Statement,
        StatementInner, StringLiteral, StringLiteralInner, ThisExpression, ThisExpressionInner,
        TokenTree, TokenTreeInner, UnaryExpression, UnaryExpressionInner, VariableDeclaration,
        VariableDeclarationInner, Visitor, WhileStatement, WhileStatementInner,
    },
    error::{self, Eof},
    lexer::{Token, TokenKind},
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

    MemberAccess = 15, // Member/field access (.)
    MemberAccessRight = 16,
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

#[derive(Debug, PartialEq, Eq)]
pub enum ScopeType {
    Global,
    Module,
    Enclosed,
}
#[derive(Debug)]
pub struct Scope<'de> {
    pub scope_type: ScopeType,
    pub bindings: HashMap<Cow<'de, str>, (usize, usize)>,
}

impl<'de> Scope<'de> {
    pub fn new(scope_type: ScopeType) -> Self {
        Self {
            scope_type,
            bindings: HashMap::default(),
        }
    }

    pub fn is_global(&self) -> bool {
        self.scope_type == ScopeType::Global
    }

    pub fn is_module(&self) -> bool {
        self.scope_type == ScopeType::Module
    }

    pub fn is_enclosed(&self) -> bool {
        self.scope_type == ScopeType::Enclosed
    }
}

#[derive(Debug)]
struct FunctionContext {
    depth: u32,
}

#[derive(Debug)]
pub struct ParserState<'de> {
    scopes: Vec<Scope<'de>>,
    function_context: FunctionContext,
}
impl<'de> ParserState<'de> {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(ScopeType::Global)],
            function_context: FunctionContext { depth: 0 },
        }
    }

    pub fn push_scope(&mut self, scope_type: ScopeType) {
        self.scopes.push(Scope::new(scope_type))
    }

    pub fn pop_scope(&mut self) -> bool {
        self.scopes.pop().is_some()
    }

    pub fn current_scope(&self) -> Result<&Scope, Error> {
        self.scopes.last().ok_or_else(|| {
            error::ParseInternalError {
                message: "".to_string(),
            }
            .into()
        })
    }

    pub fn current_scope_mut(&mut self) -> Result<&mut Scope<'de>, Error> {
        self.scopes.last_mut().ok_or_else(|| {
            error::ParseInternalError {
                message: "".to_string(),
            }
            .into()
        })
    }
}

#[derive(Debug)]
pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
    state: ParserState<'de>,
}

fn prefix_binding_power(op: Op) -> ((), BindingPower) {
    match op {
        // Op::Print | Op::Return => ((), BindingPower::SpecialCall), // Low precedence for statements
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
            state: ParserState::new(),
        }
    }

    pub fn parse(&mut self) -> Result<Program<'de>, Error> {
        self.state.push_scope(ScopeType::Global);
        self.state.push_scope(ScopeType::Module);

        let mut statements = vec![];
        loop {
            match self.parse_statement() {
                Ok(Some(statement)) => {
                    statements.push(statement);
                }
                Ok(None) => {
                    break;
                }
                Err(err) => {
                    self.state.pop_scope();
                    return Err(err);
                }
            }
        }

        self.state.pop_scope();
        self.state.pop_scope();
        let program = Program {
            inner: ProgramInner { body: statements },
            range: (0, self.whole.len()),
        };
        Ok(program)
    }

    pub fn whole(&self) -> &str {
        self.whole
    }

    pub fn state(&self) -> &ParserState {
        &self.state
    }

    pub fn parse_expression(&mut self) -> Result<Option<Expression<'de>>, Error> {
        self.parse_expression_within(BindingPower::None)
    }

    pub fn parse_block(
        &mut self,
        processed_left_brace: Option<Token<'de>>,
        parameters: Option<&[Token<'de>]>, // for function
    ) -> Result<BlockStatement<'de>, Error> {
        let left_brace = match processed_left_brace {
            Some(token) => token,
            None => self
                .lexer
                .expect(TokenKind::LeftBrace, "missing {")
                .wrap_err("in block expression")?,
        };

        self.state.push_scope(ScopeType::Enclosed);

        if let Some(parameters) = parameters {
            let bindings = &mut self.state.current_scope_mut()?.bindings;
            for param in parameters {
                if bindings.contains_key(&Cow::Borrowed(param.origin)) {}
                bindings.insert(
                    Cow::Borrowed(param.origin),
                    (param.offset, param.offset + param.origin.len()),
                );
            }
        }

        let mut statements = vec![];

        let peek = self.lexer.peek();
        let is_right_brace = matches!(
            peek,
            Some(Ok(Token {
                kind: TokenKind::RightBrace,
                ..
            }))
        );

        if is_right_brace {
            // If the next token is a right brace, we can return an empty block
            let right_brace = self
                .lexer
                .next()
                .expect("checked above")
                .expect("checked above"); // consume the right brace
            let end = right_brace.offset; // +1 for the right brace
            let block = BlockStatement {
                inner: BlockStatementInner { statements },
                range: (left_brace.offset, end),
            };

            self.state.pop_scope();
            return Ok(block);
        }

        loop {
            let statement = self
                .parse_statement_expected()
                .wrap_err("in block statement")?;

            statements.push(statement);

            let next_token = self.lexer.peek();
            if next_token.is_none() {
                // return error in next iteration
                continue;
            }

            if next_token
                .unwrap()
                .as_ref()
                .map_or(false, |t| t.kind == TokenKind::RightBrace)
            {
                // If the next token is None or a right brace, we can stop parsing statements
                break;
            }
        }

        let start = left_brace.offset;

        let right_brace_token = self
            .lexer
            .expect(TokenKind::RightBrace, "missing }")
            .wrap_err("after block expression")?;

        let end = right_brace_token.offset + 1;

        let block = BlockStatement {
            inner: BlockStatementInner { statements },
            range: (start, end),
        };

        self.state.pop_scope();

        Ok(block)
    }

    /// The usize in Result is the offset of right paren, the end position of the call expression
    pub fn parse_fn_call_arguments(&mut self) -> Result<(Vec<Expression<'de>>, usize), Error> {
        let mut arguments = Vec::new();

        let peek = self.lexer.peek();

        let is_peek_right_paren = matches!(
            peek,
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                ..
            }))
        );

        if is_peek_right_paren {
            let right_paren = self.lexer.next().expect("checked in match arm")?;
            return Ok((arguments, right_paren.offset + 1));
        }

        let mut end;

        loop {
            let argument = self
                .parse_expression_within(BindingPower::None)
                .wrap_err_with(|| format!("in argument #{} of function call", arguments.len() + 1))?
                .expect("checked by the match arm");

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

    // return position of semicolon
    fn maybe_semicolon(&mut self) -> Option<usize> {
        match self.lexer.peek() {
            Some(Ok(peek)) if peek.kind == TokenKind::Semicolon => {
                let offset = peek.offset;
                let _ = self.lexer.next();
                Some(offset)
            }
            _ => None,
        }
    }

    pub fn parse_statement(&mut self) -> Result<Option<Statement<'de>>, Error> {
        let is_expr = !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::Var
                    | TokenKind::If
                    | TokenKind::While
                    | TokenKind::For
                    | TokenKind::LeftBrace
                    | TokenKind::Print
                    | TokenKind::Return
                    | TokenKind::Class
                    | TokenKind::Fun
                    | TokenKind::Break
                    | TokenKind::Continue,
                ..
            }))
        );

        if is_expr {
            let Some(expr) = self.parse_expression()? else {
                // if parse_expression returns None, it means there was an error
                return Ok(None);
            };

            let range = if let Some(semi_offset) = self.maybe_semicolon() {
                (expr.range.0, semi_offset)
            } else {
                (expr.range.0, expr.range.1)
            };

            return Ok(Some(Statement {
                range,
                inner: StatementInner::Expression(expr),
            }));
        }

        let start = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(None),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut statement: Statement<'de> = match start {
            Token {
                kind: TokenKind::Var,
                offset,
                origin,
            } => {
                // variable declaration
                let mut variable_declaration = self.parse_variable_declaration_statement(start)?;

                let mut range = variable_declaration.range;

                if let Some(semi_offset) = self.maybe_semicolon() {
                    range.1 = semi_offset;
                }

                // validate AST
                variable_declaration.validate(self)?;

                self.state.current_scope_mut()?.bindings.insert(
                    variable_declaration.id.name.clone(),
                    variable_declaration.id.range,
                );

                Statement {
                    range: variable_declaration.range,
                    inner: StatementInner::Declaration(Declaration {
                        range: variable_declaration.range,
                        inner: DeclarationInner::Variable(variable_declaration),
                    }),
                }
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
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("in if condition")?;

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in if condition")?;

                let peek = self.lexer.peek();

                let truthy_branch = self.parse_statement_expected().wrap_err("in body of if")?;

                let mut falsy_branch = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        offset: _,
                        origin: _
                    }))
                ) {
                    self.lexer.next();

                    let falsy_branch_statement = self
                        .parse_statement_expected()
                        .wrap_err("in body of else")?;

                    falsy_branch = Some(falsy_branch_statement);
                }

                let end = falsy_branch
                    .as_ref()
                    .map_or(truthy_branch.range.1, |falsy_branch_statement| {
                        falsy_branch_statement.range.1
                    });

                // validate AST
                match truthy_branch.inner {
                    StatementInner::Block(_)
                    | StatementInner::Expression(_)
                    | StatementInner::Return(_) => {}
                    _ => {
                        return Err(error::SyntaxError {
                            src: self.whole.to_string(),
                            message: "if statement body must be a block or an expression"
                                .to_string(),
                            err_span: (truthy_branch.range.0..truthy_branch.range.1).into(),
                        }
                        .into());
                    }
                }

                match &falsy_branch {
                    Some(fb) => match fb.inner {
                        StatementInner::Block(_)
                        | StatementInner::Expression(_)
                        | StatementInner::Break
                        | StatementInner::Continue
                        | StatementInner::If(_)
                        | StatementInner::Return(_) => {}

                        _ => {
                            return Err(error::SyntaxError {
                                src: self.whole.to_string(),
                                message: "else statement body cannot be this kind of statement"
                                    .to_string(),
                                err_span: (fb.range.0..fb.range.1).into(),
                            }
                            .into());
                        }
                    },
                    None => {}
                }

                let range = (start.offset, end);

                Statement {
                    inner: StatementInner::If(IfStatement {
                        inner: IfStatementInner {
                            test: Box::new(cond),
                            consequent: Box::new(truthy_branch),
                            alternate: falsy_branch.map(|fb| Box::new(fb)),
                        },
                        range,
                    }),
                    range,
                }
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
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("in condition of while loop")?;

                let right_paren_token = self
                    .lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in while loop condition")?;

                let block = self
                    .parse_statement_expected()
                    .wrap_err("in body of while loop")?;

                let range = (offset, block.range.1);

                Statement {
                    inner: StatementInner::While(WhileStatement {
                        inner: WhileStatementInner {
                            test: Box::new(cond),
                            body: Box::new(block),
                        },
                        range,
                    }),
                    range,
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

                let for_init = if let Some(Ok(Token {
                    kind: TokenKind::Semicolon,
                    ..
                })) = self.lexer.peek()
                {
                    None
                } else {
                    Some(
                        self.parse_for_init()
                            .wrap_err("in init condition of for loop")?,
                    )
                };
                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let test = if let Some(Ok(Token {
                    kind: TokenKind::Semicolon,
                    ..
                })) = self.lexer.peek()
                {
                    None
                } else {
                    Some(Box::new(
                        self.parse_expression_within_expected(BindingPower::None)
                            .wrap_err("in condition of for loop")?,
                    ))
                };
                self.lexer
                    .expect(TokenKind::Semicolon, "missing ;")
                    .wrap_err("in for loop condition")?;

                let update = if let Some(Ok(Token {
                    kind: TokenKind::RightParen,
                    ..
                })) = self.lexer.peek()
                {
                    None
                } else {
                    Some(Box::new(
                        self.parse_expression_within_expected(BindingPower::None)
                            .wrap_err("in increment condition of for loop")?,
                    ))
                };

                self.lexer
                    .expect(TokenKind::RightParen, "missing )")
                    .wrap_err("in for loop condition")?;

                let body = self
                    .parse_statement_expected()
                    .wrap_err("in body of for loop")?;

                let range = (offset, body.range.1);

                // validate AST
                match &body.inner {
                    StatementInner::Block(_) | StatementInner::Expression(_) => {}
                    _ => {
                        return Err(error::SyntaxError {
                            src: self.whole.to_string(),
                            message: "for loop body must be a block or an expression".to_string(),
                            err_span: (body.range.0..body.range.1).into(),
                        }
                        .into());
                    }
                }

                Statement {
                    inner: StatementInner::For(ForStatement {
                        inner: ForStatementInner {
                            init: for_init,
                            test,
                            update,
                            body: Box::new(body),
                        },
                        range,
                    }),
                    range,
                }
            }

            left_brace_token @ Token {
                kind: TokenKind::LeftBrace,
                origin,
                offset,
            } => {
                let block = self
                    .parse_block(Some(left_brace_token), None)
                    .wrap_err("in block expression")?;

                let range = (offset, block.range.1);

                Statement {
                    range,
                    inner: StatementInner::Block(block),
                }
            }

            Token {
                kind: TokenKind::Print,
                offset,
                origin,
            } => {
                let expr = self
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("on the right-hand side of print")?;

                let mut range = (offset, expr.range.1);

                if let Some(semi_offset) = self.maybe_semicolon() {
                    range.1 = semi_offset;
                }

                Statement {
                    range,
                    inner: StatementInner::Print(Spanned { inner: PrintStatementInner { expression: expr.clone() }, range: expr.range }),
                }
            }

            Token {
                kind: TokenKind::Return,
                offset,
                origin,
            } => {
                if self.state.function_context.depth == 0 {
                    return Err(error::SyntaxError {
                        src: self.whole.to_string(),
                        message: "return statement is not allowed outside of a function"
                            .to_string(),
                        err_span: (offset..offset + origin.len()).into(),
                    }
                    .into());
                }

                let peek = self.lexer.peek();

                if matches!(
                    peek,
                    Some(Ok(Token {
                        kind: TokenKind::Semicolon,
                        ..
                    }))
                ) {
                    // return without value
                    let semi_offset: usize = self
                        .lexer
                        .next()
                        .expect("checked in if matches")
                        .expect("checked in if matches")
                        .offset;
                    return Ok(Some(Statement {
                        range: (offset, semi_offset),
                        inner: StatementInner::Return(None),
                    }));
                }

                let expr = self
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("on the right-hand side of return")?;

                let mut range = (offset, expr.range.1);

                if let Some(semi_offset) = self.maybe_semicolon() {
                    range.1 = semi_offset;
                }

                Statement {
                    range,
                    inner: StatementInner::Return(Some(expr)),
                }
            }

            Token {
                kind: TokenKind::Class,
                offset,
                ..
            } => {
                let ident = self
                    .parse_identifier()
                    .wrap_err("in class name declaration")?;

                let has_super_class = matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Less,
                        ..
                    }))
                );

                let superclass = if has_super_class {
                    self.lexer.next();
                    Some(self.parse_identifier()?)
                } else {
                    None
                };

                let class_body = self.parse_class_body()?;

                let range = (offset, class_body.range.1);

                Statement {
                    range,
                    inner: StatementInner::Declaration(Declaration {
                        range,
                        inner: DeclarationInner::Class(ClassDeclaration {
                            range,
                            inner: ClassDeclarationInner {
                                id: ident,
                                superclass,
                                body: class_body,
                            },
                        }),
                    }),
                }
            }

            Token {
                kind: TokenKind::Fun,
                offset,
                origin,
            } => {
                let func_decl = self.parse_function(Some(offset))?;
                let range = func_decl.range;

                Statement {
                    inner: StatementInner::Declaration(Declaration {
                        range,
                        inner: DeclarationInner::Function(func_decl),
                    }),
                    range,
                }
            }

            Token {
                kind: TokenKind::Break,
                offset,
                origin,
            } => {
                let range = if let Some(semi_offset) = self.maybe_semicolon() {
                    (offset, semi_offset)
                } else {
                    (offset, offset + origin.len())
                };

                Statement {
                    range,
                    inner: StatementInner::Break,
                }
            }

            Token {
                kind: TokenKind::Continue,
                offset,
                origin,
            } => {
                let range = if let Some(semi_offset) = self.maybe_semicolon() {
                    (offset, semi_offset)
                } else {
                    (offset, offset + origin.len())
                };

                Statement {
                    range,
                    inner: StatementInner::Continue,
                }
            }

            _ => {
                // expression statement
                // let expr = self.parse_expression()?.expect("it cannot be None here because it's validated in the if in the beginning of the function");

                // let range = if let Some(semi_offset) = self.maybe_semicolon() {
                //     (start.offset, semi_offset)
                // } else {
                //     (start.offset, expr.range.1)
                // };

                // Statement {
                //     range,
                //     inner: StatementInner::Expression(expr),
                // }
                unreachable!()
            }
        };

        Ok(Some(statement))
    }

    pub fn parse_statement_expected(&mut self) -> Result<Statement<'de>, Error> {
        self.parse_statement().and_then(|opt| {
            opt.ok_or_else(|| {
                error::Eof {
                    src: self.whole.to_string(),
                    err_span: (self.whole.len() - 1, self.whole.len() - 1).into(),
                }
                .into()
            })
        })
    }

    pub fn parse_expression_within_expected(
        &mut self,
        min_bp: BindingPower,
    ) -> Result<Expression<'de>, Error> {
        self.parse_expression_within(min_bp)
            .and_then(|opt| opt.ok_or_else(|| error::Eof::new(self.whole).into()))
    }

    pub fn parse_expression_within(
        &mut self,
        min_bp: BindingPower,
    ) -> Result<Option<Expression<'de>>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Ok(None),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            // literals
            Token {
                kind: TokenKind::String,
                origin,
                offset,
            } => Expression {
                inner: ExpressionInner::Literal(Literal {
                    inner: LiteralInner::String(StringLiteral {
                        inner: StringLiteralInner(Token::unescape(origin)),
                        range: (offset, offset + origin.len()),
                    }),
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },

            Token {
                kind: TokenKind::Number(n),
                offset,
                origin,
            } => Expression {
                inner: ExpressionInner::Literal(Literal {
                    inner: LiteralInner::Number(NumberLiteral {
                        inner: NumberLiteralInner(n),
                        range: (offset, offset + origin.len()),
                    }),
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::True,
                offset,
                origin,
            } => Expression {
                inner: ExpressionInner::Literal(Literal {
                    inner: LiteralInner::Bool(BoolLiteral {
                        inner: BoolLiteralInner(true),
                        range: (offset, offset + origin.len()),
                    }),
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::False,
                offset,
                origin,
            } => Expression {
                inner: ExpressionInner::Literal(Literal {
                    inner: LiteralInner::Bool(BoolLiteral {
                        inner: BoolLiteralInner(false),
                        range: (offset, offset + origin.len()),
                    }),
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Nil,
                offset,
                origin,
            } => Expression {
                inner: ExpressionInner::Literal(Literal {
                    inner: LiteralInner::Nil(NilLiteral {
                        inner: NilLiteralInner,
                        range: (offset, offset + origin.len()),
                    }),
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },
            Token {
                kind: TokenKind::Ident,
                origin,
                offset,
            } => Expression {
                inner: ExpressionInner::Identifier(Identifier {
                    inner: IdentifierInner {
                        name: Cow::Borrowed(origin),
                    },
                    range: (offset, offset + origin.len()),
                }),
                range: (offset, offset + origin.len()),
            },
            // Token {
            //     kind: TokenKind::Super,
            //     offset,
            //     origin,
            // } => TokenTree {
            //     inner: TokenTreeInner::Atom(Atom::Super),
            //     range: (offset, offset + origin.len()),
            // },
            Token {
                kind: TokenKind::This,
                offset,
                origin,
            } => Expression {
                inner: ExpressionInner::This(ThisExpression {
                    range: (offset, offset + origin.len()),
                    inner: ThisExpressionInner,
                }),
                range: (offset, offset + origin.len()),
            },

            // group
            Token {
                kind: TokenKind::LeftParen,
                offset,
                origin,
            } => {
                let lhs = self
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("in bracketed expression")?;

                let right_paren_token = self
                    .lexer
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression terminator",
                    )
                    .wrap_err("after bracketed expression")?;

                let range = (offset, right_paren_token.offset + 1);
                Expression {
                    inner: ExpressionInner::Group(GroupExpression {
                        inner: GroupExpressionInner {
                            expression: Box::new(lhs),
                        },
                        range,
                    }),
                    range,
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
                    .parse_expression_within_expected(r_bp)
                    .wrap_err("in right-hand side")?;

                let range = (offset, rhs.range.1);

                Expression {
                    inner: ExpressionInner::Unary(UnaryExpression {
                        inner: UnaryExpressionInner {
                            operator: op,
                            right: Box::new(rhs),
                        },
                        range,
                    }),
                    range,
                }
            }

            token => {
                return Err(error::SyntaxError {
                    src: self.whole.to_string(),
                    message: format!("Expected an expression, found {}", token.origin),
                    err_span: (token.offset..token.offset + token.origin.len()).into(),
                }
                .into());
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
                }) => {
                    // Handle dot operator specially - member access
                    if BindingPower::MemberAccess < min_bp {
                        break;
                    }
                    self.lexer.next();

                    let rhs = self
                        .parse_expression_within_expected(BindingPower::MemberAccessRight)
                        .wrap_err_with(|| format!("on the right-hand side of {lhs}."))?;

                    let range = (lhs.range.0, rhs.range.1);

                    lhs = Expression {
                        range,
                        inner: ExpressionInner::Member(MemberExpression {
                            inner: MemberExpressionInner {
                                computed: false,
                                object: Box::new(lhs),
                                property: Box::new(rhs),
                            },
                            range,
                        }),
                    };

                    continue;
                }
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

                        Expression {
                            inner: ExpressionInner::Call(CallExpression {
                                inner: CallExpressionInner {
                                    callee: Box::new(lhs),
                                    arguments,
                                },
                                range: (op_start, end),
                            }),
                            range: (op_start, end),
                        }
                    }
                    // _ => TokenTree {
                    //     range: (op_start, lhs.range.1),
                    //     inner: TokenTreeInner::Cons(op, vec![lhs]),
                    // },
                    _ => unreachable!("postfix binding power should only be for Call"),
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
                            .parse_expression_within_expected(r_bp)
                            .wrap_err_with(|| format!("on the right-hand side of {lhs} {op}"))?;

                        let range = (lhs.range.0, rhs.range.1);

                        Expression {
                            range,
                            inner: ExpressionInner::Binary(BinaryExpression {
                                inner: BinaryExpressionInner {
                                    operator: op,
                                    left: Box::new(lhs),
                                    right: Box::new(rhs),
                                },
                                range,
                            }),
                        }
                    }
                };

                continue;
            }

            break;
        }

        Ok(Some(lhs))
    }

    fn parse_for_init(&mut self) -> Result<ForInit<'de>, Error> {
        let is_expr = !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::Var,
                ..
            }))
        );

        if is_expr {
            // If the next token is not a variable declaration, we assume it's an expression
            // and parse it as such.
            return self
                .parse_expression_within_expected(BindingPower::None)
                .map(|expr| ForInit {
                    range: expr.range,
                    inner: ForInitInner::Expression(expr),
                });
        }

        let start = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => return Err(error::Eof::new(self.whole).into()),
            Some(Err(e)) => {
                return Err(e).wrap_err("in for loop initialization");
            }
        };

        match start.kind {
            TokenKind::Var => {
                let decl = self
                    .parse_variable_declaration_statement(start)
                    .wrap_err("in for loop variable declaration")?;

                Ok(ForInit {
                    range: decl.range,
                    inner: ForInitInner::VariableDeclaration(decl),
                })
            }

            _ => {
                // try to parse an expression
                let expr = self
                    .parse_expression_within_expected(BindingPower::None)
                    .wrap_err("in for loop initialization expression")?;

                Ok(ForInit {
                    range: expr.range,
                    inner: ForInitInner::Expression(expr),
                })
            }
        }
    }

    fn parse_variable_declaration_statement(
        &mut self,
        start: Token<'de>,
    ) -> Result<VariableDeclaration<'de>, Error> {
        let offset = start.offset;

        let ident = self.parse_identifier().wrap_err("in variable assignment")?;
        let ident_range = ident.range;

        // Prepare variable declaration result
        let variable_declaration = if matches!(
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

            let init = self
                .parse_expression_within_expected(BindingPower::None)
                .wrap_err("in variable assignment expression")?;

            let range = (offset, init.range.1);

            VariableDeclaration {
                range,
                inner: VariableDeclarationInner {
                    id: ident,
                    init: Some(init),
                },
            }
        } else {
            let range = (offset, ident_range.1);
            // Variable declaration without initialization expression

            VariableDeclaration {
                range,
                inner: VariableDeclarationInner {
                    id: ident,
                    init: None,
                },
            }
        };

        Ok(variable_declaration)
    }

    fn parse_identifier(&mut self) -> Result<Identifier<'de>, Error> {
        let ident = self.lexer.expect(TokenKind::Ident, "expected identifier")?;

        let name = ident.origin;

        let ident = Identifier {
            range: (ident.offset, ident.offset + name.len()),
            inner: IdentifierInner {
                name: Cow::Borrowed(name),
            },
        };

        Ok(ident)
    }

    // if it's function, start is the start of `fun` keyword
    // if it's method, start is None
    fn parse_function(&mut self, start: Option<usize>) -> Result<FunctionDeclaration<'de>, Error> {
        let ident = self
            .parse_identifier()
            .wrap_err("in function name declaration")?;
        let name = ident.name.as_ref();

        let start = start.unwrap_or(ident.range.0);

        let mut parameters = Vec::new();
        let mut parameters_bindings: HashMap<&str, (usize, usize)> = HashMap::default();

        self.lexer
            .expect(TokenKind::LeftParen, "missing (")
            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                ..
            }))
        ) {
            // immediate parameter list end
            self.lexer.next(); // consume the right paren
        } else {
            loop {
                let parameter = self
                    .lexer
                    .expect(TokenKind::Ident, "unexpected token")
                    .wrap_err_with(|| {
                        format!("in parameter #{} of function {name}", parameters.len() + 1)
                    })?;

                if let Some(range) = parameters_bindings.get(parameter.origin) {
                    return Err(error::RedeclarationError {
                        src: self.whole().to_string(),
                        name: parameter.origin.to_string(),
                        err_span: (parameter.offset, parameter.origin.len()).into(),
                        existing_span: (range.0..range.1).into(),
                    }
                    .into());
                }

                parameters.push(parameter);
                parameters_bindings.insert(
                    parameter.origin,
                    (parameter.offset, parameter.offset + parameter.origin.len()),
                );

                let token = self
                    .lexer
                    .expect_where(
                        |token| matches!(token.kind, TokenKind::RightParen | TokenKind::Comma),
                        "continuing parameter list",
                    )
                    .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                if token.kind == TokenKind::RightParen {
                    break;
                }
            }
        }

        self.state.function_context.depth += 1;

        let block = self
            .parse_block(None, Some(&parameters))
            .wrap_err_with(|| format!("in body of function {name}"))?;

        if self.state.function_context.depth == 0 {
            // If we are at the top level, we should not allow function declarations
            return Err(error::ParseInternalError {
                message: "Function depths should not be 0 at this point".to_string(),
            }
            .into());
        }

        self.state.function_context.depth -= 1;

        let range = (start, block.range.1);

        Ok(FunctionDeclaration {
            range,
            inner: FunctionDeclarationInner {
                name: ident,
                parameters: parameters
                    .into_iter()
                    .map(|param| Identifier {
                        range: (param.offset, param.offset + param.origin.len()),
                        inner: IdentifierInner {
                            name: Cow::Borrowed(param.origin),
                        },
                    })
                    .collect(),
                body: Box::new(block),
            },
        })
    }

    fn parse_class_body(&mut self) -> Result<ClassBody<'de>, Error> {
        let left_brace = self
            .lexer
            .expect(TokenKind::LeftBrace, "expected left brace")?;

        let start = left_brace.offset;

        let mut body_items = vec![];

        // only support methods now
        loop {
            let peek = self.lexer.peek();

            let is_right_brace = matches!(
                peek,
                Some(Ok(Token {
                    kind: TokenKind::RightBrace,
                    ..
                }))
            );

            if is_right_brace {
                let right_brace = self
                    .lexer
                    .next()
                    .expect("checked above")
                    .expect("checked above"); // consume the right brace
                let end = right_brace.offset; // +1 for the right brace
                let body = ClassBody {
                    range: (start, end),
                    inner: ClassBodyInner(body_items),
                };

                return Ok(body);
            }

            let func = self.parse_function(None)?;
            body_items.push(ClassBodyItem {
                range: func.range,
                inner: ClassBodyItemInner::ClassMethod(func),
            });
        }
    }
}

// impl<'de> Iterator for Parser<'de> {
//     type Item = Result<Program<'de>, Error>;

//     fn next(&mut self) -> Option<Self::Item> {
//         match self.parse() {
//             Ok(program) => Some(Ok(program)),
//             Err(err) => Some(Err(err)),
//         }
//     }
// }

#[cfg(test)]
mod tests;
