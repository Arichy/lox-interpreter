use std::{borrow::Cow, fmt, marker::PhantomData, ops::Deref};

use miette::Error;

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{error, lexer::Token, parser::ParserState, Parser};

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub inner: T,
    pub range: (usize, usize),
}
impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

// String Literal as separate struct
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteralInner<'de>(pub Cow<'de, str>);
pub type StringLiteral<'de> = Spanned<StringLiteralInner<'de>>;
impl fmt::Display for StringLiteral<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.inner.0)
    }
}

// Number Literal
#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteralInner(pub f64);
pub type NumberLiteral = Spanned<NumberLiteralInner>;
impl fmt::Display for NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.0)
    }
}

// Bool Literal
#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteralInner(pub bool);
pub type BoolLiteral = Spanned<BoolLiteralInner>;
impl fmt::Display for BoolLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.0)
    }
}

// Nil Literal
#[derive(Debug, Clone, PartialEq)]
pub struct NilLiteralInner;
pub type NilLiteral = Spanned<NilLiteralInner>;
impl fmt::Display for NilLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "nil")
    }
}

// Updated Literal enum to reference the separate structs
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralInner<'de> {
    String(StringLiteral<'de>),
    Number(NumberLiteral),
    Bool(BoolLiteral),
    Nil(NilLiteral),
}
pub type Literal<'de> = Spanned<LiteralInner<'de>>;
impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            LiteralInner::String(s) => write!(f, "{}", s),
            LiteralInner::Number(n) => write!(f, "{}", n),
            LiteralInner::Bool(b) => write!(f, "{}", b),
            LiteralInner::Nil(n) => write!(f, "{}", n),
        }
    }
}

// Identifier with proper Inner struct
#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierInner<'de> {
    pub name: Cow<'de, str>,
}
pub type Identifier<'de> = Spanned<IdentifierInner<'de>>;
impl fmt::Display for Identifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.name)
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
    Field,
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
                Op::Field => ".",
            }
        )
    }
}

// Variable Declaration
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarationInner<'de> {
    pub id: Identifier<'de>,
    pub init: Option<Expression<'de>>,
}
pub type VariableDeclaration<'de> = Spanned<VariableDeclarationInner<'de>>;
impl fmt::Display for VariableDeclaration<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var {}", self.inner.id)?;
        if let Some(init) = &self.inner.init {
            write!(f, " = {}", init)?;
        }
        Ok(())
    }
}
impl<'de> VariableDeclaration<'de> {
    pub fn validate<'p>(&self, parser: &'p Parser<'de>) -> Result<(), Error> {
        // https://app.codecrafters.io/courses/interpreter/stages/pt7
        if !parser.state().current_scope()?.is_enclosed() {
            return Ok(());
        }

        // println!("here: {}", self.id);
        // println!("{:?}", parser.state());

        if let Some(range) = parser.state().current_scope()?.bindings.get(&self.id.name) {
            return Err(error::RedeclarationError {
                src: parser.whole().to_string(),
                name: self.id.name.to_string(),
                err_span: (self.range.0..self.range.1).into(),
                existing_span: (range.0..range.1).into(),
            }
            .into());
        }

        struct ExprVisitor<'de> {
            identifiers: HashSet<&'de str>,
        }
        impl<'ast, 'de> Visitor<'ast, 'de> for ExprVisitor<'ast> {
            type Output = ();
            fn visit_identifier(&mut self, _identifier: &'ast Identifier<'de>) -> Self::Output {
                self.identifiers.insert(&_identifier.name);
            }
        }

        struct VariableDeclarationVisitor<'ast, 'p, 'de> {
            id: &'ast Identifier<'de>,
            parser: &'p Parser<'de>,
        }
        impl<'ast, 's, 'de> Visitor<'ast, 'de> for VariableDeclarationVisitor<'ast, 's, 'de> {
            type Output = bool;

            fn visit_variable_declaration(
                &mut self,
                decl: &'ast VariableDeclaration<'de>,
            ) -> Self::Output {
                if let Some(decl) = &decl.init {
                    self.visit_expression(decl)
                } else {
                    true
                }
            }

            fn visit_expression(&mut self, expr: &Expression<'de>) -> Self::Output {
                let mut expr_visitor = ExprVisitor {
                    identifiers: HashSet::default(),
                };

                expr_visitor.visit_expression(expr);

                let res = !expr_visitor.identifiers.contains(self.id.name.as_ref());

                res
            }
        }
        let mut visitor = VariableDeclarationVisitor {
            id: &self.id,
            parser,
        };

        if visitor.visit_variable_declaration(self) {
            Ok(())
        } else {
            Err(error::SyntaxError {
                src: parser.whole().to_string(),
                message: "Can't read local variable in its own initializer.".to_string(),
                err_span: (self.range.0..self.range.1).into(),
            }
            .into())
        }
    }
}

// Function Declaration
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclarationInner<'de> {
    pub name: Identifier<'de>,
    pub parameters: Vec<Identifier<'de>>,
    pub body: Box<BlockStatement<'de>>,
}
pub type FunctionDeclaration<'de> = Spanned<FunctionDeclarationInner<'de>>;
impl fmt::Display for FunctionDeclaration<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(def {}", self.inner.name)?;
        for param in &self.inner.parameters {
            write!(f, " {}", param)?;
        }
        write!(f, " {})", self.inner.body)
    }
}

// Class Declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclarationInner<'de> {
    pub id: Identifier<'de>,
    pub superclass: Option<Token<'de>>,
    pub body: Vec<ClassBody>,
}
pub type ClassDeclaration<'de> = Spanned<ClassDeclarationInner<'de>>;
impl fmt::Display for ClassDeclaration<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(class {}", self.inner.id)?;
        if let Some(superclass) = &self.inner.superclass {
            write!(f, " < {}", superclass)?;
        }
        for method in &self.inner.body {
            write!(f, " {}", method)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ClassBodyInner {
    // @todo: Add more class body elements as needed
    ClassMethod(),
    ClassProperty(),
}
pub type ClassBody = Spanned<ClassBodyInner>;
impl fmt::Display for ClassBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            ClassBodyInner::ClassMethod() => write!(f, "class method"),
            ClassBodyInner::ClassProperty() => write!(f, "class property"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationInner<'de> {
    Variable(VariableDeclaration<'de>),
    Function(FunctionDeclaration<'de>),
    Class(ClassDeclaration<'de>),
}
pub type Declaration<'de> = Spanned<DeclarationInner<'de>>;
impl fmt::Display for Declaration<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            DeclarationInner::Variable(var_decl) => {
                write!(f, "var {}", var_decl.inner.id)?;
                if let Some(init) = &var_decl.inner.init {
                    write!(f, " = {}", init)?;
                }
                Ok(())
            }
            DeclarationInner::Function(fun_decl) => {
                write!(f, "(def {}", fun_decl.inner.name)?;
                for param in &fun_decl.inner.parameters {
                    write!(f, " {}", param)?;
                }
                write!(f, " {})", fun_decl.inner.body)
            }
            DeclarationInner::Class(class_decl) => {
                write!(f, "(class {}", class_decl.inner.id)?;
                if let Some(superclass) = &class_decl.inner.superclass {
                    write!(f, " < {}", superclass)?;
                }
                for method in &class_decl.inner.body {
                    write!(f, " {}", method)?;
                }
                write!(f, ")")
            }
        }
    }
}

// Unary Expression
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpressionInner<'de> {
    pub operator: Op,
    pub right: Box<Expression<'de>>,
}
pub type UnaryExpression<'de> = Spanned<UnaryExpressionInner<'de>>;
impl fmt::Display for UnaryExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {})", self.inner.operator, self.inner.right)
    }
}

// Binary Expression
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpressionInner<'de> {
    pub left: Box<Expression<'de>>,
    pub operator: Op,
    pub right: Box<Expression<'de>>,
}
pub type BinaryExpression<'de> = Spanned<BinaryExpressionInner<'de>>;
impl fmt::Display for BinaryExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.inner.operator, self.inner.left, self.inner.right
        )
    }
}

// Group Expression
#[derive(Debug, Clone, PartialEq)]
pub struct GroupExpressionInner<'de> {
    pub expression: Box<Expression<'de>>,
}
pub type GroupExpression<'de> = Spanned<GroupExpressionInner<'de>>;
impl fmt::Display for GroupExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(group {})", self.inner.expression)
    }
}

// Assignment Expression
#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpressionInner<'de> {
    pub operator: Op,
    pub left: Identifier<'de>,
    pub right: Box<Expression<'de>>,
}
pub type AssignmentExpression<'de> = Spanned<AssignmentExpressionInner<'de>>;
impl fmt::Display for AssignmentExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.inner.operator, self.inner.left, self.inner.right
        )
    }
}

// Call Expression
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpressionInner<'de> {
    pub callee: Box<Expression<'de>>,
    pub arguments: Vec<Expression<'de>>,
}
pub type CallExpression<'de> = Spanned<CallExpressionInner<'de>>;
impl fmt::Display for CallExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.inner.callee)?;
        for arg in &self.inner.arguments {
            write!(f, " {}", arg)?;
        }
        write!(f, ")")
    }
}

// Member Expression
#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpressionInner<'de> {
    pub object: Box<Expression<'de>>,
    pub property: Box<Expression<'de>>,
}
pub type MemberExpression<'de> = Spanned<MemberExpressionInner<'de>>;
impl fmt::Display for MemberExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}.{})", self.inner.object, self.inner.property)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionInner<'de> {
    Literal(Literal<'de>),
    Identifier(Identifier<'de>),
    Unary(UnaryExpression<'de>),
    Binary(BinaryExpression<'de>),
    Group(GroupExpression<'de>),
    Assignment(AssignmentExpression<'de>),
    Call(CallExpression<'de>),
    Member(MemberExpression<'de>),
}
pub type Expression<'de> = Spanned<ExpressionInner<'de>>;
impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            ExpressionInner::Literal(lit) => write!(f, "{}", lit),
            ExpressionInner::Identifier(id) => write!(f, "{}", id),
            ExpressionInner::Unary(unary) => {
                write!(f, "({} {})", unary.inner.operator, unary.inner.right)
            }
            ExpressionInner::Binary(binary) => write!(
                f,
                "({} {} {})",
                binary.inner.operator, binary.inner.left, binary.inner.right
            ),
            ExpressionInner::Group(group) => write!(f, "(group {})", group.inner.expression),
            ExpressionInner::Assignment(assignment) => write!(
                f,
                "({} {} {})",
                assignment.inner.operator, assignment.inner.left, assignment.inner.right
            ),
            ExpressionInner::Call(call) => {
                write!(f, "({}", call.inner.callee)?;
                for arg in &call.inner.arguments {
                    write!(f, " {}", arg)?;
                }
                write!(f, ")")
            }
            ExpressionInner::Member(member) => {
                write!(f, "({}.{})", member.inner.object, member.inner.property)
            }
        }
    }
}

// Block Statement
#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatementInner<'de> {
    pub statements: Vec<Statement<'de>>,
}
pub type BlockStatement<'de> = Spanned<BlockStatementInner<'de>>;
impl fmt::Display for BlockStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for statement in &self.inner.statements {
            write!(f, " {}", statement)?;
        }
        write!(f, " }}")
    }
}

// If Statement
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementInner<'de> {
    pub test: Box<Expression<'de>>,
    pub consequent: Box<Statement<'de>>,
    pub alternate: Option<Box<Statement<'de>>>,
}
pub type IfStatement<'de> = Spanned<IfStatementInner<'de>>;
impl fmt::Display for IfStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(if {} {}", self.inner.test, self.inner.consequent)?;
        if let Some(alternate) = &self.inner.alternate {
            write!(f, " {}", alternate)?;
        }
        write!(f, ")")
    }
}

// While Statement
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatementInner<'de> {
    pub test: Box<Expression<'de>>,
    pub body: Box<Statement<'de>>,
}
pub type WhileStatement<'de> = Spanned<WhileStatementInner<'de>>;
impl fmt::Display for WhileStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(while {} {})", self.inner.test, self.inner.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ForInitInner<'de> {
    VariableDeclaration(VariableDeclaration<'de>),
    Expression(Expression<'de>),
}
pub type ForInit<'de> = Spanned<ForInitInner<'de>>;
impl fmt::Display for ForInit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            ForInitInner::VariableDeclaration(decl) => write!(f, "{}", decl),
            ForInitInner::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

// For Statement
#[derive(Debug, Clone, PartialEq)]
pub struct ForStatementInner<'de> {
    pub init: Option<ForInit<'de>>, // for convenience, we
    pub test: Option<Box<Expression<'de>>>,
    pub update: Option<Box<Expression<'de>>>,
    pub body: Box<Statement<'de>>,
}
pub type ForStatement<'de> = Spanned<ForStatementInner<'de>>;
impl fmt::Display for ForStatement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(for")?;
        if let Some(init) = &self.inner.init {
            write!(f, " {}", init)?;
        }
        if let Some(test) = &self.inner.test {
            write!(f, " {}", test)?;
        }
        if let Some(update) = &self.inner.update {
            write!(f, " {}", update)?;
        }
        write!(f, " {})", self.inner.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementInner<'de> {
    Expression(Expression<'de>),
    Print(Expression<'de>),
    Return(Option<Expression<'de>>),
    Block(BlockStatement<'de>),
    Declaration(Declaration<'de>),
    If(IfStatement<'de>),
    While(WhileStatement<'de>),
    For(ForStatement<'de>),
    Break,
    Continue,
}
pub type Statement<'de> = Spanned<StatementInner<'de>>;
impl fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            StatementInner::Expression(expr) => write!(f, "{}", expr),
            StatementInner::Print(expr) => write!(f, "(print {})", expr),
            StatementInner::Return(ret) => {
                write!(f, "(return")?;
                if let Some(value) = ret {
                    write!(f, " {}", value)?;
                }
                write!(f, ")")
            }
            StatementInner::Block(block) => {
                write!(f, "{{")?;
                for statement in &block.inner.statements {
                    write!(f, " {}", statement)?;
                }
                write!(f, " }}")
            }
            StatementInner::Declaration(decl) => write!(f, "{}", decl),
            StatementInner::If(if_stmt) => {
                write!(f, "(if {} {}", if_stmt.inner.test, if_stmt.inner.consequent)?;
                if let Some(alternate) = &if_stmt.inner.alternate {
                    write!(f, " {}", alternate)?;
                }
                write!(f, ")")
            }
            StatementInner::While(while_stmt) => {
                write!(
                    f,
                    "(while {} {})",
                    while_stmt.inner.test, while_stmt.inner.body
                )
            }
            StatementInner::For(for_stmt) => {
                write!(f, "(for")?;
                if let Some(init) = &for_stmt.inner.init {
                    write!(f, " {}", init)?;
                }
                if let Some(test) = &for_stmt.inner.test {
                    write!(f, " {}", test)?;
                }
                if let Some(update) = &for_stmt.inner.update {
                    write!(f, " {}", update)?;
                }
                write!(f, " {})", for_stmt.inner.body)
            }
            StatementInner::Break => write!(f, "(break)"),
            StatementInner::Continue => write!(f, "(continue)"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTreeInner<'de> {
    Statement(Statement<'de>),
    Expression(Expression<'de>),
}
pub type TokenTree<'de> = Spanned<TokenTreeInner<'de>>;
impl fmt::Display for TokenTreeInner<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTreeInner::Expression(expr) => write!(f, "{}", expr),
            TokenTreeInner::Statement(statement) => write!(f, "{}", statement),
        }
    }
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

// visitor
pub trait Visitor<'ast, 'de> {
    type Output: Default;

    /// Visit an expression by dispatching to the appropriate visit method
    fn visit_expression(&mut self, expr: &'ast Expression<'de>) -> Self::Output {
        match &expr.inner {
            ExpressionInner::Literal(lit) => self.visit_literal(lit),
            ExpressionInner::Identifier(id) => self.visit_identifier(id),
            ExpressionInner::Unary(unary) => self.visit_unary_expression(unary),
            ExpressionInner::Binary(binary) => self.visit_binary_expression(binary),
            ExpressionInner::Group(group) => self.visit_group_expression(group),
            ExpressionInner::Assignment(assign) => self.visit_assignment_expression(assign),
            ExpressionInner::Call(call) => self.visit_call_expression(call),
            ExpressionInner::Member(member) => self.visit_member_expression(member),
        }
    }

    fn visit_literal(&mut self, _literal: &'ast Literal<'de>) -> Self::Output {
        Self::Output::default()
    }

    fn visit_identifier(&mut self, _identifier: &'ast Identifier<'de>) -> Self::Output {
        Self::Output::default()
    }

    fn visit_unary_expression(&mut self, expr: &'ast UnaryExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.right);
        Self::Output::default()
    }

    fn visit_binary_expression(&mut self, expr: &'ast BinaryExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.left);
        self.visit_expression(&expr.inner.right);
        Self::Output::default()
    }

    fn visit_group_expression(&mut self, expr: &'ast GroupExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.expression);
        Self::Output::default()
    }

    fn visit_assignment_expression(
        &mut self,
        expr: &'ast AssignmentExpression<'de>,
    ) -> Self::Output {
        self.visit_identifier(&expr.inner.left);
        self.visit_expression(&expr.inner.right);
        Self::Output::default()
    }

    fn visit_call_expression(&mut self, expr: &'ast CallExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.callee);
        for arg in &expr.inner.arguments {
            self.visit_expression(arg);
        }
        Self::Output::default()
    }

    fn visit_member_expression(&mut self, expr: &'ast MemberExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.object);
        self.visit_expression(&expr.inner.property);
        Self::Output::default()
    }

    fn visit_block_statement(&mut self, block: &'ast BlockStatement<'de>) -> Self::Output {
        for stmt in &block.inner.statements {
            self.visit_statement(stmt);
        }
        Self::Output::default()
    }

    fn visit_if_statement(&mut self, if_stmt: &'ast IfStatement<'de>) -> Self::Output {
        self.visit_expression(&if_stmt.inner.test);
        self.visit_statement(&if_stmt.inner.consequent);
        if let Some(alternate) = &if_stmt.inner.alternate {
            self.visit_statement(alternate);
        }
        Self::Output::default()
    }

    fn visit_while_statement(&mut self, while_stmt: &'ast WhileStatement<'de>) -> Self::Output {
        self.visit_expression(&while_stmt.inner.test);
        self.visit_statement(&while_stmt.inner.body);
        Self::Output::default()
    }

    fn visit_for_statement(&mut self, for_stmt: &'ast ForStatement<'de>) -> Self::Output {
        if let Some(init) = &for_stmt.inner.init {
            match &init.inner {
                ForInitInner::VariableDeclaration(decl) => {
                    self.visit_variable_declaration(decl);
                }
                ForInitInner::Expression(expr) => {
                    self.visit_expression(expr);
                }
            };
        }
        if let Some(test) = &for_stmt.inner.test {
            self.visit_expression(test);
        }
        if let Some(update) = &for_stmt.inner.update {
            self.visit_expression(update);
        }
        self.visit_statement(&for_stmt.inner.body);
        Self::Output::default()
    }

    fn visit_variable_declaration(&mut self, decl: &'ast VariableDeclaration<'de>) -> Self::Output {
        self.visit_identifier(&decl.inner.id);
        if let Some(init) = &decl.inner.init {
            self.visit_expression(init);
        }
        Self::Output::default()
    }

    fn visit_function_declaration(&mut self, decl: &'ast FunctionDeclaration<'de>) -> Self::Output {
        self.visit_identifier(&decl.inner.name);
        for param in &decl.inner.parameters {
            self.visit_identifier(param);
        }
        self.visit_block_statement(&decl.inner.body);
        Self::Output::default()
    }

    fn visit_class_declaration(&mut self, decl: &'ast ClassDeclaration<'de>) -> Self::Output {
        self.visit_identifier(&decl.inner.id);
        Self::Output::default()
    }

    fn visit_statement(&mut self, statement: &'ast Statement<'de>) -> Self::Output {
        match &statement.inner {
            StatementInner::Expression(expr) => self.visit_expression(expr),
            StatementInner::Print(expr) => self.visit_expression(expr),
            StatementInner::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expression(expr);
                }
                Self::Output::default()
            }
            StatementInner::Block(block) => self.visit_block_statement(block),
            StatementInner::Declaration(decl) => match &decl.inner {
                DeclarationInner::Variable(var_decl) => self.visit_variable_declaration(var_decl),
                DeclarationInner::Function(func_decl) => self.visit_function_declaration(func_decl),
                DeclarationInner::Class(class_decl) => self.visit_class_declaration(class_decl),
            },
            StatementInner::If(if_stmt) => self.visit_if_statement(if_stmt),
            StatementInner::While(while_stmt) => self.visit_while_statement(while_stmt),
            StatementInner::For(for_stmt) => self.visit_for_statement(for_stmt),
            StatementInner::Break => Self::Output::default(),
            StatementInner::Continue => Self::Output::default(),
        }
    }
}

// Example usage of the Visitor trait with default implementations
// This demonstrates how to create a custom visitor that only overrides specific methods

/// Example visitor that counts different types of AST nodes
/// Only needs to override the methods it's interested in
pub struct NodeCounter {
    pub identifier_count: usize,
    pub function_count: usize,
    pub binary_expr_count: usize,
}

impl NodeCounter {
    pub fn new() -> Self {
        Self {
            identifier_count: 0,
            function_count: 0,
            binary_expr_count: 0,
        }
    }
}

impl<'ast, 'de> Visitor<'ast, 'de> for NodeCounter {
    type Output = ();

    // Override only the methods we care about
    fn visit_identifier(&mut self, _identifier: &Identifier<'de>) -> Self::Output {
        self.identifier_count += 1;
        // Use default implementation to continue traversal
        ()
    }

    fn visit_function_declaration(&mut self, decl: &FunctionDeclaration<'de>) -> Self::Output {
        self.function_count += 1;
        // Call default implementation to visit children
        self.visit_identifier(&decl.inner.name);
        for param in &decl.inner.parameters {
            self.visit_identifier(param);
        }
        self.visit_block_statement(&decl.inner.body);
        ()
    }

    fn visit_binary_expression(&mut self, expr: &BinaryExpression<'de>) -> Self::Output {
        self.binary_expr_count += 1;
        // Call default implementation to visit children
        self.visit_expression(&expr.inner.left);
        self.visit_expression(&expr.inner.right);
        ()
    }
}

/// Example visitor that collects all identifier names
/// Demonstrates how to use the visitor pattern to collect data
pub struct IdentifierCollector {
    pub identifiers: Vec<String>,
}

impl IdentifierCollector {
    pub fn new() -> Self {
        Self {
            identifiers: Vec::new(),
        }
    }
}

impl<'ast, 'de> Visitor<'ast, 'de> for IdentifierCollector {
    type Output = ();

    fn visit_identifier(&mut self, identifier: &Identifier<'de>) -> Self::Output {
        self.identifiers
            .push(identifier.inner.name.as_ref().to_string());
        ()
    }
}

#[cfg(test)]
mod visitor_tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_node_counter_visitor() {
        let source = r#"
            fun add(a, b) {
                return a + b;
            }
            var x = add(5, 3);
        "#;

        let mut parser = Parser::new(source);
        let statements = parser.parse().unwrap();

        let mut counter = NodeCounter::new();
        for stmt in &statements {
            counter.visit_statement(stmt);
        }

        assert!(counter.identifier_count > 0);
        assert_eq!(counter.function_count, 1);
        assert!(counter.binary_expr_count > 0);
    }

    #[test]
    fn test_identifier_collector_visitor() {
        let source = r#"
            fun test(param) {
                var local = param;
                return local;
            }
        "#;

        let mut parser = Parser::new(source);
        let statements = parser.parse().unwrap();

        let mut collector = IdentifierCollector::new();
        for stmt in &statements {
            collector.visit_statement(stmt);
        }

        assert!(collector.identifiers.contains(&"test".to_string()));
        assert!(collector.identifiers.contains(&"param".to_string()));
        assert!(collector.identifiers.contains(&"local".to_string()));
    }

    #[test]
    fn test_custom_visitor_with_default_behavior() {
        /// A visitor that only counts function declarations but uses default behavior for everything else
        struct FunctionCounter {
            count: usize,
        }

        impl FunctionCounter {
            fn new() -> Self {
                Self { count: 0 }
            }
        }

        impl<'ast, 'de> Visitor<'ast, 'de> for FunctionCounter {
            type Output = ();

            fn visit_function_declaration(
                &mut self,
                decl: &FunctionDeclaration<'de>,
            ) -> Self::Output {
                self.count += 1;
                // Use default implementation to visit children
                self.visit_identifier(&decl.inner.name);
                for param in &decl.inner.parameters {
                    self.visit_identifier(param);
                }
                self.visit_block_statement(&decl.inner.body);
                ()
            }
        }

        let source = r#"
            fun outer() {
                fun inner() {
                    return 42;
                }
                return inner;
            }
        "#;

        let mut parser = Parser::new(source);
        let statements = parser.parse().unwrap();

        let mut counter = FunctionCounter::new();
        for stmt in &statements {
            counter.visit_statement(stmt);
        }

        assert_eq!(counter.count, 2); // outer and inner functions
    }
}
