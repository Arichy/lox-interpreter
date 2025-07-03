use std::{borrow::Cow, fmt, marker::PhantomData, ops::Deref};

use crate::lexer::Token;

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
