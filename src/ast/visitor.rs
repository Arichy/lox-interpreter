pub use super::*;
use rustc_hash::FxHashMap as HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

// ================== Node Definition ==================

#[derive(Clone, Copy, Debug)]
pub enum Node<'ast, 'de> {
    Program(&'ast Program<'de>),
    Statement(&'ast Statement<'de>),
    Expression(&'ast Expression<'de>),
    Declaration(&'ast Declaration<'de>),
    VariableDeclaration(&'ast VariableDeclaration<'de>),
    FunctionDeclaration(&'ast FunctionDeclaration<'de>),
    ClassDeclaration(&'ast ClassDeclaration<'de>),
    PrintStatement(&'ast PrintStatement<'de>),
    ReturnStatement(&'ast ReturnStatement<'de>),
    Identifier(&'ast Identifier<'de>),
    Literal(&'ast Literal<'de>),
    UnaryExpression(&'ast UnaryExpression<'de>),
    BinaryExpression(&'ast BinaryExpression<'de>),
    GroupExpression(&'ast GroupExpression<'de>),
    AssignmentExpression(&'ast AssignmentExpression<'de>),
    CallExpression(&'ast CallExpression<'de>),
    MemberExpression(&'ast MemberExpression<'de>),
    ThisExpression(&'ast ThisExpression),
    SuperExpressoin(&'ast SuperExpression),
    BlockStatement(&'ast BlockStatement<'de>),
    IfStatement(&'ast IfStatement<'de>),
    WhileStatement(&'ast WhileStatement<'de>),
    ForStatement(&'ast ForStatement<'de>),
}

// ================== Scope and Context Definitions ==================

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ScopeId(usize);

impl ScopeId {
    fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        ScopeId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Module,
    Function,
    Block,
}

#[derive(Debug, Clone)]
pub struct Scope<'de> {
    pub id: ScopeId,
    pub scope_type: ScopeType,
    pub variables: HashMap<Cow<'de, str>, (usize, usize)>,
}

#[derive(Debug)]
pub struct VisitContext<'ast, 'de> {
    pub parent: Option<Node<'ast, 'de>>,
    pub ancestors: Vec<Node<'ast, 'de>>,
    pub scope_stack: Vec<Scope<'de>>,
    pub current_scope_id: ScopeId,
}

impl<'ast, 'de> VisitContext<'ast, 'de> {
    pub fn new() -> Self {
        let global_scope = Scope {
            id: ScopeId::new(),
            scope_type: ScopeType::Global,
            variables: HashMap::default(),
        };
        let global_scope_id = global_scope.id;
        Self {
            parent: None,
            ancestors: Vec::new(),
            scope_stack: vec![global_scope],
            current_scope_id: global_scope_id,
        }
    }

    pub fn push_scope(&mut self, scope_type: ScopeType) -> ScopeId {
        let scope = Scope {
            id: ScopeId::new(),
            scope_type,
            variables: HashMap::default(),
        };
        let scope_id = scope.id;
        self.scope_stack.push(scope);
        self.current_scope_id = scope_id;
        scope_id
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
        self.current_scope_id = self.scope_stack.last().map_or(ScopeId::default(), |s| s.id);
    }

    pub fn declare_variable(&mut self, name: Cow<'de, str>, range: (usize, usize)) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.variables.insert(name, range);
        }
    }

    pub fn is_variable_declared(&self, name: &Cow<'de, str>) -> bool {
        for scope in self.scope_stack.iter().rev() {
            if scope.variables.contains_key(name) {
                return true;
            }
        }
        false
    }

    pub fn is_variable_declared_in_current_scope(
        &self,
        name: &Cow<'de, str>,
    ) -> Option<(usize, usize)> {
        let current_scope = self.scope_stack.last()?;

        current_scope.variables.get(name).copied()
    }

    pub fn push_node(&mut self, node: Node<'ast, 'de>) {
        if let Some(parent) = self.parent.take() {
            self.ancestors.push(parent);
        }
        self.parent = Some(node);
    }

    pub fn pop_node(&mut self) {
        self.parent = self.ancestors.pop();
    }

    pub fn get_current_scope_type(&self) -> Option<ScopeType> {
        self.scope_stack.last().map(|s| s.scope_type.clone())
    }

    pub fn is_in_function(&self) -> bool {
        self.scope_stack
            .iter()
            .any(|s| matches!(s.scope_type, ScopeType::Function))
    }

    pub fn depth(&self) -> usize {
        self.ancestors.len() + if self.parent.is_some() { 1 } else { 0 }
    }
}

impl<'ast, 'de> Default for VisitContext<'ast, 'de> {
    fn default() -> Self {
        Self::new()
    }
}

// ================== Visitor Trait Definition ==================

pub trait Visitor<'ast, 'de> {
    type Output: Default;
    type Error;

    fn visit_program(
        &mut self,
        program: &'ast Program<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_program(program, ctx)
    }

    fn visit_statement(
        &mut self,
        stmt: &'ast Statement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_statement(stmt, ctx)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast Expression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_expression(expr, ctx)
    }

    fn visit_declaration(
        &mut self,
        decl: &'ast Declaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_declaration(decl, ctx)
    }

    fn visit_variable_declaration(
        &mut self,
        decl: &'ast VariableDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_variable_declaration(decl, ctx)
    }

    fn visit_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_function_declaration(decl, ctx)
    }

    fn visit_class_declaration(
        &mut self,
        decl: &'ast ClassDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_class_declaration(decl, ctx)
    }

    fn visit_print_statement(
        &mut self,
        print_stmt: &'ast PrintStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_print_statement(print_stmt, ctx)
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &'ast ReturnStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_return_statement(return_stmt, ctx)
    }

    fn visit_block_statement(
        &mut self,
        block: &'ast BlockStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_block_statement(block, ctx)
    }

    fn visit_if_statement(
        &mut self,
        if_stmt: &'ast IfStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_if_statement(if_stmt, ctx)
    }

    fn visit_while_statement(
        &mut self,
        while_stmt: &'ast WhileStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_while_statement(while_stmt, ctx)
    }

    fn visit_for_statement(
        &mut self,
        for_stmt: &'ast ForStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_for_statement(for_stmt, ctx)
    }

    fn visit_unary_expression(
        &mut self,
        unary: &'ast UnaryExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_unary_expression(unary, ctx)
    }

    fn visit_binary_expression(
        &mut self,
        binary: &'ast BinaryExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_binary_expression(binary, ctx)
    }

    fn visit_group_expression(
        &mut self,
        group: &'ast GroupExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_group_expression(group, ctx)
    }

    fn visit_assignment_expression(
        &mut self,
        assign: &'ast AssignmentExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_assignment_expression(assign, ctx)
    }

    fn visit_call_expression(
        &mut self,
        call: &'ast CallExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_call_expression(call, ctx)
    }

    fn visit_member_expression(
        &mut self,
        member: &'ast MemberExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.walk_member_expression(member, ctx)
    }

    fn visit_identifier(
        &mut self,
        _ident: &'ast Identifier<'de>,
        _ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        // Default is to do nothing for leaf nodes
        Ok(Default::default())
    }

    fn visit_literal(
        &mut self,
        _literal: &'ast Literal<'de>,
        _ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        Ok(Default::default())
    }

    fn visit_this_expression(
        &mut self,
        _this: &'ast ThisExpression,
        _ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        Ok(Default::default())
    }

    fn visit_super_expression(
        &mut self,
        _super: &'ast SuperExpression,
        _ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        Ok(Default::default())
    }

    // Default walk implementations
    fn walk_program(
        &mut self,
        program: &'ast Program<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::Program(program));
        for stmt in &program.inner.body {
            self.visit_statement(stmt, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_statement(
        &mut self,
        stmt: &'ast Statement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::Statement(stmt));
        match &stmt.inner {
            StatementInner::Expression(expr) => {
                self.visit_expression(expr, ctx)?;
            }
            StatementInner::Print(print_stmt) => {
                self.visit_print_statement(print_stmt, ctx)?;
            }
            StatementInner::Declaration(decl) => {
                self.visit_declaration(decl, ctx)?;
            }
            StatementInner::Block(block) => {
                ctx.push_scope(ScopeType::Block);
                self.visit_block_statement(block, ctx)?;
                ctx.pop_scope();
            }
            StatementInner::If(if_stmt) => {
                self.visit_if_statement(if_stmt, ctx)?;
            }
            StatementInner::Return(ret_stmt) => {
                self.visit_return_statement(ret_stmt, ctx)?;
            }
            StatementInner::While(while_stmt) => {
                self.visit_while_statement(while_stmt, ctx)?;
            }
            StatementInner::For(for_stmt) => {
                self.visit_for_statement(for_stmt, ctx)?;
            }
            StatementInner::Break | StatementInner::Continue => {}
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_expression(
        &mut self,
        expr: &'ast Expression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::Expression(expr));
        match &expr.inner {
            ExpressionInner::Identifier(ident) => {
                self.visit_identifier(ident, ctx)?;
            }
            ExpressionInner::Literal(literal) => {
                self.visit_literal(literal, ctx)?;
            }
            ExpressionInner::Unary(unary) => {
                self.visit_unary_expression(unary, ctx)?;
            }
            ExpressionInner::Binary(binary) => {
                self.visit_binary_expression(binary, ctx)?;
            }
            ExpressionInner::Group(group) => {
                self.visit_group_expression(group, ctx)?;
            }
            ExpressionInner::Assignment(assign) => {
                self.visit_assignment_expression(assign, ctx)?;
            }
            ExpressionInner::Call(call) => {
                self.visit_call_expression(call, ctx)?;
            }
            ExpressionInner::Member(member) => {
                self.visit_member_expression(member, ctx)?;
            }
            ExpressionInner::This(this) => {
                self.visit_this_expression(this, ctx)?;
            }
            ExpressionInner::Super(_super) => {
                self.visit_super_expression(_super, ctx)?;
            }
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_declaration(
        &mut self,
        decl: &'ast Declaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::Declaration(decl));
        match &decl.inner {
            DeclarationInner::Variable(var_decl) => {
                self.visit_variable_declaration(var_decl, ctx)?;
            }
            DeclarationInner::Function(fn_decl) => {
                self.visit_function_declaration(fn_decl, ctx)?;
            }
            DeclarationInner::Class(class_decl) => {
                self.visit_class_declaration(class_decl, ctx)?;
            }
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_variable_declaration(
        &mut self,
        decl: &'ast VariableDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::VariableDeclaration(decl));
        ctx.declare_variable(decl.inner.id.inner.name.clone(), decl.range);
        self.visit_identifier(&decl.inner.id, ctx)?;
        if let Some(init) = &decl.inner.init {
            self.visit_expression(init, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::FunctionDeclaration(decl));
        ctx.declare_variable(decl.inner.name.inner.name.clone(), decl.range);
        self.visit_identifier(&decl.inner.name, ctx)?;

        ctx.push_scope(ScopeType::Function);
        for param in &decl.inner.parameters {
            ctx.declare_variable(param.inner.name.clone(), param.range);
            self.visit_identifier(param, ctx)?;
        }
        self.visit_block_statement(&decl.inner.body, ctx)?;
        ctx.pop_scope();

        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_class_declaration(
        &mut self,
        decl: &'ast ClassDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::ClassDeclaration(decl));
        ctx.declare_variable(decl.inner.id.inner.name.clone(), decl.range);
        self.visit_identifier(&decl.inner.id, ctx)?;

        if let Some(superclass) = &decl.inner.superclass {
            self.visit_identifier(superclass, ctx)?;
        }

        for item in &decl.inner.body.inner.0 {
            match &item.inner {
                ClassBodyItemInner::ClassMethod(method_decl) => {
                    self.visit_function_declaration(method_decl, ctx)?;
                }
                ClassBodyItemInner::ClassProperty() => {
                    // Handle class properties if they contain identifiers
                }
            }
        }

        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_print_statement(
        &mut self,
        print_stmt: &'ast PrintStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::PrintStatement(print_stmt));
        self.visit_expression(&print_stmt.inner.expression, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_return_statement(
        &mut self,
        return_stmt: &'ast ReturnStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::ReturnStatement(return_stmt));
        if let Some(expr) = &return_stmt.inner.expression {
            self.visit_expression(expr, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_block_statement(
        &mut self,
        block: &'ast BlockStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::BlockStatement(block));
        for stmt in &block.inner.statements {
            self.visit_statement(stmt, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_if_statement(
        &mut self,
        if_stmt: &'ast IfStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::IfStatement(if_stmt));
        self.visit_expression(&if_stmt.inner.test, ctx)?;
        self.visit_statement(&if_stmt.inner.consequent, ctx)?;
        if let Some(alternate) = &if_stmt.inner.alternate {
            self.visit_statement(alternate, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_while_statement(
        &mut self,
        while_stmt: &'ast WhileStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::WhileStatement(while_stmt));
        self.visit_expression(&while_stmt.inner.test, ctx)?;
        self.visit_statement(&while_stmt.inner.body, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_for_statement(
        &mut self,
        for_stmt: &'ast ForStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::ForStatement(for_stmt));
        ctx.push_scope(ScopeType::Block);

        if let Some(init) = &for_stmt.inner.init {
            match &init.inner {
                ForInitInner::VariableDeclaration(decl) => {
                    self.visit_variable_declaration(decl, ctx)?;
                }
                ForInitInner::Expression(expr) => {
                    self.visit_expression(expr, ctx)?;
                }
            }
        }
        if let Some(test) = &for_stmt.inner.test {
            self.visit_expression(test, ctx)?;
        }
        if let Some(update) = &for_stmt.inner.update {
            self.visit_expression(update, ctx)?;
        }
        self.visit_statement(&for_stmt.inner.body, ctx)?;

        ctx.pop_scope();
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_unary_expression(
        &mut self,
        unary: &'ast UnaryExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::UnaryExpression(unary));
        self.visit_expression(&unary.inner.right, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_binary_expression(
        &mut self,
        binary: &'ast BinaryExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::BinaryExpression(binary));
        self.visit_expression(&binary.inner.left, ctx)?;
        self.visit_expression(&binary.inner.right, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_group_expression(
        &mut self,
        group: &'ast GroupExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::GroupExpression(group));
        self.visit_expression(&group.inner.expression, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_assignment_expression(
        &mut self,
        assign: &'ast AssignmentExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::AssignmentExpression(assign));
        self.visit_identifier(&assign.inner.left, ctx)?;
        self.visit_expression(&assign.inner.right, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_call_expression(
        &mut self,
        call: &'ast CallExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::CallExpression(call));
        self.visit_expression(&call.inner.callee, ctx)?;
        for arg in &call.inner.arguments {
            self.visit_expression(arg, ctx)?;
        }
        ctx.pop_node();
        Ok(Default::default())
    }

    fn walk_member_expression(
        &mut self,
        member: &'ast MemberExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        ctx.push_node(Node::MemberExpression(member));
        self.visit_expression(&member.inner.object, ctx)?;
        self.visit_expression(&member.inner.property, ctx)?;
        ctx.pop_node();
        Ok(Default::default())
    }
}

// Helper function to start traversal
pub fn traverse<'ast, 'de, V: Visitor<'ast, 'de>>(
    program: &'ast Program<'de>,
    visitor: &mut V,
) -> Result<V::Output, V::Error> {
    let mut context = VisitContext::new();
    visitor.visit_program(program, &mut context)
}

// ================== Example Visitor Implementations ==================

/// Variable Analyzer - tracks variable declarations and usage
pub struct VariableAnalyzer<'de> {
    pub declared_variables: HashMap<Cow<'de, str>, ()>,
    pub used_variables: Vec<Cow<'de, str>>,
    pub undeclared_variables: Vec<Cow<'de, str>>,
}

impl<'de> VariableAnalyzer<'de> {
    pub fn new() -> Self {
        Self {
            declared_variables: HashMap::default(),
            used_variables: Vec::new(),
            undeclared_variables: Vec::new(),
        }
    }

    pub fn report(&self) -> String {
        let mut report = String::new();
        report.push_str(
            "=== Variable Analysis Report ===
",
        );
        report.push_str(&format!(
            "Declared Variables: {:?}
",
            self.declared_variables
        ));
        report.push_str(&format!(
            "Used Variables: {:?}
",
            self.used_variables
        ));
        if !self.undeclared_variables.is_empty() {
            report.push_str(&format!(
                "Undeclared Variables: {:?}
",
                self.undeclared_variables
            ));
        }
        report
    }
}

impl<'ast, 'de> Visitor<'ast, 'de> for VariableAnalyzer<'de> {
    type Output = ();
    type Error = Error;

    fn visit_variable_declaration(
        &mut self,
        decl: &'ast VariableDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.declared_variables
            .insert(decl.inner.id.inner.name.clone(), Default::default());
        self.walk_variable_declaration(decl, ctx)?;
        Ok(())
    }

    fn visit_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.declared_variables
            .insert(decl.inner.name.inner.name.clone(), Default::default());
        self.walk_function_declaration(decl, ctx)?;
        Ok(())
    }

    fn visit_identifier(
        &mut self,
        ident: &'ast Identifier<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        let name = ident.inner.name.clone();
        self.used_variables.push(name.clone());

        if !ctx.is_variable_declared(&name) {
            self.undeclared_variables.push(name);
        }
        Ok(())
    }
}

/// Function Call Collector - tracks function calls
pub struct FunctionCallCollector {
    pub function_calls: Vec<String>,
    pub calls_in_function: std::collections::HashMap<String, Vec<String>>,
    pub current_function: Option<String>,
}

impl FunctionCallCollector {
    pub fn new() -> Self {
        Self {
            function_calls: Vec::new(),
            calls_in_function: std::collections::HashMap::new(),
            current_function: None,
        }
    }

    pub fn report(&self) -> String {
        let mut report = String::new();
        report.push_str(
            "=== Function Call Analysis ===
",
        );
        report.push_str(&format!(
            "All function calls: {:?}
",
            self.function_calls
        ));

        for (func, calls) in &self.calls_in_function {
            report.push_str(&format!(
                "In function '{}': {:?}
",
                func, calls
            ));
        }

        report
    }
}

impl<'ast, 'de> Visitor<'ast, 'de> for FunctionCallCollector {
    type Output = ();
    type Error = Error;

    fn visit_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        let func_name = decl.inner.name.inner.name.to_string();
        let prev_function = self.current_function.replace(func_name.clone());

        self.walk_function_declaration(decl, ctx)?;

        self.current_function = prev_function;
        Ok(())
    }

    fn visit_call_expression(
        &mut self,
        call: &'ast CallExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        if let ExpressionInner::Identifier(func_name) = &call.inner.callee.inner {
            let name = func_name.inner.name.to_string();
            self.function_calls.push(name.clone());

            if let Some(current_func) = &self.current_function {
                self.calls_in_function
                    .entry(current_func.clone())
                    .or_default()
                    .push(name);
            }
        }

        self.walk_call_expression(call, ctx)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_variable_analyzer() {
        let source = r#"
            var x = 42;
            var y = x + 1;
            print z;
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mut analyzer = VariableAnalyzer::new();
        traverse(&program, &mut analyzer).unwrap();

        // assert!(analyzer.used_variables.contains("x"));
        // assert!(analyzer.used_variables.contains(&"z".()));
        // assert!(analyzer.undeclared_variables.contains(&"z".to_string()));
        // assert!(!analyzer.undeclared_variables.contains(&"x".to_string()));
        assert_eq!(analyzer.used_variables, ["x", "y", "x", "z"]); // @XXX: maybe incorrect
        assert_eq!(analyzer.undeclared_variables, ["z"]);
    }

    #[test]
    fn test_function_call_collector() {
        let source = r#"
            fun add(a, b) {
                return multiply(a, b);
            }

            var result = add(1, 2);
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mut collector = FunctionCallCollector::new();
        traverse(&program, &mut collector).unwrap();

        assert!(collector.function_calls.contains(&"multiply".to_string()));
        assert!(collector.function_calls.contains(&"add".to_string()));

        if let Some(calls) = collector.calls_in_function.get("add") {
            assert!(calls.contains(&"multiply".to_string()));
        }
    }

    #[test]
    fn test_scope_tracking() {
        let source = r#"
            var global = 1;
            fun test() {
                var local = 2;
                {
                    var block = 3;
                }
            }
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mut analyzer = VariableAnalyzer::new();
        traverse(&program, &mut analyzer).unwrap();

        // Should have multiple variables declared
        assert!(analyzer.declared_variables.len() > 1);

        // Check that variables are correctly declared
        assert!(analyzer.declared_variables.contains_key("global"));
        assert!(analyzer.declared_variables.contains_key("local"));
        assert!(analyzer.declared_variables.contains_key("block"));
        assert!(analyzer.declared_variables.contains_key("test"));
    }

    #[test]
    fn test_node_context_tracking() {
        /// Test visitor that tracks parent-child relationships
        struct ContextTracker {
            identifier_parents: Vec<String>,
        }

        impl ContextTracker {
            fn new() -> Self {
                Self {
                    identifier_parents: Vec::new(),
                }
            }
        }

        impl<'ast, 'de> Visitor<'ast, 'de> for ContextTracker {
            type Output = ();
            type Error = Error;

            fn visit_identifier(
                &mut self,
                ident: &'ast Identifier<'de>,
                ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                if let Some(parent) = &ctx.parent {
                    let parent_desc = match parent {
                        Node::VariableDeclaration(_) => "VarDecl".to_string(),
                        Node::FunctionDeclaration(_) => "FuncDecl".to_string(),
                        Node::CallExpression(_) => "Call".to_string(),
                        Node::BinaryExpression(_) => "Binary".to_string(),
                        _ => "Other".to_string(),
                    };
                    self.identifier_parents
                        .push(format!("{}:{}", ident.inner.name, parent_desc));
                }
                Ok(())
            }
        }

        let source = r#"
            var x = add(y, z);
            fun test(param) {
                return param + 1;
            }
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mut tracker = ContextTracker::new();
        traverse(&program, &mut tracker).unwrap();

        // Verify some key parent-child relationships
        assert!(tracker
            .identifier_parents
            .iter()
            .any(|s| s.contains("x:VarDecl")));
        assert!(tracker
            .identifier_parents
            .iter()
            .any(|s| s.contains("test:FuncDecl")));
        assert!(tracker
            .identifier_parents
            .iter()
            .any(|s| s.contains("add:Call")));
    }

    #[test]
    fn test_walk_vs_visit_control() {
        /// Test that demonstrates walk vs visit method differences - can control traversal
        struct SelectiveTraverser {
            visited_functions: Vec<String>,
            visited_identifiers: Vec<String>,
            skip_function_bodies: bool,
        }

        impl SelectiveTraverser {
            fn new(skip_function_bodies: bool) -> Self {
                Self {
                    visited_functions: Vec::new(),
                    visited_identifiers: Vec::new(),
                    skip_function_bodies,
                }
            }
        }

        impl<'ast, 'de> Visitor<'ast, 'de> for SelectiveTraverser {
            type Output = ();
            type Error = Error;

            fn visit_function_declaration(
                &mut self,
                decl: &'ast FunctionDeclaration<'de>,
                ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                let name = decl.inner.name.inner.name.to_string();
                self.visited_functions.push(name);

                if self.skip_function_bodies {
                    // Only visit function name and parameters, skip function body
                    self.visit_identifier(&decl.inner.name, ctx)?;
                    for param in &decl.inner.parameters {
                        self.visit_identifier(param, ctx)?;
                    }
                } else {
                    // Use walk to traverse the entire function
                    self.walk_function_declaration(decl, ctx)?;
                }
                Ok(())
            }

            fn visit_identifier(
                &mut self,
                identifier: &'ast Identifier<'de>,
                _ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                self.visited_identifiers
                    .push(identifier.inner.name.to_string());
                Ok(())
            }
        }

        let source = r#"
            fun outer() {
                var x = 42;
                fun inner() {
                    var y = x;
                    return y;
                }
                return inner;
            }
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        // Test skipping function bodies
        let mut selective_traverser = SelectiveTraverser::new(true);
        traverse(&program, &mut selective_traverser).unwrap();

        // When skipping function bodies, inner function declarations should not be visited
        assert_eq!(selective_traverser.visited_functions, vec!["outer"]);
        // Should only visit function names, not function body identifiers
        assert!(selective_traverser
            .visited_identifiers
            .contains(&"outer".to_string()));
        assert!(!selective_traverser
            .visited_identifiers
            .contains(&"x".to_string()));
        assert!(!selective_traverser
            .visited_identifiers
            .contains(&"y".to_string()));

        // Test full traversal
        let mut full_traverser = SelectiveTraverser::new(false);
        traverse(&program, &mut full_traverser).unwrap();

        // Full traversal should visit all functions
        assert_eq!(full_traverser.visited_functions, vec!["outer", "inner"]);
        // Should visit all identifiers
        assert!(full_traverser
            .visited_identifiers
            .contains(&"outer".to_string()));
        assert!(full_traverser
            .visited_identifiers
            .contains(&"x".to_string()));
        assert!(full_traverser
            .visited_identifiers
            .contains(&"inner".to_string()));
        assert!(full_traverser
            .visited_identifiers
            .contains(&"y".to_string()));
    }

    #[test]
    fn test_scope_depth_analysis() {
        /// Test scope nesting depth tracking
        struct ScopeDepthTracker {
            max_depth: usize,
            function_depths: Vec<usize>,
        }

        impl ScopeDepthTracker {
            fn new() -> Self {
                Self {
                    max_depth: 0,
                    function_depths: Vec::new(),
                }
            }
        }

        impl<'ast, 'de> Visitor<'ast, 'de> for ScopeDepthTracker {
            type Output = ();
            type Error = Error;

            fn visit_function_declaration(
                &mut self,
                decl: &'ast FunctionDeclaration<'de>,
                ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                let depth = ctx.scope_stack.len();
                self.max_depth = self.max_depth.max(depth);
                self.function_depths.push(depth);
                self.walk_function_declaration(decl, ctx)?;
                Ok(())
            }

            fn visit_block_statement(
                &mut self,
                block: &'ast BlockStatement<'de>,
                ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                let depth = ctx.scope_stack.len();
                self.max_depth = self.max_depth.max(depth);
                self.walk_block_statement(block, ctx)?;
                Ok(())
            }
        }

        let source = r#"
            fun outer() {
                {
                    fun inner() {
                        {
                            var nested = 42;
                        }
                    }
                }
            }
        "#;

        let mut parser = Parser::new(source);
        let program = parser.parse().unwrap();

        let mut tracker = ScopeDepthTracker::new();
        traverse(&program, &mut tracker).unwrap();

        // Verify maximum scope depth
        assert!(tracker.max_depth >= 3); // Global + Function + Block + Function + Block

        // Verify function depths
        assert_eq!(tracker.function_depths.len(), 2);
        assert!(tracker.function_depths[0] == 1); // outer function at global level
        assert!(tracker.function_depths[1] > tracker.function_depths[0]); // inner function nested deeper
    }
}
