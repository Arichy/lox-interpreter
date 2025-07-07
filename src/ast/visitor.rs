pub use super::*;

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
            ExpressionInner::This(this) => self.visit_this_epxression(this),
        }
    }

    fn visit_this_epxression(&mut self, this_expr: &'ast ThisExpression) -> Self::Output {
        Self::Output::default()
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
            StatementInner::Print(expr) => self.visit_print_statement(statement),
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

    fn visit_print_statement(&mut self, print_statement: &'ast Statement<'de>) -> Self::Output {
        let StatementInner::Print(expr) = &**print_statement else {
            unreachable!()
        };

        self.visit_expression(expr)
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
