use std::borrow::Cow;

use miette::Error;

use crate::{
    ast::{
        ClassBodyItem, ClassBodyItemInner, ClassDeclaration, Expression, ExpressionInner,
        Identifier, Node, Program, ScopeType, VariableDeclaration, VisitContext, Visitor,
    },
    error,
};

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

pub struct Resolver<'de> {
    whole: &'de str,
}

impl<'ast, 'de> Resolver<'de> {
    pub fn new(input: &'de str) -> Self {
        Self { whole: input }
    }

    pub fn resolve(&mut self, program: &'ast mut Program<'de>) -> Result<(), Error> {
        let mut ctx = VisitContext::new();

        // if let Err(err) = self.visit_program(program, &mut ctx) {
        //     Err(err.into())
        // } else {
        //     Ok(())
        // }

        let res = self.visit_program(program, &mut ctx);

        // println!("resolve res: {res:?}");

        res
    }
}

impl<'ast, 'de> Visitor<'ast, 'de> for Resolver<'de> {
    type Output = ();
    type Error = Error;

    fn visit_variable_declaration(
        &mut self,
        decl: &'ast crate::ast::VariableDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        // https://app.codecrafters.io/courses/interpreter/stages/pt7
        // https://app.codecrafters.io/courses/interpreter/stages/pz7

        let scope_type = ctx
            .get_current_scope_type()
            .ok_or_else(|| error::ParseInternalError {
                message: "no scope".to_string(),
            })?;

        if scope_type == ScopeType::Global || scope_type == ScopeType::Module {
            return self.walk_variable_declaration(decl, ctx);
        }

        if let Some(range) = ctx.is_variable_declared_in_current_scope(&decl.id.name) {
            return Err(error::RedeclarationError {
                src: self.whole.to_string(),
                name: decl.id.name.to_string(),
                err_span: (decl.range.0..decl.range.1).into(),
                existing_span: (range.0..range.1).into(),
            }
            .into());
        }

        struct ExprVisitor<'de> {
            identifiers: HashSet<&'de str>,
        }
        impl<'ast, 'de> Visitor<'ast, 'de> for ExprVisitor<'de>
        where
            'ast: 'de,
        {
            type Output = ();
            type Error = Error;

            fn visit_identifier(
                &mut self,
                _identifier: &'ast Identifier<'de>,
                _ctx: &mut VisitContext<'ast, 'de>,
            ) -> Result<Self::Output, Self::Error> {
                self.identifiers.insert(&_identifier.name);
                Ok(())
            }
        }

        if let Some(init) = &decl.init {
            let mut expr_visitor = ExprVisitor {
                identifiers: HashSet::default(),
            };
            let mut expr_ctx = VisitContext::new();

            expr_visitor.visit_expression(init, &mut expr_ctx);

            // println!(
            //     "used vars: {:?} in init of var {}",
            //     expr_visitor.identifiers, decl.id
            // );

            if expr_visitor.identifiers.contains(decl.id.name.as_ref()) {
                Err(error::SyntaxError {
                    src: self.whole.to_string(),
                    message: "Can't read local variable in its own initializer.".to_string(),
                    err_span: (decl.range.0..decl.range.1).into(),
                }
                .into())
            } else {
                self.walk_variable_declaration(decl, ctx)
            }
        } else {
            self.walk_variable_declaration(decl, ctx)
        }
    }

    fn visit_function_declaration(
        &mut self,
        decl: &'ast crate::ast::FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        let mut declared_params: HashMap<&str, (usize, usize)> = HashMap::default();

        for param in &decl.parameters {
            if let Some(range) = declared_params.get(param.name.as_ref()) {
                return Err(error::RedeclarationError {
                    src: self.whole.to_string(),
                    name: param.name.to_string(),
                    err_span: (param.range.0..param.range.1).into(),
                    existing_span: (range.0..range.1).into(),
                }
                .into());
            }
            declared_params.insert(param.name.as_ref(), param.range);
        }

        self.walk_function_declaration(decl, ctx)
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &'ast crate::ast::ReturnStatement<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        for scope in ctx.scope_stack.iter().rev() {
            if scope.scope_type == ScopeType::Function {
                return self.walk_return_statement(return_stmt, ctx);
            }
        }

        Err(error::SyntaxError {
            src: self.whole.to_string(),
            message: "return statement is not allowed outside of a function".to_string(),
            err_span: (return_stmt.range.0..return_stmt.range.1).into(),
        }
        .into())
    }

    fn visit_call_expression(
        &mut self,
        call: &'ast crate::ast::CallExpression<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        if matches!(call.callee.inner, ExpressionInner::This(_)) {
            return Err(error::SyntaxError {
                src: self.whole.to_string(),
                message: "this keyword is not allowed in a function call".to_string(),
                err_span: (call.range.0..call.range.1).into(),
            }
            .into());
        }

        self.walk_call_expression(call, ctx)
    }

    fn visit_this_expression(
        &mut self,
        this: &'ast crate::ast::ThisExpression,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        for ancestor in ctx.ancestors.iter().rev() {
            if let &Node::ClassDeclaration(_) = ancestor {
                return Ok(());
            }
        }

        Err(error::SyntaxError {
            src: self.whole.to_string(),
            message: "this keyword is not allowed outside of a class".to_string(),
            err_span: (this.range.0..this.range.1).into(),
        }
        .into())
    }

    fn visit_class_declaration(
        &mut self,
        decl: &'ast ClassDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        for method in &decl.body.0 {
            if let ClassBodyItemInner::ClassMethod(method) = &method.inner {
                if method.name.name.as_ref() == "init" {
                    struct ReturnVisitor {};
                    impl<'ast, 'de> Visitor<'ast, 'de> for ReturnVisitor {
                        type Output = ();
                        type Error = (usize, usize);
                        fn visit_return_statement(
                            &mut self,
                            return_stmt: &'ast crate::ast::ReturnStatement<'de>,
                            ctx: &mut VisitContext<'ast, 'de>,
                        ) -> Result<Self::Output, Self::Error> {
                            if let None = return_stmt.expression {
                                // init method cannot return anything
                                Ok(())
                            } else {
                                Err(return_stmt.range)
                            }
                        }
                    }
                    let mut return_visitor = ReturnVisitor {};
                    let mut return_visitor_ctx = VisitContext::new();
                    if let Err(err_span) =
                        return_visitor.visit_function_declaration(&method, &mut return_visitor_ctx)
                    {
                        return Err(error::SyntaxError {
                            src: self.whole.to_string(),
                            message: "Can't return a value from an initializer".to_string(),
                            err_span: (err_span.0..err_span.1).into(),
                        }
                        .into());
                    }
                }
            }
        }

        if let Some(superclass) = &decl.superclass {
            if superclass.name == decl.id.name {
                return Err(error::SyntaxError {
                    src: self.whole.to_string(),
                    message: "A class can't inherit from itself.".to_string(),
                    err_span: (decl.range.0..decl.range.1).into(),
                }
                .into());
            }
        }

        self.walk_class_declaration(decl, ctx)
    }
}
