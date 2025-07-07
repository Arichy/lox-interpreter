use codecrafters_interpreter::{
    ast::{traverse, VisitContext, Visitor},
    Parser,
};

fn main() {
    let code = r#"
        fun add(a, b) {
            return a + b;
        }

        var result = add(1, 2);
        print result;
        "#;

    let mut parser = Parser::new(code);
    let ast = parser.parse().unwrap();

    struct IdVisitor {}

    impl<'ast, 'de> Visitor<'ast, 'de> for IdVisitor {
        fn visit_identifier(
            &mut self,
            _ident: &'ast codecrafters_interpreter::ast::Identifier<'de>,
            _ctx: &mut VisitContext<'ast, 'de>,
        ) -> () {
            println!("found id:{}", _ident);
            println!("parent: {:?}", _ctx.parent);
        }
    }

    let mut visitor = IdVisitor {};
    let mut ctx = VisitContext::new();

    visitor.visit_program(&ast, &mut ctx);
}
