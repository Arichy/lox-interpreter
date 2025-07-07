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

    // let code = r#"
    //     fun factorial(n) {
    //         if (n <= 1) {
    //             return 1;
    //         }
    //         return n * factorial(n - 1);
    //     }

    //     fun add(a, b) {
    //         var result = a + b;
    //         return result;
    //     }

    //     var x = 10;
    //     var y = add(x, 5);
    //     var z = factorial(y);
    //     print z;

    //     // 使用未声明的变量
    //     print undeclared_var;
    // "#;

    // let mut parser = Parser::new(code);
    // let program = parser.parse().unwrap();

    // println!("=== AST Visitor Pattern Examples ===\n");

    // // 示例1: 标识符收集器 - 展示基本的 visitor 使用
    // println!("1. Identifier Collector:");
    // struct IdentifierCollector {
    //     identifiers: Vec<String>,
    // }

    // impl IdentifierCollector {
    //     fn new() -> Self {
    //         Self { identifiers: Vec::new() }
    //     }
    // }

    // impl<'ast, 'de> Visitor<'ast, 'de> for IdentifierCollector {
    //     type Output = ();

    //     fn visit_identifier(&mut self, ident: &'ast codecrafters_interpreter::ast::Identifier<'de>, ctx: &mut VisitContext) -> Self::Output {
    //         let name = ident.inner.name.to_string();
    //         self.identifiers.push(name.clone());

    //         // 展示如何获取上下文信息
    //         println!("  Found identifier '{}' at depth {}", name, ctx.depth);
    //         if let Some(parent) = &ctx.parent {
    //             println!("    Parent: {:?}", parent);
    //         }

    //         // 不需要调用 walk，因为标识符是叶子节点
    //         Self::Output::default()
    //     }
    // }

    // let mut collector = IdentifierCollector::new();
    // traverse(&program, &mut collector);
    // println!("  Total identifiers found: {}\n", collector.identifiers.len());

    // // 示例2: 变量分析器 - 展示作用域跟踪
    // println!("2. Variable Analyzer:");
    // let mut analyzer = codecrafters_interpreter::ast::VariableAnalyzer::new();
    // traverse(&program, &mut analyzer);
    // println!("{}\n", analyzer.report());

    // // 示例3: 函数调用分析器 - 展示函数上下文跟踪
    // println!("3. Function Call Analyzer:");
    // let mut call_analyzer = codecrafters_interpreter::ast::FunctionCallCollector::new();
    // traverse(&program, &mut call_analyzer);
    // println!("{}\n", call_analyzer.report());

    // // 示例4: 作用域深度分析器 - 展示如何使用上下文信息
    // println!("4. Scope Depth Analyzer:");
    // struct ScopeDepthAnalyzer {
    //     max_depth: usize,
    //     depth_counts: std::collections::HashMap<usize, usize>,
    //     function_depths: Vec<usize>,
    // }

    // impl ScopeDepthAnalyzer {
    //     fn new() -> Self {
    //         Self {
    //             max_depth: 0,
    //             depth_counts: std::collections::HashMap::new(),
    //             function_depths: Vec::new(),
    //         }
    //     }

    //     fn report(&self) -> String {
    //         let mut report = String::new();
    //         report.push_str("=== Scope Depth Analysis ===\n");
    //         report.push_str(&format!("Max depth: {}\n", self.max_depth));

    //         report.push_str("Depth distribution:\n");
    //         for depth in 0..=self.max_depth {
    //             if let Some(&count) = self.depth_counts.get(&depth) {
    //                 report.push_str(&format!("  Depth {}: {} nodes\n", depth, count));
    //             }
    //         }

    //         report.push_str(&format!("Functions found at depths: {:?}\n", self.function_depths));
    //         report
    //     }
    // }

    // impl<'ast, 'de> Visitor<'ast, 'de> for ScopeDepthAnalyzer {
    //     type Output = ();

    //     fn visit_statement(&mut self, stmt: &'ast codecrafters_interpreter::ast::Statement<'de>, ctx: &mut VisitContext) -> Self::Output {
    //         self.max_depth = self.max_depth.max(ctx.depth);
    //         *self.depth_counts.entry(ctx.depth).or_default() += 1;

    //         // 继续遍历子节点
    //         self.walk_statement(stmt, ctx)
    //     }

    //     fn visit_function_declaration(&mut self, decl: &'ast codecrafters_interpreter::ast::FunctionDeclaration<'de>, ctx: &mut VisitContext) -> Self::Output {
    //         self.function_depths.push(ctx.depth);
    //         println!("  Function '{}' declared at depth {} (in_function: {})",
    //                  decl.inner.name.inner.name, ctx.depth, ctx.in_function);

    //         // 继续遍历函数的子节点
    //         self.walk_function_declaration(decl, ctx)
    //     }
    // }

    // let mut depth_analyzer = ScopeDepthAnalyzer::new();
    // traverse(&program, &mut depth_analyzer);
    // println!("{}\n", depth_analyzer.report());

    // // 示例5: 条件控制流分析器 - 展示如何选择性地遍历
    // println!("5. Conditional Control Flow Analyzer:");
    // struct ControlFlowAnalyzer {
    //     if_count: usize,
    //     nested_if_count: usize,
    //     return_count: usize,
    // }

    // impl ControlFlowAnalyzer {
    //     fn new() -> Self {
    //         Self {
    //             if_count: 0,
    //             nested_if_count: 0,
    //             return_count: 0,
    //         }
    //     }
    // }

    // impl<'ast, 'de> Visitor<'ast, 'de> for ControlFlowAnalyzer {
    //     type Output = ();

    //     fn visit_if_statement(&mut self, if_stmt: &'ast codecrafters_interpreter::ast::IfStatement<'de>, ctx: &mut VisitContext) -> Self::Output {
    //         self.if_count += 1;

    //         // 检查是否是嵌套的 if
    //         if ctx.ancestors.iter().any(|ancestor| matches!(ancestor, codecrafters_interpreter::ast::NodeRef::Statement(s) if s.contains("If"))) {
    //             self.nested_if_count += 1;
    //         }

    //         println!("  If statement at depth {}", ctx.depth);

    //         // 继续遍历 if 语句的子节点
    //         self.walk_if_statement(if_stmt, ctx)
    //     }

    //     fn visit_return_statement(&mut self, expr: &'ast Option<codecrafters_interpreter::ast::Expression<'de>>, ctx: &mut VisitContext) -> Self::Output {
    //         self.return_count += 1;
    //         println!("  Return statement at depth {} (in_function: {})", ctx.depth, ctx.in_function);

    //         // 继续处理返回值表达式
    //         self.walk_return_statement(expr, ctx)
    //     }
    // }

    // let mut control_flow_analyzer = ControlFlowAnalyzer::new();
    // traverse(&program, &mut control_flow_analyzer);
    // println!("  Total if statements: {}", control_flow_analyzer.if_count);
    // println!("  Nested if statements: {}", control_flow_analyzer.nested_if_count);
    // println!("  Total return statements: {}\n", control_flow_analyzer.return_count);

    // // 示例6: 自定义遍历控制 - 展示如何控制遍历行为
    // println!("6. Custom Traversal Control:");
    // struct SelectiveVisitor {
    //     function_names: Vec<String>,
    //     skip_function_bodies: bool,
    // }

    // impl SelectiveVisitor {
    //     fn new(skip_function_bodies: bool) -> Self {
    //         Self {
    //             function_names: Vec::new(),
    //             skip_function_bodies,
    //         }
    //     }
    // }

    // impl<'ast, 'de> Visitor<'ast, 'de> for SelectiveVisitor {
    //     type Output = ();

    //     fn visit_function_declaration(&mut self, decl: &'ast codecrafters_interpreter::ast::FunctionDeclaration<'de>, ctx: &mut VisitContext) -> Self::Output {
    //         let name = decl.inner.name.inner.name.to_string();
    //         self.function_names.push(name.clone());
    //         println!("  Found function: {}", name);

    //         if self.skip_function_bodies {
    //             // 只访问函数名和参数，跳过函数体
    //             self.visit_identifier(&decl.inner.name, ctx);
    //             for param in &decl.inner.parameters {
    //                 self.visit_identifier(param, ctx);
    //             }
    //             println!("    Skipping function body");
    //             Self::Output::default()
    //         } else {
    //             // 正常遍历整个函数
    //             self.walk_function_declaration(decl, ctx)
    //         }
    //     }
    // }

    // let mut selective_visitor = SelectiveVisitor::new(true);
    // traverse(&program, &mut selective_visitor);
    // println!("  Function names collected: {:?}", selective_visitor.function_names);

    // println!("\n=== All examples completed ===");
}
