# Visitor Pattern 改进设计文档

## 背景

在原有的 `Visitor` trait 实现中，每个具体的 visitor 都需要实现所有的 visit 方法，即使大部分方法只是简单地遍历子节点。这导致了大量的样板代码，使得实现新的 visitor 变得繁琐。

## 解决方案

我们为 `Visitor` trait 提供了默认实现，实现了标准的 visitor pattern：

### 主要特性

1. **默认遍历行为**：所有 visit 方法都有默认实现，自动遍历 AST 的子节点
2. **选择性重写**：只需要重写感兴趣的方法，其他方法使用默认实现
3. **类型安全**：`Output` 类型必须实现 `Default` trait，确保类型安全
4. **统一接口**：提供 `visit_expression` 和 `visit_statement` 作为入口点

### 核心设计

```rust
pub trait Visitor<'de> {
    type Output: Default;

    // 统一的表达式访问入口
    fn visit_expression(&mut self, expr: &Expression<'de>) -> Self::Output {
        match &expr.inner {
            ExpressionInner::Literal(lit) => self.visit_literal(lit),
            ExpressionInner::Identifier(id) => self.visit_identifier(id),
            // ... 其他类型的表达式
        }
    }

    // 默认实现：访问子节点
    fn visit_binary_expression(&mut self, expr: &BinaryExpression<'de>) -> Self::Output {
        self.visit_expression(&expr.inner.left);
        self.visit_expression(&expr.inner.right);
        Self::Output::default()
    }

    // 叶子节点的默认实现
    fn visit_identifier(&mut self, _identifier: &Identifier<'de>) -> Self::Output {
        Self::Output::default()
    }

    // ... 其他方法都有类似的默认实现
}
```

## 使用示例

### 示例 1: 节点计数器

```rust
pub struct NodeCounter {
    pub identifier_count: usize,
    pub function_count: usize,
    pub binary_expr_count: usize,
}

impl<'de> Visitor<'de> for NodeCounter {
    type Output = ();

    // 只需重写感兴趣的方法
    fn visit_identifier(&mut self, _identifier: &Identifier<'de>) -> Self::Output {
        self.identifier_count += 1;
        () // 使用默认行为继续遍历
    }

    fn visit_function_declaration(&mut self, decl: &FunctionDeclaration<'de>) -> Self::Output {
        self.function_count += 1;
        // 手动调用默认实现来遍历子节点
        self.visit_identifier(&decl.inner.name);
        for param in &decl.inner.parameters {
            self.visit_identifier(param);
        }
        self.visit_block_statement(&decl.inner.body);
        ()
    }

    fn visit_binary_expression(&mut self, expr: &BinaryExpression<'de>) -> Self::Output {
        self.binary_expr_count += 1;
        // 调用默认实现来遍历子节点
        self.visit_expression(&expr.inner.left);
        self.visit_expression(&expr.inner.right);
        ()
    }
}
```

### 示例 2: 标识符收集器

```rust
pub struct IdentifierCollector {
    pub identifiers: Vec<String>,
}

impl<'de> Visitor<'de> for IdentifierCollector {
    type Output = ();

    fn visit_identifier(&mut self, identifier: &Identifier<'de>) -> Self::Output {
        self.identifiers.push(identifier.inner.name.as_ref().to_string());
        ()
    }
}
```

### 示例 3: 函数计数器（最简实现）

```rust
struct FunctionCounter {
    count: usize,
}

impl<'de> Visitor<'de> for FunctionCounter {
    type Output = ();
    
    fn visit_function_declaration(&mut self, decl: &FunctionDeclaration<'de>) -> Self::Output {
        self.count += 1;
        // 使用默认实现遍历子节点
        self.visit_identifier(&decl.inner.name);
        for param in &decl.inner.parameters {
            self.visit_identifier(param);
        }
        self.visit_block_statement(&decl.inner.body);
        ()
    }
}
```

## 与原有实现的对比

### 原有实现（AstVisitor）

```rust
impl<'a, 'de> AstVisitor<'a, 'de> {
    // 需要自定义的 helper 方法
    fn visit_expression_helper(&mut self, expression: &Expression<'de>) {
        match &expression.inner {
            ExpressionInner::Literal(lit) => self.visit_literal(lit),
            ExpressionInner::Identifier(id) => self.visit_identifier(id),
            // ... 手动匹配所有表达式类型
        }
    }
}

impl<'a, 'de> Visitor<'de> for AstVisitor<'a, 'de> {
    type Output = ();

    fn visit_literal(&mut self, _literal: &Literal<'de>) -> Self::Output {}
    fn visit_identifier(&mut self, identifier: &Identifier<'de>) -> Self::Output { /* 业务逻辑 */ }
    fn visit_unary_expression(&mut self, expr: &UnaryExpression<'de>) -> Self::Output {
        self.visit_expression_helper(&expr.inner.right);
    }
    fn visit_binary_expression(&mut self, expr: &BinaryExpression<'de>) -> Self::Output {
        self.visit_expression_helper(&expr.inner.left);
        self.visit_expression_helper(&expr.inner.right);
    }
    // ... 需要实现所有 17 个方法
}
```

### 新的实现（AstVisitor）

```rust
impl<'a, 'de> Visitor<'de> for AstVisitor<'a, 'de> {
    type Output = ();

    // 只需重写有特殊逻辑的方法
    fn visit_identifier(&mut self, identifier: &Identifier<'de>) -> Self::Output {
        // 业务逻辑
    }

    fn visit_assignment_expression(&mut self, expr: &AssignmentExpression<'de>) -> Self::Output {
        // 现在直接使用标准的 visit_expression 方法
        self.visit_expression(&expr.inner.right);
        // 业务逻辑
    }

    fn visit_variable_declaration(&mut self, decl: &VariableDeclaration<'de>) -> Self::Output {
        // 业务逻辑
        if let Some(init) = &decl.inner.init {
            self.visit_expression(init); // 使用标准方法而非 visit_expression_helper
        }
    }

    fn visit_function_declaration(&mut self, decl: &FunctionDeclaration<'de>) -> Self::Output {
        // 业务逻辑
    }

    fn visit_class_declaration(&mut self, decl: &ClassDeclaration<'de>) -> Self::Output {
        // 业务逻辑
    }

    // 所有其他方法都使用默认实现
    // 不再需要自定义的 visit_expression_helper 方法
}
```

## 主要改进总结

1. **移除样板代码**：从 17 个必须实现的方法减少到只需 4 个
2. **统一接口**：提供标准的 `visit_expression` 和 `visit_statement` 方法
3. **移除 helper 方法**：不再需要自定义的 `visit_expression_helper` 方法
4. **自动遍历**：默认实现自动处理子节点的遍历

## 最佳实践

1. **最小化重写**：只重写需要特殊处理的方法
2. **使用标准方法**：使用 `visit_expression` 和 `visit_statement` 而非自定义 helper
3. **类型约束**：确保 `Output` 类型实现了 `Default` trait
4. **文档化**：为自定义 visitor 提供清晰的文档说明其用途

## 扩展性

这种设计使得添加新的 visitor 变得极其简单：

```rust
// 只需要关注特定的 AST 节点
struct MySpecialVisitor;

impl<'de> Visitor<'de> for MySpecialVisitor {
    type Output = ();
    
    fn visit_call_expression(&mut self, expr: &CallExpression<'de>) -> Self::Output {
        // 只处理函数调用
        println!("Found function call");
        // 默认实现会自动遍历子节点
        self.visit_expression(&expr.inner.callee);
        for arg in &expr.inner.arguments {
            self.visit_expression(arg);
        }
        ()
    }
}
```

这种改进大大减少了样板代码，提高了代码的可维护性和可读性。