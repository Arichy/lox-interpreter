# AST Visitor Pattern Design

## 1. Background

The visitor pattern is a crucial component of this interpreter, enabling powerful static analysis, code validation, and other AST transformations. The design implemented in `src/ast/visitor.rs` provides a flexible and context-aware mechanism for traversing the Abstract Syntax Tree (AST).

This design separates the traversal logic from the operations performed on the nodes, allowing for clean and modular implementation of new analyses. Its key features are fine-grained traversal control and rich contextual information at every step of the traversal.

## 2. Core Components

### The `Visitor` Trait

This is the central trait that any visitor must implement.

```rust
pub trait Visitor<'ast, 'de> {
    type Output: Default;
    type Error;

    // visit_* methods for every AST node...
    fn visit_program(...) -> Result<Self::Output, Self::Error> { ... }
    fn visit_statement(...) -> Result<Self::Output, Self::Error> { ... }
    fn visit_expression(...) -> Result<Self::Output, Self::Error> { ... }

    // walk_* methods for every AST node...
    fn walk_program(...) -> Result<Self::Output, Self::Error> { ... }
    fn walk_statement(...) -> Result<Self::Output, Self::Error> { ... }
    fn walk_expression(...) -> Result<Self::Output, Self::Error> { ... }
}
```

- It is generic over the lifetimes `'ast` and `'de`.
- It defines two associated types: `Output` for the result of a visit and `Error` for any errors that may occur.
- It provides a `visit_*` and `walk_*` method for every node in the AST.

### The `visit_*` vs. `walk_*` Distinction

This separation is the core of the traversal control mechanism.

- **`visit_*` methods**: These are the primary interface for implementers. You override these methods to implement custom logic for a specific node type. By default, a call to `visit_node(node, ctx)` simply delegates to `walk_node(node, ctx)`.

- **`walk_*` methods**: These methods contain the default AST traversal logic. For example, `walk_binary_expression` will recursively call `visit_expression` on its `left` and `right` children. They also manage the `VisitContext` by pushing and popping nodes from the ancestor stack.

This design allows a visitor to:
1.  **Augment behavior**: Execute code before or after the default traversal by calling the `walk_*` method inside your `visit_*` implementation.
2.  **Take full control**: Override a `visit_*` method and *not* call the corresponding `walk_*` method to implement a completely custom traversal logic for that branch of the tree.

### The `VisitContext`

This is a stateful object passed mutably through the traversal, providing rich contextual information about the current node.

```rust
pub struct VisitContext<'ast, 'de> {
    pub parent: Option<Node<'ast, 'de>>,
    pub ancestors: Vec<Node<'ast, 'de>>,
    pub scope_stack: Vec<Scope<'de>>,
    pub current_scope_id: ScopeId,
}
```

- `parent`: The direct parent of the current node.
- `ancestors`: A stack of all ancestor nodes, from the root to the parent.
- `scope_stack`: A stack of lexical scopes (`Global`, `Function`, `Block`) used to track variable declarations and visibility.
- It provides methods for scope management (`push_scope`, `pop_scope`) and variable tracking (`declare_variable`, `is_variable_declared`).

## 3. How to Implement a Visitor

#### Step 1: Define Your Visitor Struct
Create a struct to hold any state your visitor needs to maintain during traversal (e.g., counters, collected identifiers).

```rust
struct FunctionCounter {
    count: usize,
}
```

#### Step 2: Implement the `Visitor` Trait
Implement the `Visitor` trait for your struct, defining the `Output` and `Error` types.

```rust
use super::{Visitor, FunctionDeclaration, VisitContext};

impl<'ast, 'de> Visitor<'ast, 'de> for FunctionCounter {
    type Output = ();
    type Error = std::io::Error; // Or your custom error type
    
    // ... override methods here
}
```

#### Step 3: Override `visit_*` Methods
For each AST node you want to handle, override the corresponding `visit_*` method.

```rust
fn visit_function_declaration(
    &mut self,
    decl: &'ast FunctionDeclaration<'de>,
    ctx: &mut VisitContext<'ast, 'de>,
) -> Result<Self::Output, Self::Error> {
    // Custom logic: increment the counter
    self.count += 1;
    
    // Delegate to the default traversal logic to visit children
    self.walk_function_declaration(decl, ctx)
}
```

## 4. Usage Examples

### Example 1: Simple Node Counter

This visitor counts the number of function declarations. It only needs to override one method and relies on the default `walk_*` implementation to continue the traversal.

```rust
struct FunctionCounter {
    count: usize,
}

impl<'ast, 'de> Visitor<'ast, 'de> for FunctionCounter {
    type Output = ();
    type Error = (); // No errors expected

    fn visit_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.count += 1;
        // Continue traversal to find nested functions
        self.walk_function_declaration(decl, ctx)
    }
}
```

### Example 2: Context-Aware Variable Analyzer

This visitor uses the `VisitContext` to find uses of undeclared variables.

```rust
struct VariableAnalyzer {
    undeclared_variables: Vec<String>,
}

impl<'ast, 'de> Visitor<'ast, 'de> for VariableAnalyzer {
    type Output = ();
    type Error = ();

    // When a variable is declared, record it in the current scope
    fn visit_variable_declaration(
        &mut self,
        decl: &'ast VariableDeclaration<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        // The walk_variable_declaration method handles this automatically
        self.walk_variable_declaration(decl, ctx)
    }

    // When an identifier is used, check if it was declared in any visible scope
    fn visit_identifier(
        &mut self,
        ident: &'ast Identifier<'de>,
        ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        if !ctx.is_variable_declared(&ident.inner.name) {
            self.undeclared_variables.push(ident.inner.name.to_string());
        }
        Ok(())
    }
}
```

### Example 3: Controlling Traversal

This visitor collects function names but intentionally skips traversing into their bodies.

```rust
struct FunctionNameCollector {
    names: Vec<String>,
}

impl<'ast, 'de> Visitor<'ast, 'de> for FunctionNameCollector {
    type Output = ();
    type Error = ();

    fn visit_function_declaration(
        &mut self,
        decl: &'ast FunctionDeclaration<'de>,
        _ctx: &mut VisitContext<'ast, 'de>,
    ) -> Result<Self::Output, Self::Error> {
        self.names.push(decl.inner.name.inner.name.to_string());
        
        // By NOT calling walk_function_declaration, we prevent the
        // visitor from descending into the function's parameters and body.
        Ok(())
    }
}
```

## 5. Key Benefits of This Design

1.  **Maximum Flexibility**: The `visit_*` vs. `walk_*` separation provides complete control over the traversal process.
2.  **Context-Awareness**: The `VisitContext` provides rich, stateful information about the current node's scope and ancestry.
3.  **Low Boilerplate**: For simple visitors, you only need to override the methods for the nodes you care about. The default `walk_*` methods handle the rest.
4.  **Clean State Management**: Visitor-specific state is held within the visitor struct, while traversal-specific state is managed by the `VisitContext`.
