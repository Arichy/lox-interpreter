# Deep Dive: Closure Implementation in Lox

## 1. Introduction

Closures are a fundamental feature of modern programming languages, allowing a function to "remember" and access the environment in which it was created. This interpreter implements full-featured closures that support:

-   **Variable Capture**: Capturing variables from enclosing scopes.
-   **State Mutation**: Modifying the state of captured variables.
-   **Variable Lifetime**: Captured variables live as long as the closure that captures them, even after the original scope has exited.

This document provides a detailed technical breakdown of how these features are achieved.

## 2. Core Data Structures

The implementation hinges on a few key data structures and Rust's smart pointers (`Rc` and `RefCell`) to manage shared, mutable state across different scopes.

### `Value` and `Function`

A Lox function is represented by `Value::Function`, which contains a `Function` struct. This struct is the heart of a closure.

```rust
// in src/evaluator/mod.rs

pub struct Function<'de> {
    pub name: Cow<'de, str>,
    pub parameters: Vec<Identifier<'de>>,
    pub body: BlockStatement<'de>,
    pub closure_bindings: Bindings<'de>,
    // ... other fields like 'this' for classes
}

// The key to our closure implementation!
pub type Bindings<'de> = HashMap<String, Rc<RefCell<Value<'de>>>>;
```

-   `closure_bindings`: This `HashMap` is the essence of the closure. It doesn't store copies of the captured variables' *values*. Instead, it stores **shared, mutable pointers** to them.

### The Power of `Rc<RefCell<Value>>`

This combination is what makes mutable closures possible:

-   `Rc<T>` (Reference Counted): This is a smart pointer that allows for **multiple owners** of the same piece of data. When a variable is captured by one or more closures, they all share ownership of it via `Rc`. The variable will only be deallocated when the last reference to it is dropped.

-   `RefCell<T>`: This provides **interior mutability**. Normally, you cannot get a mutable reference (`&mut T`) to data inside an `Rc`. `RefCell` bypasses this compile-time check, allowing the code to borrow a mutable reference at runtime. This is how a closure can modify a captured variable's value, and have that change be visible to other closures that captured the same variable.

### `Environment` and the Scope Chain

The `Environment` struct represents a lexical scope. It contains its own set of variable bindings and, crucially, a pointer to its parent environment.

```rust
// A simplified view of src/runner/mod.rs
pub struct Environment<'de> {
    // Each variable maps to a shared, mutable Value
    pub bindings: RefCell<HashMap<String, Rc<RefCell<Value<'de>>>>>,
    pub parent: Option<Environment<'de>>,
}
```

This `parent` link forms a **scope chain**, which is how the interpreter performs variable lookups across nested scopes.

## 3. The Lifecycle of a Closure

The implementation can be broken down into three key phases.

### Phase 1: Declaration (Capturing Upvalues)

When the interpreter encounters a function declaration (`fun` keyword), it doesn't just parse it; it analyzes its body to determine which variables to capture.

1.  **Trigger**: The `Evaluator`'s `run_statement` method encounters a `DeclarationInner::Function`.
2.  **Analysis**: It immediately calls `collect_closure_bindings`.
3.  **AST Traversal**: This function uses a specialized, temporary `AstVisitor` to traverse the *entire body* of the new function.
4.  **Identifying Upvalues**: The visitor maintains a set of variables that are local to the function (its parameters and any variables declared with `var`). When the visitor encounters an identifier, it checks:
    -   Is this identifier in the local variable set?
    -   Is it a global variable?
    -   If it's neither, it must be an **upvalue**—a variable from an enclosing scope that needs to be captured.
5.  **Capturing the Pointer**: For each upvalue, the visitor searches the current `Environment` chain. When it finds the variable, it **does not copy its value**. Instead, it clones the `Rc<RefCell<Value>>` pointer associated with that variable.
6.  **Storing the Capture**: This pointer is stored in the `closure_bindings` `HashMap` of the `Function` struct being created. The key is the variable name, and the value is the shared pointer.

### Phase 2: Invocation (Creating the Execution Environment)

When a closure is called, a new, temporary environment is created for its execution.

1.  **Trigger**: The `Evaluator`'s `evaluate_expression` method encounters a `CallExpression` where the callee is a `Value::Function`.
2.  **Environment Setup**: The `call_func` method is invoked. It calls `vm.enter_function()`.
3.  **Linking to the Past**: `enter_function` creates a new `StackFrame`. It takes the `closure_bindings` that were stored in the `Function` object during declaration and places them into the new frame. This step is critical: the new execution scope now has direct (pointer) access to the variables it needs from its parent environments.
4.  **Argument Binding**: The arguments passed to the function are evaluated and bound as local variables in the new environment.

### Phase 3: Variable Access and Mutation

This is where the `Rc<RefCell<...>>` shines.

-   **Reading a Variable**: When the code inside the closure accesses a variable, the interpreter first looks in the current function's local scope. If not found, it checks the `closure_bindings` stored in its `StackFrame`. If found, it uses the `Rc` to access the `RefCell` and calls `borrow()` to get an immutable reference to the `Value`.

-   **Writing a Variable**: When the closure assigns a new value to a captured variable (`count = count + 1;`), the lookup process is the same. Once the `Rc<RefCell<Value>>` is found, the interpreter calls `borrow_mut()` on the `RefCell`. This provides a mutable reference, allowing the underlying `Value` to be changed in place. Because the pointer is shared, this modification is instantly visible to any other part of the program that holds a reference to the same variable (e.g., another closure).

## 4. Example Walkthrough: `makeCounter`

Let's trace the execution of the classic closure example:

```lox
fun makeCounter() {
  var count = 0;
  fun increment() {
    count = count + 1;
    print count;
  }
  return increment;
}

var counter = makeCounter();
counter(); // 1
counter(); // 2
```

1.  **`makeCounter` Declaration**: The global function `makeCounter` is defined. It has no upvalues.

2.  **`makeCounter()` Call**: `makeCounter` is called.
    -   A new environment is created for this call.
    -   Inside this environment, `var count = 0;` is executed. A variable named `count` is created, and its value is stored in an `Rc<RefCell<Value(0)>>`.

3.  **`increment` Declaration**: Inside the `makeCounter` call, the `increment` function is declared.
    -   `collect_closure_bindings` is called for `increment`.
    -   The visitor sees the identifier `count`. It's not a parameter of `increment` and not a global. It's an upvalue.
    -   The visitor looks in the current environment (the one for the `makeCounter` call), finds `count`, and **clones the `Rc<RefCell<Value(0)>>` pointer**.
    -   The `increment` `Function` object is created, with `{"count": Rc<..._>}` in its `closure_bindings`.

4.  **Return `increment`**: `makeCounter` returns the `increment` function value. The environment for the `makeCounter` call is popped from the stack. However, the `count` variable is **not deallocated**, because the `increment` closure still holds an `Rc` reference to it.

5.  **`counter()` First Call**:
    -   `call_func` is invoked on the `increment` function value (now stored in the `counter` variable).
    -   `vm.enter_function` creates a new stack frame and populates it with the `closure_bindings` from `increment`. The new frame now has access to the original `count` variable.
    -   `count = count + 1` is executed. The interpreter finds `count` in the closure bindings, gets a mutable reference via `borrow_mut()`, and updates the value to `1`.

6.  **`counter()` Second Call**:
    -   The process repeats. The same `closure_bindings` are used. `count` is found, and its value is incremented to `2`.

This mechanism correctly preserves the state of `count` across calls, demonstrating a successful closure implementation.