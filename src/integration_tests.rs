// Integration tests for the Lox interpreter
//
// This module serves as the entry point for all integration tests.
// Tests are organized into separate modules by functionality:
// - basic_functionality: Simple print and variable tests
// - closures: All closure-related functionality
// - classes: Class declaration and instantiation
// - methods: Instance methods and 'this' keyword
// - member_expressions: Member expression parsing

// Re-export all test modules
#[cfg(test)]
pub use crate::tests::*;