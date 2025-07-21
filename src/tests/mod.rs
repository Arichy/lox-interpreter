// Test utility functions shared across all test modules
pub mod test_utils;

// Lexer/tokenizer tests (scanning stages)
pub mod lexer;

// Parser tests (parsing expressions stages)
pub mod parser;

// Evaluator tests (evaluating expressions stages)
pub mod evaluator;

// Runner tests (run command, statements & state stages)
pub mod runner;

// Function tests (function declarations, calls, closures)
pub mod functions;

// Basic functionality tests (print, variables, etc.)
pub mod basic_functionality;

// Closure-related tests
pub mod closures;

// Class declaration and instantiation tests
pub mod classes;

// Instance methods and 'this' keyword tests
pub mod methods;

// Member expression parsing tests
pub mod member_expressions;

// Inheritance
pub mod inheritance;
