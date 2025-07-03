#![allow(unused)]

pub mod lexer;
pub use lexer::Lexer;

pub mod parser;
pub use parser::Parser;

mod ast;

pub mod evaluator;

pub mod runner;

pub mod error;

#[macro_export]
macro_rules! log_stdout {
    ($($arg:tt)*) => {
        println!($($arg)*);
    };
}

#[macro_export]
macro_rules! log_stderr {
    ($($arg:tt)*) => {
        eprintln!($($arg)*);
    };
}
