#![allow(unused)]

pub mod lex;
pub use lex::Lexer;

pub mod parse;
pub use parse::Parser;

pub mod evaluate;

pub mod run;

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
