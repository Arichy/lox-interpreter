#![allow(unused)]

pub mod lexer;
pub use lexer::Lexer;

pub mod parser;
pub use parser::Parser;

mod ast;

pub mod evaluator;

pub mod runner;

pub mod error;

#[cfg(test)]
mod integration_tests;

// Thread-local storage for capturing output during tests
#[cfg(test)]
thread_local! {
    static CAPTURED_STDOUT: std::cell::RefCell<Option<Vec<String>>> = std::cell::RefCell::new(None);
    static CAPTURED_STDERR: std::cell::RefCell<Option<Vec<String>>> = std::cell::RefCell::new(None);
}

// Helper functions for testing
#[cfg(test)]
pub fn start_capture() {
    CAPTURED_STDOUT.with(|output| {
        *output.borrow_mut() = Some(Vec::new());
    });
    CAPTURED_STDERR.with(|output| {
        *output.borrow_mut() = Some(Vec::new());
    });
}

#[cfg(test)]
pub fn end_capture() -> (Vec<String>, Vec<String>) {
    let stdout = CAPTURED_STDOUT.with(|output| output.borrow_mut().take().unwrap_or_default());
    let stderr = CAPTURED_STDERR.with(|output| output.borrow_mut().take().unwrap_or_default());
    (stdout, stderr)
}

#[cfg(test)]
pub fn get_captured_stdout() -> String {
    CAPTURED_STDOUT.with(|output| {
        if let Some(ref captured) = *output.borrow() {
            captured.join("\n")
        } else {
            String::new()
        }
    })
}

#[cfg(test)]
pub fn get_captured_stderr() -> String {
    CAPTURED_STDERR.with(|output| {
        if let Some(ref captured) = *output.borrow() {
            captured.join("\n")
        } else {
            String::new()
        }
    })
}

// Internal functions to handle stdout/stderr capture
#[cfg(test)]
pub fn capture_stdout_line(line: String) {
    CAPTURED_STDOUT.with(|output| {
        if let Some(ref mut captured) = *output.borrow_mut() {
            captured.push(line);
        } else {
            println!("{}", line);
        }
    });
}

#[cfg(test)]
pub fn capture_stderr_line(line: String) {
    CAPTURED_STDERR.with(|output| {
        if let Some(ref mut captured) = *output.borrow_mut() {
            captured.push(line);
        } else {
            eprintln!("{}", line);
        }
    });
}

#[cfg(not(test))]
pub fn capture_stdout_line(line: String) {
    println!("{}", line);
}

#[cfg(not(test))]
pub fn capture_stderr_line(line: String) {
    eprintln!("{}", line);
}

#[macro_export]
macro_rules! log_stdout {
    ($($arg:tt)*) => {{
        let formatted = format!($($arg)*);
        $crate::capture_stdout_line(formatted);
    }};
}

#[macro_export]
macro_rules! log_stderr {
    ($($arg:tt)*) => {{
        let formatted = format!($($arg)*);
        $crate::capture_stderr_line(formatted);
    }};
}
