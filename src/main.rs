#![allow(unused)]

use clap::{Parser, Subcommand};
use codecrafters_interpreter as imp;
use imp::{evaluator, runner};
use miette::IntoDiagnostic;
use miette::WrapErr;
use std::fs;
use std::os::unix::process;
use std::path::PathBuf;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    Evaluate { filename: PathBuf },
    Run { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            let mut any_cc_error = false;
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read file: {}", filename.display()))?;

            let lexer = imp::Lexer::new(&file_contents);

            for token in lexer {
                let token = match token {
                    Ok(token) => token,
                    Err(e) => {
                        imp::log_stderr!("{e:?}");
                        if let Some(unrecognized) = e.downcast_ref::<imp::error::SingleTokenError>()
                        {
                            any_cc_error = true;

                            imp::log_stderr!(
                                "[line {}] Error: Unexpected character: {}",
                                unrecognized.line(),
                                unrecognized.token
                            );
                        } else if let Some(unterminated) =
                            e.downcast_ref::<imp::error::StringTerminationError>()
                        {
                            any_cc_error = true;
                            imp::log_stderr!(
                                "[line {}] Error: Unterminated string.",
                                unterminated.line(),
                            );
                        }
                        continue;
                    }
                };
                imp::log_stdout!("{token}");
            }
            imp::log_stdout!("EOF  null");

            if any_cc_error {
                std::process::exit(65);
            }
        }

        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read file: {}", filename.display()))?;

            let mut parser = imp::Parser::new(&file_contents);

            match parser.parse() {
                Ok(tt_list) => {
                    for tt in tt_list {
                        imp::log_stdout!("{tt}")
                    }
                }
                Err(e) => {
                    // TODO: match error line format
                    imp::log_stderr!("{e:?}");
                    std::process::exit(65);
                }
            }
        }

        Commands::Evaluate { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read file: {}", filename.display()))?;

            let mut evaluator = evaluator::Evaluator::new(&file_contents);
            let mut vm = runner::Vm::new();

            match evaluator.evaluate_command(&mut vm) {
                Ok(evaluate_result) => {
                    imp::log_stdout!("{evaluate_result}");
                }
                Err(e) => {
                    imp::log_stderr!("{e:?}");
                    std::process::exit(70);
                }
            }
        }

        Commands::Run { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read file: {}", filename.display()))?;

            let runner = runner::Runner::new(&file_contents);

            if let Err(e) = runner.run() {
                imp::log_stderr!("{e:?}");
                std::process::exit(70);
            }
        }

        _ => {
            unimplemented!()
        }
    }

    Ok(())
}
