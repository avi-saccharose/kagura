use std::io::{self, Write};

use error::KaguError;
use interpreter::Interpreter;
mod error;
mod expr;
mod interpreter;
mod lexer;
mod parser;
mod token;

fn print_error(e: KaguError, source: &str) {
    let line = e.line as usize;
    let column = e.column as usize;
    let text = source.lines().nth(line - 1).unwrap();
    eprintln!("{}", e);
    eprintln!("{} | {}\n", line, text);

    eprintln!("{}", text);
    eprintln!("{: >column$} here", "^");
}

fn print_help() {
    println!("Kagura Compiler");
    println!("-help: Show this");
    println!("-env: Print the env stack");
    println!("-q: Exit the repl");
}

fn repl() {
    let mut interpreter = Interpreter::new();

    println!("Kagura Compiler input '-help' for help");

    loop {
        let mut line = String::new();
        print!(">> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read line");

        match line.trim_ascii() {
            "-help" => {
                print_help();
                continue;
            }
            "-env" => {
                dbg!(&interpreter.env);
                continue;
            }
            "-q" => {
                std::process::exit(0);
            }
            _ => {}
        }
        let lexed = lexer::tokenize(&line);
        if let Err(e) = lexed {
            print_error(e, &line);
            continue;
        }

        let ast = parser::parse(&line);
        match ast {
            Err(e) => {
                print_error(e, &line);
                continue;
            }
            Ok(res) => {
                if let Err(e) = interpreter.eval(&res) {
                    print_error(e, &line);
                }
            }
        }
    }
}

fn main() {
    repl();
}
