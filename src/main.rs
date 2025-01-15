use std::io::{self, Read, Write};

use error::KaguError;
use interpreter::Interpreter;
mod error;
mod expr;
mod interpreter;
mod lexer;
mod parser;
mod token;
mod values;
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
    println!("-ast: Toggle print ast nodes");
    println!("-q: Exit the repl");
}

fn repl() {
    let mut interpreter = Interpreter::new();
    let mut print_ast = false;
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
            "-ast" => {
                print_ast = !print_ast;
                println!("print ast = {print_ast:}");
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
                if print_ast {
                    dbg!(&res);
                }

                if let Err(e) = interpreter.eval(&res) {
                    print_error(e, &line);
                }
            }
        }
    }
}

fn read_file(name: &str) -> io::Result<String> {
    let mut source = std::fs::File::open(name)?;
    let mut string = String::new();
    source.read_to_string(&mut string)?;
    println!("{:?}", source);
    Ok(string)
}

fn run_file(name: &str) {
    let source = read_file(name);
    match source {
        Ok(source) => {
            let mut interpreter = Interpreter::new();
            let lexed = lexer::tokenize(&source);
            if let Err(e) = lexed {
                print_error(e, &source);
                std::process::exit(3);
            }

            let ast = parser::parse(&source);
            match ast {
                Err(e) => {
                    print_error(e, &source);
                    std::process::exit(3);
                }
                Ok(res) => {
                    dbg!(&res);
                    if let Err(e) = interpreter.eval(&res) {
                        print_error(e, &source);
                    }
                }
            }
        }
        Err(e) => {
            eprint!("{:?}", e);
            std::process::exit(1);
        }
    }
}

fn main() {
    if std::env::args().len() > 1 {
        let name: Vec<String> = std::env::args().collect();
        run_file(&name[1]);
    } else {
        repl();
    }
}
