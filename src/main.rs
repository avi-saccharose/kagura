use std::io::{self, Write};

use error::KaguError;

mod error;
mod expr;
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

// For now just parse and print the error if any
fn repl() {
    loop {
        let mut line = String::new();
        print!(">> ");
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut line)
            .expect("failed to read line");

        let lexed = lexer::tokenize(&line);
        if let Err(e) = lexed {
            print_error(e, &line);
            continue;
        }

        let ast = parser::parse(&line);

        if let Err(e) = ast {
            print_error(e, &line);
            continue;
        }
    }
}

fn main() {
    repl();
}
