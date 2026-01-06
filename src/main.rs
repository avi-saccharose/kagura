use crate::parser::Parser;
use crate::parser::printer::AstPrinter;
mod lexer;
mod parser;
fn main() {
    let mut parser = Parser::new(
        r#"
        var x: int 
        var i = 8
        "#,
    );
    let ast = parser.parse().unwrap();
    let mut printer = AstPrinter::new();
    printer.print_tree(&ast);
    println!("{}", printer.finish());
}
