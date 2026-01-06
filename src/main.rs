use crate::parser::Parser;
use crate::parser::printer::AstPrinter;
mod lexer;
mod parser;
fn main() {
    let mut parser = Parser::new(
        r#"
        x = 1 + 1
        var b: bool = if true then true else false + false
        var x: int 
        var i = 8
        "#,
    );
    let ast = parser.parse().unwrap();
    let mut printer = AstPrinter::new();
    printer.print_tree(&ast);
    println!("{}", printer.finish());
}
