use crate::analyzer::TypeChecker;
use crate::parser::Parser;
use crate::printer::AstPrinter;
mod analyzer;
mod lexer;
mod parser;
mod printer;
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

    let mut type_checker = TypeChecker::new();
    let checked_ast = type_checker.check_program(
        Parser::new(
            "
1 + true",
        )
        .parse()
        .unwrap(),
    );

    match checked_ast {
        Ok(ast) => println!("{}", AstPrinter::new().print_tree(ast).finish()),
        Err(e) => dbg!(e),
    };
}
