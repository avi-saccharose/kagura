use crate::analyzer::TypeChecker;
use crate::parser::Parser;
use crate::pretty_print::AstPrinter;
mod analyzer;
mod lexer;
mod parser;
mod pretty_print;
mod printer;
fn main() {
    let mut parser = Parser::new(
        r#"
        1 + 2 + 4
        "#,
    );
    let ast = parser.parse().unwrap();
    let mut printer = AstPrinter::new();
    printer.print_program(&ast);
    let result = printer.finish();
    println!("{result}");

    //     printer.print_tree(ast.iter().map(|s| s as &dyn StmtView));
    //     println!("{}", printer.finish());
    //
    //     let mut type_checker = TypeChecker::new();
    //     let checked_ast = type_checker.check_program(
    //         Parser::new(
    //             "
    // 1 + 1",
    //         )
    //         .parse()
    //         .unwrap(),
    //     );
    //
    //     match checked_ast {
    //         Ok(ast) => {
    //             let mut tree = AstPrinter::new();
    //             tree.print_tree(ast.iter().map(|s| s as &dyn StmtView));
    //             println!("{}", tree.finish());
    //         }
    //         Err(e) => {
    //             dbg!(e);
    //         }
    //     };
}
