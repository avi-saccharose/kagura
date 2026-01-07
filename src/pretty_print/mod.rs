use crate::parser::ast::{Expr, Stmt};

pub struct AstPrinter {
    output: String,
    is_stmt: bool,
}

trait PrintStmt {
    fn print_stmt(&self, p: &mut AstPrinter, prefix: &str, is_last: bool);
}

trait PrintExpr {
    fn print_expr(&self, p: &mut AstPrinter, prefix: &str, is_last: bool);
}

impl PrintStmt for Stmt {
    fn print_stmt(&self, p: &mut AstPrinter, prefix: &str, is_last: bool) {
        match self {
            Self::Expr(expr) => expr.print_expr(p, prefix, is_last),
            _ => todo!(),
        }
    }
}

impl PrintExpr for Expr {
    fn print_expr(&self, p: &mut AstPrinter, prefix: &str, is_last: bool) {
        match self {
            Expr::Lit(lit) => {
                let lit = lit.to_string();
                p.line(prefix, is_last, &format!("Lit({lit})"))
            }
            Expr::Bin(bin) => {
                p.line(prefix, is_last, &format!("Bin({})", bin.op));
                let child_prefix = AstPrinter::child_prefix(prefix, is_last);
                bin.left.print_expr(p, &child_prefix, false);
                bin.right.print_expr(p, &child_prefix, true);
            }
            _ => todo!(),
        }
    }
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            is_stmt: true,
        }
    }

    pub fn finish(self) -> String {
        self.output
    }

    fn line(&mut self, prefix: &str, is_last: bool, text: &str) {
        self.output.push_str(prefix);

        if is_last && !self.is_stmt {
            self.output.push_str("└──");
        } else if !self.is_stmt {
            self.output.push_str("├──");
        }
        self.is_stmt = false;

        self.output.push_str(text);
        self.output.push('\n')
    }

    fn child_prefix(prefix: &str, is_last: bool) -> String {
        let mut p = String::from(prefix);
        if is_last {
            p.push_str("    ");
        } else {
            p.push_str("│   ");
        }
        p
    }

    pub fn print_program<S: PrintStmt>(&mut self, stmts: &[S]) {
        for stmt in stmts {
            self.is_stmt = true;
            stmt.print_stmt(self, "", true);
        }
    }
}
