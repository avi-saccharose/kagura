use crate::parser::ast::{Assign, Bin, Expr, If, Lit, Stmt, VarDecl};

pub struct AstPrinter {
    indent: usize,
    output: String,
    is_stmt: bool,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            indent: 0,
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

    pub fn print_tree(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.is_stmt = true;
            self.print_stmt(stmt, "", true);
        }
    }

    fn print_stmt(&mut self, stmt: &Stmt, prefix: &str, is_last: bool) {
        match stmt {
            Stmt::Expr(expr) => self.print_expr(expr, prefix, is_last),
            Stmt::VarDecl(var) => self.print_var_decl(var, prefix, is_last),
            _ => todo!(),
        }
    }

    fn print_var_decl(&mut self, var: &VarDecl, prefix: &str, is_last: bool) {
        let ident = &var.name;
        self.line(prefix, is_last, &format!("Var({ident})"));

        let child_prefix = Self::child_prefix(prefix, is_last);

        if let Some(kind) = &var.kind {
            let is_last = var.init.is_none();
            self.line(&child_prefix, is_last, "Type");
            self.line(
                &Self::child_prefix(&child_prefix, is_last),
                true,
                &kind.to_string(),
            );
        }

        if let Some(init) = &var.init {
            self.line(&child_prefix, true, "Init");
            self.print_expr(init, &Self::child_prefix(&child_prefix, true), is_last);
        }
    }

    fn print_expr(&mut self, expr: &Expr, prefix: &str, is_last: bool) {
        match expr {
            Expr::If(expr) => self.print_if(expr, prefix, is_last),
            Expr::Assign(assign) => self.print_assign(assign, prefix, is_last),
            Expr::Bin(bin) => self.print_binary(bin, prefix, is_last),
            Expr::Lit(lit) => self.print_lit(lit, prefix, is_last),
            _ => todo!(),
        }
    }

    fn print_if(&mut self, expr: &If, prefix: &str, is_last: bool) {
        self.line(prefix, is_last, "If");

        let child_prefix = Self::child_prefix(prefix, is_last);

        self.line(&child_prefix, false, "Cond");
        self.print_expr(
            &expr.cond,
            &Self::child_prefix(&child_prefix, false),
            is_last,
        );

        self.line(&child_prefix, false, "Then");
        self.print_expr(
            &expr.then_expr,
            &Self::child_prefix(&child_prefix, false),
            is_last,
        );
        self.line(&child_prefix, true, "Else");
        self.print_expr(
            &expr.else_expr,
            &Self::child_prefix(&child_prefix, true),
            true,
        );
    }

    fn print_assign(&mut self, assign: &Assign, prefix: &str, is_last: bool) {
        let ident = &assign.name;
        self.line(prefix, is_last, &format!("Assign({ident})"));

        let child_prefix = Self::child_prefix(prefix, is_last);
        self.line(&child_prefix, true, "Value");
        self.print_expr(
            &assign.value,
            &Self::child_prefix(&child_prefix, is_last),
            true,
        );
    }

    fn print_binary(&mut self, bin: &Bin, prefix: &str, is_last: bool) {
        let op = bin.op.to_string();
        self.line(prefix, is_last, &format!("Bin({op})"));

        let child_prefix = Self::child_prefix(prefix, is_last);

        self.print_expr(&bin.left, &child_prefix, false);
        self.print_expr(&bin.right, &child_prefix, true);
    }

    fn print_lit(&mut self, lit: &Lit, prefix: &str, is_last: bool) {
        let lit = lit.to_string();
        self.line(prefix, is_last, &format!("Lit({lit})"));
    }
}
