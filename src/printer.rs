// use crate::{
//     analyzer::{Op, TExpr, TExprKind, TLit, TStmt, Ty},
//     parser::ast::{Assign, Bin, Expr, If, Lit, Stmt},
// };
//
// pub trait StmtView {
//     fn kind(&self) -> StmtKindView<'_>;
// }
//
// trait ExprView {
//     fn kind(&self) -> ExprKindView<'_>;
//     fn ty(&self) -> Option<Ty>;
// }
//
// enum StmtKindView<'a> {
//     Block(&'a [&'a dyn StmtView]),
//     If {
//         cond: &'a dyn ExprView,
//         then_body: &'a dyn StmtView,
//         else_body: Option<&'a dyn StmtView>,
//     },
//     Def {
//         name: &'a dyn ExprView,
//         args: &'a [&'a dyn ExprView],
//         ret_kind: &'a [&'a dyn ExprView],
//         body: &'a [&'a dyn StmtView],
//     },
//     Puts {
//         expr: &'a dyn ExprView,
//     },
//     Expr(&'a dyn ExprView),
//     VarDecl {
//         name: &'a str,
//         init: Option<&'a dyn ExprView>,
//     },
// }
//
// struct VarDecl<'a> {
//     name: &'a str,
//     init: Option<&'a dyn ExprView>,
// }
//
// enum ExprKindView<'a> {
//     Bin {
//         left: &'a dyn ExprView,
//         right: &'a dyn ExprView,
//         op: Op,
//     },
//     Lit(LitView<'a>),
// }
//
// #[derive(Debug)]
// enum LitView<'a> {
//     Int(i64),
//     Bool(bool),
//     Float(f64),
//     Str(&'a str),
// }
//
// impl StmtView for Stmt {
//     fn kind(&self) -> StmtKindView<'_> {
//         match self {
//             Stmt::Block(block) => {
//                 let stmts = block.stmts;
//                 StmtKindView::Block(stmts.as_ref())
//             }
//             Stmt::Expr(expr) => StmtKindView::Expr(expr),
//             Stmt::VarDecl(var) => StmtKindView::VarDecl {
//                 name: &var.name,
//                 init: var.init.as_ref().map(|e| e as &'_ dyn ExprView),
//             },
//             _ => todo!(),
//         }
//     }
// }
//
// impl StmtView for TStmt {
//     fn kind(&self) -> StmtKindView<'_> {
//         match self {
//             TStmt::Expr(expr) => StmtKindView::Expr(expr),
//         }
//     }
// }
//
// impl ExprView for Expr {
//     fn kind(&self) -> ExprKindView<'_> {
//         match self {
//             Expr::Bin(bin) => ExprKindView::Bin {
//                 left: bin.left.as_ref(),
//                 right: bin.right.as_ref(),
//                 op: bin.op.into(),
//             },
//             Expr::Lit(lit) => ExprKindView::Lit(match lit {
//                 Lit::Int(val, _) => LitView::Int(*val),
//                 Lit::Bool(val, _) => LitView::Bool(*val),
//                 Lit::Str(val, _) => LitView::Str(val),
//                 _ => todo!(),
//             }),
//             _ => todo!(),
//         }
//     }
//     fn ty(&self) -> Option<Ty> {
//         None
//     }
// }
//
// impl ExprView for TExpr {
//     fn kind(&self) -> ExprKindView<'_> {
//         match &self.expr {
//             TExprKind::Bin(bin) => ExprKindView::Bin {
//                 left: bin.left.as_ref(),
//                 right: bin.right.as_ref(),
//                 op: bin.op,
//             },
//             TExprKind::Lit(lit) => ExprKindView::Lit(match lit {
//                 TLit::Int(val, _) => LitView::Int(*val),
//                 TLit::Str(val, _) => LitView::Str(&val),
//                 TLit::Bool(val, _) => LitView::Bool(*val),
//                 _ => todo!(),
//             }),
//             _ => todo!(),
//         }
//     }
//
//     fn ty(&self) -> Option<Ty> {
//         Some(self.ty)
//     }
// }
//
// pub struct AstPrinter {
//     output: String,
//     is_stmt: bool,
// }
//
// impl AstPrinter {
//     pub fn new() -> Self {
//         Self {
//             output: String::new(),
//             is_stmt: true,
//         }
//     }
//     pub fn finish(self) -> String {
//         self.output
//     }
//
//     fn line(&mut self, prefix: &str, is_last: bool, text: &str) {
//         self.output.push_str(prefix);
//
//         if is_last && !self.is_stmt {
//             self.output.push_str("└──");
//         } else if !self.is_stmt {
//             self.output.push_str("├──");
//         }
//         self.is_stmt = false;
//
//         self.output.push_str(text);
//         self.output.push('\n')
//     }
//
//     fn child_prefix(prefix: &str, is_last: bool) -> String {
//         let mut p = String::from(prefix);
//         if is_last {
//             p.push_str("    ");
//         } else {
//             p.push_str("│   ");
//         }
//         p
//     }
//
//     pub fn print_tree<'a>(&mut self, stmts: impl IntoIterator<Item = &'a dyn StmtView>) {
//         for stmt in stmts {
//             self.is_stmt = true;
//             self.print_stmt(stmt, "", true);
//         }
//     }
//
//     fn print_stmt(&mut self, stmt: &dyn StmtView, prefix: &str, is_last: bool) {
//         match stmt.kind() {
//             StmtKindView::Expr(expr) => self.print_expr(expr, prefix, is_last),
//             // Stmt::VarDecl(var) => self.print_var_decl(var, prefix, is_last),
//             _ => todo!(),
//         }
//     }
//
//     // fn print_var_decl(&mut self, var: &VarDecl, prefix: &str, is_last: bool) {
//     //     let ident = &var.name;
//     //     self.line(prefix, is_last, &format!("Var({ident})"));
//     //
//     //     let child_prefix = Self::child_prefix(prefix, is_last);
//     //
//     //     if let Some(kind) = &var.kind {
//     //         let is_last = var.init.is_none();
//     //         self.line(&child_prefix, is_last, "Type");
//     //         self.line(
//     //             &Self::child_prefix(&child_prefix, is_last),
//     //             true,
//     //             &kind.to_string(),
//     //         );
//     //     }
//     //
//     //     if let Some(init) = &var.init {
//     //         self.line(&child_prefix, true, "Init");
//     //         self.print_expr(init, &Self::child_prefix(&child_prefix, true), is_last);
//     //     }
//     // }
//
//     fn print_expr(&mut self, expr: &dyn ExprView, prefix: &str, is_last: bool) {
//         match expr.kind() {
//             // Expr::If(expr) => self.print_if(expr, prefix, is_last),
//             // Expr::Assign(assign) => self.print_assign(assign, prefix, is_last),
//             ExprKindView::Bin { left, right, op } => {
//                 let mut text = format!("Bin({op})");
//                 if let Some(ty) = expr.ty() {
//                     text.push_str(": ");
//                     text.push_str(&ty.to_string());
//                 }
//                 self.line(prefix, is_last, &text);
//                 self.print_binary(left, right, op, prefix, is_last);
//                 let child_prefix = Self::child_prefix(prefix, is_last);
//
//                 self.print_expr(left, &child_prefix, false);
//                 self.print_expr(right, &child_prefix, true);
//             }
//             ExprKindView::Lit(lit) => {
//                 let mut text = format!("Lit({:?})", lit);
//                 if let Some(ty) = expr.ty() {
//                     text.push_str(&format!(": {}", ty.to_string()));
//                 }
//                 self.line(prefix, is_last, &text);
//             }
//             // Expr::Lit(lit) => self.print_lit(lit, prefix, is_last),
//             _ => todo!(),
//         }
//     }
//
//     // fn print_if(&mut self, expr: &If, prefix: &str, is_last: bool) {
//     //     self.line(prefix, is_last, "If");
//     //
//     //     let child_prefix = Self::child_prefix(prefix, is_last);
//     //
//     //     self.line(&child_prefix, false, "Cond");
//     //     self.print_expr(
//     //         &expr.cond,
//     //         &Self::child_prefix(&child_prefix, false),
//     //         is_last,
//     //     );
//     //
//     //     self.line(&child_prefix, false, "Then");
//     //     self.print_expr(
//     //         &expr.then_expr,
//     //         &Self::child_prefix(&child_prefix, false),
//     //         is_last,
//     //     );
//     //     self.line(&child_prefix, true, "Else");
//     //     self.print_expr(
//     //         &expr.else_expr,
//     //         &Self::child_prefix(&child_prefix, true),
//     //         true,
//     //     );
//     // }
//     //
//     // fn print_assign(&mut self, assign: &Assign, prefix: &str, is_last: bool) {
//     //     let ident = &assign.name;
//     //     self.line(prefix, is_last, &format!("Assign({ident})"));
//     //
//     //     let child_prefix = Self::child_prefix(prefix, is_last);
//     //     self.line(&child_prefix, true, "Value");
//     //     self.print_expr(
//     //         &assign.value,
//     //         &Self::child_prefix(&child_prefix, is_last),
//     //         true,
//     //     );
//     // }
//
//     fn print_binary(
//         &mut self,
//         left: &dyn ExprView,
//         right: &dyn ExprView,
//         op: Op,
//         prefix: &str,
//         is_last: bool,
//     ) {
//         //let op = bin.op.to_string();
//     }
//
//     fn print_lit(&mut self, lit: &Lit, prefix: &str, is_last: bool) {
//         let lit = lit.to_string();
//         self.line(prefix, is_last, &format!("Lit({lit})"));
//     }
// }
