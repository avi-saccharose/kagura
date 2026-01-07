use std::fmt::{self};

use crate::lexer::token::{Kind, Token};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Stmt {
    Block(Block),
    If(IfStmt),
    Def(Def),
    Puts(Puts),
    Expr(Expr),
    VarDecl(VarDecl),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct IfStmt {
    pub cond: Expr,
    pub then_body: Box<Stmt>,
    pub else_body: Option<Box<Stmt>>,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Def {
    pub name: Ident,
    pub ret_kind: Option<Kind>,
    pub args: Vec<(Expr, Kind)>,
    pub body: Vec<Stmt>,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Puts {
    pub expr: Expr,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct VarDecl {
    pub name: String,
    pub kind: Option<Kind>,
    pub init: Option<Expr>,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    If(If),
    Assign(Assign),
    Bin(Bin),
    Unary(Unary),
    Call(Call),
    Ident(Ident),
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct If {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Assign {
    pub name: String,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Bin {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub op: Kind,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Unary {
    pub right: Box<Expr>,
    pub op: Kind,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Ident(pub String, pub Token);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Lit {
    Int(i64, Token),
    Float(f64, Token),
    Bool(bool, Token),
    Str(String, Token),
    Nil,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int(val, _) => val.to_string(),
                Self::Float(val, _) => val.to_string(),
                Self::Bool(val, _) => val.to_string(),
                Self::Str(val, _) => val.to_string(),
                Self::Nil => "nil".to_string(),
            }
        )
    }
}
