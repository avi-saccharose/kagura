#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Idx(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Arena<T> {
    pub(crate) data: Vec<T>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::<T>::new(),
        }
    }
    pub(crate) fn alloc(&mut self, value: T) -> Idx {
        let idx = self.data.len();
        self.data.push(value);
        Idx(idx)
    }

    pub(crate) fn get(&self, idx: Idx) -> &T {
        &self.data[idx.0]
    }
}

// TODO: combine both statements and expressions into a single enum to use a single ast
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Stmt {
    Expr(Idx),
    Puts(Idx),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Node {
    BinExpr(Bin),
    Unary(Unary),
    Lit(Lit),
    Puts(Idx),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Block {
    pub(crate) start: Idx,
    pub(crate) end: Idx,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Bin(Bin),
    Lit(Lit),
    Unary(Unary),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Bin {
    pub(crate) left: Idx,
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Unary {}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Lit {
    Ident(String),
    Int(i64),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Ast {
    pub(crate) stmts: Arena<Stmt>,
    pub(crate) exprs: Arena<Expr>,
}

impl Ast {
    pub fn new(stmts: Arena<Stmt>, exprs: Arena<Expr>) -> Self {
        Self { stmts, exprs }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn ast() {
        let _stmts = Arena::<Stmt>::new();
        //let stmt = Stmt::Expr(Expr::Lit(Lit::Bool(true)));
        //stmts.alloc(stmt);
    }
}
