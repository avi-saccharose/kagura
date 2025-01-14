#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Idx(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Arena<T> {
    pub(crate) data: Vec<T>,
}

#[derive(Debug)]
pub struct Ast {
    pub nodes: Arena<Node>,
    pub indices: Vec<Idx>,
}

impl Default for Ast {
    fn default() -> Self {
        Self {
            nodes: Arena::new(),
            indices: Vec::new(),
        }
    }
}

impl Ast {
    pub(crate) fn get(&self, idx: Idx) -> &Node {
        self.nodes.get(idx)
    }
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
pub(crate) struct Bin {
    pub(crate) left: Idx,
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Unary {
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Lit {
    Ident(String),
    String(String),
    Int(i64),
    Bool(bool),
    Nil,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn ast() {
        let _stmts = Arena::<Node>::new();
        //let stmt = Stmt::Expr(Expr::Lit(Lit::Bool(true)));
        //stmts.alloc(stmt);
    }
}
