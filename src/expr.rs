#![allow(dead_code)]

use crate::token::Token;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Idx(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Arena<T> {
    pub(crate) data: Vec<T>,
}

/*
impl From<Block> for ops::Range<Idx> {
    fn from(block: Block) -> Self {
        block.start..block.end
    }
}
*/

#[derive(Debug, Clone)]
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

    pub(crate) fn get_from_range(&self, start: Idx, end: Idx) -> &[Node] {
        self.nodes.get_from_range(start, end)
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

    pub(crate) fn get_from_range(&self, start: Idx, end: Idx) -> &[T] {
        &self.data[start.0..end.0]
    }

    pub(crate) fn pop(&mut self) -> Option<T> {
        self.data.pop()
    }
}

// TODO: For loops
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Node {
    // Statements
    // Functions
    Def(Def),
    // IF Statement
    If(If),
    // Variable Declaration
    VarDecl(VarDecl),
    // While Statements
    While(While),
    // Return Statement
    Return(Return),
    // Block Statements
    // Also used for def param and args
    Block(Vec<Idx>),
    // Puts Statement
    Puts(Idx),

    // Expressions

    // Assignment
    Assign(Assign),
    // Binary Expression
    BinExpr(Bin),
    // Function calls
    Call(Call),
    // Unary Expression
    Unary(Unary),
    // Logical Expression
    Logical(Logical),
    // Variable Expression
    Var(Var),
    // Literal Expression
    Lit(Lit),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Def {
    pub(crate) name: String,
    pub(crate) arity: u16,
    pub(crate) args: Range,
    pub(crate) body: Idx,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct If {
    pub(crate) cond: Idx,
    pub(crate) then_block: Idx,
    pub(crate) else_block: Option<Idx>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct VarDecl {
    pub(crate) name: String,
    pub(crate) token: Token,
    pub(crate) init: Option<Idx>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub(crate) struct While {
    pub(crate) cond: Idx,
    pub(crate) body: Idx,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub(crate) struct Return {
    pub(crate) token: Token,
    pub(crate) expr: Option<Idx>,
}

// TODO: there should be a better name for this
#[derive(Debug, Clone, PartialEq, Copy)]
pub(crate) struct Range {
    pub(crate) start: Idx,
    pub(crate) end: Idx,
}

impl Range {
    pub(crate) fn iter(&self) -> RangeIterator {
        RangeIterator {
            index: self.start.0,
            block: self,
        }
    }
}

pub(crate) struct RangeIterator<'a> {
    index: usize,
    block: &'a Range,
}

impl Iterator for RangeIterator<'_> {
    type Item = Idx;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.block.end.0 {
            return None;
        }
        self.index += 1;
        Some(Idx(self.index - 1))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Assign {
    pub(crate) name: Idx,
    pub(crate) token: Token,
    pub(crate) value: Idx,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Bin {
    pub(crate) left: Idx,
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

// INFO: vec is used instead of a range as range executes every expression in its range
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Call {
    pub(crate) callee: Idx,
    pub(crate) token: Token,
    pub(crate) args: Vec<Idx>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Unary {
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Logical {
    pub(crate) left: Idx,
    pub(crate) right: Idx,
    pub(crate) op: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Var {
    pub(crate) name: String,
    pub(crate) token: Token,
}

// TODO: remove ident
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Lit {
    Ident(String),
    String(String),
    Int(i64),
    Bool(bool),
    Nil,
}
