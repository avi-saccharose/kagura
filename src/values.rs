use std::fmt;

use crate::expr::{Block, Idx};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(i64),
    String(String),
    Bool(bool),
    Ident(String),
    Def(KaguDef),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KaguDef {
    pub name: String,
    pub arity: u16,
    pub args: Block,
    pub body: Idx,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(val) => write!(f, "{val}"),
            Self::Bool(val) => write!(f, "{val}"),
            Self::Nil => write!(f, "nil"),
            Self::Ident(str) | Self::String(str) => write!(f, "{str}"),
            Self::Def(def) => write!(f, "<Def>{}", def.name),
        }
    }
}
