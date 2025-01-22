use std::fmt;

use crate::{
    error::KaguError,
    expr::{Idx, Range},
    interpreter::Env,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(i64),
    String(String),
    Bool(bool),
    Ident(String),
    Def(KaguDef),
    NativeDef(NativeDef),
    Nil,
}

// TODO: Since we store the ast indices in arguments and body, our repl crashes when calling
// functions
#[derive(Debug, Clone, PartialEq)]
pub struct KaguDef {
    pub name: String,
    pub arity: u16,
    pub args: Range,
    pub body: Idx,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeDef {
    pub name: &'static str,
    pub arity: u16,
    pub args: Option<Vec<String>>,
    pub exec: fn(env: &mut Env, &[Value]) -> Result<Value, KaguError>,
}

pub trait Callable {
    fn arity(&self) -> u16;
    fn call(&mut self, env: &mut Env, args: &[Value]) -> Result<Value, KaguError>;
}

impl Callable for KaguDef {
    fn arity(&self) -> u16 {
        self.arity
    }
    fn call(&mut self, env: &mut Env, args: &[Value]) -> Result<Value, KaguError> {
        todo!()
    }
}

impl Callable for NativeDef {
    fn arity(&self) -> u16 {
        self.arity
    }
    fn call(&mut self, env: &mut Env, args: &[Value]) -> Result<Value, KaguError> {
        (self.exec)(env, args)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(val) => write!(f, "{val}"),
            Self::Bool(val) => write!(f, "{val}"),
            Self::Nil => write!(f, "nil"),
            Self::Ident(str) | Self::String(str) => write!(f, "{str}"),
            Self::Def(def) => write!(f, "<Def>{}", def.name),
            Self::NativeDef(def) => write!(f, "<NativeDef> {}", def.name),
        }
    }
}
