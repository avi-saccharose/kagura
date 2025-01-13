#![allow(dead_code)]
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    error::{ErrorType, KaguError},
    expr::Ast,
};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Value {
    Number(i64),
    String(String),
    Bool(bool),
    Ident(String),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(val) => write!(f, "{val}"),
            Self::Bool(val) => write!(f, "{val}"),
            Self::Nil => write!(f, "nil"),
            Self::Ident(str) | Self::String(str) => write!(f, "{str}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Env {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn get(&self, name: &str) -> Result<Value, String> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        if self.enclosing.is_none() {
            return Err(format!("undefined variable {}", name));
        }
        let enclosing = self.enclosing.as_ref().unwrap().borrow();
        enclosing.get(name)
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), String> {
        if let Some(_) = self.values.get(&name) {
            self.define(name, value);
            return Ok(());
        }
        if self.enclosing.is_some() {
            let mut enclosing = self.enclosing.as_ref().unwrap().borrow_mut();
            return enclosing.assign(name, value);
        }
        Err(format!("undefined variable {}", name))
    }
}

#[derive(Debug)]
struct Interpreter {
    ast: Ast,
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    fn new() -> Self {
        Self {
            ast: Ast::default(),
            env: Rc::new(RefCell::new(Env::new())),
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.env.borrow_mut().define(name, value);
    }

    fn get(&self, name: &str) -> Result<Value, KaguError> {
        self.env.borrow().get(name).map_err(|e| KaguError {
            line: 0,
            start: 0,
            column: 0,
            error_type: ErrorType::Runtime,
            msg: e,
        })
    }

    fn assign(&mut self, name: String, value: Value) -> Result<(), KaguError> {
        self.env
            .borrow_mut()
            .assign(name, value)
            .map_err(|msg| KaguError {
                line: 0,
                start: 0,
                column: 0,
                error_type: ErrorType::Runtime,
                msg,
            })
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Bool(bool) => *bool,
            _ => true,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_vars() {
        let mut env = Env::new();
        assert!(env.get("undefined").is_err());
        env.define("a".to_string(), Value::Bool(true));
        assert!(env.get("a").is_ok());
        assert_eq!(env.get("a").unwrap(), Value::Bool(true));
    }
}
