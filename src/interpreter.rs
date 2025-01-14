#![allow(dead_code)]
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    error::{ErrorType, KaguError},
    expr::{Ast, Bin, Idx, Lit, Node, Unary},
    token::{Kind, Token},
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
pub(crate) struct Env {
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

// TODO: Way to store reference to the ast that can live shorter than the Interpreter
// Another option would be to directly own the ast
#[derive(Debug)]
pub(crate) struct Interpreter {
    pub env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
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

    fn is_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (_, _) => false,
        }
    }

    fn make_error(&self, msg: &str, token: Token) -> KaguError {
        KaguError {
            msg: msg.to_string(),
            line: token.line,
            column: token.column,
            start: token.span.start,
            error_type: ErrorType::Runtime,
        }
    }

    pub fn eval(&mut self, ast: &Ast) -> Result<(), KaguError> {
        let mut indices = ast.indices.iter();
        while let Some(idx) = indices.next() {
            self.eval_node(ast, *idx)?;
        }
        Ok(())
    }

    // Handle only statements as expressions produce a value
    fn eval_node(&mut self, ast: &Ast, idx: Idx) -> Result<(), KaguError> {
        let node = ast.get(idx);
        match node {
            Node::Puts(idx) => self.eval_puts(ast, *idx),
            _ => {
                self.eval_expr(ast, idx)?;
                Ok(())
            }
        }
    }

    fn eval_puts(&mut self, ast: &Ast, idx: Idx) -> Result<(), KaguError> {
        let expr = self.eval_expr(ast, idx)?;
        println!("{}", expr);
        Ok(())
    }

    fn eval_expr(&mut self, ast: &Ast, idx: Idx) -> Result<Value, KaguError> {
        let node = ast.get(idx);
        match node {
            Node::BinExpr(bin) => self.eval_bin(ast, bin),
            Node::Unary(unary) => self.eval_unary(ast, unary),
            Node::Lit(lit) => self.eval_lit(ast, lit),
            _ => unreachable!(),
        }
    }

    fn eval_bin(&mut self, ast: &Ast, bin: &Bin) -> Result<Value, KaguError> {
        let left = self.eval_expr(ast, bin.left)?;
        let right = self.eval_expr(ast, bin.right)?;
        let op = bin.op;

        match (left, right) {
            (Value::String(a), Value::String(b)) => match op.kind {
                Kind::Plus => Ok(Value::String(format!("{a}{b}"))),
                Kind::EqEq => Ok(Value::Bool(a == b)),
                Kind::NtEq => Ok(Value::Bool(a != b)),
                _ => Err(self.make_error("Invalid operation on strings", op)),
            },
            (Value::Number(a), Value::Number(b)) => match op.kind {
                Kind::Minus => Ok(Value::Number(a - b)),
                Kind::Plus => Ok(Value::Number(a + b)),
                Kind::Slash => Ok(Value::Number(a / b)),
                Kind::Star => Ok(Value::Number(a * b)),
                Kind::Lt => Ok(Value::Bool(a < b)),
                Kind::LtEq => Ok(Value::Bool(a <= b)),
                Kind::Gt => Ok(Value::Bool(a > b)),
                Kind::GtEq => Ok(Value::Bool(a >= b)),
                Kind::EqEq => Ok(Value::Bool(a == b)),
                Kind::NtEq => Ok(Value::Bool(a != b)),
                _ => unreachable!(),
            },
            (a, b) => match op.kind {
                Kind::EqEq => Ok(Value::Bool(self.is_equal(&a, &b))),
                Kind::NtEq => Ok(Value::Bool(!self.is_equal(&a, &b))),
                _ => Err(self.make_error("Invalid operands", op)),
            },
        }
    }

    fn eval_unary(&mut self, ast: &Ast, unary: &Unary) -> Result<Value, KaguError> {
        let right = self.eval_expr(ast, unary.right)?;
        let op = unary.op;
        match op.kind {
            Kind::Bang => Ok(Value::Bool(!self.is_truthy(&right))),
            Kind::Minus => {
                if let Value::Number(a) = right {
                    Ok(Value::Number(-a))
                } else {
                    Err(self.make_error("operand must be a number", op))
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_lit(&mut self, _ast: &Ast, lit: &Lit) -> Result<Value, KaguError> {
        let literal = match lit {
            Lit::Ident(ident) => Value::Ident(ident.clone()),
            Lit::Nil => Value::Nil,
            Lit::Int(int) => Value::Number(*int),
            Lit::Bool(bool) => Value::Bool(*bool),
            Lit::String(string) => Value::String(string.clone()),
        };
        Ok(literal)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;

    use super::*;

    #[test]
    fn env_vars() {
        let mut env = Env::new();
        assert!(env.get("undefined").is_err());
        env.define("a".to_string(), Value::Bool(true));
        assert!(env.get("a").is_ok());
        assert_eq!(env.get("a").unwrap(), Value::Bool(true));
    }

    #[test]
    fn eval_bin() {
        let parsed = parse("1 + 2;").unwrap();
        let mut interpreter = Interpreter::new();
        assert_eq!(
            interpreter.eval_expr(&parsed, Idx(2)).unwrap(),
            Value::Number(3)
        );
    }

    #[test]
    fn eval_unary() {
        let parsed = parse("-2;").unwrap();
        let mut interpreter = Interpreter::new();
        assert_eq!(
            interpreter.eval_expr(&parsed, Idx(1)),
            Ok(Value::Number(-2))
        );
    }
    #[test]
    fn eval_unary_bang() {
        let parsed = parse("!true;").unwrap();
        let mut interpreter = Interpreter::new();
        assert_eq!(
            interpreter.eval_expr(&parsed, Idx(1)),
            Ok(Value::Bool(false))
        );
    }
}
