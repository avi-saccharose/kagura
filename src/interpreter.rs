#![allow(dead_code)]
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

use crate::error::{ErrorType, KaguError};
use crate::expr::{Call, Def, Return, While};
use crate::values::KaguDef;
use crate::{
    expr::{Assign, Ast, Bin, Idx, If, Lit, Logical, Node, Unary, Var, VarDecl},
    token::{Kind, Token},
    values::Value,
};

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

    fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    fn get(&self, name: &str) -> Result<Value, String> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        if self.enclosing.is_none() {
            dbg!(self);
            return Err(format!("undefined variable {}", name));
        }
        let enclosing = self.enclosing.as_ref().unwrap().borrow();
        enclosing.get(name)
    }

    #[allow(clippy::redundant_pattern_matching)]
    fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
        if let Some(_) = self.values.get(name) {
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
    ret_value: Option<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            env: Rc::new(RefCell::new(Env::new())),
            ret_value: None,
        }
    }

    fn define(&mut self, name: &str, value: Value) {
        self.env.borrow_mut().define(name, value);
    }

    fn get(&self, name: &str, token: Token) -> Result<Value, KaguError> {
        self.env.borrow().get(name).map_err(|e| KaguError {
            line: token.line,
            start: token.span.start,
            column: token.column,
            error_type: ErrorType::Runtime,
            msg: e,
        })
    }

    fn assign(&mut self, name: &str, value: Value, token: Token) -> Result<(), KaguError> {
        self.env
            .borrow_mut()
            .assign(name, value)
            .map_err(|msg| KaguError {
                line: token.line,
                start: token.span.start,
                column: token.column,
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
        let indices = ast.indices.iter();
        for idx in indices {
            self.eval_node(ast, *idx)?;
        }
        Ok(())
    }

    // Handle only statements as expressions produce a value
    fn eval_node(&mut self, ast: &Ast, idx: Idx) -> Result<(), KaguError> {
        let node = ast.get(idx);
        match node {
            Node::Def(def) => self.eval_def(ast, def),
            Node::If(stmt) => self.eval_if(ast, stmt),
            Node::Block(block) => {
                // Empty block
                self.eval_block(ast, block)
            }
            Node::Puts(idx) => self.eval_puts(ast, *idx),
            Node::VarDecl(var_decl) => self.eval_var_decl(ast, var_decl),
            Node::While(stmt) => self.eval_while(ast, stmt),
            Node::Return(stmt) => self.eval_return(ast, stmt),
            _ => {
                // Evalute the expression and discard the result
                self.eval_expr(ast, idx)?;
                Ok(())
            }
        }
    }

    // TODO: Remove value name and maybe change the body type
    fn eval_def(&mut self, _ast: &Ast, def: &Def) -> Result<(), KaguError> {
        let name = def.name.clone();
        let args = def.args;
        let arity = def.arity;
        let body = def.body;
        let def = Value::Def(KaguDef {
            name: name.clone(),
            args,
            arity,
            body,
        });
        self.define(&name, def);
        Ok(())
    }

    fn eval_if(&mut self, ast: &Ast, stmt: &If) -> Result<(), KaguError> {
        let cond = self.eval_expr(ast, stmt.cond)?;

        if self.is_truthy(&cond) {
            self.eval_node(ast, stmt.then_block)?;
        } else if let Some(else_block) = stmt.else_block {
            self.eval_node(ast, else_block)?;
        }
        Ok(())
    }

    fn eval_block(&mut self, ast: &Ast, block: &[Idx]) -> Result<(), KaguError> {
        self.execute_block(ast, block, Env::new())
    }

    // INFO: We catch err and replace env as return calls invokes an error which is not truly an
    // error
    fn execute_block(&mut self, ast: &Ast, block: &[Idx], env: Env) -> Result<(), KaguError> {
        let previous = std::mem::replace(&mut self.env, Rc::new(RefCell::new(env)));
        self.env.borrow_mut().enclosing = Some(Rc::clone(&previous));
        for node in block.iter() {
            if let Err(e) = self.eval_node(ast, *node) {
                let _ = std::mem::replace(&mut self.env, previous);
                return Err(e);
            }
        }
        let _ = std::mem::replace(&mut self.env, previous);
        Ok(())
    }

    fn eval_puts(&mut self, ast: &Ast, idx: Idx) -> Result<(), KaguError> {
        let expr = self.eval_expr(ast, idx)?;
        println!("{}", expr);
        Ok(())
    }

    fn eval_var_decl(&mut self, ast: &Ast, var: &VarDecl) -> Result<(), KaguError> {
        let name = &var.name;
        let mut init = Value::Nil;
        if let Some(expr) = var.init {
            init = self.eval_expr(ast, expr)?;
        }
        self.define(name, init);
        Ok(())
    }

    fn eval_while(&mut self, ast: &Ast, stmt: &While) -> Result<(), KaguError> {
        loop {
            let cond = self.eval_expr(ast, stmt.cond)?;
            if !self.is_truthy(&cond) {
                break;
            }
            self.eval_node(ast, stmt.body)?;
        }

        Ok(())
    }

    fn placeholder_error(&self, token: Token) -> KaguError {
        KaguError {
            start: token.span.start,
            line: token.line,
            column: token.column,
            msg: "implicit use of return outside function".to_string(),
            error_type: ErrorType::Return,
        }
    }

    fn eval_return(&mut self, ast: &Ast, ret: &Return) -> Result<(), KaguError> {
        if let Some(expr) = ret.expr {
            if let Ok(val) = self.eval_expr(ast, expr) {
                self.ret_value = Some(val);
            }
        }
        Err(self.placeholder_error(ret.token))
    }

    fn eval_expr(&mut self, ast: &Ast, idx: Idx) -> Result<Value, KaguError> {
        let node = ast.get(idx);
        match node {
            Node::Assign(assign) => self.eval_assign(ast, assign),
            Node::BinExpr(bin) => self.eval_bin(ast, bin),
            Node::Call(call) => self.eval_call(ast, call),
            Node::Unary(unary) => self.eval_unary(ast, unary),
            Node::Logical(logical) => self.eval_logical(ast, logical),
            Node::Var(var) => self.eval_var(ast, var),
            Node::Lit(lit) => self.eval_lit(ast, lit),
            _ => unreachable!("{:?}", node),
        }
    }

    // Every identifier from variables to function names/calls is of the type Var so we need a
    // function to extract the underlying identifier
    fn ident_string<'a>(&mut self, ast: &'a Ast, idx: Idx) -> &'a String {
        let assign = ast.get(idx);
        if let Node::Var(var) = assign {
            return &var.name;
        }
        unreachable!()
    }

    fn eval_assign(&mut self, ast: &Ast, assign: &Assign) -> Result<Value, KaguError> {
        let name = self.ident_string(ast, assign.name);
        let value = self.eval_expr(ast, assign.value)?;
        let token = assign.token;
        self.assign(name, value, token)?;
        Ok(Value::Nil)
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

    fn eval_call(&mut self, ast: &Ast, call: &Call) -> Result<Value, KaguError> {
        let callee = self.ident_string(ast, call.callee);
        let def = self.get(callee, call.token)?;

        if let Value::Def(def) = def {
            let mut args: Vec<Value> = Vec::new();

            if def.arity != call.args.len() as u16 {
                return Err(self.make_error("Number of arguments do not match", call.token));
            }
            for idx in call.args.iter() {
                args.push(self.eval_expr(ast, *idx)?);
            }
            return self.call(ast, def, args);
        }
        Err(self.make_error("can only call functions", call.token))
    }

    fn call(&mut self, ast: &Ast, def: KaguDef, args: Vec<Value>) -> Result<Value, KaguError> {
        let mut env = Env::new();
        let mut idx = 0;
        if def.arity != 0 {
            // TODO: def.iter() currently doesnt work as it returns None
            //for param in def.args.start.0..=def.args.end.0 {
            for param in def.args.iter() {
                let name = self.ident_string(ast, param);
                env.define(name, args[idx].clone());
                idx += 1;
            }
        }

        let block = match ast.get(def.body) {
            Node::Block(block) => block,
            _ => unreachable!(),
        };

        match self.execute_block(ast, block, env) {
            Err(e) => {
                if e.error_type == ErrorType::Return {
                    return Ok(self.ret_value.take().unwrap());
                }
                Err(e)
            }
            Ok(_) => Ok(Value::Nil),
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

    fn eval_logical(&mut self, ast: &Ast, logical: &Logical) -> Result<Value, KaguError> {
        let left = self.eval_expr(ast, logical.left)?;
        if logical.op.kind == Kind::Or {
            if self.is_truthy(&left) {
                return Ok(left);
            }
        } else if !self.is_truthy(&left) {
            return Ok(left);
        }

        let right = self.eval_expr(ast, logical.right)?;
        Ok(right)
    }

    fn eval_var(&mut self, _ast: &Ast, lit: &Var) -> Result<Value, KaguError> {
        self.get(&lit.name, lit.token)
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

    use crate::{parser::parse, token::Span};

    use super::*;

    fn default_token() -> Token {
        Token {
            span: Span { start: 0, end: 0 },
            line: 0,
            column: 0,
            kind: Kind::Eof,
        }
    }
    #[test]
    fn env_vars() {
        let mut env = Env::new();
        assert!(env.get("undefined").is_err());
        env.define("a", Value::Bool(true));
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

    #[test]
    fn eval_block() {
        let parsed = parse("var b; {var a;}").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(interpreter.get("b", default_token()).unwrap(), Value::Nil);
        assert!(interpreter.get("a", default_token()).is_err());
    }

    #[test]
    fn eval_if() {
        let parsed = parse("var a; if (true) a = true;").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(
            interpreter.get("a", default_token()).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn eval_if_else() {
        let parsed = parse("var a; if(false) a = 1; else a = 2;").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(
            interpreter.get("a", default_token()).unwrap(),
            Value::Number(2)
        );
    }

    #[test]
    fn eval_logical() {
        let parsed = parse("var x = true and false; var y = true or false;").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(
            interpreter.get("x", default_token()).unwrap(),
            Value::Bool(false)
        );
        assert_eq!(
            interpreter.get("y", default_token()).unwrap(),
            Value::Bool(true)
        );
    }

    #[test]
    fn eval_while() {
        let parsed =
            parse("var a = 10; var i = 0; while(i < 4) { a = a + 1; i = i + 1; }").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(
            interpreter.get("a", default_token()).unwrap(),
            Value::Number(14)
        );
        assert_eq!(
            interpreter.get("i", default_token()).unwrap(),
            Value::Number(4)
        );
    }

    #[test]
    fn eval_def_decl() {
        let parsed = parse("def hi() {}").unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert!(matches!(
            interpreter.get("hi", default_token()).unwrap(),
            Value::Def(..)
        ));
    }

    #[test]
    fn eval_def_program() {
        let parsed = parse(
            "
            def mutate() { 
                x = 9;
            } 
            var x = 1;
            mutate();
        ",
        )
        .unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parsed).unwrap();
        assert_eq!(
            interpreter.get("x", default_token()).unwrap(),
            Value::Number(9)
        );
    }

    #[test]
    fn eval_fibonacci_program() {
        let program = parse(
            "
            def fib(n) {
            if (n <= 1) return n;
            return fib(n - 1) + fib(n - 2);
            }
            var a = fib(15);
            ",
        )
        .unwrap();
        let mut interpreter = Interpreter::new();
        interpreter.eval(&program).unwrap();
        assert_eq!(
            interpreter.get("a", default_token()).unwrap(),
            Value::Number(610)
        );
    }
}
