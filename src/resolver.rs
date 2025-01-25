use std::collections::HashMap;

use crate::{
    error::KaguError,
    expr::{
        Assign, Ast, Bin, Call, Def, Idx, If, Logical, Node, Return, Unary, Var, VarDecl, While,
    },
    interpreter::Interpreter,
    token::Token,
};

#[derive(Debug, Clone)]
struct Resolver<'a> {
    ast: &'a Ast,
    stack: Vec<HashMap<String, bool>>,
    interpreter: *mut Interpreter,
}

impl<'a> Resolver<'a> {
    fn new(ast: &'a Ast, interpreter: *mut Interpreter) -> Self {
        Self {
            ast,
            stack: Vec::new(),
            interpreter,
        }
    }

    fn begin_scope(&mut self) {
        self.stack.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.stack.pop();
    }

    fn declare(&mut self, name: String) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(name, false);
        }
    }

    fn define(&mut self, name: String) {
        if let Some(scope) = self.stack.last_mut() {
            scope.insert(name, true);
        }
    }

    fn make_error(&self, token: Token, msg: &str) -> Result<(), KaguError> {
        Err(KaguError {
            msg: msg.to_string(),
            line: token.line,
            start: token.span.start,
            column: token.column,
            error_type: crate::error::ErrorType::Resolve,
        })
    }

    fn resolve(&mut self, expr: Idx, depth: usize) -> Result<(), KaguError> {
        let int = unsafe { &mut *self.interpreter };
        int.insert_local(expr, depth);
        Ok(())
    }

    fn resolve_nodes(&mut self) -> Result<(), KaguError> {
        for idx in &self.ast.indices {
            self.resolve_node(*idx)?;
        }
        Ok(())
    }

    fn resolve_node(&mut self, idx: Idx) -> Result<(), KaguError> {
        let node = self.ast.get(idx);
        match node {
            Node::Block(blocks) => self.resolve_blocks(blocks),
            Node::VarDecl(vardecl) => self.resolve_var_decl(vardecl),
            Node::Var(var) => self.resolve_var_call(var),
            Node::Assign(assign) => self.resolve_assign(assign),
            Node::Def(def) => self.resolve_def(def),
            Node::If(stmt) => self.resolve_if(stmt),
            Node::While(stmt) => self.resolve_while(stmt),
            Node::Return(expr) => self.resolve_return(expr),
            Node::Puts(expr) => self.resolve_puts(*expr),
            Node::BinExpr(expr) => self.resolve_bin(expr),
            Node::Call(call) => self.resolve_call(call),
            Node::Unary(unary) => self.resolve_unary(unary),
            Node::Logical(logical) => self.resolve_logical(logical),
            Node::Lit(_) => Ok(()),
        }
    }

    fn resolve_blocks(&mut self, blocks: &[Idx]) -> Result<(), KaguError> {
        self.begin_scope();
        for idx in blocks {
            self.resolve_node(*idx)?;
        }
        self.end_scope();
        Ok(())
    }

    fn resolve_var_decl(&mut self, var: &VarDecl) -> Result<(), KaguError> {
        self.declare(var.name.clone());
        if let Some(init) = var.init {
            self.resolve_node(init)?;
        }
        self.define(var.name.clone());
        Ok(())
    }

    fn resolve_local(&mut self, name: &str) -> Result<(), KaguError> {
        Ok(())
    }

    fn resolve_var_call(&mut self, var: &Var) -> Result<(), KaguError> {
        if let Some(scope) = self.stack.last_mut() {
            if let Some(val) = scope.get(&var.name) {
                if !*val {
                    return self
                        .make_error(var.token, "cant read variable in its own Uninitializer");
                }
            }
        }

        self.resolve_local(&var.name)?;
        Ok(())
    }

    fn resolve_assign(&mut self, assign: &Assign) -> Result<(), KaguError> {
        self.resolve_node(assign.value)?;
        let name = self.ast.ident_string(assign.name);
        self.resolve_local(name)
    }

    fn resolve_def(&mut self, def: &Def) -> Result<(), KaguError> {
        self.declare(def.name.clone());
        self.define(def.name.clone());
        self.begin_scope();
        for param in def.args.iter() {
            let arg = self.ast.ident_string(param);
            self.declare(arg.clone());
            self.define(arg.clone());
        }
        self.resolve_node(def.body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_if(&mut self, stmt: &If) -> Result<(), KaguError> {
        self.resolve_node(stmt.cond)?;
        self.resolve_node(stmt.then_block)?;
        if let Some(idx) = stmt.else_block {
            self.resolve_node(idx)?;
        }
        Ok(())
    }

    fn resolve_puts(&mut self, expr: Idx) -> Result<(), KaguError> {
        self.resolve_node(expr)
    }

    fn resolve_return(&mut self, ret: &Return) -> Result<(), KaguError> {
        if let Some(expr) = ret.expr {
            self.resolve_node(expr)?;
        }
        Ok(())
    }

    fn resolve_while(&mut self, stmt: &While) -> Result<(), KaguError> {
        self.resolve_node(stmt.cond)?;
        self.resolve_node(stmt.body)
    }

    fn resolve_bin(&mut self, expr: &Bin) -> Result<(), KaguError> {
        self.resolve_node(expr.left)?;
        self.resolve_node(expr.right)
    }

    fn resolve_call(&mut self, call: &Call) -> Result<(), KaguError> {
        self.resolve_node(call.callee)?;
        for args in &call.args {
            self.resolve_node(*args)?;
        }
        Ok(())
    }

    fn resolve_unary(&mut self, unary: &Unary) -> Result<(), KaguError> {
        self.resolve_node(unary.right)
    }

    fn resolve_logical(&mut self, logical: &Logical) -> Result<(), KaguError> {
        self.resolve_node(logical.left)?;
        self.resolve_node(logical.right)
    }
}
pub fn resolve(ast: &Ast, interpreter: &mut Interpreter) -> Result<(), KaguError> {
    let mut resolver = Resolver::new(ast, interpreter);
    resolver.resolve()
}
