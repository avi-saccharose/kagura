use std::collections::HashMap;

use crate::{
    error::KaguError,
    expr::{Ast, Idx, Node, Var, VarDecl},
};

#[derive(Debug, Clone)]
struct Resolver<'a> {
    ast: &'a Ast,
    stack: Vec<HashMap<String, bool>>,
}

impl<'a> Resolver<'a> {
    fn new(ast: &'a Ast) -> Self {
        Self {
            ast,
            stack: Vec::new(),
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

    fn resolve(&mut self) -> Result<(), KaguError> {
        for idx in &self.ast.indices {
            self.resolve_node(*idx)?;
        }
        Ok(())
    }

    fn resolve_node(&mut self, idx: Idx) -> Result<(), KaguError> {
        let node = self.ast.get(idx);
        match node {
            Node::VarDecl(vardecl) => self.resolve_var_decl(vardecl),
            Node::Var(var) = self.resolve_var_call(var),
            _ => todo!(),
        }
    }

    fn resolve_var_decl(&mut self, var: &VarDecl) -> Result<(), KaguError> {
        self.declare(var.name.clone());
        if let Some(init) = var.init {
            self.resolve_node(init)?;
        }
        self.define(var.name.clone());
        Ok(())
    }

    fn resolve_var_call(&mut self, var: &Var) -> Result<(), KaguError>{
        if let Some(scope) = self.stack.last_mut() {
            if let Some(val) = scope.get(var.name) {
                if val == false {
                    return self.make_error(var.token, "Uninitialized variable");
                }
            }
        }
        Ok(())
    }
}
pub fn resolve(ast: &Ast) -> Result<(), KaguError> {
    let mut resolver = Resolver::new(ast);
    resolver.resolve()
}
