use std::collections::HashMap;

use thiserror::Error;

use crate::{
    lexer::token::{Kind, Token},
    parser::ast::{Bin, Expr, Lit, Stmt},
};

#[derive(Debug, Clone, Error)]
pub enum TypeError {
    #[error("error {0}")]
    Error(String),
}
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Ty {
    Int,
    Float,
    Bool,
    String,
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Hash, Eq)]
pub enum Op {
    Add,
    Mul,
    Sub,
    Div,
    Lt,
    LtEQ,
    NtEq,
    Nt,
    Gt,
    GtEq,
    EqEq,
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
struct BinRule {
    op: Kind,
    left: Ty,
    right: Ty,
}

// Helper function that takes in operator and opertor to return the corresponding op enum and type
fn bin_rule(op: Kind, operands: Ty, ty: Ty) -> (BinRule, Ty) {
    (
        BinRule {
            op,
            left: operands,
            right: operands,
        },
        ty,
    )
}

fn bin_op_rules() -> HashMap<BinRule, Ty> {
    use Kind::*;
    HashMap::from([
        // Arithmetic
        bin_rule(Plus, Ty::Int, Ty::Int),
        bin_rule(Minus, Ty::Int, Ty::Int),
        bin_rule(Slash, Ty::Int, Ty::Int),
        bin_rule(Star, Ty::Int, Ty::Int),
        // String concat
        bin_rule(Plus, Ty::String, Ty::String),
        // Comparison
        bin_rule(Lt, Ty::Int, Ty::Bool),
        bin_rule(LtEq, Ty::Int, Ty::Bool),
        bin_rule(GtEq, Ty::Int, Ty::Bool),
        bin_rule(Gt, Ty::Int, Ty::Bool),
        // Equality
        bin_rule(EqEq, Ty::Int, Ty::Bool),
        bin_rule(NtEq, Ty::Int, Ty::Bool),
        bin_rule(NtEq, Ty::String, Ty::Bool),
        bin_rule(EqEq, Ty::String, Ty::Bool),
        // Logical
        bin_rule(And, Ty::Bool, Ty::Bool),
        bin_rule(Or, Ty::Bool, Ty::Bool),
    ])
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TExpr {
    pub expr: TExprKind,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TExprKind {
    If(TIf),
    Bin(TBin),
    Lit(TLit),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TIf {
    pub cond: Box<TExpr>,
    pub then_expr: Box<TExpr>,
    pub else_expr: Box<TExpr>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct TBin {
    pub left: Box<TExpr>,
    pub right: Box<TExpr>,
    pub op: Op,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TLit {
    Int(i64, Token),
    Str(String, Token),
    Bool(bool, Token),
    Float(f64, Token),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TStmt {
    Expr(TExpr),
}

pub struct TypeChecker {
    variables: HashMap<String, Ty>,
    bin_rules: HashMap<BinRule, Ty>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            bin_rules: bin_op_rules(),
        }
    }

    pub fn check_program(&mut self, stmts: Vec<Stmt>) -> Result<Vec<TStmt>, TypeError> {
        let mut typed_stmts = Vec::new();
        for stmt in stmts {
            typed_stmts.push(self.check_stmt(stmt)?);
        }
        Ok(typed_stmts)
    }

    fn check_stmt(&mut self, stmt: Stmt) -> Result<TStmt, TypeError> {
        match stmt {
            Stmt::Expr(expr) => {
                let typed_expr = self.check_expr(expr)?;
                Ok(TStmt::Expr(typed_expr))
            }
            _ => todo!(),
        }
    }

    fn check_expr(&mut self, expr: Expr) -> Result<TExpr, TypeError> {
        match expr {
            Expr::Bin(binary) => self.check_binary(binary),
            Expr::Lit(lit) => self.check_lit(lit),
            _ => todo!(),
        }
    }

    fn check_binary(&mut self, bin: Bin) -> Result<TExpr, TypeError> {
        let left = self.check_expr(*bin.left)?;
        let right = self.check_expr(*bin.right)?;
        let result_type = self.check_binary_op(bin.op, left.ty, right.ty)?;
        let op = self.check_op(bin.op);
        Ok(TExpr {
            expr: TExprKind::Bin(TBin {
                left: Box::new(left),
                right: Box::new(right),
                op,
            }),
            ty: result_type,
        })
    }

    fn check_lit(&mut self, lit: Lit) -> Result<TExpr, TypeError> {
        let (ty, lit) = match lit {
            Lit::Int(val, token) => (Ty::Int, TLit::Int(val, token)),
            Lit::Bool(val, token) => (Ty::Bool, TLit::Bool(val, token)),
            Lit::Str(val, token) => (Ty::String, TLit::Str(val, token)),
            Lit::Float(val, token) => (Ty::Float, TLit::Float(val, token)),
            Lit::Nil => todo!(),
        };
        Ok(TExpr {
            expr: TExprKind::Lit(lit),
            ty,
        })
    }

    fn check_binary_op(&self, op: Kind, left: Ty, right: Ty) -> Result<Ty, TypeError> {
        let key = BinRule { op, left, right };
        self.bin_rules
            .get(&key)
            .copied()
            .ok_or_else(|| TypeError::Error("invalid operand".to_string()))
    }

    fn check_op(&self, op: Kind) -> Op {
        match op {
            Kind::Plus => Op::Add,
            Kind::Minus => Op::Sub,
            Kind::Slash => Op::Div,
            Kind::Star => Op::Mul,
            Kind::Lt => Op::Lt,
            Kind::LtEq => Op::LtEQ,
            Kind::Gt => Op::Gt,
            Kind::GtEq => Op::GtEq,
            Kind::EqEq => Op::EqEq,
            Kind::NtEq => Op::NtEq,
            Kind::Bang => Op::Not,
            _ => unreachable!(),
        }
    }
}
