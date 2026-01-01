#![allow(dead_code)]
use crate::{
    lexer::{
        Lexer,
        token::{Kind, Span, Token},
    },
    parser::ast::{Assign, Bin, Call, Def, Expr, Ident, If, IfStmt, Lit, Puts, Stmt, Var, VarKind},
};

use std::{iter::Peekable, vec};
use thiserror::Error;

pub mod ast;

#[derive(Debug, Clone, Error)]
#[error("error on line {line}: {kind}")]
pub struct ParseError {
    kind: ParseErrorKind,
    line: usize,
    span: Span,
}

#[derive(Debug, Clone, Error)]
enum ParseErrorKind {
    #[error("Invalid character {0}")]
    InvalidKind(Kind),
    #[error("Unterminated string")]
    UnterminatedString,
    #[error("Expected literal or expression")]
    UnexpectedEof,
    #[error("Expected {0} at end of file")]
    Eof(Kind),
    #[error("Expected {0} found {1}")]
    Mismatched(Kind, Kind),
    #[error("Syntax error")]
    SyntaxError,
    #[error("Value cannot be assigned")]
    InvalidAssignment,
    #[error("Invalid character")]
    InvalidToken,
    #[error("Invalid mumber")]
    InvalidNumber,
}

pub struct Parser<'input> {
    input: &'input str,
    tokens: Peekable<Lexer<'input>>,
    current: Option<Token>,
}

impl<'input> Iterator for Parser<'input> {
    type Item = Result<Stmt, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof() {
            return None;
        }
        Some(self.parse_stmt())
    }
}

impl<'input> Parser<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            tokens: Lexer::new(input).peekable(),
            current: None,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.collect()
    }

    pub fn text(&self, token: Token) -> &'input str {
        token.text(self.input)
    }

    fn eof(&mut self) -> bool {
        self.peek() == Kind::Eof
    }

    fn peek(&mut self) -> Kind {
        self.tokens
            .peek()
            .map(|token| token.kind)
            .unwrap_or(Kind::Eof)
    }

    fn match_kind(&mut self, kind: Kind) -> bool {
        self.peek() == kind
    }

    fn next(&mut self) -> Option<Token> {
        self.current = self.tokens.next();
        self.current
    }

    fn consume(&mut self, expected: Kind) -> Result<Token, ParseError> {
        let kind = self.peek();

        if kind == Kind::Eof {
            return self.error(ParseErrorKind::Eof(expected));
        }

        if kind == expected {
            Ok(self.next().unwrap())
        } else {
            self.error(ParseErrorKind::Mismatched(expected, kind))
        }
    }

    fn error<T>(&mut self, kind: ParseErrorKind) -> Result<T, ParseError> {
        let (line, span) = match self.current {
            Some(token) => (token.line, token.span),
            None => (1, Span { start: 0, end: 0 }),
        };
        Err(ParseError { line, span, kind })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        match self.peek() {
            Kind::Def => self.stmt_def(),
            Kind::If => self.stmt_if(),
            Kind::Do => self.stmt_block(false),
            Kind::Var => self.stmt_var(),
            Kind::Puts => self.stmt_puts(),
            _ => self.stmt_expr(),
        }
    }

    // check if the function call is from an if statement
    // and we dont consume the end token
    fn stmt_block(&mut self, is_if: bool) -> Result<Stmt, ParseError> {
        let mut stmts = vec![];

        if is_if {
            while !matches!(self.peek(), Kind::Else | Kind::End) {
                stmts.push(self.parse_stmt()?);
            }
        } else {
            self.next().unwrap(); // consume "do"
            while !matches!(self.peek(), Kind::End) {
                stmts.push(self.parse_stmt()?);
            }
            self.consume(Kind::End)?;
        }

        Ok(Stmt::Block(ast::Block { stmts }))
    }

    // DefStmt -> "def" def_name "(" (expr) ")" (-> ret_type) "do"
    // stmts
    // "end"
    fn stmt_def(&mut self) -> Result<Stmt, ParseError> {
        let token = self.next().unwrap();
        let name = {
            let token = self.consume(Kind::Ident)?;
            let name = token.text(self.input).to_string();
            Ident(name, token)
        };
        let mut ret_kind: Option<VarKind> = None;
        let mut args = vec![];
        let mut body = vec![];
        self.consume(Kind::Lparen)?;

        if !self.match_kind(Kind::Rparen) {
            loop {
                let arg = self.expr()?;
                self.consume(Kind::Colon)?;
                let kind = {
                    let kind = self.peek();
                    let kind = self.var_kind(kind)?;
                    self.next();
                    kind
                };

                args.push((arg, kind));
                if !self.match_kind(Kind::Comma) {
                    break;
                }
                self.next();
            }
        }

        self.consume(Kind::Rparen)?;

        if self.match_kind(Kind::Arrow) {
            self.next();
            let current = self.peek();
            ret_kind = Some(self.var_kind(current)?);
            self.next();
        }
        self.consume(Kind::Do)?;
        while !self.match_kind(Kind::End) {
            body.push(self.parse_stmt()?);
        }
        self.consume(Kind::End)?;

        Ok(Stmt::Def(Def {
            token,
            name,
            args,
            ret_kind,
            body,
        }))
    }

    fn var_kind(&mut self, kind: Kind) -> Result<VarKind, ParseError> {
        match kind {
            Kind::Int => Ok(VarKind::Int),
            Kind::Str => Ok(VarKind::Str),
            Kind::Bool => Ok(VarKind::Bool),
            Kind::Const => {
                let token = self.next().unwrap();
                let ident = token.text(self.input);
                Ok(VarKind::Ident(ident.to_string()))
            }
            kind => self.error(ParseErrorKind::InvalidKind(kind)),
        }
    }
    // VarStmt -> "var" ident (":" kind) = expr
    fn stmt_var(&mut self) -> Result<Stmt, ParseError> {
        self.next();
        let ident_token = self.consume(Kind::Ident)?;
        let name = ident_token.text(self.input).to_string();
        let mut kind: Option<VarKind> = None;
        if matches!(self.peek(), Kind::Colon) {
            self.next();
            let current = self.peek();
            kind = Some(self.var_kind(current)?);
            self.next();
        }

        let mut init: Option<Expr> = None;
        if matches!(self.peek(), Kind::Eq) {
            self.next();
            init = Some(self.expr()?);
        }

        Ok(Stmt::Var(Var {
            kind,
            name,
            init,
            token: ident_token,
        }))
    }

    // IFStmt -> "if" expr "then" stmt ("else" stmt) end
    fn stmt_if(&mut self) -> Result<Stmt, ParseError> {
        let token = self.next().unwrap();
        let cond = self.expr()?;
        let mut else_body = None;
        self.consume(Kind::Then)?;
        let then_body = self.stmt_block(true)?;
        if matches!(self.peek(), Kind::Else) {
            self.next();
            else_body = Some(Box::new(self.stmt_block(true)?));
        }
        self.consume(Kind::End)?;
        Ok(Stmt::If(IfStmt {
            token,
            cond,
            then_body: Box::new(then_body),
            else_body,
        }))
    }

    // Puts -> "puts" expr
    fn stmt_puts(&mut self) -> Result<Stmt, ParseError> {
        let token = self.next().unwrap();
        let expr = self.expr()?;
        Ok(Stmt::Puts(Puts { expr, token }))
    }

    fn stmt_expr(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expr()?;
        Ok(Stmt::Expr(expr))
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Kind::If => self.expr_if(),
            _ => self.assignment(),
        }
    }

    fn expr_if(&mut self) -> Result<Expr, ParseError> {
        self.next();
        let cond = self.expr()?;
        self.consume(Kind::Then)?;
        let then_expr = self.expr()?;
        self.consume(Kind::Else)?;
        let else_expr = self.expr()?;
        Ok(Expr::If(If {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }))
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.and()?;

        if matches!(self.peek(), Kind::Eq) {
            self.next();
            let value = self.and()?;
            match expr {
                Expr::Ident(token) => {
                    return Ok(Expr::Assign(Assign {
                        name: token.0,
                        value: Box::new(value),
                    }));
                }
                _ => return self.error(ParseErrorKind::InvalidAssignment),
            }
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.or()?;
        while matches!(self.peek(), Kind::And) {
            let op = self.next().unwrap();
            let right = self.or()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.equality()?;
        while matches!(self.peek(), Kind::Or) {
            let op = self.next().unwrap();
            let right = self.equality()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.comparison()?;
        while matches!(self.peek(), Kind::EqEq | Kind::NtEq) {
            let op = self.next().unwrap();
            let right = self.comparison()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.terimal()?;
        while matches!(self.peek(), Kind::Gt | Kind::GtEq | Kind::Lt | Kind::LtEq) {
            let op = self.next().unwrap();
            let right = self.terimal()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    fn terimal(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.factor()?;
        while matches!(self.peek(), Kind::Plus | Kind::Minus) {
            let op = self.next().unwrap();
            let right = self.factor()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.unary()?;
        while matches!(self.peek(), Kind::Star | Kind::Slash) {
            let op = self.next().unwrap();
            let right = self.unary()?;
            left = Expr::Bin(Bin {
                left: Box::new(left),
                right: Box::new(right),
                op: op.kind,
                token: op,
            })
        }
        Ok(left)
    }

    // TODO:
    fn unary(&mut self) -> Result<Expr, ParseError> {
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        if matches!(self.peek(), Kind::Lparen) {
            let token = self.next().unwrap();
            let mut args = Vec::new();
            if !matches!(self.peek(), Kind::Rparen) {
                loop {
                    args.push(self.expr()?);
                    if matches!(self.peek(), Kind::Comma) {
                        self.next();
                        continue;
                    }
                    break;
                }
                self.consume(Kind::Rparen)?;
            }
            expr = Expr::Call(Call {
                callee: Box::new(expr),
                args,
                token,
            })
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Kind::IntLit | Kind::True | Kind::False | Kind::StringLit => self.make_literal(),
            Kind::Ident => {
                let token = self.next().unwrap();
                let ident = token.text(self.input).to_string();
                Ok(Expr::Ident(Ident(ident, token)))
            }
            Kind::Error => self.error(ParseErrorKind::InvalidToken),
            Kind::UnterminatedStr => self.error(ParseErrorKind::UnterminatedString),
            _ => {
                println!("{}", self.peek());
                let kind = self.peek();
                if kind == Kind::Eof {
                    return self.error(ParseErrorKind::UnexpectedEof);
                }
                self.error(ParseErrorKind::InvalidKind(kind))
            }
        }
    }

    fn make_literal(&mut self) -> Result<Expr, ParseError> {
        let token = self.next().unwrap();
        let lit = match token.kind {
            Kind::True => Lit::Bool(true, token),
            Kind::False => Lit::Bool(false, token),
            Kind::StringLit => {
                let value = token.text(self.input).to_string();
                Lit::Str(value, token)
            }
            Kind::IntLit => {
                let value = token.text(self.input);
                match value.parse() {
                    Ok(val) => Lit::Int(val, token),
                    Err(_) => {
                        return self.error(ParseErrorKind::InvalidNumber);
                    }
                }
            }
            _ => unreachable!(),
        };
        Ok(Expr::Lit(lit))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Bin, Call};

    use super::*;

    #[test]
    fn consume_error() {
        let mut parser = Parser::new(r#"--"#);
        parser.next();
        let result = parser.consume(Kind::Plus);
        assert!(result.is_err());
        let error = result.unwrap_err();
        println!("{error}");
        assert_eq!(error.line, 1);
        assert_eq!(error.span.start, 0);
        assert_eq!(error.span.end, 1);
    }

    #[test]
    fn parse_expr() {
        let mut parser = Parser::new(r#"1 + 2"#);
        let stmts = parser.parse().unwrap();
        assert!(matches!(stmts[0], Stmt::Expr(Expr::Bin(Bin { .. }))))
    }

    #[test]
    fn parse_fn_call() {
        let mut parser = Parser::new(r#"add(1, 2 + 3)"#);
        let stmts = parser.parse().unwrap();
        assert!(matches!(stmts[0], Stmt::Expr(Expr::Call(Call { .. },),)))
    }

    #[test]
    fn parse_var_expr() {
        let mut parser = Parser::new(r#"var x: int = 1 + 1"#);
        let stmts = parser.parse().unwrap();
        assert!(matches!(stmts[0], Stmt::Var(Var { .. })))
    }

    #[test]
    fn parse_if() {
        let mut parser = Parser::new(
            r#"
        if x then x end"#,
        );
        let stmts = parser.parse().unwrap();
        assert!(matches!(stmts[0], Stmt::If(IfStmt { .. })));

        let err = Parser::new(
            r#"
        if x then 
        y"#,
        )
        .parse();
        assert!(err.is_err());
    }

    #[test]
    fn parse_assign_err() {
        let err = Parser::new(r#"1 = 2"#).parse();
        assert!(err.is_err());
    }

    #[test]
    fn test_var_decl() {
        let mut parser = Parser::new(r#"var x: int = 1"#);
        let ast = parser.parse();
        assert!(ast.is_ok());
    }

    #[test]
    fn test_if_expr() {
        let mut parser = Parser::new(r#"var err: int = if true then 1 else 2 "#);
        let ast = parser.parse();
        dbg!(&ast.clone().unwrap());
        assert!(ast.is_ok());
    }

    #[test]
    fn test_def_with_no_return_value() {
        let mut parser = Parser::new(
            r#"
            def none() do
            end
            "#,
        );
        let ast = parser.parse();
        assert!(ast.is_ok());
    }

    #[test]
    fn test_def_no_args() {
        let mut parser = Parser::new(
            r#"
        def ok() -> int do
        do
            ret 1
            end
        end
        "#,
        );
        let ast = parser.parse();
        dbg!(&ast);
        assert!(ast.is_ok());
    }

    #[test]
    fn test_def_with_args() {
        let mut parser = Parser::new(
            r#"
        def ok(val: int) -> int do
            ret 1 + 1
        end
        "#,
        );
        let ast = parser.parse();
        assert!(ast.is_ok());
    }

    #[test]
    fn test_if_stmt() {
        let mut parser = Parser::new(
            r#"
        if true then
            water("fall")
        end
        "#,
        );
        let ast = parser.parse();
        assert!(ast.is_ok());
    }
}
