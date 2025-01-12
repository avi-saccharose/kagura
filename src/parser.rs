#![allow(dead_code)]
use std::{iter::Peekable, vec::IntoIter};

use crate::{
    error::{ErrorType, KaguError},
    expr::{Arena, Ast, Bin, Block, Expr, Idx, Lit, Stmt},
    lexer,
    token::{Kind, Token},
};

struct Parser<'a> {
    stmts: Arena<Stmt>,
    exprs: Arena<Expr>,
    tokens: Peekable<IntoIter<Token>>,
    source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            stmts: Arena::new(),
            exprs: Arena::new(),
            tokens: tokens.into_iter().peekable(),
            source,
        }
    }

    fn peek(&mut self) -> Kind {
        match self.tokens.peek() {
            Some(token) => token.kind,
            None => Kind::Eof,
        }
    }

    fn eof(&mut self) -> bool {
        self.peek() == Kind::Eof
    }

    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn check(&mut self, kind: Kind) -> bool {
        self.peek() == kind
    }

    fn matches(&mut self, kinds: &[Kind]) -> bool {
        for kind in kinds.iter() {
            if self.check(*kind) {
                return true;
            }
        }
        false
    }

    fn block_start(&self) -> Idx {
        Idx(self.stmts.data.len())
    }

    fn consume(&mut self, kind: Kind, msg: &str) -> Result<Token, KaguError> {
        if self.eof() {
            return Err(KaguError {
                msg: msg.to_string(),
                line: 0,
                start: 0,
                error_type: ErrorType::ParseEof,
            });
        }
        let token = self.advance().unwrap();
        if token.kind == kind {
            Ok(token)
        } else {
            Err(KaguError {
                msg: msg.to_string(),
                line: token.line,
                start: token.span.start,
                error_type: ErrorType::Parse,
            })
        }
    }

    fn parse(&mut self) -> Result<(), KaguError> {
        while !self.eof() {
            self.stmt_declaration()?;
        }
        Ok(())
    }

    fn stmt_declaration(&mut self) -> Result<Idx, KaguError> {
        self.stmt()
    }

    fn stmt(&mut self) -> Result<Idx, KaguError> {
        match self.peek() {
            Kind::Puts => self.stmt_puts(),
            Kind::Lbrace => self.stmt_block(),
            _ => self.stmt_expr(),
        }
    }

    fn stmt_puts(&mut self) -> Result<Idx, KaguError> {
        self.advance();
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "expect ';' after puts")?;
        let idx = self.stmts.alloc(Stmt::Puts(expr));
        Ok(idx)
    }

    fn stmt_block(&mut self) -> Result<Idx, KaguError> {
        self.advance();
        let start = self.block_start();
        let mut end = Idx(0);
        while !self.eof() && !self.check(Kind::Rbrace) {
            end = self.stmt_declaration()?;
        }

        self.consume(Kind::Rbrace, "expect '}'")?;
        let idx = self.stmts.alloc(Stmt::Block(Block { start, end }));
        Ok(idx)
    }

    fn stmt_expr(&mut self) -> Result<Idx, KaguError> {
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "Expect ';' after expr")?;
        let stmt = Stmt::Expr(expr);
        let idx = self.stmts.alloc(stmt);

        Ok(idx)
    }

    fn expr(&mut self) -> Result<Idx, KaguError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Idx, KaguError> {
        self.equality()
    }
    fn equality(&mut self) -> Result<Idx, KaguError> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Idx, KaguError> {
        self.term()
    }

    fn term(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.factor()?;

        while self.matches(&[Kind::Plus, Kind::Minus]) {
            let op = self.advance().unwrap();
            let right = self.factor()?;
            let expr = Expr::Bin(Bin { left, right, op });
            left = self.exprs.alloc(expr);
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.unary()?;

        while self.matches(&[Kind::Star, Kind::Slash]) {
            let op = self.advance().unwrap();
            let right = self.unary()?;
            let expr = Expr::Bin(Bin { left, right, op });
            left = self.exprs.alloc(expr);
        }

        Ok(left)
    }

    fn unary(&mut self) -> Result<Idx, KaguError> {
        self.primary()
    }

    fn primary(&mut self) -> Result<Idx, KaguError> {
        match self.peek() {
            Kind::Int | Kind::Ident | Kind::Str => self.make_literal(),
            Kind::True => {
                self.advance();
                let lit = Expr::Lit(Lit::Bool(true));
                Ok(self.exprs.alloc(lit))
            }
            Kind::False => {
                self.advance();
                let lit = Expr::Lit(Lit::Bool(false));
                Ok(self.exprs.alloc(lit))
            }
            Kind::Nil => {
                self.advance();
                let lit = Expr::Lit(Lit::Nil);
                Ok(self.exprs.alloc(lit))
            }
            _ => Err(KaguError {
                msg: format!("Expected character found {}", self.peek()),
                line: 0,
                start: 0,
                error_type: ErrorType::Parse,
            }),
        }
    }

    fn make_literal(&mut self) -> Result<Idx, KaguError> {
        let token = self.advance().unwrap();
        let lit = token.text(self.source);
        match token.kind {
            Kind::Int => {
                let int: i64 = match lit.parse() {
                    Ok(val) => val,
                    Err(_) => {
                        return Err(KaguError {
                            msg: "Error parsing number".to_string(),
                            line: token.line,
                            start: token.span.start,
                            error_type: ErrorType::Parse,
                        })
                    }
                };
                let expr = Expr::Lit(Lit::Int(int));
                Ok(self.exprs.alloc(expr))
            }
            Kind::Ident => {
                todo!()
            }
            Kind::Str => {
                todo!()
            }
            _ => unreachable!(),
        }
    }
}

pub(crate) fn parse(input: &str) -> Result<Ast, KaguError> {
    let tokens = lexer::tokenize(input)?;
    let mut parser = Parser::new(tokens, input);
    parser.parse()?;
    let ast = Ast::new(parser.stmts, parser.exprs);
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use crate::lexer::{self};

    use super::*;

    fn tokenize(source: &str) -> Result<Vec<Token>, KaguError> {
        lexer::tokenize(source)
    }

    #[test]
    fn parse_expr() {
        let input = "puts 1; puts 2; puts 3;";
        let parsed = parse(input);
        assert!(parsed.is_ok());
        let parsed = parsed.unwrap();
        let stmt = parsed.stmts;
        assert!(matches!(stmt.data[0], Stmt::Puts(..)));
    }

    #[test]
    fn parse_block() {
        let input = "1; { puts 1; puts 2; } puts 3; {}";
        let parsed = parse(input).unwrap();
        dbg!(parsed);
    }
}
