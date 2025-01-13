#![allow(dead_code)]
use std::{iter::Peekable, vec::IntoIter};

use crate::{
    error::{ErrorType, KaguError},
    expr::{Arena, Ast, Bin, Block, Idx, Lit, Node},
    lexer,
    token::{Kind, Token},
};

struct Parser<'a> {
    nodes: Arena<Node>,
    indices: Vec<Idx>,
    tokens: Peekable<IntoIter<Token>>,
    source: &'a str,
    prev: Option<Token>,
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            nodes: Arena::new(),
            indices: Vec::new(),
            tokens: tokens.into_iter().peekable(),
            source,
            prev: None,
        }
    }

    fn peek_kind(&mut self) -> Kind {
        match self.tokens.peek() {
            Some(token) => token.kind,
            None => Kind::Eof,
        }
    }

    fn peek(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    fn eof(&mut self) -> bool {
        self.peek_kind() == Kind::Eof
    }

    fn advance(&mut self) -> Option<Token> {
        self.prev = Some(*self.peek());
        self.tokens.next()
    }

    fn previous(&mut self) -> Token {
        self.prev.unwrap()
    }

    fn check(&mut self, kind: Kind) -> bool {
        self.peek_kind() == kind
    }

    fn matches(&mut self, kinds: &[Kind]) -> bool {
        for kind in kinds.iter() {
            if self.check(*kind) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn block_start(&self) -> Idx {
        Idx(self.nodes.data.len())
    }

    fn add_node(&mut self, node: Node) -> Idx {
        self.nodes.alloc(node)
    }

    fn make_error(&mut self, msg: &str) -> KaguError {
        let token = self.previous();
        KaguError {
            msg: msg.to_string(),
            line: token.line,
            column: token.column,
            start: token.span.start,
            error_type: ErrorType::Parse,
        }
    }

    fn consume(&mut self, kind: Kind, msg: &str) -> Result<Token, KaguError> {
        if self.peek_kind() == kind {
            return Ok(self.advance().unwrap());
        }
        Err(self.make_error(msg))
    }

    fn parse(&mut self) -> Result<(), KaguError> {
        while !self.eof() {
            let idx = self.stmt_declaration()?;
            self.indices.push(idx);
        }
        Ok(())
    }

    fn stmt_declaration(&mut self) -> Result<Idx, KaguError> {
        self.stmt()
    }

    fn stmt(&mut self) -> Result<Idx, KaguError> {
        match self.peek_kind() {
            Kind::Puts => self.stmt_puts(),
            Kind::Lbrace => self.stmt_block(),
            _ => self.stmt_expr(),
        }
    }

    fn stmt_puts(&mut self) -> Result<Idx, KaguError> {
        self.advance();
        let expr = self.expr()?;
        self.consume(Kind::Semicolon, "expect ';' after puts")?;
        let idx = self.add_node(Node::Puts(expr));
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
        let idx = self.add_node(Node::Block(Block { start, end }));
        Ok(idx)
    }

    fn stmt_expr(&mut self) -> Result<Idx, KaguError> {
        let idx = self.expr()?;
        self.consume(Kind::Semicolon, "Expect ';' after expr")?;
        Ok(idx)
    }

    fn expr(&mut self) -> Result<Idx, KaguError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Idx, KaguError> {
        self.equality()
    }
    fn equality(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.comparison()?;

        while self.matches(&[Kind::EqEq, Kind::NtEq]) {
            let op = self.previous();
            let right = self.comparison()?;
            let expr = Node::BinExpr(Bin { left, right, op });
            left = self.add_node(expr);
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.term()?;
        while self.matches(&[Kind::Lt, Kind::LtEq, Kind::Gt, Kind::GtEq]) {
            let op = self.previous();
            let right = self.term()?;
            let expr = Node::BinExpr(Bin { left, right, op });
            left = self.add_node(expr);
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.factor()?;

        while self.matches(&[Kind::Plus, Kind::Minus]) {
            let op = self.previous();
            let right = self.factor()?;
            let expr = Node::BinExpr(Bin { left, right, op });
            left = self.add_node(expr);
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Idx, KaguError> {
        let mut left = self.unary()?;

        while self.matches(&[Kind::Star, Kind::Slash]) {
            let op = self.previous();
            let right = self.unary()?;
            let expr = Node::BinExpr(Bin { left, right, op });
            left = self.add_node(expr);
        }

        Ok(left)
    }

    // TODO: Unary
    fn unary(&mut self) -> Result<Idx, KaguError> {
        self.primary()
    }

    fn primary(&mut self) -> Result<Idx, KaguError> {
        match self.peek_kind() {
            Kind::Int | Kind::Ident | Kind::Str => self.make_literal(),
            Kind::True => {
                self.advance();
                let lit = Node::Lit(Lit::Bool(true));
                Ok(self.add_node(lit))
            }
            Kind::False => {
                self.advance();
                let lit = Node::Lit(Lit::Bool(false));
                Ok(self.add_node(lit))
            }
            Kind::Nil => {
                self.advance();
                let lit = Node::Lit(Lit::Nil);
                Ok(self.add_node(lit))
            }
            Kind::Lparen => {
                self.advance();
                let expr = self.expr()?;
                self.consume(Kind::Rparen, "Expected ')' after expression")?;
                Ok(expr)
            }
            _ => {
                // If its not the last token we need to consume it because make_error uses the
                // previous token
                if !self.eof() {
                    self.advance();
                }
                Err(self.make_error("Expected expression"))
            }
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
                            column: token.column,
                            start: token.span.start,
                            error_type: ErrorType::Parse,
                        })
                    }
                };
                let lit = Node::Lit(Lit::Int(int));
                Ok(self.add_node(lit))
            }
            Kind::Ident => {
                let lit = Node::Lit(Lit::Ident(lit.to_string()));
                Ok(self.add_node(lit))
            }
            Kind::Str => {
                let lit = Node::Lit(Lit::String(lit.to_string()));
                Ok(self.add_node(lit))
            }
            _ => unreachable!(),
        }
    }

    // WARN: Function only used while running tests
    // TODO: Move to ast impl
    fn get(&mut self, index: usize) -> Node {
        let idx = self.indices.get(index).unwrap();
        let node = self.nodes.get(*idx);
        node.clone()
    }
}

pub(crate) fn parse(input: &str) -> Result<Ast, KaguError> {
    let tokens = lexer::tokenize(input)?;
    let mut parser = Parser::new(tokens, input);
    parser.parse()?;
    let ast = Ast {
        nodes: parser.nodes,
        indices: parser.indices,
    };
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use crate::lexer::{self};

    use super::*;

    #[track_caller]
    fn run(source: &str) -> Result<Parser, KaguError> {
        let tokens = lexer::tokenize(source);
        assert!(tokens.is_ok());
        let mut parsed = Parser::new(tokens.unwrap(), source);
        parsed.parse()?;
        Ok(parsed)
    }

    #[test]
    fn parse_expr() {
        let input = "1 + 1 * 2;";
        let mut parsed = run(input).unwrap();

        assert!(matches!(
            parsed.get(0),
            Node::BinExpr(Bin {
                op: Token {
                    kind: Kind::Plus,
                    ..
                },
                ..
            })
        ));
    }

    #[test]
    fn parse_error() {
        let input = "1 + ";
        let parsed = run(input);
        assert!(parsed.is_err());
    }

    #[test]
    fn parse_binary() {
        let input = "1 <= 1;";
        let mut parsed = run(input).unwrap();
        assert!(matches!(
            parsed.get(0),
            Node::BinExpr(Bin {
                op: Token {
                    kind: Kind::LtEq,
                    ..
                },
                ..
            })
        ));
    }

    #[test]
    fn parse_grouping() {
        let input = "(1 + 1) * 4;";
        let mut parsed = run(input).unwrap();
        assert!(matches!(
            parsed.get(0),
            Node::BinExpr(Bin {
                op: Token {
                    kind: Kind::Star,
                    ..
                },
                ..
            })
        ));
    }

    #[test]
    fn parse_block() {
        let input = "1; { puts 1; puts 2; } puts 3; {}";
        let mut parsed = run(input).unwrap();
        assert!(matches!(parsed.get(0), Node::Lit(..)));
        assert!(matches!(parsed.get(1), Node::Block(..)));
        assert!(matches!(parsed.get(2), Node::Puts(..)));
    }
}
