#![allow(dead_code)]
use std::{iter::Peekable, str::CharIndices};

use crate::{
    error::{ErrorType, KaguError},
    token::{Kind, Span, Token},
};

pub(crate) struct Lexer<'a> {
    source: Peekable<CharIndices<'a>>,
    text: &'a str,
    current: usize,
    line: u32,
    column: u32,
}

pub(crate) fn tokenize(input: &str) -> Result<Vec<Token>, KaguError> {
    Lexer::new(input).tokenize()
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            source: input.char_indices().peekable(),
            text: input,
            current: 0,
            line: 1,
            column: 0,
        }
    }

    // TODO: return char instead
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.source.peek()
    }

    fn peek_match(&mut self, ch: char) -> bool {
        if let Some((_, i)) = self.peek() {
            if *i == ch {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> (usize, char) {
        self.current += 1;
        self.column += 1;
        self.source.next().unwrap_or((self.current, '\0'))
    }

    fn is_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn is_newline(&mut self) -> bool {
        if let Some((_, ch)) = self.peek() {
            return *ch == '\n';
        }
        false
    }

    fn make_token(&self, kind: Kind, start: usize) -> Token {
        Token {
            kind,
            line: self.line,
            column: self.column,
            span: Span {
                start,
                end: self.current,
            },
        }
    }

    fn text(&self, start: usize) -> &'a str {
        &self.text[start..self.current]
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, KaguError> {
        let mut tokens = Vec::new();
        loop {
            let (start, ch) = self.advance();
            if ch == '\0' {
                break;
            }

            if ch == '#' {
                while !self.is_eof() && !self.is_newline() {
                    self.advance();
                }
                continue;
            }

            if ch.is_whitespace() {
                if ch == '\n' {
                    self.line += 1;
                }
                continue;
            }

            if ch.is_ascii_digit() {
                tokens.push(self.digit(start)?);
                continue;
            }

            if ch.is_alphabetic() {
                tokens.push(self.ident(start)?);
                continue;
            }

            if ch == '"' || ch == '\'' {
                tokens.push(self.string(start, ch)?);
                continue;
            }

            let token = match ch {
                '+' => self.make_token(Kind::Plus, start),
                '-' => self.make_token(Kind::Minus, start),
                '*' => self.make_token(Kind::Star, start),
                '/' => self.make_token(Kind::Slash, start),
                ':' => self.make_token(Kind::Colon, start),
                ',' => self.make_token(Kind::Comma, start),
                '.' => self.make_token(Kind::Dot, start),
                ';' => self.make_token(Kind::Semicolon, start),
                '{' => self.make_token(Kind::Lbrace, start),
                '}' => self.make_token(Kind::Rbrace, start),
                '(' => self.make_token(Kind::Lparen, start),
                ')' => self.make_token(Kind::Rparen, start),

                '=' => {
                    if self.peek_match('=') {
                        self.make_token(Kind::EqEq, start)
                    } else {
                        self.make_token(Kind::Eq, start)
                    }
                }
                '!' => {
                    if self.peek_match('=') {
                        self.make_token(Kind::NtEq, start)
                    } else {
                        self.make_token(Kind::Bang, start)
                    }
                }
                '>' => {
                    if self.peek_match('=') {
                        self.make_token(Kind::GtEq, start)
                    } else {
                        self.make_token(Kind::Gt, start)
                    }
                }
                '<' => {
                    if self.peek_match('=') {
                        self.make_token(Kind::LtEq, start)
                    } else {
                        self.make_token(Kind::Lt, start)
                    }
                }
                _ => {
                    return Err(KaguError {
                        msg: format!("unexpected character {}", ch),
                        line: self.line,
                        column: self.column,
                        start,
                        error_type: ErrorType::Lexer,
                    });
                }
            };
            tokens.push(token);
        }
        Ok(tokens)
    }

    // TODO: add floating support
    fn digit(&mut self, start: usize) -> Result<Token, KaguError> {
        while let Some((_, ch)) = self.peek() {
            if !ch.is_ascii_digit() {
                break;
            }
            self.advance();
        }
        Ok(self.make_token(Kind::Int, start))
    }

    fn ident(&mut self, start: usize) -> Result<Token, KaguError> {
        while let Some((_, ch)) = self.peek() {
            if !ch.is_alphanumeric() && *ch != '_' {
                break;
            }
            self.advance();
        }
        let kind = match self.text(start) {
            "if" => Kind::If,
            "else" => Kind::Else,
            "while" => Kind::While,
            "for" => Kind::For,
            "loop" => Kind::Loop,
            "def" => Kind::Def,
            "class" => Kind::Class,
            "then" => Kind::Then,
            "do" => Kind::Do,
            "end" => Kind::End,
            "true" => Kind::True,
            "false" => Kind::False,
            "nil" => Kind::Nil,
            "var" => Kind::Var,
            "and" => Kind::And,
            "or" => Kind::Or,
            "puts" => Kind::Puts,
            _ => Kind::Ident,
        };
        Ok(self.make_token(kind, start))
    }

    fn string(&mut self, start: usize, init: char) -> Result<Token, KaguError> {
        let column = self.column;
        while let Some((_, ch)) = self.peek() {
            if *ch == init {
                self.advance(); // Consume the char
                return Ok(self.make_token(Kind::Str, start));
            }
            self.advance();
        }
        Err(KaguError {
            msg: "Unterminated string".to_string(),
            line: self.line,
            column,
            start,
            error_type: ErrorType::Lexer,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lex_single() {
        let input = "1 <= 2";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].text(input), "1".to_string());
        assert_eq!(tokens[1].text(input), "<=".to_string());
    }

    #[test]
    fn lex_whitespace() {
        let input = "     \n+";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].text(input), "+".to_string());
        assert_eq!(tokens[0].line, 2);
    }

    #[test]
    fn skip_comment() {
        let input = "#this is a comment
           1 
        ";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].kind, Kind::Int);
    }

    #[test]
    fn keywords() {
        let input = "true def ident";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].kind, Kind::True);
        assert_eq!(tokens[1].kind, Kind::Def);
        assert_eq!(tokens[2].kind, Kind::Ident);
        assert_eq!(tokens[2].text(input), "ident".to_string());
    }

    #[test]
    fn identifiers() {
        let input = "valid_ident";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].kind, Kind::Ident);
        assert_eq!(tokens[0].text(input), "valid_ident".to_string());
    }

    #[test]
    fn strings() {
        let input = "\"this is a string\"";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].kind, Kind::Str);
        assert_eq!(tokens[0].text(input), "\"this is a string\"".to_string());
    }

    #[test]
    fn unterminated_string() {
        let input = "\"uh oh";
        let lexer = Lexer::new(input).tokenize();
        assert!(lexer.is_err());
    }

    #[test]
    fn multi_strings() {
        let input = "\'string1\'\"string2\"";
        let lexer = Lexer::new(input).tokenize();
        let tokens = lexer.unwrap();
        assert_eq!(tokens[0].kind, Kind::Str);
        assert_eq!(tokens[0].text(input), "\'string1\'".to_string());
        assert_eq!(tokens[1].kind, Kind::Str);
        assert_eq!(tokens[1].text(input), "\"string2\"".to_string());
    }
}
