use std::{iter::Peekable, str::CharIndices};

use crate::lexer::token::{Kind, Span, Token};

pub mod token;

#[derive(Debug)]
pub struct Lexer<'input> {
    input: &'input str,
    chars: Peekable<CharIndices<'input>>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

// TODO: implement string
impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            input,
            chars: input.char_indices().peekable(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        self.chars.next().map(|(i, c)| {
            self.current = i + c.len_utf8();
            (i, c)
        })
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next().map(|(_, ch)| ch)
    }

    fn peek_match(&self, check: char) -> bool {
        if let Some(ch) = self.peek()
            && ch == check
        {
            return true;
        }
        false
    }

    fn skip_whitespace(&mut self) {
        while let Some(' ' | '\t' | '\n' | '\r') = self.peek() {
            if self.peek().unwrap() == '\n' {
                self.line += 1;
            }
            self.next_char();
        }
    }

    fn skip_comment(&mut self) {
        self.next_char(); // consume the #
        while !self.peek_match('\n') {
            self.next_char();
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.peek_match('#') {
            self.skip_comment();
        }

        self.skip_whitespace();

        self.start = self.current;
        let (_, ch) = self.next_char()?;
        let kind = match ch {
            '+' => Kind::Plus,
            ',' => Kind::Comma,
            '*' => Kind::Star,
            '/' => Kind::Slash,
            ':' => Kind::Colon,
            //  ';' => Kind::Semicolon,
            '{' => Kind::Lbrace,
            '}' => Kind::Rbrace,
            '(' => Kind::Lparen,
            ')' => Kind::Rparen,
            '-' => {
                if self.peek_match('>') {
                    self.next_char();
                    Kind::Arrow
                } else {
                    Kind::Minus
                }
            }
            '=' => {
                if self.peek_match('=') {
                    self.next_char();
                    Kind::EqEq
                } else {
                    Kind::Eq
                }
            }
            '!' => {
                if self.peek_match('=') {
                    self.next_char();
                    Kind::NtEq
                } else {
                    Kind::Bang
                }
            }
            '<' => {
                if self.peek_match('=') {
                    self.next_char();
                    Kind::LtEq
                } else {
                    Kind::Lt
                }
            }
            '>' => {
                if self.peek_match('=') {
                    self.next_char();
                    Kind::GtEq
                } else {
                    Kind::Gt
                }
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.ident(),
            _ => Kind::Error,
        };
        Some(Token {
            kind,
            line: self.line,
            span: Span {
                start: self.start as u32,
                end: self.current as u32,
            },
        })
    }

    fn string(&mut self) -> Kind {
        while !matches!(self.peek(), Some('"')) {
            self.next_char();
        }
        if self.peek_match('"') {
            self.next_char();
            return Kind::StringLit;
        }
        Kind::UnterminatedStr
    }

    // TODO: Add decimal support
    fn number(&mut self) -> Kind {
        while matches!(self.peek(), Some('0'..='9')) {
            self.next_char();
        }
        Kind::IntLit
    }

    // TODO: Add remaining keywords
    fn ident(&mut self) -> Kind {
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
            self.next_char();
        }
        let lexme = &self.input[self.start..self.current];
        match lexme {
            "do" => Kind::Do,
            "end" => Kind::End,

            "def" => Kind::Def,

            "puts" => Kind::Puts,
            "var" => Kind::Var,

            "and" => Kind::And,
            "or" => Kind::Or,

            "if" => Kind::If,
            "then" => Kind::Then,
            "else" => Kind::Else,

            "true" => Kind::True,
            "false" => Kind::False,
            "int" => Kind::Int,
            "bool" => Kind::Bool,
            "str" => Kind::Str,
            _ => {
                if self.input.chars().next().unwrap().is_ascii_uppercase() {
                    Kind::Const
                } else {
                    Kind::Ident
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_two_characters() {
        let mut lexer = Lexer::new("<=");
        assert!(matches!(
            lexer.next_token(),
            Some(Token {
                kind: Kind::LtEq,
                ..
            })
        ))
    }

    #[test]
    fn lex_newline() {
        let mut lexer = Lexer::new("    \n");
        lexer.next_token();
        assert_eq!(lexer.line, 2);
    }

    #[test]
    fn lex_number() {
        let input = "134";
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, Kind::IntLit);
        assert_eq!("134", &input[token.span]);
    }

    #[test]
    fn skip_comment() {
        let input = "#this is a comment
                    123";
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, Kind::IntLit);
    }

    #[test]
    fn lex_ident() {
        let input = "var value = 1";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<Kind> = lexer.tokenize().iter().map(|token| token.kind).collect();
        assert_eq!(tokens, vec![Kind::Var, Kind::Ident, Kind::Eq, Kind::IntLit])
    }

    #[test]
    fn lex_const() {
        let input = "Value = 1";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<Kind> = lexer.tokenize().iter().map(|token| token.kind).collect();
        assert_eq!(tokens, vec![Kind::Const, Kind::Eq, Kind::IntLit])
    }

    #[test]
    fn lex_expr() {
        let input = "fn(12) + 3";
        let mut lexer = Lexer::new(input);
        let tokens: Vec<Kind> = lexer.tokenize().iter().map(|token| token.kind).collect();
        assert_eq!(
            tokens,
            vec![
                Kind::Ident,
                Kind::Lparen,
                Kind::IntLit,
                Kind::Rparen,
                Kind::Plus,
                Kind::IntLit
            ]
        )
    }

    #[test]
    fn lex_string() {
        let input = "\"hello world\"";
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token().unwrap();
        assert_eq!(token.kind, Kind::StringLit);
        assert_eq!(&input[token.span], "\"hello world\"")
    }
}
