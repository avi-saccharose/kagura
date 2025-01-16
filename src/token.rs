#![allow(dead_code)]
use core::fmt;
use std::ops::{Index, Range};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub(crate) enum Kind {
    Lt,   // <
    Gt,   // >
    LtEq, // <=
    GtEq, // >=
    NtEq, // !=
    EqEq, // ==

    Eq,        // =
    Star,      // *
    Plus,      // +
    Minus,     // -
    Slash,     // /
    Dot,       // .
    Comma,     // ,
    Lparen,    // (
    Rparen,    // )
    Lbrace,    // {
    Rbrace,    // }
    Lbracket,  // [
    Rbracket,  // ]
    Ampersand, // &
    At,        // @
    Bang,      // !
    Semicolon, // ;
    Colon,     // :

    And,
    Or,

    If,
    Else,
    While,
    For,
    Loop,
    Def,
    Class,
    Then,
    Do,
    End,
    Var,
    Puts,
    Return,

    True,
    False,
    Nil,

    Str,
    Int,
    Float,
    Ident,

    Eof,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) line: u32,
    pub(crate) column: u32,
    pub(crate) span: Span<usize>,
}

impl Token {
    pub(crate) fn text<'a>(&self, input: &'a str) -> &'a str {
        &input[self.span]
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Default, Eq)]
pub(crate) struct Span<T> {
    pub(crate) start: T,
    pub(crate) end: T,
}

impl From<Span<usize>> for Range<usize> {
    fn from(span: Span<usize>) -> Self {
        span.start..span.end
    }
}

impl From<Range<usize>> for Span<usize> {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Index<Span<usize>> for str {
    type Output = str;
    fn index(&self, index: Span<usize>) -> &Self::Output {
        &self[Range::<usize>::from(index)]
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Lt => "<",
            Self::Gt => "<",
            Self::LtEq => "<=",
            Self::GtEq => ">=",
            Self::NtEq => "!=",
            Self::EqEq => "==",

            Self::Eq => "=",
            Self::Star => "*",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Slash => "/",
            Self::Dot => ".",
            Self::Comma => ",",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::Lbracket => "[",
            Self::Rbracket => "]",
            Self::Ampersand => "&",
            Self::At => "@",
            Self::Bang => "!",
            Self::Semicolon => ";",
            Self::Colon => ":",

            Self::And => "and",
            Self::Or => "or",

            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::For => "for",
            Self::Loop => "loop",
            Self::Def => "def",
            Self::Class => "class",
            Self::Then => "then",
            Self::Do => "do",
            Self::End => "end",
            Self::Puts => "puts",
            Self::Return => "return",

            Self::Var => "var",
            Self::True => "true",
            Self::False => "false",
            Self::Nil => "nil",

            Self::Str => "string",
            Self::Int => "integer",
            Self::Float => "float",
            Self::Ident => "identifier",

            Self::Eof => "\0",
        };
        f.write_str(str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kind_to_string() {
        let kind = Kind::Ampersand;
        assert_eq!(kind.to_string(), "&".to_string());
    }

    #[test]
    fn span_text() {
        let text = "while";
        let token = Token {
            kind: Kind::While,
            line: 0,
            column: 0,
            span: Span { start: 0, end: 5 },
        };
        assert_eq!(token.text(text), "while");
    }
}
