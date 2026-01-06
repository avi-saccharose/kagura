#![allow(dead_code)]

use std::{
    fmt::{self},
    ops::{Index, Range},
};

#[derive(Debug, Copy, Clone, Hash, PartialEq, PartialOrd)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start as u32,
            end: value.end as u32,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{},{}>", self.start, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.start as usize..value.end as usize
    }
}

impl Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::from(index)]
    }
}

#[derive(Copy, Debug, Clone, Hash, PartialEq, PartialOrd)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
    pub line: usize,
}

impl Token {
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span]
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, PartialOrd)]
pub enum Kind {
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    Comma,
    Colon,
    //   Semicolon,
    At,
    Bar,
    Underscore,

    Eq,
    EqEq,
    Bang,
    NtEq,
    Lt,
    LtEq,
    Gt,
    GtEq,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lblock,
    Rblock,

    Arrow,

    Puts,
    Var,
    Def,
    If,
    Then,
    Else,
    Do,
    End,
    And,
    Or,

    Int,
    Str,
    Bool,

    Const,
    Ident,
    True,
    False,
    StringLit,
    Comment,
    IntLit,

    Error,
    Eof,
    UnterminatedStr,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Dot => ".",
            Self::Comma => ",",
            Self::Colon => ":",
            //  Self::Semicolon => ";",
            Self::At => "@",
            Self::Bar => "|",
            Self::Underscore => "_",

            Self::Eq => "=",
            Self::EqEq => "==",
            Self::Bang => "!",
            Self::NtEq => "!=",
            Self::Lt => "<",
            Self::LtEq => "<=",
            Self::Gt => ">",
            Self::GtEq => ">=",

            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::Lblock => "[",
            Self::Rblock => "]",

            Self::Arrow => "->",

            Self::Puts => "puts",
            Self::Var => "var",
            Self::Def => "def",
            Self::If => "if",
            Self::Then => "then",
            Self::Else => "else",
            Self::Do => "do",
            Self::End => "end",
            Self::And => "and",
            Self::Or => "or",

            Self::Int => "int",
            Self::Str => "str",
            Self::Bool => "bool",

            Self::Const => "const",
            Self::Ident => "ident",
            Self::True => "true",
            Self::False => "false",
            Self::StringLit => "string",
            Self::Comment => "comment",
            Self::IntLit => "int",

            Self::Error => "<?>",
            Self::UnterminatedStr => "Unterminated string",
            Self::Eof => "eof",
        };
        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenkind() {
        assert_eq!(Kind::Plus.to_string(), "+".to_string());
    }
}
