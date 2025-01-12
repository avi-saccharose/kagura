use core::fmt;
use std::ops::Range;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub(crate) enum Kind {
    Lt,   // <
    Gt,   // >
    LtEq, // <=
    GtEq, // >=
    NtEq, // !=
    EqEq, // ==

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

    True,
    False,
    Nil,

    Str,
    Int,
    Float,
    Ident,

    EOF,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) span: Range<usize>,
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

            Self::And => "&&",
            Self::Or => "||",

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

            Self::True => "true",
            Self::False => "false",
            Self::Nil => "nil",

            Self::Str => "string",
            Self::Int => "integer",
            Self::Float => "float",
            Self::Ident => "identifier",

            Self::EOF => "\0",
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
}
