use core::fmt;
use std::error::Error;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct KaguError {
    pub(crate) msg: String,
    pub(crate) line: u32,
    pub(crate) start: usize,
    pub(crate) column: u32,
    pub(crate) error_type: ErrorType,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum ErrorType {
    Lexer,
    Parse,
    Runtime,
    ParseEof,
}

impl Error for KaguError {}

impl fmt::Display for KaguError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[line: {}] {}", self.line, self.msg))
    }
}
