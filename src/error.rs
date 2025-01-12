use core::fmt;
use std::error::Error;

use crate::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct KaguError {
    pub(crate) msg: String,
    pub(crate) line: usize,
    pub(crate) start: usize,
}

impl Error for KaguError {}

impl fmt::Display for KaguError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("[line: ] {}", self.msg))
    }
}
