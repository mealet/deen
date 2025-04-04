#![allow(unused)]

use miette::{NamedSource, Diagnostic};
use std::collections::HashMap;
use deen_lexer::{
    token::Token,
    token_type::TokenType
};
use crate::{
    error::{ParserError, ParserWarning}
};

mod error;
pub mod statements;
pub mod expressions;
pub mod value;
pub mod types;

const BINARY_OPERATORS: [TokenType; 4] = [
    TokenType::Plus,     // +
    TokenType::Minus,    // -
    TokenType::Divide,   // /
    TokenType::Multiply, // *
];

const BOOLEAN_OPERATORS: [TokenType; 6] = [
    TokenType::Lt,  // <
    TokenType::Bt,  // >
    TokenType::Eq,  // ==
    TokenType::Ne,  // !
    TokenType::Or,  // ||
    TokenType::And, // &&
];

const BITWISE_OPERATORS: [TokenType; 5] = [
    TokenType::LShift,    // <<
    TokenType::RShift,    // >>
    TokenType::Ampersand, // &
    TokenType::Verbar,    // |
    TokenType::Xor,       // ^
];

const PRIORITY_BINARY_OPERATORS: [TokenType; 2] = [TokenType::Multiply, TokenType::Divide];
const PRIORITY_BOOLEAN_OPERATORS: [TokenType; 2] = [TokenType::Or, TokenType::And];

const END_STATEMENT: TokenType = TokenType::Semicolon;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parser {
    source: NamedSource<String>,

    tokens: Vec<Token>,
    position: usize,

    errors: Vec<ParserError>,
    warnings: Vec<ParserWarning>,
    eof: bool
}
