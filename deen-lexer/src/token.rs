//! ## Token
//! **Token** - a structure that contains information of current lexeme: value, token_type and
//! position in source code.
//!
//! ## Example:
//! ```rust
//! use deen_lexer::{token::Token, token_type::TokenType};
//!
//! let number = Token::new(String::from("123"), TokenType::Number, (0, 2));
//!
//! assert!(number.value == "123");
//! assert!(number.token_type == TokenType::Number);
//! assert!(number.span == (0, 2));
//! ```

use crate::token_type::TokenType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    pub span: (usize, usize),
}

impl Token {
    pub fn new(value: String, token_type: TokenType, span: (usize, usize)) -> Self {
        Self {
            value,
            token_type,
            span,
        }
    }
}
