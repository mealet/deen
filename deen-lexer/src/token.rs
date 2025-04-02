use miette::SourceSpan;

use crate::token_type::TokenType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    pub span: SourceSpan
}

impl Token {
    pub fn new(value: String, token_type: TokenType, span: (usize, usize)) -> Self {
        Self {
            value,
            token_type,
            span: span.into()
        }
    }
}
