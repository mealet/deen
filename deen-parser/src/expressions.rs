use crate::{
    error::{ParserError, ParserWarning},
    statements::Statements,
    value::Value,
    types::Type,
    Parser
};
use deen_lexer::{
    token::Token,
    token_type::TokenType
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expressions {
    Binary {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize)
    },
    Boolean {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize)
    },
    Bitwise {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize)
    },

    Argument {
        name: String,
        r#type: Type,
        span: (usize, usize)
    },
    SubElement {
        parent: Box<Expressions>,
        child: Box<Expressions>,
        span: (usize, usize)
    },

    FnCall {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize)
    },
    Reference {
        object: Box<Expressions>,
        span: (usize, usize)
    },
    Dereference {
        object: Box<Expressions>,
        span: (usize, usize)
    },

    Array {
        values: Vec<Expressions>,
        len: usize,
        span: (usize, usize)
    },
    Slice {
        object: Box<Expressions>,
        index: Box<Expressions>,
        span: (usize, usize)
    },

    Value(Value, (usize, usize)),
    None,
}

impl Parser {
    pub fn subelement_expression(&mut self, parent: Expressions, separator: TokenType) -> Expressions {
        todo!()
    }
}
