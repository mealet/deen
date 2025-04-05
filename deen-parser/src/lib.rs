#![allow(unused)]

use miette::{NamedSource, Diagnostic};
use std::collections::HashMap;
use deen_lexer::{
    token::Token,
    token_type::TokenType
};
use crate::{
    error::{ParserError, ParserWarning},
    statements::Statements,
    expressions::Expressions,
    value::Value,
    types::Type
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

impl Parser {
    // main

    pub fn new(tokens: Vec<Token>, source: String, filename: String) -> Self {
        Self {
            source: NamedSource::new(filename, source),

            tokens,
            position: 0,

            errors: Vec::new(),
            warnings: Vec::new(),
            eof: false
        }
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1 - span.0);

        self.errors.push(
            error::ParserError {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        );
    }

    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1 - span.0);

        self.warnings.push(
            error::ParserWarning {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        )
    }

    fn get_basic_type(&mut self, datatype: String, span: (usize, usize)) -> Type {
        match datatype.as_str() {
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,

            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,

            "string" => Type::String,
            "char" => Type::Char,

            "bool" => Type::Bool,
            "void" => Type::Void,

            _ => {
                self.error(
                    String::from("Unable to parse datatype"),
                    span
                );
                Type::Void
            }
        }
    }
    
    // fundamental
    
    fn next(&mut self) -> Token {
        self.position += 1;

        if self.position < self.tokens.len() {
            self.tokens[self.position].clone()
        } else {
            Token::new(String::from(""), TokenType::EOF, (0, 1))
        }
    }

    fn current(&self) -> Token {
        self.tokens[self.position].clone()
    }

    fn expect(&self, expected: TokenType) -> bool {
        self.current().token_type == expected
    }

    fn skip_eos(&mut self) {
        if self.current().token_type == END_STATEMENT {
            let _ = self.next();
        }
    }
}

impl Parser {
    fn term(&mut self) -> Expressions {
        let current = self.current();
        let _ = self.next();

        match current.token_type {
            TokenType::Number => Expressions::Value(Value::Integer(current.value.trim().parse().unwrap()), current.span),
            TokenType::String => Expressions::Value(Value::String(current.value), current.span),
            TokenType::Char => Expressions::Value(Value::Char(current.value.chars().nth(0).unwrap()), current.span),
            TokenType::Boolean => Expressions::Value(Value::Boolean(current.value == "true"), current.span),
            TokenType::Keyword => Expressions::Value(Value::Keyword(current.value), current.span),

            TokenType::Identifier => {
                let output = Expressions::Value(Value::Identifier(current.value), current.span);

                match self.next().token_type {
                    TokenType::LParen => {
                        todo!()
                    },
                    TokenType::LBrack => {
                        todo!()
                    }
                    _ => {}
                }

                output
            }
            TokenType::Type => {
                let datatype = self.get_basic_type(current.value, current.span);
                let identifier = self.current();

                if !self.expect(TokenType::Identifier) {
                    return Expressions::Value(Value::Type(datatype), current.span);
                }

                let _ = self.next();

                Expressions::Argument {
                    name: identifier.value,
                    r#type: datatype,
                    span: current.span
                }
            }
            TokenType::LBrack => {
                todo!()
            }

            _ => {
                self.error(
                    String::from("Undefined term found"),
                    current.span
                );
                Expressions::None
            }
        }
    }

    fn expression(&mut self) -> Expressions {
        let mut node = self.term();
        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                todo!()
            }
            tty if BOOLEAN_OPERATORS.contains(&tty) => {
                todo!()
            }
            tty if BITWISE_OPERATORS.contains(&tty) => {
                todo!()
            }

            TokenType::LBrack => {
                let span = current.span;
                let _ = self.next();

                if self.expect(TokenType::RBrack) {
                    return node;
                }

                let slice_index = self.expression();

                if !self.expect(TokenType::RBrack) {
                    self.error(
                        String::from("Unclosed brackets in expression found"),
                        span
                    );
                    return Expressions::None;
                }

                let span = (span.0, self.current().span.1);

                let _ = self.next();

                return Expressions::Slice {
                    object: Box::new(node),
                    index: Box::new(slice_index),
                    span
                }
            }

            END_STATEMENT => {
                self.next();
                node
            }

            _ => node
        }
    }

    fn statement(&mut self) -> Statements {
        let current = self.current();

        match current.token_type {
            TokenType::Keyword => {
                match current.value.as_str() {
                    "let" => self.annotation_statement(),
                    "import" => self.import_statement(),
                    "if" => self.if_statement(),
                    "else" => {
                        self.error(
                            String::from("Unexpected `else` outside construction usage"),
                            current.span
                        );
                        Statements::None
                    }

                    "while" => self.while_statement(),
                    "for" => self.for_statement(),
                    
                    "fn" => self.fn_statement(),
                    "return" => self.return_statement(),
                    "break" => self.break_statement(),
                    _ => unreachable!()
                }
            },
            TokenType::Multiply => {
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Identifier => {
                        let span_start = self.current().span.0;
                        let stmt = self.statement();
                        let span_end = self.current().span.1;

                        match stmt {
                            Statements::AssignStatement { identifier, value, span } => {
                                Statements::DerefAssignStatement {
                                    identifier,
                                    value,
                                    span
                                }
                            }
                            _ => {
                                self.error(
                                    String::from("Unsupported for dereference statement found"),
                                    (span_start, span_end)
                                );
                                Statements::None
                            }
                        }
                    }
                    _ => {
                        self.error(
                            String::from("Unsupported for dereference statement found"),
                            self.current().span
                        );
                        Statements::None
                    }
                    TokenType::Identifier => {
                        let next = self.next();
                        match next.token_type {
                            TokenType::Equal => self.assign_statement(current.value),
                            TokenType::Dot => {
                                let _ = self.next();
                                let sub_expr = self.subelement_expression(
                                    Expressions::Value(Value::Identifier(current.value), self.current().span),
                                    TokenType::Dot
                                );

                                Statements::Expression(sub_expr)
                            }
                            TokenType::LParen => self.call_statement(current.value),
                            TokenType::LBrack => self.slice_assign_statement(current.value),

                            tty if BINARY_OPERATORS.contains(&tty) => {
                                match self.next().token_type {
                                    TokenType::Equal => {
                                        self.binary_assign_statement(current.value, next.value)
                                    }
                                    TokenType::Plus | TokenType::Minus => {
                                        let span_start = next.span.0;
                                        let span_end = self.current().span.1;
                                        let (op1, op2) = (next.value, self.current().value);

                                        if op1 != op2 {
                                            self.error(
                                                String::from("Unexpected variation of increment/decrement found!"),
                                                (span_start, span_end)
                                            );
                                            return Statements::None;
                                        }

                                        let _ = self.next();
                                        let _ = self.skip_eos();

                                        Statements::BinaryAssignStatement {
                                            identifier: current.value,
                                            operand: op1,
                                            value: Expressions::Value(Value::Integer(1), (current.span.0, span_end)),
                                            span: (current.span.0, span_end)
                                        }
                                    }
                                    _ => {
                                        self.error(
                                            String::from("Unexpected binary operation in statement found"),
                                            (current.span.0, self.current().span.1)
                                        );
                                        return Statements::None;
                                    }
                                }
                            }
                            END_STATEMENT => {
                                Statements::Expression(Expressions::Value(Value::Identifier(current.value), current.span))
                            }
                            _ => {
                                self.error(
                                    String::from("Unexpected expression/statement found after identifier"),
                                    (current.span.0, next.span.1)
                                );
                                Statements::None
                            }
                        }
                    }
                    TokenType::EOF => {
                        self.eof = true;
                        Statements::None
                    }
                    _ => Statements::Expression(self.expression())
                }
            }
            _ => Statements::Expression(self.expression())
        }
    }
}
