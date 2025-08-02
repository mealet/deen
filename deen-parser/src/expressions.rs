//! # Expressions
//! Each expression in Deen has its own syntax (such as Statements). <br/>
//! _To see syntax rules for every Expression, check the [`Expressions`] enum_
//!
//! **Expression** is a syntactic entity in programming language that combines types and values
//! into single instance. <br/>
//! Expressions can be components of [`Statements`]. <br/>
//! Read: <https://en.wikipedia.org/wiki/Expression_(computer_science)>

use crate::{
    BINARY_OPERATORS, BITWISE_OPERATORS, BOOLEAN_OPERATORS, PRIORITY_BINARY_OPERATORS,
    PRIORITY_BOOLEAN_OPERATORS, Parser,
    error::{self, ParserError},
    statements::Statements,
    types::Type,
    value::Value,
};
use deen_lexer::token_type::TokenType;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions {
    /// `OBJECT BINOP EXPRESSION`
    Binary {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },
    /// `UNOP OBJECT`
    Unary {
        operand: String,
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `OBJECT BOOLOP EXPRESSION`
    Boolean {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },
    /// `OBJECT BITOP EXPRESSION`
    Bitwise {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize),
    },

    /// `IDENTIFIER: TYPE`
    Argument {
        name: String,
        r#type: Type,
        span: (usize, usize),
    },
    /// `OBJECT.SUBELEMENT_1.SUBELEMENT_2`
    SubElement {
        head: Box<Expressions>,
        subelements: Vec<Expressions>,
        span: (usize, usize),
    },

    /// `IDENTIFIER ( EXPRESSION, EXPRESSION, ... )`
    FnCall {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },
    /// `IDENTIFIER! ( EXPRESSION, EXPRESSION, ... )`
    MacroCall {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    /// `&EXPRESSION`
    Reference {
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `*EXPRESSION`
    Dereference {
        object: Box<Expressions>,
        span: (usize, usize),
    },

    /// `[EXPRESSION, EXPRESSION, ...]`
    Array {
        values: Vec<Expressions>,
        len: usize,
        span: (usize, usize),
    },
    /// `(EXPRESSION, EXPRESSION, ...)`
    Tuple {
        values: Vec<Expressions>,
        span: (usize, usize),
    },
    /// `OBJECT[EXPRESSION]`
    Slice {
        object: Box<Expressions>,
        index: Box<Expressions>,
        span: (usize, usize),
    },
    /// `IDENTIFIER { .IDENTIFIER = EXPRESSION, .IDENTIFIER = EXPRESSION }`
    Struct {
        name: String,
        fields: HashMap<String, Expressions>,
        span: (usize, usize),
    },
    /// `{ STATEMENTS }`
    Scope {
        block: Vec<Statements>,
        span: (usize, usize),
    },

    Value(Value, (usize, usize)),
    None,
}

impl Parser {
    #[inline]
    pub fn get_span_expression(expr: &Expressions) -> (usize, usize) {
        match expr {
            Expressions::Binary { span, .. } => *span,
            Expressions::Boolean { span, .. } => *span,
            Expressions::Bitwise { span, .. } => *span,
            Expressions::Argument { span, .. } => *span,
            Expressions::SubElement { span, .. } => *span,
            Expressions::FnCall { span, .. } => *span,
            Expressions::MacroCall { span, .. } => *span,
            Expressions::Reference { span, .. } => *span,
            Expressions::Dereference { span, .. } => *span,
            Expressions::Array { span, .. } => *span,
            Expressions::Tuple { span, .. } => *span,
            Expressions::Slice { span, .. } => *span,
            Expressions::Struct { span, .. } => *span,
            Expressions::Unary { span, .. } => *span,
            Expressions::Scope { span, .. } => *span,
            Expressions::Value(_, span) => *span,
            Expressions::None => (0, 0),
        }
    }

    #[inline]
    pub fn span_expression(&self, expr: Expressions) -> (usize, usize) {
        Self::get_span_expression(&expr)
    }
}

impl Parser {
    pub fn subelement_expression(
        &mut self,
        head: Expressions,
        separator: TokenType,
    ) -> Expressions {
        // if self.expect(separator) {
        //     let _ = self.next();
        // }

        let head = Box::new(head);
        let span_start = self.current().span.1;
        let mut subelements = Vec::new();
        let mut span_end = self.current().span.1;

        while self.expect(separator.clone()) {
            if self.expect(separator.clone()) {
                let _ = self.next();
            }
            subelements.push(self.term());
            span_end = self.current().span.1;
        }

        Expressions::SubElement {
            head,
            subelements,
            span: (span_start, span_end),
        }
    }

    pub fn binary_expression(&mut self, node: Expressions) -> Expressions {
        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression();

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

                if PRIORITY_BINARY_OPERATORS.contains(&tty) {
                    let new_node = rhs.clone();
                    let old_lhs = lhs.clone();

                    if let Expressions::Binary {
                        operand,
                        lhs,
                        rhs,
                        span,
                    } = new_node
                    {
                        let lhs_new = Box::new(old_lhs);
                        let rhs_new = lhs;

                        let priority_node = Expressions::Binary {
                            operand: current.value,
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span,
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: Box::new(priority_node),
                            rhs,
                            span: (node_span.0, span_end),
                        };
                    }
                }

                Expressions::Binary {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn boolean_expression(&mut self, node: Expressions) -> Expressions {
        // FIXME: Expressions like `true || false` returns error "Undefined term found"

        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            op if PRIORITY_BOOLEAN_OPERATORS.contains(&op) => node,
            op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression();

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

                if PRIORITY_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().value;
                    let lhs_node = Expressions::Boolean {
                        operand: current.value,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span: (current.span.0, self.current().span.1),
                    };

                    let _ = self.next();
                    let rhs_node = self.expression();

                    return Expressions::Boolean {
                        operand,
                        lhs: Box::new(lhs_node),
                        rhs: Box::new(rhs_node),
                        span: (node_span.0, span_end),
                    };
                }

                Expressions::Boolean {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions) -> Expressions {
        let node_span = Self::get_span_expression(&node);
        let span_end;

        let current = self.current();

        match current.token_type {
            tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = Box::new(node);
                let rhs = Box::new(self.expression());

                self.position -= 2;
                span_end = self.current().span.1;
                self.position += 2;

                Expressions::Bitwise {
                    operand: current.value,
                    lhs,
                    rhs,
                    span: (node_span.0, span_end),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn call_expression(&mut self, fname: String, span: (usize, usize)) -> Expressions {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                self.error(ParserError::UnknownExpression {
                    exception: "unknown call expression syntax".to_string(),
                    help: "Consider using right syntax: `identifier(value, ...)".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span((span.0, self.current().span.1)),
                });

                return Expressions::None;
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        Expressions::FnCall {
            name: fname,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_expression(&mut self, name: String, span: (usize, usize)) -> Expressions {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        Expressions::MacroCall {
            name,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn slice_expression(&mut self, expr: Expressions) -> Expressions {
        if let TokenType::LBrack = self.current().token_type {
            let _ = self.next();
        }

        let object = Box::new(expr.clone());
        let index = Box::new(self.expression());

        if self.current().token_type != TokenType::RBrack {
            self.error(ParserError::UnclosedExpression {
                exception: "unclosed slice index".to_string(),
                help: "Close slice index with brackets".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((
                    self.span_expression(expr).0,
                    self.current().span.1,
                )),
            });

            return Expressions::None;
        }

        let _ = self.next();
        let span_end = self.current().span.1;
        Expressions::Slice {
            object,
            index,
            span: (self.span_expression(expr).0, span_end),
        }
    }

    pub fn struct_expression(&mut self, name: String) -> Expressions {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Identifier) {
            let _ = self.next();
        }

        if !self.expect(TokenType::LBrace) {
            self.error(ParserError::SyntaxError {
                exception: "expected curly brackets for structure initialization".to_string(),
                help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                src: self.source.clone(),
                span: error::position_to_span((span_start, self.current().span.1)),
            });

            self.skip_statement();
            return Expressions::None;
        };

        let _ = self.next();
        let mut fields = HashMap::new();

        while !self.expect(TokenType::RBrace) {
            if !self.expect(TokenType::Dot) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let _ = self.next();
            if !self.expect(TokenType::Identifier) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let id = self.current().value;
            let _ = self.next();

            if !self.expect(TokenType::Equal) {
                self.error(ParserError::SyntaxError {
                    exception: "wrong field initialization syntax".to_string(),
                    help: "Use syntax: `identifier { .field = value, ... }`".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            let _ = self.next();
            let value = self.expression();

            if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                self.error(ParserError::SyntaxError {
                    exception: "values must be separated by commas".to_string(),
                    help: "Separate fields with commas".to_string(),
                    src: self.source.clone(),
                    span: error::position_to_span(self.current().span),
                });

                while !self.expect(TokenType::RBrace)
                    && !self.expect(TokenType::Semicolon)
                    && !self.expect(TokenType::EOF)
                {
                    let _ = self.next();
                }
                break;
            }

            if !self.expect(TokenType::RBrace) {
                let _ = self.next();
            }
            fields.insert(id, value);
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        let span = (span_start, self.current().span.1);
        Expressions::Struct { name, fields, span }
    }

    pub fn expressions_enum(
        &mut self,
        start: TokenType,
        end: TokenType,
        separator: TokenType,
    ) -> Vec<Expressions> {
        let mut current = self.current();

        if self.expect(start) {
            current = self.next()
        } else if self.expect(end.clone()) {
            let _ = self.next();
            return Vec::new();
        }

        let mut output = Vec::new();

        while current.token_type != end {
            current = self.current();

            if current.token_type == separator {
                let _ = self.next();
            } else if current.token_type == end {
                break;
            } else {
                output.push(self.expression());
            }
        }

        if self.expect(end) {
            let _ = self.next();
        }

        output
    }
}
