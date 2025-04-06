use crate::{
    error::{ParserError, ParserWarning},
    statements::Statements,
    value::Value,
    types::Type,
    Parser,

    BINARY_OPERATORS,
    BOOLEAN_OPERATORS,
    BITWISE_OPERATORS,
    PRIORITY_BINARY_OPERATORS,
    PRIORITY_BOOLEAN_OPERATORS
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

    pub fn binary_expression(&mut self, node: Expressions) -> Expressions {
        let current = self.current();
        
        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression();

                if PRIORITY_BINARY_OPERATORS.contains(&tty) {
                    let mut new_node = rhs.clone();
                    let old_lhs = lhs.clone();

                    if let Expressions::Binary { operand, lhs, rhs, span } = new_node {
                        let lhs_new = Box::new(old_lhs);
                        let rhs_new = lhs;

                        let priority_node = Expressions::Binary {
                            operand: current.value,
                            lhs: lhs_new,
                            rhs: rhs_new,
                            span
                        };

                        return Expressions::Binary {
                            operand,
                            lhs: Box::new(priority_node),
                            rhs,
                            span
                        }
                    }
                }

                return Expressions::Binary {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (current.span.0, self.current().span.1)
                }
            },
            _ => unreachable!()
        }
    }

    pub fn boolean_expression(&mut self, node: Expressions) -> Expressions {
        // FIXME: Expressions like `true || false` returns error "Undefined term found"

        let current = self.current();

        match current.token_type {
            op if PRIORITY_BOOLEAN_OPERATORS.contains(&op) => node,
            op if BOOLEAN_OPERATORS.contains(&op) => {
                let _ = self.next();

                let lhs = node.clone();
                let rhs = self.expression();

                if PRIORITY_BOOLEAN_OPERATORS.contains(&self.current().token_type) {
                    let operand = self.current().value;
                    let lhs_node = Expressions::Boolean {
                        operand: current.value,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        span: (current.span.0, self.current().span.1)
                    };

                    let _ = self.next();
                    let rhs_node = self.expression();

                    return Expressions::Boolean { operand, lhs: Box::new(lhs_node), rhs: Box::new(rhs_node), span: (current.span.0, self.current().span.1) }
                }

                Expressions::Boolean {
                    operand: current.value,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span: (current.span.0, self.current().span.1)
                }
            }
            _ => unreachable!()
        }
    }

    pub fn bitwise_expression(&mut self, node: Expressions) -> Expressions {
        let current = self.current();

        match current.token_type {
            tty if BITWISE_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = Box::new(node);
                let rhs = Box::new(self.expression());

                Expressions::Bitwise {
                    operand: current.value,
                    lhs,
                    rhs,
                    span: (current.span.0, self.current().span.1)
                }
            }
            _ => unreachable!()
        }
    }

    pub fn call_expression(&mut self, fname: String) -> Expressions {
        todo!()
    }

    pub fn slice_expression(&mut self, expr: Expressions) -> Expressions {
        todo!()
    }

    pub fn expressions_enum(&mut self, start: TokenType, end: TokenType, separator: TokenType) -> Vec<Expressions> {
        todo!()
    }
}
