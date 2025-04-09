use crate::{
    value::Value,
    types::Type,
    Parser,

    BINARY_OPERATORS,
    BOOLEAN_OPERATORS,
    BITWISE_OPERATORS,
    PRIORITY_BINARY_OPERATORS,
    PRIORITY_BOOLEAN_OPERATORS
};
use deen_lexer::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum Expressions {
    Binary {
        operand: String,
        lhs: Box<Expressions>,
        rhs: Box<Expressions>,
        span: (usize, usize)
    },
    Unary {
        operand: String,
        object: Box<Expressions>,
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
    #[inline]
    pub fn span_expression(&self, expr: Expressions) -> (usize, usize) {
        match expr {
            Expressions::Binary { operand: _, lhs: _, rhs: _, span } => span,
            Expressions::Boolean { operand: _, lhs: _, rhs: _, span } => span,
            Expressions::Bitwise { operand: _, lhs: _, rhs: _, span } => span,
            Expressions::Argument { name: _, r#type: _, span } => span,
            Expressions::SubElement { parent: _, child: _, span } => span,
            Expressions::FnCall { name: _, arguments: _, span } => span,
            Expressions::Reference { object: _, span } => span,
            Expressions::Dereference { object: _, span } => span,
            Expressions::Array { values: _, len: _, span } => span,
            Expressions::Slice { object: _, index: _, span } => span,
            Expressions::Value(_, span) => span,
            Expressions::Unary { operand: _, object: _, span } => span,

            Expressions::None => (0, 0)
        }
    }
}

impl Parser {
    pub fn subelement_expression(&mut self, parent: Expressions, separator: TokenType) -> Expressions {
        if self.expect(separator) {
            let _ = self.next();
        }

        let span_start = self.current().span.1;
        let child = self.expression();
        let span_end = self.span_expression(child.clone()).1;

        let span = (span_start, span_end);
        Expressions::SubElement {
            parent: Box::new(parent),
            child: Box::new(child),
            span,
        }
    }

    pub fn binary_expression(&mut self, node: Expressions) -> Expressions {
        let current = self.current();
        
        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => {
                let _ = self.next();

                let lhs = node;
                let rhs = self.expression();

                if PRIORITY_BINARY_OPERATORS.contains(&tty) {
                    let new_node = rhs.clone();
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

                Expressions::Binary {
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

    pub fn call_expression(&mut self, fname: String, span: (usize, usize)) -> Expressions {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            },
            TokenType::LParen => {},
            _ => {
                self.error(
                    String::from("Unexpected variation of function call"),
                    (span.0, self.current().span.1)
                );
                return Expressions::None;
            }
        };

        let arguments = self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);
        let span_end = self.current().span.1;
        self.skip_eos();

        Expressions::FnCall { name: fname, arguments, span: (span.0, span_end) }
    }

    pub fn slice_expression(&mut self, expr: Expressions) -> Expressions {
        if let TokenType::LBrack = self.current().token_type {
            let _ = self.next();
        }

        let object = Box::new(expr.clone());
        let index = Box::new(self.expression());
        let span_end = self.current().span.1;

        if self.current().token_type != TokenType::RBrack {
            self.error(
                String::from(""),
                (self.span_expression(expr).0, span_end)
            );
            return Expressions::None;
        }

        let _ = self.next();
        Expressions::Slice { object, index, span: (self.span_expression(expr).0, span_end)}
    }

    #[allow(unused)]
    pub fn expressions_enum(&mut self, start: TokenType, end: TokenType, separator: TokenType) -> Vec<Expressions> {
        let mut current = self.current();

        match current.token_type {
            start => current = self.next(),
            end => {
                self.error(
                    String::from("Unexpected enumeration end found"),
                    current.span
                );
                return Vec::new();
            }
        }

        let mut output = Vec::new();

        while current.token_type != end {
            current = self.current();

            if current.token_type == separator {
                let _ = self.next();
            } else if current.token_type == end {
                break
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
