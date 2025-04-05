use crate::{
    error::{ParserError, ParserWarning},
    expressions::Expressions,
    value::Value,
    types::Type,
    END_STATEMENT,
    Parser
};
use deen_lexer::{
    token::Token,
    token_type::TokenType
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statements {
    AssignStatement {
        identifier: String,
        value: Expressions,
        span: (usize, usize)
    },
    BinaryAssignStatement {
        identifier: String,
        operand: String,
        value: Expressions,
        span: (usize, usize)
    },
    DerefAssignStatement {
        identifier: String,
        value: Expressions,
        span: (usize, usize)
    },
    SliceAssignStatement {
        identifier: String,
        index: Expressions,
        value: Expressions,
        span: (usize, usize)
    },

    AnnotationStatement {
        identifier: String,
        datatype: Option<Type>,
        value: Option<Expressions>,
        span: (usize, usize)
    },

    FunctionDefineStatement {
        name: String,
        datatype: Type,
        arguments: Vec<(String, Type)>,
        block: Vec<Statements>,
        span: (usize, usize)
    },
    FunctionCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize)
    },

    IfStatement {
        condition: Expressions,
        then_block: Vec<Statements>,
        else_block: Option<Vec<Statements>>,
        span: (usize, usize)
    },
    WhileStatement {
        condition: Expressions,
        block: Vec<Statements>,
        span: (usize, usize)
    },
    ForStatement {
        initializer: Box<Statements>,
        condition: Expressions,
        iterator: Box<Statements>,
        block: Vec<Statements>,
        span: (usize, usize)
    },

    ImportStatement {
        path: Expressions,
        span: (usize, usize)
    },

    BreakStatements {
        span: (usize, usize)
    },
    ReturnStatement {
        value: Expressions,
        span: (usize, usize)
    },

    Expression(Expressions),
    None
}

impl Parser {
    pub fn annotation_statement(&mut self) -> Statements {
        // TODO: Add structures/arrays/tuples types parsing

        let span_start = self.current().span.0;
        
        if self.current().value == String::from("let") {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(
                String::from("Identifier expected after `let` keyword"),
                (span_start, self.current().span.1 - span_start)
            );
            return Statements::None;
        }

        let id = self.current().value;
        let mut datatype = None;

        if self.next().token_type == TokenType::DoubleDots {
            let str_type = self.next();

            if str_type.token_type != TokenType::Type {
                self.error(
                    String::from("Undefined type in annotation found"),
                    str_type.span
                );
                return Statements::None;
            }

            datatype = Some(self.get_basic_type(str_type.value, str_type.span));
            let _ = self.next();
        }

        match self.current().token_type {
            TokenType::Equal => {
                let _ = self.next();
                let value = self.expression();

                let span_end = self.current().span.1;

                self.skip_eos();
                Statements::AnnotationStatement { identifier: id, datatype, value: Some(value), span: (span_start, span_end) }
            },
            END_STATEMENT => {
                let span_end = self.current().span.1;
                self.skip_eos();

                Statements::AnnotationStatement { identifier: id, datatype, value: None, span: (span_start, span_end) }
            }
            _ => {
                self.error(
                    String::from("Expected `=` or `;` after variable declaration"),
                    (span_start, self.current().span.1 - span_start)
                );
                return Statements::None;
            }
        }
    }
}
