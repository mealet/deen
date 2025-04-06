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
            let _ = self.next();
            datatype = Some(self.parse_type());
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
    
    pub fn import_statement(&mut self) -> Statements {
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let span_start = self.current().span.0;
        let path = self.expression();

        let span_end = self.current().span.1;
        let _ = self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Statements::ImportStatement { path, span: (span_start, span_end) }
        } else {
            self.error(
                String::from("Unexpected import syntax found"),
                (span_start, span_end)
            );
            Statements::None
        }
    }

    pub fn if_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;       
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let condition = self.expression();

        if self.current().token_type != TokenType::LBrace {
            self.error(
                String::from("Expected new block after condition"),
                (span_start, self.current().span.1)
            );
            return Statements::None;
        }

        let _ = self.next();
        let span_block_start = self.current().span.0;
        let mut then_block = Vec::new();

        while self.current().token_type != TokenType::RBrace {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1)
                );
                return Statements::None;
            }

            then_block.push(self.statement())
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        match self.current().token_type {
            TokenType::Keyword => {
                if self.current().value != "else" {
                    let span_end = self.current().span.1;
                    self.skip_eos();
                    return Statements::IfStatement {
                        condition,
                        then_block,
                        else_block: None,
                        span: (span_start, span_end)
                    };
                }

                let else_span_start = self.current().span.0;
                let _ = self.next();

                if !self.expect(TokenType::LBrace) {
                    self.error(
                        String::from("New block expected after `else` keyword"),
                        (else_span_start, self.current().span.1)
                    );
                    return Statements::None
                }
                
                let _ = self.next();

                let mut else_block = Vec::new();

                while self.current().token_type != TokenType::RBrace {
                    if self.current().token_type == TokenType::EOF {
                        self.error(
                            String::from("Expected block end, but found EOF"),
                            (else_span_start, self.current().span.1)
                        );
                        return Statements::None;
                    }

                    else_block.push(self.statement());
                }

                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                let span_end = self.current().span.1;
                let _ = self.skip_eos();
                Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: Some(else_block),
                    span: (span_start, span_end)
                }
            },
            _ => {
                let span_end = self.current().span.1;
                self.skip_eos();
                Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: None,
                    span: (span_start, span_end)
                }
            }
        }
    }

    pub fn while_statement(&mut self) -> Statements {
        todo!()
    }

    pub fn for_statement(&mut self) -> Statements {
        todo!()
    }

    pub fn fn_statement(&mut self) -> Statements {
        todo!()
    }

    pub fn return_statement(&mut self) -> Statements {
        todo!()
    }

    pub fn break_statement(&mut self) -> Statements {
        todo!()
    }

    pub fn assign_statement(&mut self, id: String) -> Statements {
        todo!()
    }

    pub fn binary_assign_statement(&mut self, id: String, op: String) -> Statements {
        todo!()
    }

    pub fn slice_assign_statement(&mut self, id: String) -> Statements {
        todo!()
    }

    pub fn call_statement(&mut self, id: String) -> Statements {
        todo!()
    }
}
