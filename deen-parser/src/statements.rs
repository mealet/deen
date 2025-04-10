use crate::{
    expressions::Expressions,
    value::Value,
    types::Type,
    END_STATEMENT,
    Parser
};
use deen_lexer::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
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
        binding: Expressions,
        iterator: Expressions,
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
    ScopeStatement {
        block: Vec<Statements>,
        span: (usize, usize)
    },

    Expression(Expressions),
    None
}

impl Parser {
    pub fn annotation_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        
        if self.current().value == *"let" {
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

                self.skip_eos();
                Statements::AnnotationStatement { identifier: id, datatype, value: Some(value.clone()), span: (span_start - 1, self.span_expression(value).1) }
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
                Statements::None
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
        self.skip_eos();

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
        let mut span_block_start = self.current().span.0;
        let mut then_block = Vec::new();

        while self.current().token_type != TokenType::RBrace {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1)
                );
                return Statements::None;
            }

            then_block.push(self.statement());
            span_block_start = self.current().span.0;
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

                let mut else_span_start = self.current().span.0;
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
                else_span_start = self.current().span.0;

                while self.current().token_type != TokenType::RBrace {
                    if self.current().token_type == TokenType::EOF {
                        self.error(
                            String::from("Expected block end, but found EOF"),
                            (else_span_start, self.current().span.1)
                        );
                        return Statements::None;
                    }

                    else_block.push(self.statement());
                    else_span_start = self.current().span.0;
                }

                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                let span_end = self.current().span.1;
                self.skip_eos();
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
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
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
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1)
                );
                return Statements::None;
            }

            block.push(self.statement());
            span_block_start = self.current().span.0;
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::WhileStatement { condition, block, span: (span_start, self.current().span.1) }
    }

    pub fn for_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }
        let binding = self.term();

        if !self.expect(TokenType::Equal) {
            self.error(
                String::from("Expected binding for iterator in loop: \"for binding : iterator {}\""),
                (span_start, self.current().span.1)
            );
            return Statements::None;
        }

        let _ = self.next();
        let iterator = self.expression();

        if self.current().token_type != TokenType::LBrace {
            self.error(
                String::from("Expected new block after condition"),
                (span_start, self.current().span.1)
            );
            return Statements::None;
        }
    
        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1)
                );
                return Statements::None;
            }

            block.push(self.statement());
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::ForStatement { binding, iterator, block, span: (span_start, span_end) }
    }

    pub fn fn_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let identifier = self.current().value;
        
        let _ = self.next();
        let arguments = self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        let arguments_tuples = arguments.iter().map(|arg| {
            if let Expressions::Argument { name, r#type, span: _ } = arg {
                (name.clone(), r#type.clone())
            } else {
                self.error(
                    String::from("Unexpected argument declaration found"),
                    self.span_expression(arg.clone())
                );
                (String::new(), Type::Void)
            }
        }).collect();
        
        let mut datatype = Type::Void;
        if !self.expect(TokenType::LBrace) {
            datatype = self.parse_type();
        }

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1)
                );
                return Statements::None;
            }

            block.push(self.statement());
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.0;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::FunctionDefineStatement { name: identifier, datatype, arguments: arguments_tuples, block, span: (span_start, span_end) }
    }

    pub fn return_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let return_expr = self.expression();
        Statements::ReturnStatement { value: return_expr.clone(), span: (span_start, self.span_expression(return_expr).1) }
    }

    pub fn break_statement(&mut self) -> Statements {
        let span = self.current().span;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        self.skip_eos();

        Statements::BreakStatements { span }
    }

    pub fn assign_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::AssignStatement { identifier: id, value, span: (span.0, span_end) }
    }

    pub fn binary_assign_statement(&mut self, id: String, op: String, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::BinaryAssignStatement { operand: op, identifier: id, value, span: (span.0, span_end) }
    }

    pub fn slice_assign_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        let brackets_span_start = self.current().span.0;
        if self.expect(TokenType::LBrack) {
            let _ = self.next();
        }

        let ind =  self.expression();
        let brackets_span_end = self.current().span.1;

        if !self.expect(TokenType::RBrack) {
            self.error(
                String::from("Brackets close expected in slice"),
                (brackets_span_start, brackets_span_end)
            );
            return Statements::None;
        }

        let _ = self.next();
        if !self.expect(TokenType::Equal) {
            self.error(
                String::from("Expected `=` in slice assign"),
                (span.0, self.current().span.1)
            );
            return Statements::None;
        }

        let _ = self.next();
        let val = self.expression();
        self.skip_eos();

        let span_end = self.current().span.1;

        Statements::SliceAssignStatement { identifier: id, index: ind, value: val, span: (span.0, span_end) }
    }

    pub fn call_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {},
            _ => {
                self.error(
                    String::from("Unexpected variation of call statement"),
                    (span.0, self.current().span.1)
                );
                return Statements::None;
            }
        };

        let arguments = self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::FunctionCallStatement { name: id, arguments, span: (span.0, span_end) }
    }
}
