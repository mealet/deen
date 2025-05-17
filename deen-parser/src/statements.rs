use crate::{END_STATEMENT, Parser, expressions::Expressions, types::Type, value::Value};
use deen_lexer::token_type::TokenType;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Statements {
    AssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },
    BinaryAssignStatement {
        object: Expressions,
        operand: String,
        value: Expressions,
        span: (usize, usize),
    },
    DerefAssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },
    SliceAssignStatement {
        object: Expressions,
        index: Expressions,
        value: Expressions,
        span: (usize, usize),
    },
    FieldAssignStatement {
        object: Expressions,
        value: Expressions,
        span: (usize, usize),
    },

    AnnotationStatement {
        identifier: String,
        datatype: Option<Type>,
        value: Option<Expressions>,
        span: (usize, usize),
    },

    FunctionDefineStatement {
        name: String,
        datatype: Type,
        arguments: Vec<(String, Type)>,
        block: Vec<Statements>,
        public: bool,
        span: (usize, usize),
        header_span: (usize, usize),
    },
    FunctionCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    MacroCallStatement {
        name: String,
        arguments: Vec<Expressions>,
        span: (usize, usize),
    },

    StructDefineStatement {
        name: String,
        fields: HashMap<String, Type>,
        functions: HashMap<String, Statements>,
        public: bool,
        span: (usize, usize),
    },

    EnumDefineStatement {
        name: String,
        fields: Vec<String>,
        functions: HashMap<String, Statements>,
        public: bool,
        span: (usize, usize),
    },
    TypedefStatement {
        alias: String,
        datatype: Type,
        span: (usize, usize),
    },

    IfStatement {
        condition: Expressions,
        then_block: Vec<Statements>,
        else_block: Option<Vec<Statements>>,
        span: (usize, usize),
    },
    WhileStatement {
        condition: Expressions,
        block: Vec<Statements>,
        span: (usize, usize),
    },
    ForStatement {
        binding: String,
        iterator: Expressions,
        block: Vec<Statements>,
        span: (usize, usize),
    },

    ImportStatement {
        path: Expressions,
        span: (usize, usize),
    },

    BreakStatements {
        span: (usize, usize),
    },
    ReturnStatement {
        value: Expressions,
        span: (usize, usize),
    },
    ScopeStatement {
        block: Vec<Statements>,
        span: (usize, usize),
    },

    Expression(Expressions),
    None,
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
                (span_start, self.current().span.1 - span_start),
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

                Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: Some(value.clone()),
                    span: (span_start, self.span_expression(value).1),
                }
            }
            END_STATEMENT => {
                let span_end = self.current().span.1;
                self.skip_eos();

                Statements::AnnotationStatement {
                    identifier: id,
                    datatype,
                    value: None,
                    span: (span_start, span_end),
                }
            }
            _ => {
                self.error(
                    String::from("Expected `=` or `;` after variable declaration"),
                    (span_start, self.current().span.1 - span_start),
                );
                Statements::None
            }
        }
    }

    pub fn import_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.current().token_type == TokenType::Keyword {
            let _ = self.next();
        }

        let path = self.expression();
        self.position -= 1;

        let span_end = self.current().span.1;
        let _ = self.next();
        self.skip_eos();

        if let Expressions::Value(Value::String(_), _) = path {
            Statements::ImportStatement {
                path,
                span: (span_start, span_end),
            }
        } else {
            self.error(
                String::from("Unexpected import syntax found"),
                (span_start, span_end),
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
                (span_start, self.current().span.1),
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
                    (span_block_start, self.current().span.1),
                );
                return Statements::None;
            }

            then_block.push(self.statement());
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        match self.current().token_type {
            TokenType::Keyword => {
                if self.current().value != "else" {
                    self.skip_eos();
                    return Statements::IfStatement {
                        condition,
                        then_block,
                        else_block: None,
                        span: (span_start, span_end),
                    };
                }

                let mut else_span_start = self.current().span.0;
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Keyword => {
                        if self.current().value != *"if" {
                            self.error(
                                String::from("Unexpected keyword found after `else`"),
                                (else_span_start, self.current().span.1),
                            );
                            return Statements::None;
                        }

                        let stmt = self.if_statement();
                        let span_end = self.current().span.1;
                        return Statements::IfStatement {
                            condition,
                            then_block,
                            else_block: Some(vec![stmt]),
                            span: (span_start, span_end),
                        };
                    }
                    TokenType::LBrace => {}
                    _ => {
                        self.error(
                            String::from("New block expected after `else` keyword"),
                            (else_span_start, self.current().span.1),
                        );
                        return Statements::None;
                    }
                }

                let _ = self.next();

                let mut else_block = Vec::new();
                else_span_start = self.current().span.0;

                while self.current().token_type != TokenType::RBrace {
                    if self.current().token_type == TokenType::EOF {
                        self.error(
                            String::from("Expected block end, but found EOF"),
                            (else_span_start, self.current().span.1),
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
                    span: (span_start, span_end),
                }
            }
            _ => {
                let span_end = self.current().span.1;
                self.skip_eos();
                Statements::IfStatement {
                    condition,
                    then_block,
                    else_block: None,
                    span: (span_start, span_end),
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
                (span_start, self.current().span.1),
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
                    (span_block_start, self.current().span.1),
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
        Statements::WhileStatement {
            condition,
            block,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn for_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }
        let binding = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::Equal) {
            self.error(
                String::from(
                    "Expected binding for iterator in loop: \"for binding = iterator {}\"",
                ),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }

        let _ = self.next();
        let iterator = self.expression();

        if self.current().token_type != TokenType::LBrace {
            self.error(
                String::from("Expected new block after condition"),
                (span_start, self.current().span.1),
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
                    (span_block_start, self.current().span.1),
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
        Statements::ForStatement {
            binding,
            iterator,
            block,
            span: (span_start, span_end),
        }
    }

    pub fn fn_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        let identifier = self.current().value;

        let _ = self.next();
        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        let arguments_tuples = arguments
            .iter()
            .map(|arg| {
                if let Expressions::Argument {
                    name,
                    r#type,
                    span: _,
                } = arg
                {
                    (name.clone(), r#type.clone())
                } else {
                    // okay lets just skip this piece of code.
                    // I've made a mistake creating this embedded code, but we all make mistakes.
                    // Anyways I just wanted short code to unwrap and compare identifier inside
                    // embedded boxed expressions blocks, so...

                    if let Expressions::Reference { object, span: _ } = arg {
                        if let Expressions::Value(Value::Identifier(id), _) = *object.clone() {
                            if id == "self" { return (id, Type::SelfRef) }
                        }
                    }

                    self.error(
                        String::from("Unexpected argument declaration found"),
                        self.span_expression(arg.clone()),
                    );
                    (String::new(), Type::Void)
                }
            })
            .collect();

        let mut datatype = Type::Void;
        if !self.expect(TokenType::LBrace) {
            datatype = self.parse_type();
        }

        if !self.expect(TokenType::LBrace) {
            self.error(
                String::from("Expected new block after function declaration"),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }

        let header_span = (span_start, self.current().span.1);

        let _ = self.next();
        let mut span_block_start = self.current().span.0;
        let mut block = Vec::new();

        while !self.expect(TokenType::RBrace) {
            if self.current().token_type == TokenType::EOF {
                self.error(
                    String::from("Expected block end, but found EOF"),
                    (span_block_start, self.current().span.1),
                );
                return Statements::None;
            }

            block.push(self.statement());
            span_block_start = self.current().span.0;
        }

        let span_end = self.current().span.0 + 1;
        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }

        self.skip_eos();
        Statements::FunctionDefineStatement {
            name: identifier,
            datatype,
            arguments: arguments_tuples,
            block,
            public: false,
            span: (span_start, span_end),
            header_span,
        }
    }

    pub fn return_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        let return_expr = if self.expect(TokenType::Semicolon) {
            let _ = self.next();
            Expressions::Value(Value::Void, self.current().span)
        } else {
            self.expression()
        };

        Statements::ReturnStatement {
            value: return_expr.clone(),
            span: (span_start, self.span_expression(return_expr).1),
        }
    }

    pub fn break_statement(&mut self) -> Statements {
        let span = self.current().span;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        self.skip_eos();

        Statements::BreakStatements { span }
    }

    pub fn assign_statement(&mut self, object: Expressions, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;

        Statements::AssignStatement {
            object,
            value,
            span: (span.0, span_end - 3),
        }
    }

    pub fn binary_assign_statement(
        &mut self,
        object: Expressions,
        op: String,
        span: (usize, usize),
    ) -> Statements {
        if self.expect(TokenType::Equal) {
            let _ = self.next();
        }

        let value = self.expression();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::BinaryAssignStatement {
            operand: op,
            object,
            value,
            span: (span.0, span_end - 3),
        }
    }

    pub fn slice_assign_statement(&mut self, object: Expressions, span: (usize, usize)) -> Statements {
        let brackets_span_start = self.current().span.0;
        if self.expect(TokenType::LBrack) {
            let _ = self.next();
        }

        let ind = self.expression();
        let brackets_span_end = self.current().span.1;

        if !self.expect(TokenType::RBrack) {
            self.error(
                String::from("Brackets close expected in slice"),
                (brackets_span_start, brackets_span_end),
            );
            return Statements::None;
        }

        let _ = self.next();
        if !self.expect(TokenType::Equal) {
            self.error(
                String::from("Expected `=` in slice assign"),
                (span.0, self.current().span.1),
            );
            self.skip_statement();
            return Statements::None;
        }

        let _ = self.next();
        let val = self.expression();
        self.skip_eos();

        let span_end = self.current().span.1;

        Statements::SliceAssignStatement {
            object,
            index: ind,
            value: val,
            span: (span.0, span_end),
        }
    }

    pub fn call_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        match self.current().token_type {
            TokenType::Identifier => {
                let _ = self.next();
            }
            TokenType::LParen => {}
            _ => {
                self.error(
                    String::from("Unexpected variation of call statement"),
                    (span.0, self.current().span.1),
                );
                return Statements::None;
            }
        };

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        self.skip_eos();

        Statements::FunctionCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn macrocall_statement(&mut self, id: String, span: (usize, usize)) -> Statements {
        if self.expect(TokenType::Not) {
            let _ = self.next();
        }

        let arguments =
            self.expressions_enum(TokenType::LParen, TokenType::RParen, TokenType::Comma);

        self.position -= 1;
        let span_end = self.current().span.1;
        self.position += 1;

        self.skip_eos();

        Statements::MacroCallStatement {
            name: id,
            arguments,
            span: (span.0, span_end),
        }
    }

    pub fn struct_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(
                String::from("Identifier for structure expected"),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            self.error(
                String::from("Expected new block after struct definition"),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }

        let _ = self.next();
        let mut fields = HashMap::new();
        let mut functions = HashMap::new();

        while !self.expect(TokenType::RBrace) {
            match self.current().token_type {
                TokenType::RBrace => break,
                TokenType::Keyword => {
                    if self.current().value != "fn" {
                        self.error(
                            String::from("Undefined keyword found at the struct declaration"),
                            self.current().span,
                        );
                        return Statements::None;
                    }

                    let stmt = self.fn_statement();

                    if let Statements::FunctionDefineStatement {
                        name,
                        datatype: _,
                        arguments: _,
                        public: _,
                        block: _,
                        span: _,
                        header_span: _,
                    } = &stmt
                    {
                        functions.insert(name.to_owned(), stmt);
                    } else {
                        unreachable!()
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }
                }
                TokenType::Identifier => {
                    let name = self.current().value;
                    let span = self.current().span;
                    let _ = self.next();

                    if !self.expect(TokenType::DoubleDots) {
                        self.error(
                            String::from("Unexpected field declaration"),
                            (span.0, self.current().span.1),
                        );
                        return Statements::None;
                    }

                    let _ = self.next();
                    let field_type = self.parse_type();

                    self.position -= 1;
                    let extra_span = self.current().span;
                    self.position += 1;

                    if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                        self.error(String::from("Expected COMMA"), (extra_span.1, extra_span.1));
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }

                    fields.insert(name, field_type);
                }
                _ => {
                    self.error(
                        String::from("Undefined term found at the struct declaration"),
                        self.current().span,
                    );
                    return Statements::None;
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public: false,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn enum_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if let TokenType::Keyword = self.current().token_type {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(
                String::from("Identifier for enum expected"),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }

        let name = self.current().value;
        let _ = self.next();

        if !self.expect(TokenType::LBrace) {
            self.error(
                String::from("Expected new block after enum declaration"),
                (span_start, self.current().span.1),
            );
            return Statements::None;
        }
        let _ = self.next();

        let mut fields = Vec::new();
        let mut functions = HashMap::new();

        while !self.expect(TokenType::RBrace) {
            match self.current().token_type {
                TokenType::RBrace => break,
                TokenType::Keyword => {
                    if self.current().value != "fn" {
                        self.error(
                            String::from("Undefined keyword found at the enum declaration"),
                            self.current().span,
                        );
                        return Statements::None;
                    }

                    let stmt = self.fn_statement();

                    if let Statements::FunctionDefineStatement {
                        name,
                        datatype: _,
                        arguments: _,
                        public: _,
                        block: _,
                        span: _,
                        header_span: _,
                    } = &stmt
                    {
                        functions.insert(name.to_owned(), stmt);
                    } else {
                        unreachable!()
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }
                }
                TokenType::Identifier => {
                    let name = self.current().value;
                    let extra_span = self.current().span;
                    let _ = self.next();

                    if !self.expect(TokenType::Comma) && !self.expect(TokenType::RBrace) {
                        self.error(String::from("Expected COMMA"), (extra_span.1, extra_span.1));
                    }

                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                    }

                    fields.push(name);
                }
                _ => {
                    self.error(
                        String::from("Unexpected term found at enum"),
                        self.current().span,
                    );
                    return Statements::None;
                }
            }
        }

        if self.expect(TokenType::RBrace) {
            let _ = self.next();
        }
        self.skip_eos();

        Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public: false,
            span: (span_start, self.current().span.1),
        }
    }

    pub fn typedef_statement(&mut self) -> Statements {
        let span_start = self.current().span.0;
        if self.expect(TokenType::Keyword) {
            let _ = self.next();
        }

        if !self.expect(TokenType::Identifier) {
            self.error(
                String::from("Expected alias for typedef"),
                self.current().span,
            );
        };

        let alias = self.current().value;
        let _ = self.next();

        let datatype = self.parse_type();
        let span_end = self.current().span.1;
        self.skip_eos();

        Statements::TypedefStatement {
            alias,
            datatype,
            span: (span_start, span_end),
        }
    }
}
