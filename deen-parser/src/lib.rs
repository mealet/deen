use crate::{
    error::{ParserError, ParserWarning},
    expressions::Expressions,
    statements::Statements,
    types::Type,
    value::Value,
};
use deen_lexer::{token::Token, token_type::TokenType};
use miette::NamedSource;

pub mod error;
pub mod expressions;
pub mod statements;
pub mod types;
pub mod value;

type ParserOk = (Vec<Statements>, Vec<ParserWarning>);
type ParserErr = (Vec<ParserError>, Vec<ParserWarning>);

const BINARY_OPERATORS: [TokenType; 5] = [
    TokenType::Plus,     // +
    TokenType::Minus,    // -
    TokenType::Divide,   // /
    TokenType::Multiply, // *
    TokenType::Modulus, // %
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

const PRIORITY_BINARY_OPERATORS: [TokenType; 3] = [TokenType::Multiply, TokenType::Divide, TokenType::Modulus];
const PRIORITY_BOOLEAN_OPERATORS: [TokenType; 2] = [TokenType::Or, TokenType::And];

const END_STATEMENT: TokenType = TokenType::Semicolon;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parser {
    source: NamedSource<String>,

    tokens: Vec<Token>,
    position: usize,

    errors: Vec<ParserError>,
    warnings: Vec<ParserWarning>,
    eof: bool,
}

impl Parser {
    // main

    pub fn new(tokens: Vec<Token>, source: &str, filename: &str) -> Self {
        Self {
            source: NamedSource::new(filename, source.to_owned()),

            tokens,
            position: 0,

            errors: Vec::new(),
            warnings: Vec::new(),
            eof: false,
        }
    }

    pub fn parse(&mut self) -> Result<ParserOk, ParserErr> {
        let mut output = Vec::new();

        while self.position < self.tokens.len() - 1 {
            output.push(self.statement());

            if self.eof {
                break;
            };
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }
        Ok((output, self.warnings.clone()))
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

        self.errors.push(error::ParserError {
            message,
            span: span.into(),
            src: self.source.clone(),
        });
    }

    #[allow(unused)]
    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

        self.warnings.push(error::ParserWarning {
            message,
            span: span.into(),
            src: self.source.clone(),
        })
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
            "usize" => Type::USIZE,

            "string" => Type::String,
            "char" => Type::Char,

            "bool" => Type::Bool,
            "void" => Type::Void,

            _ => {
                self.error(String::from("Unable to parse datatype"), span);
                Type::Void
            }
        }
    }

    fn parse_type(&mut self) -> Type {
        let current = self.current();

        match current.token_type {
            TokenType::Type => {
                let _ = self.next();
                self.get_basic_type(current.value, current.span)
            }
            TokenType::LBrack => {
                let _ = self.next();

                if self.current().token_type == TokenType::RBrack {
                    // dynamic array
                    let _ = self.next();
                    let array_type = self.parse_type();

                    return Type::DynamicArray(Box::new(array_type));
                }

                // default array

                let array_type = self.parse_type();

                self.skip_eos();
                if !self.expect(TokenType::Number) {
                    self.error(
                        String::from("Array size must be integer constant"),
                        self.current().span,
                    );
                    return Type::Void;
                }

                let array_size = self.current().value.parse::<usize>().unwrap();

                let _ = self.next();
                if !self.expect(TokenType::RBrack) {
                    self.error(
                        String::from("Unclosed brackets at array type"),
                        self.current().span,
                    );
                    return Type::Void;
                }

                let _ = self.next();
                Type::Array(Box::new(array_type), array_size)
            }
            TokenType::Multiply => {
                let _ = self.next();
                let ptr_type = self.parse_type();

                Type::Pointer(Box::new(ptr_type))
            }
            TokenType::LParen => {
                let _ = self.next();
                let mut types = Vec::new();

                while !self.expect(TokenType::RParen) {
                    if self.expect(TokenType::Comma) {
                        let _ = self.next();
                        continue;
                    };
                    types.push(self.parse_type());
                }

                let _ = self.next();
                Type::Tuple(types)
            }
            TokenType::Identifier => {
                let _ = self.next();
                Type::Alias(current.value)
            }
            _ => {
                let _ = self.next();
                self.error(
                    String::from("Undefined type found"),
                    (current.span.0 - 1, current.span.1 - 1),
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
        let mut cur = self.tokens[self.position].clone();

        if let TokenType::EOF = cur.token_type {
            cur.span = self.tokens[self.position - 1].clone().span;
        }

        cur
    }

    fn expect(&self, expected: TokenType) -> bool {
        self.current().token_type == expected
    }

    fn skip_eos(&mut self) {
        if self.current().token_type == END_STATEMENT {
            let _ = self.next();
        }
    }

    fn skip_statement(&mut self) {
        while !self.expect(TokenType::Semicolon) {
            let _ = self.next();
        }
        self.skip_eos();
    }
}

impl Parser {
    fn term(&mut self) -> Expressions {
        let current = self.current();
        let _ = self.next();

        match current.token_type {
            TokenType::Number => Expressions::Value(
                Value::Integer(current.value.trim().parse().unwrap()),
                current.span,
            ),
            TokenType::FloatNumber => Expressions::Value(
                Value::Float(current.value.trim().parse().unwrap()),
                current.span,
            ),
            TokenType::String => Expressions::Value(Value::String(current.value), current.span),
            TokenType::Char => Expressions::Value(
                Value::Char(current.value.chars().nth(0).unwrap()),
                current.span,
            ),
            TokenType::Boolean => {
                Expressions::Value(Value::Boolean(current.value == "true"), current.span)
            }
            TokenType::Keyword => Expressions::Value(Value::Keyword(current.value), current.span),

            TokenType::Minus | TokenType::Not => {
                let object = self.term();

                Expressions::Unary {
                    operand: current.value,
                    object: Box::new(object.clone()),
                    span: (current.span.0, self.span_expression(object).1),
                }
            }
            TokenType::LParen => {
                let span_start = self.current().span.0;
                let expr = self.expression();

                if self.expect(TokenType::Comma) {
                    let mut values = vec![expr];
                    let _ = self.next();

                    while !self.expect(TokenType::RParen) {
                        if self.expect(TokenType::Comma) {
                            let _ = self.next();
                        } else if self.expect(TokenType::RParen) {
                            break;
                        } else {
                            values.push(self.expression());
                        }
                    }

                    let span_end = self.current().span.1;
                    if self.expect(TokenType::RParen) {
                        let _ = self.next();
                    }

                    return Expressions::Tuple {
                        values,
                        span: (span_start, span_end),
                    };
                }

                if self.expect(TokenType::RParen) {
                    let _ = self.next();
                }
                expr
            }

            TokenType::Identifier => {
                let output =
                    Expressions::Value(Value::Identifier(current.value.clone()), current.span);

                match self.current().token_type {
                    TokenType::LParen => return self.call_expression(current.value, current.span),
                    TokenType::LBrack => return self.slice_expression(output),
                    TokenType::Dot => {
                        return self.subelement_expression(output, TokenType::Dot);
                    }
                    TokenType::LBrace => {
                        let _ = self.next();
                        if self.expect(TokenType::Dot) {
                            self.position -= 2;
                            return self.struct_expression(current.value.clone());
                        }
                        self.position -= 1;
                        return output

                    }
                    TokenType::Not => {
                        let _ = self.next();
                        if self.expect(TokenType::LParen) {
                            self.position -= 1;
                            return self.macrocall_expression(current.value, current.span);
                        }
                        self.position -= 1;
                        return output;
                    }
                    TokenType::DoubleDots => {
                        let _ = self.next();

                        let datatype = self.parse_type();
                        return Expressions::Argument {
                            name: current.value,
                            r#type: datatype,
                            span: (current.span.0, self.current().span.1),
                        };
                    }
                    _ => {}
                }

                return output;
            }

            TokenType::Ref => Expressions::Reference {
                object: Box::new(self.term()),
                span: (current.span.0, self.current().span.1),
            },
            TokenType::Multiply => Expressions::Dereference {
                object: Box::new(self.term()),
                span: (current.span.0, self.current().span.1),
            },

            // This case looks is for C-like syntax: `type name`,
            // but syntax must be like: `name: type`
            // *-----------------------*
            // TokenType::Type => {
            //     let datatype = self.get_basic_type(current.value, current.span);
            //     let identifier = self.current();
            //
            //     if !self.expect(TokenType::Identifier) {
            //         return Expressions::Value(Value::Type(datatype), current.span);
            //     }
            //
            //     let _ = self.next();
            //
            //     Expressions::Argument {
            //         name: identifier.value,
            //         r#type: datatype,
            //         span: current.span
            //     }
            // }
            // *-----------------------*
            TokenType::LBrack => {
                let span_start = current.span.0;
                let values =
                    self.expressions_enum(TokenType::LBrack, TokenType::RBrack, TokenType::Comma);
                let len = values.len();

                self.position -= 1;
                let span_end = self.current().span.1;
                let _ = self.next();

                Expressions::Array {
                    values,
                    len,
                    span: (span_start, span_end),
                }
            }
            TokenType::LBrace => {
                let span_start = current.span.0;
                let mut block = Vec::new();

                while !self.expect(TokenType::RBrace) {
                    block.push(self.statement());
                }

                let span_end = self.current().span.1;
                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }

                Expressions::Scope {
                    block,
                    span: (span_start, span_end),
                }
            }

            TokenType::Type => {
                let datatype = self.get_basic_type(current.value, current.span);
                Expressions::Argument { name: "@deen_type".to_string(), r#type: datatype, span: current.span }
            }

            _ => {
                self.error(
                    String::from("Undefined term found"),
                    (current.span.0, current.span.1),
                );
                Expressions::None
            }
        }
    }

    fn expression(&mut self) -> Expressions {
        let node = self.term();
        let current = self.current();

        match current.token_type {
            tty if BINARY_OPERATORS.contains(&tty) => self.binary_expression(node),
            tty if BOOLEAN_OPERATORS.contains(&tty) => self.boolean_expression(node),
            tty if BITWISE_OPERATORS.contains(&tty) => self.bitwise_expression(node),

            TokenType::LBrack => {
                let span = current.span;
                let _ = self.next();

                if self.expect(TokenType::RBrack) {
                    return node;
                }

                let slice_index = self.expression();

                if !self.expect(TokenType::RBrack) {
                    self.error(String::from("Unclosed brackets in expression found"), span);
                    return Expressions::None;
                }

                let span = (span.0, self.current().span.1);

                let _ = self.next();

                Expressions::Slice {
                    object: Box::new(node),
                    index: Box::new(slice_index),
                    span,
                }
            }

            END_STATEMENT => {
                self.next();
                node
            }

            _ => node,
        }
    }

    fn statement(&mut self) -> Statements {
        let current = self.current();

        match current.token_type {
            TokenType::Keyword => match current.value.as_str() {
                "let" => self.annotation_statement(),
                "import" => self.import_statement(),
                "extern" => self.extern_statement(),
                "if" => self.if_statement(),
                "else" => {
                    self.error(
                        String::from("Unexpected `else` outside construction usage"),
                        current.span,
                    );
                    Statements::None
                }

                "while" => self.while_statement(),
                "for" => self.for_statement(),

                "typedef" => self.typedef_statement(),
                "struct" => self.struct_statement(),
                "enum" => self.enum_statement(),

                "pub" => {
                    let _ = self.next();
                    let stmt = self.statement();

                    match stmt {
                        Statements::FunctionDefineStatement {
                            name,
                            datatype,
                            arguments,
                            block,
                            public: _,
                            span,
                            header_span,
                        } => Statements::FunctionDefineStatement {
                            name,
                            datatype,
                            arguments,
                            block,
                            public: true,
                            span,
                            header_span,
                        },
                        Statements::StructDefineStatement {
                            name,
                            fields,
                            functions,
                            public: _,
                            span,
                        } => Statements::StructDefineStatement {
                            name,
                            fields,
                            functions,
                            public: true,
                            span,
                        },
                        Statements::EnumDefineStatement {
                            name,
                            fields,
                            functions,
                            public: _,
                            span,
                        } => Statements::EnumDefineStatement {
                            name,
                            fields,
                            functions,
                            public: true,
                            span,
                        },

                        _ => {
                            self.error(
                                String::from("Visibility is not followed by provided item"),
                                current.span,
                            );
                            Statements::None
                        }
                    }
                }
                "fn" => self.fn_statement(),
                "return" => self.return_statement(),
                "break" => self.break_statement(),
                _ => unreachable!(),
            },
            TokenType::LBrace => {
                let span_start = current.span.0;
                let _ = self.next();

                let mut block = Vec::new();
                while !self.expect(TokenType::RBrace) {
                    block.push(self.statement());
                }

                let span = (span_start, self.current().span.1);
                if self.expect(TokenType::RBrace) {
                    let _ = self.next();
                }
                self.skip_eos();

                Statements::ScopeStatement { block, span }
            }
            TokenType::Multiply => {
                let span_start = self.current().span.0;
                let _ = self.next();

                match self.current().token_type {
                    TokenType::Identifier | TokenType::Multiply => {
                        let stmt = self.statement();

                        self.position -= 1;
                        let span_end = self.current().span.1;
                        self.position += 1;

                        match stmt {
                            Statements::AssignStatement {
                                object,
                                value,
                                span,
                            } => Statements::DerefAssignStatement {
                                object,
                                value,
                                span,
                            },
                            Statements::FieldAssignStatement { object, value, span } => Statements::DerefAssignStatement { object, value, span },
                            Statements::BinaryAssignStatement { object, operand, value, span } => Statements::DerefAssignStatement {
                                object: object.clone(),
                                value: Expressions::Binary {
                                    operand,
                                    lhs: Box::new(object),
                                    rhs: Box::new(value),
                                    span
                                },
                                span
                            },
                            Statements::SliceAssignStatement { object, index, value, span } => Statements::DerefAssignStatement {
                                object: Expressions::Slice {
                                    object: Box::new(object),
                                    index: Box::new(index),
                                    span
                                },
                                value,
                                span
                            },
                            Statements::DerefAssignStatement { object, value, span } => {
                                Statements::DerefAssignStatement {
                                    object: Expressions::Dereference { object: Box::new(object), span },
                                    value,
                                    span
                                }
                            }
                            _ => {
                                self.error(
                                    String::from("Unsupported for dereference statement found"),
                                    (span_start, span_end),
                                );
                                Statements::None
                            }
                        }
                    }
                    _ => {
                        self.error(
                            String::from("Unsupported for dereference statement found"),
                            self.current().span,
                        );
                        Statements::None
                    }
                }
            }
            TokenType::Identifier => {
                let next = self.next();
                match next.token_type {
                    TokenType::Equal => self.assign_statement(Expressions::Value(Value::Identifier(current.value), current.span), current.span),
                    TokenType::Not => self.macrocall_statement(current.value, current.span),
                    TokenType::Dot => {
                        let sub_expr = self.subelement_expression(
                            Expressions::Value(
                                Value::Identifier(current.value),
                                self.current().span,
                            ),
                            TokenType::Dot,
                        );

                        match self.current().token_type {
                            TokenType::Equal => {
                                let _ = self.next();
                                let value = self.expression();
                                let span_end = self.current().span.1;
                                self.skip_eos();

                                Statements::FieldAssignStatement {
                                    object: sub_expr,
                                    value,
                                    span: (current.span.0, span_end),
                                }
                            }
                            TokenType::Plus | TokenType::Minus | TokenType::Multiply | TokenType::Divide => {
                                let operand = self.current().value;
                                let _ = self.next();

                                if !self.expect(TokenType::Equal) {
                                    self.error(
                                        String::from("Unexpected binary expression after subelement"),
                                        (current.span.0, self.current().span.1)
                                    );
                                    return Statements::None;
                                }

                                let _ = self.next();
                                let value = self.expression();
                                let span_end = self.current().span.1;
                                self.skip_eos();

                                Statements::BinaryAssignStatement {
                                    object: sub_expr,
                                    operand,
                                    value,
                                    span: (current.span.0, span_end)
                                }
                            }
                            TokenType::Semicolon => {
                                self.skip_eos();
                                Statements::Expression(sub_expr)
                            }
                            _ => {
                                self.position -= 1;
                                let span_end = self.current().span.1;
                                self.position += 1;

                                self.error(
                                    String::from("Unexpected subelement in statement found"),
                                    (current.span.0, span_end),
                                );
                                Statements::None
                            }
                        }
                    }
                    TokenType::LParen => self.call_statement(current.value, current.span),
                    TokenType::LBrack => self.slice_assign_statement(Expressions::Value(Value::Identifier(current.value), current.span), current.span),

                    tty if BINARY_OPERATORS.contains(&tty) => match self.next().token_type {
                        TokenType::Equal => {
                            self.binary_assign_statement(Expressions::Value(Value::Identifier(current.value), current.span), next.value, current.span)
                        }
                        TokenType::Plus | TokenType::Minus => {
                            let span_start = next.span.0;
                            let span_end = self.current().span.1;
                            let (op1, op2) = (next.value, self.current().value);

                            if op1 != op2 {
                                self.error(
                                    String::from(
                                        "Unexpected variation of increment/decrement found!",
                                    ),
                                    (span_start, span_end),
                                );
                                return Statements::None;
                            }

                            let _ = self.next();
                            self.skip_eos();

                            Statements::BinaryAssignStatement {
                                object: Expressions::Value(Value::Identifier(current.value), current.span),
                                operand: op1,
                                value: Expressions::Value(
                                    Value::Integer(1),
                                    (current.span.0, span_end),
                                ),
                                span: (current.span.0, span_end),
                            }
                        }
                        _ => {
                            self.error(
                                String::from("Unexpected binary operation in statement found"),
                                (current.span.0, self.current().span.1),
                            );
                            Statements::None
                        }
                    },
                    END_STATEMENT => Statements::Expression(Expressions::Value(
                        Value::Identifier(current.value),
                        current.span,
                    )),
                    _ => {
                        self.error(
                            String::from("Unexpected expression/statement found after identifier"),
                            (current.span.0, next.span.1),
                        );
                        Statements::None
                    }
                }
            }
            TokenType::EOF => {
                self.eof = true;
                Statements::None
            }
            _ => Statements::Expression(self.expression()),
        }
    }
}
