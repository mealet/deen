#![allow(unused)]

use miette::{NamedSource, Diagnostic};
use std::collections::HashMap;
use crate::{
    error::{LexerError, LexerWarning},
    token::Token,
    token_type::TokenType,
    macros::{std_keyword, std_type, std_symbol, std_token}
};

mod error;
mod macros;
pub mod token;
pub mod token_type;

pub struct Lexer {
    source: NamedSource<String>,
    
    std_symbols: HashMap<char, Token>,
    std_words: HashMap<String, Token>,

    errors: Vec<error::LexerError>,
    warnings: Vec<error::LexerWarning>,

    input: Vec<char>,
    position: usize,
    char: char,
}

impl Lexer {
    // basic
    pub fn new(source: String, filename: String) -> Self {
        let mut lexer = Lexer {
            source: NamedSource::new(filename, source.clone()),

            std_symbols: HashMap::from([
                std_symbol!('+', TokenType::Plus),
                std_symbol!('-', TokenType::Minus),
                std_symbol!('*', TokenType::Multiply),
                std_symbol!('/', TokenType::Divide),
                std_symbol!('=', TokenType::Equal),

                std_symbol!('!', TokenType::Not),
                std_symbol!('^', TokenType::Xor),
                std_symbol!('>', TokenType::Bt),
                std_symbol!('<', TokenType::Lt),

                std_symbol!('.', TokenType::Dot),
                std_symbol!(',', TokenType::Comma),
                std_symbol!('"', TokenType::DoubleQuote),
                std_symbol!('\'', TokenType::SingleQuote),
                std_symbol!(';', TokenType::Semicolon),
                std_symbol!('&', TokenType::Ampersand),
                std_symbol!('|', TokenType::Verbar),

                std_symbol!('(', TokenType::LParen),
                std_symbol!(')', TokenType::RParen),

                std_symbol!('[', TokenType::LBrack),
                std_symbol!(']', TokenType::RBrack),

                std_symbol!('{', TokenType::LBrace),
                std_symbol!('}', TokenType::RBrace),
            ]),
            std_words: HashMap::from([
                // Constructions

                std_keyword!("if"),
                std_keyword!("else"),
                std_keyword!("while"),
                std_keyword!("for"),
                std_keyword!("break"),

                // Tech

                std_keyword!("import"),
                std_keyword!("return"),

                // Types

                std_type!("i8"),
                std_type!("i16"),
                std_type!("i32"),
                std_type!("i64"),

                std_type!("u8"),
                std_type!("u16"),
                std_type!("u32"),
                std_type!("u64"),

                std_type!("string"),
                std_type!("char"),

                std_type!("bool"),
                std_type!("void"),

                // Values

                std_token!("true", TokenType::Boolean),
                std_token!("false", TokenType::Boolean),
            ]),

            errors: Vec::new(),
            warnings: Vec::new(),

            input: source.chars().collect::<Vec<char>>(),
            position: 0,
            char: ' '
        };

        lexer.getc();
        lexer
    }

    // fundamental
    
    fn error(&mut self, message: String, span: (usize, usize)) {
        self.errors.push(
            error::LexerError {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        );
    }

    fn warning(&mut self, message: String, span: (usize, usize)) {
        self.warnings.push(
            error::LexerWarning {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        )
    }

    fn getc(&mut self) {
        if self.position < self.input.len() {
            self.char = self.input[self.position];
            self.position += 1;
        } else {
            self.char = '\0'
        }
    }

    // filters

    fn is_eof(&self) -> bool {
        self.char == '\0'
    }

    fn is_hexadecimal_literal(&self, value: &char) -> bool {
        ['a', 'b', 'c', 'd', 'e', 'f'].contains(&value.to_ascii_lowercase())
    }

    // helpful
    
    fn get_integer(&mut self) -> i64 {
        #[derive(PartialEq)]
        enum ParseMode { Decimal, Hexadecimal, Binary };

        let mut value = String::new();
        let mut mode = ParseMode::Decimal;
        let span_start = self.position;

        while self.char.is_ascii_digit()
            || ['_', 'x', 'b'].contains(&self.char)
            || self.is_hexadecimal_literal(&self.char)
        {
            if self.char == '0' {
                self.getc();

                match self.char {
                    'b' => {
                        if mode != ParseMode::Decimal || !value.is_empty() {
                            self.error(
                                String::from("Wrong number constant found"), (span_start, self.position - span_start)
                            );
                            return 0;
                        }

                        mode = ParseMode::Binary;
                        self.getc();
                        continue;
                    }
                    'x' => {
                        if mode != ParseMode::Decimal || !value.is_empty() {
                            self.error(
                                String::from("Wrong number constant found"), (span_start, self.position - span_start)
                            );
                            return 0;
                        }
                    }
                    '0' => {
                        if value.is_empty() {
                            while self.char == '0' {
                                self.getc();
                            }

                            self.warning(
                                String::from("Extra zeroes in constant"), (span_start - 1, self.position - span_start)
                            );
                            continue;
                        }

                        value.push('0');
                        continue;
                    }
                    _ => {
                        if value.is_empty() {
                            self.warning(
                                String::from("Extra zero at the constant start"), (span_start - 1, self.position - span_start)
                            );
                        }

                        value.push('0');
                        continue;
                    }
                }
            }

            if self.char != '_' {
                value.push(self.char);
            }

            self.getc();
        }

        if value.is_empty() { return 0 };

        match mode {
            ParseMode::Decimal => {
                return value.parse().unwrap_or_else(|_| {
                    self.error(
                        String::from("IO Module returned an error while parsing number"), (span_start, self.position - span_start)
                    );
                    0
                })
            },
            ParseMode::Binary => {
                return i64::from_str_radix(value.trim(), 2).unwrap_or_else(|_| {
                    self.error(
                        String::from("IO Module returned an error while parsing number"), (span_start, self.position - span_start)
                    );
                    0
                })
            },
            ParseMode::Hexadecimal => {
                return i64::from_str_radix(value.trim(), 16).unwrap_or_else(|_| {
                    self.error(
                        String::from("IO Module returned an error while parsing number"), (span_start, self.position - span_start)
                    );

                    0
                })
            }
        }
    }

    // main
    
    pub fn tokenize(&mut self) -> Result<(Vec<Token>, Vec<LexerWarning>), (Vec<LexerError>, Vec<LexerWarning>)> {
        let mut output = Vec::new();

        while !self.is_eof() {
            match self.char {
                '\0' => self.getc(),
                '\n' | ' ' => self.getc(),
                '-' => {
                    // checking possible negative constant

                    let span_start = self.position;

                    self.getc();
                    if self.char.is_ascii_digit() {
                        let value = -self.get_integer();

                        output.push(
                            Token::new(
                                value.to_string(),
                                TokenType::Number,
                                (span_start, self.position - span_start).into()
                            )
                        );

                        self.getc();
                    } else {
                        output.push(
                            Token::new(
                                String::from("-"),
                                TokenType::Minus,
                                (span_start, self.position - span_start).into()
                            )
                        );
                        self.getc();
                    }
                },
                chr if self.std_symbols.contains_key(&chr) => {
                    let matched_token = self.std_symbols.get(&chr).unwrap().clone();
                    let span_start = self.position;

                    match matched_token.token_type {
                        TokenType::DoubleQuote => {
                            self.getc();
                            let mut captured_string = String::new();

                            while self.char != '"' {
                                captured_string.push(self.char);
                                self.getc();
                            }

                            output.push(
                                Token::new(
                                    captured_string,
                                    TokenType::String,
                                    (span_start, self.position - span_start)
                                )
                            );
                            self.getc();
                        }
                        TokenType::SingleQuote => {
                            let span_start = self.position;

                            self.getc();

                            let chr = self.char;

                            self.getc();
                            if self.char != '\'' {
                                self.error(
                                    String::from("Unexpected character escape found"), (span_start, self.position - span_start)
                                );
                                self.getc();
                            }

                            output.push(
                                Token::new(
                                    chr.to_string(),
                                    TokenType::Char,
                                    (span_start, self.position - span_start)
                                )
                            );
                            self.getc();
                        }
                        TokenType::Equal => {
                            let span_start = self.position;
                            self.getc();

                            if self.char == '=' {
                                output.push(
                                    Token::new(
                                        String::from("=="),
                                        TokenType::Eq,
                                        (span_start, self.position - span_start).into()
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - span_start).into();

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Lt => {
                            self.getc();

                            match self.char {
                                '<' => {
                                    output.push(
                                        Token::new(
                                            String::from("<<"),
                                            TokenType::LShift,
                                            (span_start, self.position - span_start)
                                        )
                                    );
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - span_start).into();

                                    output.push(formatted_token);
                                }
                            }
                        }
                        TokenType::Bt => {
                            self.getc();

                            match self.char {
                                '>' => {
                                    output.push(
                                        Token::new(
                                            String::from(">>"),
                                            TokenType::RShift,
                                            (span_start, self.position - span_start)
                                        )
                                    );
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - span_start).into();

                                    output.push(formatted_token);
                                }
                            }
                        }
                        TokenType::Not => {
                            self.getc();

                            if self.char == '=' {
                                output.push(
                                    Token::new(
                                        String::from("!="),
                                        TokenType::Ne,
                                        (span_start, self.position - span_start)
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - span_start).into();

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Verbar => {
                            self.getc();

                            if self.char == '|' {
                                output.push(
                                    Token::new(
                                        String::from("||"),
                                        TokenType::Or,
                                        (span_start, self.position - span_start)
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - span_start).into();

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Ampersand => {
                            self.getc();

                            match self.char {
                                '&' => {
                                    output.push(
                                        Token::new(
                                            String::from("&&"),
                                            TokenType::And,
                                            (span_start, self.position - span_start)
                                        )
                                    );
                                    self.getc();
                                }
                                ' ' => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - span_start).into();

                                    output.push(formatted_token);                                   
                                }
                                _ => {
                                    output.push(
                                        Token::new(
                                            String::from("&"),
                                            TokenType::Ref,
                                            (span_start, self.position - span_start).into()
                                        )
                                    );
                                }
                            }
                        }
                        _ => {
                            let mut formatted_token = matched_token;
                            formatted_token.span = (span_start, self.position - span_start).into();

                            output.push(formatted_token);
                            self.getc();
                        }
                    }
                }
                _ if self.char.is_ascii_digit() => {
                    let span_start = self.position;
                    let value = self.get_integer();

                    output.push(
                        Token::new(
                            value.to_string(), TokenType::Number, (span_start, self.position - span_start)
                        )
                    );
                }
                _ if self.char.is_alphabetic() => {
                    let allowed_id_chars = ['_'];

                    let mut id = String::new();
                    let start_span = self.position;
                    while self.char.is_alphanumeric()
                        || allowed_id_chars.contains(&self.char)
                    {
                        id.push(self.char);
                        self.getc();
                    }

                    if self.std_words.contains_key(&id) {
                        let mut matched_token = self.std_words.get(&id).unwrap().clone();
                        matched_token.span = (start_span, self.position - start_span).into();
                        output.push(matched_token);
                    } else {
                        output.push(
                            Token::new(
                                id,
                                TokenType::Identifier,
                                (start_span, self.position - start_span)
                            )
                        );
                    }
                }
                _ => {
                    self.error(
                        String::from("Undefined char found"), (self.position - 1, 1).into()
                    );
                    self.getc();
                }
            }
        }

        if !output.contains(&Token::new(String::new(), TokenType::EOF, (0, 0))) {
            output.push(Token::new(String::new(), TokenType::EOF, (0, 0)));
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()))
        }

        Ok((output, self.warnings.clone()))
    }
}
