#![allow(unused)]

use miette::{NamedSource, Diagnostic};
use std::collections::HashMap;
use crate::{
    token::Token,
    token_type::TokenType,
    macros::{std_keyword, std_type, std_symbol, std_token}
};

mod errors;
mod warnings;
mod macros;
mod token;
mod token_type;

pub struct Lexer {
    source: NamedSource<String>,
    
    std_symbols: HashMap<char, Token>,
    std_words: HashMap<String, Token>,

    errors: Vec<Box<dyn Diagnostic>>,
    warnings: Vec<Box<dyn Diagnostic>>,

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

            ]),
            std_words: HashMap::from([

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
    
    fn error<T: 'static + Diagnostic>(&mut self, error: T) {
        self.errors.push(Box::new(error));
    }

    fn warning<T: 'static + Diagnostic>(&mut self, warn: T) {
        self.errors.push(Box::new(warn));
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
                                errors::WrongConst { src: self.source.clone(), at: (span_start, self.position).into() }
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
                                errors::WrongConst { src: self.source.clone(), at: (span_start, self.position).into() }
                            );
                            return 0;
                        }
                    }
                    '0' => {
                        if !value.is_empty() {
                            let extra_start = self.position;

                            while self.char == '0' {
                                self.getc();
                            }

                            self.warning(
                                warnings::ExtraZeroes { src: self.source.clone(), at: (extra_start, self.position).into() }
                            );
                            continue;
                        }

                        value.push('0');
                        continue;
                    }
                    _ => {
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

        match mode {
            ParseMode::Decimal => {
                return value.parse().unwrap_or_else(|_| {
                    self.error(
                        errors::ParseError { src: self.source.clone(), at: (span_start, self.position).into() }
                    );
                    0
                })
            },
            ParseMode::Binary => {
                return i64::from_str_radix(value.trim(), 2).unwrap_or_else(|_| {
                    self.error(
                        errors::ParseError { src: self.source.clone(), at: (span_start, self.position).into() }
                    );
                    0
                })
            },
            ParseMode::Hexadecimal => {
                return i64::from_str_radix(value.trim(), 16).unwrap_or_else(|_| {
                    self.error(
                        errors::ParseError { src: self.source.clone(), at: (span_start, self.position).into() }
                    );
                    0
                })
            }
        }
    }

    // main
    
    pub fn tokenize(&mut self) -> Result<(Vec<Token>, Vec<Box<dyn Diagnostic>>), (Vec<Box<dyn Diagnostic>>, Vec<Box<dyn Diagnostic>>)> {
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
                                (span_start, self.position).into()
                            )
                        );

                        self.getc();
                    } else {
                        output.push(
                            Token::new(
                                String::from("-"),
                                TokenType::Minus,
                                (span_start, self.position).into()
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
                                    (span_start, self.position)
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
                                    errors::CharEscape { src: self.source.clone(), at: (span_start, self.position).into() }
                                );
                                self.getc();
                            }

                            output.push(
                                Token::new(
                                    chr.to_string(),
                                    TokenType::Char,
                                    (span_start, self.position)
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
                                        (span_start, self.position).into()
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position).into();

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
                                            (span_start, self.position)
                                        )
                                    );
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position).into();

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
                                            (span_start, self.position)
                                        )
                                    );
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position).into();

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
                                        (span_start, self.position)
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position).into();

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
                                        (span_start, self.position)
                                    )
                                );
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position).into();

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
                                            (span_start, self.position)
                                        )
                                    );
                                    self.getc();
                                }
                                ' ' => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position).into();

                                    output.push(formatted_token);                                   
                                }
                                _ => {
                                    output.push(
                                        Token::new(
                                            String::from("&"),
                                            TokenType::Ref,
                                            (span_start, self.position).into()
                                        )
                                    );
                                }
                            }
                        }
                        _ => {
                            let mut formatted_token = matched_token;
                            formatted_token.span = (span_start, self.position).into();

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
                            value.to_string(), TokenType::Number, (span_start, self.position)
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
                        matched_token.span = (start_span, self.position).into();
                        output.push(matched_token);
                    } else {
                        output.push(
                            Token::new(
                                id,
                                TokenType::Identifier,
                                (start_span, self.position)
                            )
                        );
                    }
                }
                _ => {
                    self.error(
                        errors::UndefinedChar { src: self.source.clone(), at: (self.position, self.position + 1).into() }
                    );
                    self.getc();
                }
            }
        }

        if !output.contains(&Token::new(String::new(), TokenType::EOF, (0, 0))) {
            output.push(Token::new(String::new(), TokenType::EOF, (0, 0)));
        }

        if !self.errors.is_empty() {
            return Err((std::mem::take(&mut self.errors), std::mem::take(&mut self.warnings)))
        }

        Ok((output, std::mem::take(&mut self.warnings)))
    }
}
