use crate::{
    error::{LexerError, LexerWarning},
    macros::{std_keyword, std_symbol, std_token, std_type},
    token::Token,
    token_type::TokenType,
};
use miette::NamedSource;
use std::collections::HashMap;

pub mod error;
mod macros;
pub mod token;
pub mod token_type;

type LexerOk = (Vec<Token>, Vec<LexerWarning>);
type LexerErr = (Vec<LexerError>, Vec<LexerWarning>);

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
    pub fn new(source: &str, filename: &str) -> Self {
        let mut lexer = Lexer {
            source: NamedSource::new(filename, source.to_owned()),

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
                std_symbol!(':', TokenType::DoubleDots),
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
                std_keyword!("let"),
                std_keyword!("pub"),
                std_keyword!("fn"),
                std_keyword!("import"),
                std_keyword!("return"),
                std_keyword!("struct"),
                std_keyword!("enum"),
                std_keyword!("typedef"),
                // Types
                std_type!("i8"),
                std_type!("i16"),
                std_type!("i32"),
                std_type!("i64"),
                std_type!("u8"),
                std_type!("u16"),
                std_type!("u32"),
                std_type!("u64"),
                std_type!("usize"),
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
            char: ' ',
        };

        lexer.getc();
        lexer
    }

    // fundamental

    fn error(&mut self, message: String, span: (usize, usize)) {
        self.errors.push(error::LexerError {
            message,
            span: span.into(),
            src: self.source.clone(),
        });
    }

    fn warning(&mut self, message: String, span: (usize, usize)) {
        self.warnings.push(error::LexerWarning {
            message,
            span: span.into(),
            src: self.source.clone(),
        })
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

    fn get_number(&mut self) -> (String, TokenType) {
        #[derive(PartialEq)]
        enum ParseMode {
            Decimal,
            Hexadecimal,
            Binary,
            Float,
        }

        let mut value = String::new();
        let mut mode = ParseMode::Decimal;
        let span_start = self.position;

        while self.char.is_ascii_digit()
            || ['_', '.', 'x', 'b'].contains(&self.char)
            || self.is_hexadecimal_literal(&self.char)
        {
            if self.char == '0' {
                self.getc();

                match self.char {
                    'b' => {
                        if mode != ParseMode::Decimal || !value.is_empty() {
                            self.error(
                                String::from("Wrong number constant found"),
                                (span_start, self.position),
                            );
                            return (0.to_string(), TokenType::Number);
                        }

                        mode = ParseMode::Binary;
                        self.getc();
                        continue;
                    }
                    'x' => {
                        if mode != ParseMode::Decimal || !value.is_empty() {
                            self.error(
                                String::from("Wrong number constant found"),
                                (span_start, self.position),
                            );
                            return (0.to_string(), TokenType::Number);
                        }

                        mode = ParseMode::Hexadecimal;
                        self.getc();
                        continue;
                    }
                    '0' => {
                        if value.is_empty() {
                            while self.char == '0' {
                                self.getc();
                            }

                            self.warning(
                                String::from("Extra zeroes in constant"),
                                (span_start - 1, self.position),
                            );
                            continue;
                        }

                        if mode == ParseMode::Float {
                            let mut temp = Vec::new();
                            while self.char == '0' {
                                temp.push(self.char);
                                self.getc();
                            }

                            if !self.char.is_ascii_digit() {
                                self.warning(
                                    String::from("Extra zeroes at float constant end"),
                                    (span_start - 1, self.position - span_start + 1),
                                );
                                continue;
                            }

                            temp.iter().for_each(|ch| value.push(*ch));
                        }

                        value.push('0');
                        continue;
                    }
                    _ => {
                        if value.is_empty() && self.char.is_ascii_digit() {
                            self.warning(
                                String::from("Extra zero at the constant start"),
                                (span_start - 1, self.position),
                            );
                        }

                        value.push('0');
                        continue;
                    }
                }
            }

            match self.char {
                '_' => {}
                '.' => {
                    if mode != ParseMode::Decimal {
                        self.error(
                            String::from("Unexpected variation of float constant found"),
                            (span_start - 1, self.position + 1),
                        );
                        return (String::from("0"), TokenType::FloatNumber);
                    }

                    mode = ParseMode::Float;
                    value.push('.');
                }
                _ => value.push(self.char),
            }

            self.getc();
        }

        if value.is_empty() {
            return (0.to_string(), TokenType::Number);
        };

        match mode {
            ParseMode::Decimal => (
                value
                    .parse::<i64>()
                    .unwrap_or_else(|_| {
                        self.error(
                            String::from(
                                "IO Module returned an error while parsing Decimal number",
                            ),
                            (span_start, self.position),
                        );
                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Binary => (
                i64::from_str_radix(value.trim(), 2)
                    .unwrap_or_else(|_| {
                        self.error(
                            String::from("IO Module returned an error while parsing Binary number"),
                            (span_start, self.position),
                        );
                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Hexadecimal => (
                i64::from_str_radix(value.trim(), 16)
                    .unwrap_or_else(|_| {
                        self.error(
                            String::from(
                                "IO Module returned an error while parsing Hexadecimal number",
                            ),
                            (span_start, self.position),
                        );

                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Float => (
                value
                    .parse::<f64>()
                    .unwrap_or_else(|_| {
                        self.error(
                            String::from("IO Module returned an error while parsing Float number"),
                            (span_start, self.position + 1),
                        );
                        0.0
                    })
                    .to_string(),
                TokenType::FloatNumber,
            ),
        }
    }

    // main

    pub fn tokenize(&mut self) -> Result<LexerOk, LexerErr> {
        let mut output = Vec::new();

        while !self.is_eof() {
            match self.char {
                '\0' => self.getc(),
                '\n' | ' ' => self.getc(),
                '/' => {
                    self.getc();
                    if self.char == '/' {
                        while self.char != '\n' {
                            self.getc();
                        }
                        continue;
                    }
                    output.push(Token::new(
                        String::from("/"),
                        TokenType::Divide,
                        (self.position - 1, self.position - 1),
                    ));
                }
                chr if self.std_symbols.contains_key(&chr) => {
                    let matched_token = self.std_symbols.get(&chr).unwrap().clone();
                    let span_start = self.position - 1;

                    match matched_token.token_type {
                        TokenType::DoubleQuote => {
                            self.getc();
                            let mut captured_string = String::new();

                            while self.char != '"' {
                                if self.char == '\\' {
                                    self.getc();

                                    match self.char {
                                        '0' => captured_string.push('\0'),
                                        'n' => captured_string.push('\n'),
                                        'r' => captured_string.push('\r'),
                                        't' => captured_string.push('\t'),
                                        _ => {
                                            continue;
                                        }
                                    }

                                    self.getc();
                                    continue;
                                }

                                captured_string.push(self.char);
                                self.getc();
                            }

                            output.push(Token::new(
                                captured_string.clone(),
                                TokenType::String,
                                (span_start, span_start + captured_string.len() + 2),
                            ));
                            self.getc();
                        }
                        TokenType::SingleQuote => {
                            let span_start = self.position - 1;

                            self.getc();

                            let chr = self.char;

                            self.getc();
                            if self.char != '\'' {
                                self.error(
                                    String::from("Unexpected character escape found"),
                                    (span_start, self.position),
                                );
                                self.getc();
                            }

                            output.push(Token::new(
                                chr.to_string(),
                                TokenType::Char,
                                (span_start, self.position - 1),
                            ));
                            self.getc();
                        }
                        TokenType::Equal => {
                            let span_start = self.position;
                            self.getc();

                            if self.char == '=' {
                                output.push(Token::new(
                                    String::from("=="),
                                    TokenType::Eq,
                                    (span_start - 1, self.position - 1),
                                ));
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start - 1, self.position - 1);

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Lt => {
                            self.getc();

                            match self.char {
                                '<' => {
                                    output.push(Token::new(
                                        String::from("<<"),
                                        TokenType::LShift,
                                        (span_start, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - 2);

                                    output.push(formatted_token);
                                }
                            }
                        }
                        TokenType::Bt => {
                            self.getc();

                            match self.char {
                                '>' => {
                                    output.push(Token::new(
                                        String::from(">>"),
                                        TokenType::RShift,
                                        (span_start, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - 2);

                                    output.push(formatted_token);
                                }
                            }
                        }
                        TokenType::Not => {
                            self.getc();

                            if self.char == '=' {
                                output.push(Token::new(
                                    String::from("!="),
                                    TokenType::Ne,
                                    (span_start, self.position - 1),
                                ));
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - 2);

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Verbar => {
                            self.getc();

                            if self.char == '|' {
                                output.push(Token::new(
                                    String::from("||"),
                                    TokenType::Or,
                                    (span_start, self.position - 1),
                                ));
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - 2);

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Ampersand => {
                            self.getc();

                            match self.char {
                                '&' => {
                                    output.push(Token::new(
                                        String::from("&&"),
                                        TokenType::And,
                                        (span_start, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                ' ' => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - 2);

                                    output.push(formatted_token);
                                }
                                _ => {
                                    output.push(Token::new(
                                        String::from("&"),
                                        TokenType::Ref,
                                        (span_start, self.position - 2),
                                    ));
                                }
                            }
                        }
                        _ => {
                            let mut formatted_token = matched_token;
                            formatted_token.span = (span_start, span_start);

                            output.push(formatted_token);
                            self.getc();
                        }
                    }
                }
                _ if self.char.is_ascii_digit() => {
                    let span_start = self.position - 1;
                    let (value, tty) = self.get_number();

                    output.push(Token::new(value, tty, (span_start, self.position - 1)));
                }
                _ if self.char.is_alphabetic() || self.char == '_' => {
                    let allowed_id_chars = ['_'];

                    let mut id = String::new();
                    let start_span = self.position - 1;
                    while self.char.is_alphanumeric() || allowed_id_chars.contains(&self.char) {
                        id.push(self.char);
                        self.getc();
                    }

                    if self.std_words.contains_key(&id) {
                        let mut matched_token = self.std_words.get(&id).unwrap().clone();
                        matched_token.span = (start_span, self.position - 1);
                        output.push(matched_token);
                    } else {
                        output.push(Token::new(
                            id,
                            TokenType::Identifier,
                            (start_span, self.position - 1),
                        ));
                    }
                }
                _ => {
                    self.error(String::from("Undefined char found"), (self.position - 1, 1));
                    self.getc();
                }
            }
        }

        if !output.contains(&Token::new(String::new(), TokenType::EOF, (0, 0))) {
            output.push(Token::new(String::new(), TokenType::EOF, (0, 0)));
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }

        Ok((output, self.warnings.clone()))
    }
}
