//! # Deen Lexical Analyzer
//! Tools to convert user's source code into abstracted data structures - tokens. <br/>
//! Wikipedia Explanation: <https://en.wikipedia.org/wiki/Lexical_analysis> <br/>
//!
//! Main tool here is the [`Lexer`] structure (you can also check examples there)
//!
//! ## Usage
//! ```rust
//! use deen_lexer::Lexer;
//!
//! let input = "1 + 1";
//! let filename = "example.dn";
//!
//! let mut lexer = Lexer::new(input, filename);
//! let lexer_result = lexer.tokenize();
//!
//! assert!(lexer_result.is_ok());
//!
//! let (tokens, warns) = lexer_result.unwrap();
//!
//! assert!(warns.is_empty());
//! assert!(tokens.len() == 4); // 3 tokens and EOF (End Of File)
//!
//! println!("{:?}", tokens);
//! ```

use crate::{
    error::{LexerError, LexerWarning},
    macros::{std_keyword, std_symbol, std_token, std_type},
    token::Token,
    token_type::TokenType,
};
use miette::NamedSource;
use std::collections::HashMap;

/// Error Handling Module
pub mod error;
/// Helpful Macros Module
mod macros;
/// Token Object and Implementations
pub mod token;
/// Token Types Enumeration
pub mod token_type;

pub type LexerOk = (Vec<Token>, Vec<LexerWarning>);
pub type LexerErr = (Vec<LexerError>, Vec<LexerWarning>);

/// Main lexical analyzer instance
///
/// ### Usage example:
pub struct Lexer {
    /// Named source code (from [`miette`])
    source: NamedSource<String>,

    // Compiler's reserved symbols
    std_symbols: HashMap<char, Token>,
    // Compiler's reserved keywords
    std_words: HashMap<String, Token>,

    // Vector of handled Lexer errors
    errors: Vec<error::LexerError>,
    // Vector of handled Lexer warnings
    warnings: Vec<error::LexerWarning>,

    // Input's characters
    input: Vec<char>,
    // Current Lexer position
    position: usize,
    // Current Lexer character
    char: char,
}

impl Lexer {
    /// Basic analyzer structure builder
    pub fn new(source: &str, filename: &str) -> Self {
        let mut lexer = Lexer {
            source: NamedSource::new(filename, source.to_owned()),

            std_symbols: HashMap::from([
                std_symbol!('+', TokenType::Plus),
                std_symbol!('-', TokenType::Minus),
                std_symbol!('*', TokenType::Multiply),
                std_symbol!('/', TokenType::Divide),
                std_symbol!('%', TokenType::Modulus),
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
                std_keyword!("ext"),
                std_keyword!("fn"),
                std_keyword!("import"),
                std_keyword!("include"),
                std_keyword!("extern"),
                std_keyword!("return"),
                std_keyword!("struct"),
                std_keyword!("enum"),
                std_keyword!("typedef"),
                std_keyword!("_extern_declare"),
                std_keyword!("_link_c"),
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
                std_type!("f32"),
                std_type!("f64"),
                std_type!("bool"),
                std_type!("char"),
                std_type!("void"),
                // Values
                std_token!("true", TokenType::Boolean),
                std_token!("false", TokenType::Boolean),
                std_token!("NULL", TokenType::Null),
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

    // Fundamental Lexer functions

    fn error(&mut self, error: LexerError) {
        self.errors.push(error);
    }

    fn warning(&mut self, warning: LexerWarning) {
        self.warnings.push(warning)
    }

    fn getc(&mut self) {
        if self.position < self.input.len() {
            self.char = self.input[self.position];
            self.position += 1;
        } else {
            self.char = '\0'
        }
    }

    // Filters

    fn is_eof(&self) -> bool {
        self.char == '\0'
    }

    fn is_hexadecimal_literal(&self, value: &char) -> bool {
        ['a', 'b', 'c', 'd', 'e', 'f'].contains(&value.to_ascii_lowercase())
    }

    // Helpful functions

    fn character_escape(escape: char) -> Option<char> {
        match escape {
            '0' => Some('\0'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            '\\' => Some('\\'),
            _ => None,
        }
    }

    fn get_number(&mut self) -> (String, TokenType) {
        #[derive(PartialEq, Debug)]
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
                            self.error(LexerError::InvalidNumberConstant {
                                const_type: format!("{mode:?}").to_lowercase(),
                                src: self.source.clone(),
                                span: error::position_to_span(span_start, self.position),
                            });

                            return (0.to_string(), TokenType::Number);
                        }

                        mode = ParseMode::Binary;
                        self.getc();
                        continue;
                    }
                    'x' => {
                        if mode != ParseMode::Decimal || !value.is_empty() {
                            self.error(LexerError::InvalidNumberConstant {
                                const_type: format!("{mode:?}").to_lowercase(),
                                src: self.source.clone(),
                                span: error::position_to_span(span_start, self.position),
                            });

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

                            self.warning(LexerWarning::ExtraZeros {
                                src: self.source.clone(),
                                span: error::position_to_span(span_start - 1, self.position),
                            });

                            continue;
                        }

                        if mode == ParseMode::Float {
                            let mut temp = Vec::new();
                            while self.char == '0' {
                                temp.push(self.char);
                                self.getc();
                            }

                            if !self.char.is_ascii_digit() {
                                self.warning(LexerWarning::ExtraFloatZeros {
                                    src: self.source.clone(),
                                    span: (span_start - 1, self.position - span_start + 1).into(),
                                });

                                continue;
                            }

                            temp.iter().for_each(|ch| value.push(*ch));
                        }

                        value.push('0');
                        continue;
                    }
                    _ => {
                        if value.is_empty() && self.char.is_ascii_digit() {
                            self.warning(LexerWarning::ExtraZeros {
                                src: self.source.clone(),
                                span: error::position_to_span(span_start - 1, self.position),
                            });
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
                        self.error(LexerError::InvalidNumberConstant {
                            const_type: format!("{mode:?}").to_lowercase(),
                            src: self.source.clone(),
                            span: error::position_to_span(span_start - 1, self.position + 1),
                        });

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
                    .unwrap_or_else(|err| {
                        self.error(LexerError::ConstantParserError {
                            const_type: format!("{mode:?}").to_lowercase(),
                            parser_error: err.to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(span_start, self.position),
                        });

                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Binary => (
                i64::from_str_radix(value.trim(), 2)
                    .unwrap_or_else(|err| {
                        self.error(LexerError::ConstantParserError {
                            const_type: format!("{mode:?}").to_lowercase(),
                            parser_error: err.to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(span_start, self.position),
                        });
                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Hexadecimal => (
                i64::from_str_radix(value.trim(), 16)
                    .unwrap_or_else(|err| {
                        self.error(LexerError::ConstantParserError {
                            const_type: format!("{mode:?}").to_lowercase(),
                            parser_error: err.to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(span_start, self.position),
                        });

                        0
                    })
                    .to_string(),
                TokenType::Number,
            ),
            ParseMode::Float => (
                value
                    .parse::<f64>()
                    .unwrap_or_else(|err| {
                        self.error(LexerError::ConstantParserError {
                            const_type: format!("{mode:?}").to_lowercase(),
                            parser_error: err.to_string(),
                            src: self.source.clone(),
                            span: error::position_to_span(span_start, self.position),
                        });
                        0.0
                    })
                    .to_string(),
                TokenType::FloatNumber,
            ),
        }
    }

    // Main tokenizer function

    /// Tokenizer function. <br/>
    /// - **Ok**: tuple of vector with tokens and vector with warnings
    /// - **Err**: tuple of vector with errors and vector with warnings
    pub fn tokenize(&mut self) -> Result<LexerOk, LexerErr> {
        let mut output = Vec::new();

        while !self.is_eof() {
            match self.char {
                '\n' | '\0' => self.getc(),
                _ if self.char.is_whitespace() => self.getc(),
                '/' => {
                    self.getc();
                    if self.char == '/' {
                        while self.char != '\n' && self.char != '\0' {
                            self.getc();
                        }
                        continue;
                    }
                    output.push(Token::new(
                        String::from("/"),
                        TokenType::Divide,
                        (self.position - 1, self.position),
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

                                    let character_escape = Self::character_escape(self.char);

                                    if let Some(character_escape) = character_escape {
                                        captured_string.push(character_escape);
                                        self.getc();
                                    } else {
                                        self.error(LexerError::UnknownCharacterEscape {
                                            escape: format!("\\{}", self.char),

                                            src: self.source.clone(),
                                            span: (self.position - 2, 2).into(),
                                        });
                                    }

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

                            let mut chr = self.char;

                            if chr == '\\' {
                                self.getc();
                                let character_escape = Self::character_escape(self.char)
                                    .unwrap_or_else(|| {
                                        self.error(LexerError::UnknownCharacterEscape {
                                            escape: format!("\\{}", self.char),

                                            src: self.source.clone(),
                                            span: error::position_to_span(
                                                span_start,
                                                self.position,
                                            ),
                                        });
                                        ' '
                                    });

                                chr = character_escape;
                            }

                            self.getc();

                            if self.char != '\'' {
                                self.error(LexerError::UnknownCharacterEscape {
                                    escape: format!("\\{}", self.char),

                                    src: self.source.clone(),
                                    span: error::position_to_span(span_start, self.position),
                                });
                                self.getc();
                            }

                            output.push(Token::new(
                                chr.to_string(),
                                TokenType::Char,
                                (span_start, self.position),
                            ));
                            self.getc();
                        }
                        TokenType::Equal => {
                            let span_start = self.position;
                            self.getc();

                            match self.char {
                                '=' => {
                                    output.push(Token::new(
                                        String::from("=="),
                                        TokenType::Eq,
                                        (span_start - 1, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                '>' => {
                                    output.push(Token::new(
                                        String::from("=>"),
                                        TokenType::Beq,
                                        (span_start - 1, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                '<' => {
                                    output.push(Token::new(
                                        String::from("=<"),
                                        TokenType::Leq,
                                        (span_start - 1, self.position - 1),
                                    ));
                                    self.getc();
                                }
                                _ => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start - 1, self.position);

                                    output.push(formatted_token);
                                }
                            }
                        }
                        TokenType::Lt => {
                            self.getc();

                            match self.char {
                                '<' => {
                                    output.push(Token::new(
                                        String::from("<<"),
                                        TokenType::LShift,
                                        (span_start, self.position),
                                    ));
                                    self.getc();
                                }
                                '=' => {
                                    output.push(Token::new(
                                        String::from("<="),
                                        TokenType::Leq,
                                        (span_start, self.position),
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
                                        (span_start, self.position),
                                    ));
                                    self.getc();
                                }
                                '=' => {
                                    output.push(Token::new(
                                        String::from(">="),
                                        TokenType::Beq,
                                        (span_start, self.position),
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
                                    (span_start, self.position),
                                ));
                                self.getc();
                            } else {
                                let mut formatted_token = matched_token;
                                formatted_token.span = (span_start, self.position - 1);

                                output.push(formatted_token);
                            }
                        }
                        TokenType::Ampersand => {
                            self.getc();

                            match self.char {
                                '&' => {
                                    self.getc();
                                    if self.char == ' ' {
                                        self.position -= 1;
                                        output.push(Token::new(
                                            String::from("&&"),
                                            TokenType::And,
                                            (span_start, self.position),
                                        ));
                                    } else {
                                        output.push(Token::new(
                                            String::from("&"),
                                            TokenType::Ref,
                                            (span_start, self.position - 2),
                                        ));
                                        output.push(Token::new(
                                            String::from("&"),
                                            TokenType::Ref,
                                            (span_start + 1, self.position - 1),
                                        ));
                                    }
                                }
                                ' ' => {
                                    let mut formatted_token = matched_token;
                                    formatted_token.span = (span_start, self.position - 1);

                                    output.push(formatted_token);
                                }
                                _ => {
                                    output.push(Token::new(
                                        String::from("&"),
                                        TokenType::Ref,
                                        (span_start, self.position - 1),
                                    ));
                                }
                            }
                        }
                        _ => {
                            let mut formatted_token = matched_token;
                            formatted_token.span = (span_start, span_start + 1);

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
                    self.error(LexerError::UnknownCharacter {
                        character: self.char,
                        src: self.source.clone(),
                        span: (self.position - 1, 1).into(),
                    });
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getc_test() {
        let mut lexer = Lexer::new("123456789", "test.dn");

        let mut chr = lexer.char;
        let mut current = 1;

        while chr != '\0' {
            assert_eq!(chr, char::from_digit(current, 10).unwrap_or(' '));
            lexer.getc();

            current += 1;
            chr = lexer.char;
        }
    }

    #[test]
    fn is_eof_test() {
        let mut lexer = Lexer::new("1", "test.dn");
        lexer.getc();

        assert!(lexer.is_eof());
    }

    #[test]
    fn is_hexadecimal_literal_test() {
        let lexer = Lexer::new("", "test.dn");

        ['a', 'b', 'c', 'd', 'e', 'f']
            .iter()
            .for_each(|chr| assert!(lexer.is_hexadecimal_literal(chr)));

        assert!(!lexer.is_hexadecimal_literal(&' '));
    }

    #[test]
    fn character_escape_test() {
        [
            ('0', '\0'),
            ('n', '\n'),
            ('r', '\r'),
            ('t', '\t'),
            ('\\', '\\'),
        ]
        .into_iter()
        .for_each(|(chr, exp)| assert_eq!(Lexer::character_escape(chr), Some(exp)));

        assert!(Lexer::character_escape(' ').is_none())
    }
}
