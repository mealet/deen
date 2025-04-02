#![allow(unused)]

use miette::{NamedSource, Diagnostic};
use std::collections::HashMap;
use crate::{
    token::Token,
    token_type::TokenType,
    macros::{std_keyword, std_type, std_symbol, std_token}
};

mod error;
mod macros;
mod token;
mod token_type;

pub struct Lexer {
    source: NamedSource<String>,
    
    std_symbols: HashMap<char, Token>,
    std_words: HashMap<String, Token>,
    errors: Vec<Box<dyn Diagnostic>>,

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

    fn getc(&mut self) {
        if self.position < self.input.len() {
            self.char = self.input[self.position];
            self.position += 1;
        } else {
            self.char = '\0'
        }
    }
}
