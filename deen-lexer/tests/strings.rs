use deen_lexer::{Lexer, token::Token, token_type::TokenType};

#[test]
fn basic_string() {
    let mut lexer = Lexer::new("\"hello\"", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("hello"), TokenType::String, (0, 7)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn big_string() {
    let mut lexer = Lexer::new(
        "\"Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve\"",
        "test.dn",
    );
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(
                String::from(
                    "Hello, World! Here's an interesting thing: first LLVM initial release was in 2003 year. The original authors of core was Chris Lattner and Vikram Adve"
                ),
                TokenType::String,
                (0, 152)
            ),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn advanced_string() {
    let mut lexer = Lexer::new("\"¿?👉👈🤠👀\"", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("¿?👉👈🤠👀"), TokenType::String, (0, 21)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn basic_char() {
    let mut lexer = Lexer::new("'a'", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("a"), TokenType::Char, (0, 3)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn advanced_char() {
    let mut lexer = Lexer::new("'👀'", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("👀"), TokenType::Char, (0, 3)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}
