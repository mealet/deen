use deen_lexer::{Lexer, token::Token, token_type::TokenType};

#[test]
fn binary_symbols_test() {
    let mut lexer = Lexer::new("+-*/", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from("+"), TokenType::Plus, (0, 1)),
            Token::new(String::from("-"), TokenType::Minus, (1, 2)),
            Token::new(String::from("*"), TokenType::Multiply, (2, 3)),
            Token::new(String::from("/"), TokenType::Divide, (3, 4)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}

#[test]
fn boolean_symbols_test() {
    let mut lexer = Lexer::new("> < ! && ||", "test.dn");
    let tokens = lexer.tokenize().unwrap().0;

    assert_eq!(
        tokens,
        vec![
            Token::new(String::from(">"), TokenType::Bt, (0, 1)),
            Token::new(String::from("<"), TokenType::Lt, (2, 3)),
            Token::new(String::from("!"), TokenType::Not, (4, 5)),
            Token::new(String::from("&&"), TokenType::And, (6, 8)),
            Token::new(String::from("||"), TokenType::Or, (9, 11)),
            Token::new(String::new(), TokenType::EOF, (0, 0))
        ]
    )
}
