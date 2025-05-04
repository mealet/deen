use deen_lexer::{Lexer, token_type::TokenType};

#[test]
fn basic_types() {
    let input = String::from("i8 i16 i32 i64 u8 u16 u32 u64 usize string char bool void");
    let mut lexer = Lexer::new(&input, "tests.dn");

    let tokens = lexer.tokenize().unwrap().0;
    let expected = vec![
        (TokenType::Type, String::from("i8")),
        (TokenType::Type, String::from("i16")),
        (TokenType::Type, String::from("i32")),
        (TokenType::Type, String::from("i64")),
        (TokenType::Type, String::from("u8")),
        (TokenType::Type, String::from("u16")),
        (TokenType::Type, String::from("u32")),
        (TokenType::Type, String::from("u64")),
        (TokenType::Type, String::from("usize")),
        (TokenType::Type, String::from("string")),
        (TokenType::Type, String::from("char")),
        (TokenType::Type, String::from("bool")),
        (TokenType::Type, String::from("void")),
    ];

    tokens.iter().zip(expected).for_each(|(token, expected)| {
        assert_eq!(token.token_type, expected.0);
        assert_eq!(token.value, expected.1);
    });
}

#[test]
fn boolean_keywords() {
    let input = String::from("true false");
    let mut lexer = Lexer::new(&input, "tests.dn");

    let tokens = lexer.tokenize().unwrap().0;
    let expected = vec![
        (TokenType::Boolean, String::from("true")),
        (TokenType::Boolean, String::from("false")),
    ];

    tokens.iter().zip(expected).for_each(|(token, expected)| {
        assert_eq!(token.token_type, expected.0);
        assert_eq!(token.value, expected.1);
    });
}

#[test]
fn main_keywords() {
    let input = String::from("let fn import return struct enum typedef");
    let mut lexer = Lexer::new(&input, "tests.dn");

    let tokens = lexer.tokenize().unwrap().0;
    let expected = vec![
        (TokenType::Keyword, String::from("let")),
        (TokenType::Keyword, String::from("fn")),
        (TokenType::Keyword, String::from("import")),
        (TokenType::Keyword, String::from("return")),
        (TokenType::Keyword, String::from("struct")),
        (TokenType::Keyword, String::from("enum")),
        (TokenType::Keyword, String::from("typedef")),
    ];

    tokens.iter().zip(expected).for_each(|(token, expected)| {
        assert_eq!(token.token_type, expected.0);
        assert_eq!(token.value, expected.1);
    });
}

#[test]
fn constructions_keywords() {
    let input = String::from("if else while for break");
    let mut lexer = Lexer::new(&input, "tests.dn");

    let tokens = lexer.tokenize().unwrap().0;
    let expected = vec![
        (TokenType::Keyword, String::from("if")),
        (TokenType::Keyword, String::from("else")),
        (TokenType::Keyword, String::from("while")),
        (TokenType::Keyword, String::from("for")),
        (TokenType::Keyword, String::from("break")),
    ];

    tokens.iter().zip(expected).for_each(|(token, expected)| {
        assert_eq!(token.token_type, expected.0);
        assert_eq!(token.value, expected.1);
    });
}
