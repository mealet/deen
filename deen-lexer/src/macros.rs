macro_rules! std_symbol {
    ($ch: literal, $typ: expr) => {
        ($ch, Token::new(String::from($ch), $typ, (0, 0)))
    };
}

macro_rules! std_keyword {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new($name.to_string(), TokenType::Keyword, (0, 0)),
        )
    };
}

macro_rules! std_type {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new($name.to_string(), TokenType::Type, (0, 0)),
        )
    };
}

macro_rules! std_token {
    ($name: literal, $value: expr) => {
        (
            $name.to_string(),
            Token::new($name.to_string(), $value, (0, 0)),
        )
    };
}

pub(crate) use std_keyword;
pub(crate) use std_symbol;
pub(crate) use std_token;
pub(crate) use std_type;
