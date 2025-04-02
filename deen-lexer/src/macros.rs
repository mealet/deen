macro_rules! std_symbol {
    ($ch: literal, $typ: expr) => {
        ($ch, Token::new($typ, String::from($ch), (0, 0)))
    }
}

macro_rules! std_keyword {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new(TokenType::Keyword, $name.to_string(), (0, 0))
        )
    }
}

macro_rules! std_type {
    ($name: literal) => {
        (
            $name.to_string(),
            Token::new(TokenType::Type, $name.to_string(), (0, 0))
        )
    }
}

macro_rules! std_token {
    ($name: literal, $value: expr) => {
        ($name.to_string(), Token::new($value, $name.to_string(), (0, 0)))
    }
}

pub(crate) use std_symbol;
pub(crate) use std_keyword;
pub(crate) use std_type;
pub(crate) use std_token;
