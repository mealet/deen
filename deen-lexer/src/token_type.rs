
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    Identifier, // abc
    Keyword, // let
    Type, // i32

    Number, // 123
    String, // "hello"
    Char, // 'c'
    Boolean, // true/false

    Equal, // =
    Plus, // +
    Minus, // -
    Multiply, // *
    Divide, // /

    Lt, // <
    Bt, // >
    Eq, // ==
    Ne, // !=
    Or, // ||
    And, // &&
    Not, // !

    Semicolon, // ;
    Ampersand, // &
    Ref, // &abc
    Verbar, // |
    Dot, // .
    Comma, // ,
    DoubleQuote, // "
    SingleQuote, // '
    
    LShift, // <<
    RShift, // >>
    Xor, // ^

    LParen, // (
    RParen, // )

    LBrace, // {
    RBrace, // }

    LBrack, // [
    RBrack, // ]

    EOF // \0
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
