use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

// enum

#[derive(Debug, Error, Diagnostic)]
#[error("Error: {message}")]
#[diagnostic(severity(Error))]
pub struct LexerError {
    message: String,
    #[source_code]
    src: NamedSource<String>,
    #[label("here")]
    span: SourceSpan
}

// Erorrs

#[derive(Error, Diagnostic, Debug)]
#[error("Undefined character found")]
#[diagnostic(
        code(lexer::undefined_char),
        help("try to replace this char with another"),
    )
]
pub struct UndefinedChar {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}

// ---------------------------

#[derive(Error, Diagnostic, Debug)]
#[error("Unexpected binary/hexadecimal number found")]
#[diagnostic(
        code(lexer::wrong_const),
    )
]
pub struct WrongConst {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}

// ---------------------------

#[derive(Error, Diagnostic, Debug)]
#[error("IO Module returned error while parsing constant")]
#[diagnostic(
        code(lexer::parse_errror),
    )
]
pub struct ParseError {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}

// ---------------------------

#[derive(Error, Diagnostic, Debug)]
#[error("Unexpected character escape found")]
#[diagnostic(
        code(lexer::char_escape),
    )
]
pub struct CharEscape {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}
