//! ## Error Handling Module
//! Here you can find structures of custom defined errors.
//!
//! All structures implements [`miette::Diagnostic`] and [`thiserror::Error`]

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// Simple function to convert position range to miette span
pub fn position_to_span(from: usize, to: usize) -> SourceSpan {
    (from, to - from).into()
}

#[derive(Debug, Error, Diagnostic, Clone)]
pub enum LexerError {
    #[error("Invalid number constant")]
    #[diagnostic(
        severity(Error),
        code(deen::lexer::invalid_constant),
        help("Check the spelling of the constant")
    )]
    InvalidNumberConstant {
        const_type: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("expected {const_type} constant")]
        span: SourceSpan
    },

    #[error("Constant parser returned error: {parser_error}")]
    #[diagnostic(
        severity(Error),
        code(deen::lexer::constant_error),
    )]
    ConstantParserError {
        const_type: String,
        parser_error: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("constant type: {const_type}")]
        span: SourceSpan
    },

    #[error("Unknown character escape: {escape}")]
    #[diagnostic(
        severity(Error),
        code(deen::lexer::constant_error),
        help("If you meant to write a literal backslash, consider using its double version: '\\\\'")
    )]
    UnknownCharacterEscape {
        escape: String,
        
        #[source_code]
        src: NamedSource<String>,
        #[label("unknown character escape literal")]
        span: SourceSpan
    },

    #[error("Unknown character found: '{character}'")]
    #[diagnostic(
        severity(Error),
        code(deen::lexer::constant_error),
    )]
    UnknownCharacter {
        character: char,

        #[source_code]
        src: NamedSource<String>,
        #[label("unsupported character")]
        span: SourceSpan
    }
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("Warning: {message}")]
#[diagnostic(severity(Warning), code(deen::lexer))]
pub struct LexerWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan,
}
