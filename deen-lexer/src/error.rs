//! ## Error Handling Module
//! Here you can find structures of custom defined errors.
//!
//! All structures implements [`miette::Diagnostic`] and [`thiserror::Error`]

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// Simple function to convert position range to miette span
pub fn position_to_span(from: usize, to: usize) -> SourceSpan {
    (from, to.wrapping_sub(from)).into()
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
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
        span: SourceSpan,
    },

    #[error("Constant parser returned error: {parser_error}")]
    #[diagnostic(severity(Error), code(deen::lexer::constant_error))]
    ConstantParserError {
        const_type: String,
        parser_error: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("constant type: {const_type}")]
        span: SourceSpan,
    },

    #[error("Unknown character escape: {escape}")]
    #[diagnostic(
        severity(Error),
        code(deen::lexer::literal_error),
        help(
            "If you meant to write a literal backslash, consider using its double version: '\\\\'"
        )
    )]
    UnknownCharacterEscape {
        escape: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("unknown character escape literal")]
        span: SourceSpan,
    },

    #[error("Unknown character found: '{character}'")]
    #[diagnostic(severity(Error), code(deen::lexer::unknown))]
    UnknownCharacter {
        character: char,

        #[source_code]
        src: NamedSource<String>,
        #[label("unsupported character")]
        span: SourceSpan,
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum LexerWarning {
    #[error("Extra zeros in constant")]
    #[diagnostic(
        severity(Warning),
        code(deen::lexer::extra_zeros),
        help("Consider removing unnecessary zeros")
    )]
    ExtraZeros {
        #[source_code]
        src: NamedSource<String>,
        #[label("extra zeros here")]
        span: SourceSpan,
    },

    #[error("Extra zeros in floating number")]
    #[diagnostic(
        severity(Warning),
        code(deen::lexer::extra_float_zeros),
        help("Consider removing unnecessary zeros")
    )]
    ExtraFloatZeros {
        #[source_code]
        src: NamedSource<String>,
        #[label("extra zeros at the end")]
        span: SourceSpan,
    },
}
