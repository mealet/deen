use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Error: {message}")]
#[diagnostic(code(deen::semantic), severity(Error))]
pub struct SemanticError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan,
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Warning: {message}")]
#[diagnostic(code(deen::semantic), severity(Warning))]
pub struct SemanticWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan,
}

impl From<deen_lexer::error::LexerError> for SemanticError {
    fn from(_value: deen_lexer::error::LexerError) -> Self {
        Self {
            message: Default::default(),
            src: NamedSource::new("", "".to_string()),
            span: (0, 0).into(),
        }
    }
}

impl From<deen_lexer::error::LexerWarning> for SemanticWarning {
    fn from(value: deen_lexer::error::LexerWarning) -> Self {
        Self {
            message: value.message,
            src: value.src,
            span: value.span,
        }
    }
}

impl From<deen_parser::error::ParserError> for SemanticError {
    fn from(value: deen_parser::error::ParserError) -> Self {
        Self {
            message: value.message,
            src: value.src,
            span: value.span,
        }
    }
}

impl From<deen_parser::error::ParserWarning> for SemanticWarning {
    fn from(value: deen_parser::error::ParserWarning) -> Self {
        Self {
            message: value.message,
            src: value.src,
            span: value.span,
        }
    }
}
