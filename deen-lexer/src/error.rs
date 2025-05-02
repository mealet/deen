use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("Error: {message}")]
#[diagnostic(severity(Error), code(deen::lexer))]
pub struct LexerError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan,
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
