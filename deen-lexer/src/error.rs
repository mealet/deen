use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("Error: {message}")]
#[diagnostic(
    severity(Error)
)]
pub struct LexerError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}

#[derive(Debug, Error, Diagnostic, Clone)]
#[error("Warning: {message}")]
#[diagnostic(severity(Warning))]
pub struct LexerWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}
