use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Error: {message}")]
#[diagnostic(
    severity(Error)
)]
pub struct ParserError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Warning: {message}")]
#[diagnostic(severity(Warning))]
pub struct ParserWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}
