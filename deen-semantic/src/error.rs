use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Error: {message}")]
#[diagnostic(
    code(deen::semantic),
    severity(Error)
)]
pub struct SemanticError {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Warning: {message}")]
#[diagnostic(
    code(deen::semantic),
    severity(Warning)
)]
pub struct SemanticWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan
}
