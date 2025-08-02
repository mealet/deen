use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// Simple function to convert position range to miette span
pub fn position_to_span(span: (usize, usize)) -> SourceSpan {
    let (from, to) = span;
    (from, to.wrapping_sub(from)).into()
}

/// Parser Errors
#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum ParserError {
    #[error("Datatype exception found")]
    #[diagnostic(
        severity(Error),
        code(deen::parser::datatype_exception),
        help("{help}")
    )]
    DatatypeException {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Keyword exception found")]
    #[diagnostic(severity(Error), code(deen::parser::keyword_exception), help("{help}"))]
    KeywordException {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unclosed expression found")]
    #[diagnostic(
        severity(Error),
        code(deen::parser::unclosed_expression),
        help("{help}")
    )]
    UnclosedExpression {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unknown expression found")]
    #[diagnostic(
        severity(Error),
        code(deen::parser::unknown_expression),
        help("{help}")
    )]
    UnknownExpression {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unsupported expression found")]
    #[diagnostic(
        severity(Error),
        code(deen::parser::unsupported_expression),
        help("{help}")
    )]
    UnsupportedExpression {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Changing the visibility of an object is not allowed")]
    #[diagnostic(severity(Error), code(deen::parser::visibility_error), help("{help}"))]
    VisibilityError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Syntax error")]
    #[diagnostic(severity(Error), code(deen::parser::syntax_error), help("{help}"))]
    SyntaxError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Wrong declaration syntax")]
    #[diagnostic(severity(Error), code(deen::parser::declaration_error), help("{help}"))]
    DeclarationError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
#[error("Warning: {message}")]
#[diagnostic(code(deen::parser), severity(Warning))]
pub struct ParserWarning {
    pub message: String,
    #[source_code]
    pub src: NamedSource<String>,
    #[label("here")]
    pub span: SourceSpan,
}
