use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticErrors {
    #[error("Lexical Analyzer error")]
    ModuleLexerError(#[from] deen_lexer::error::LexerError),

    #[error("Syntax Analyzer error")]
    ModuleParserError(#[from] deen_parser::error::ParserError),

    #[error("Argument exception found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::argument_exception),
        help("{help}")
    )]
    ArgumentException {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("This feature is currently disabled")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::disabled_feature),
        help("{help}")
    )]
    DisabledFeature {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("String format error catched")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::format_error),
        help("{help}")
    )]
    FormatError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Illegal method implementation found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::illegal_implementation),
        help("{help}")
    )]
    IllegalImplementation {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("IO caused exception")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::io_error),
        help("{help}")
    )]
    IoError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("`main` function limitations violation")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::main_error),
        help("{help}")
    )]
    MainFunctionError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Required fields are missing")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::missing_fields),
        help("{help}")
    )]
    MissingFields {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Operator usage caused exception")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::operator_exception),
        help("{help}")
    )]
    OperatorException {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Value is out of allowed range")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::range_overflow),
        help("{help}")
    )]
    RangeOverflow {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Redefinition of reserved identifier found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::redefinition_error),
        help("{help}")
    )]
    RedefinitionError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Main semantics rules violation")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::semantic_error),
        help("{help}")
    )]
    SemanticError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Expression's types mismatched")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::types_mismatch),
        help("{help}")
    )]
    TypesMismatch {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Unable to resolve provided name")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::unresolved_name),
        help("{help}")
    )]
    UnresolvedName {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Expression is not supported in this context")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::unsupported_expression),
        help("{help}")
    )]
    UnsupportedExpression {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Type is not supported here")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::unsupported_type),
        help("{help}")
    )]
    UnsupportedType {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Unknown object catched")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::unknown_object),
        help("{help}")
    )]
    UnknownObject {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },

    #[error("Visibility rules are being violated")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::visibility_error),
        help("{help}")
    )]
    VisibilityError {
        exception: String,
        help: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticWarnings {
    #[error("Lexical Analyzer warning")]
    #[diagnostic(
        severity(Warning)
    )]
    ModuleLexerWarning(#[from] deen_lexer::error::LexerWarning),

    #[error("Syntax Analyzer warning")]
    #[diagnostic(
        severity(Warning)
    )]
    ModuleParserWarning(#[from] deen_parser::error::ParserWarning),

    #[error("Unused variable `{varname}` found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::visibility_error),
        help("Consider removing unused variable")
    )]
    UnusedVariable {
        varname: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("variable is defined here")]
        span: SourceSpan
    },

    #[error("Unused expression result found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::visibility_error),
        help("Consider assigning result to a variable")
    )]
    UnusedResult {
        varname: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("unused expression here")]
        span: SourceSpan
    },
}

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
    fn from(_value: deen_lexer::error::LexerWarning) -> Self {
        Self {
            message: Default::default(),
            src: NamedSource::new("", "".to_string()),
            span: (0, 0).into(),
        }
    }
}

impl From<deen_parser::error::ParserError> for SemanticError {
    fn from(_value: deen_parser::error::ParserError) -> Self {
        Self {
            message: Default::default(),
            src: NamedSource::new("", "".to_string()),
            span: (0, 0).into(),
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
