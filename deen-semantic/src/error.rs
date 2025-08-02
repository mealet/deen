use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

pub fn position_to_span(span: (usize, usize)) -> SourceSpan {
    let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });
    span.into()
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticError {
    #[error("Lexical Analyzer error")]
    ModuleLexerError(#[from] deen_lexer::error::LexerError),

    #[error("Syntax Analyzer error")]
    ModuleParserError(#[from] deen_parser::error::ParserError),

    #[error("{message}")]
    #[diagnostic(severity(Error), code(deen::parser::global_error))]
    GlobalError {
        message: String,
        #[help]
        help: Option<String>,
        src: NamedSource<String>,
    },

    #[error("Argument exception found")]
    #[diagnostic(severity(Error), code(deen::semantics::argument_exception))]
    ArgumentException {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("This feature is currently disabled by @compiler")]
    #[diagnostic(severity(Error), code(deen::semantics::disabled_feature))]
    DisabledFeature {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("String format error catched")]
    #[diagnostic(severity(Error), code(deen::semantics::format_error))]
    FormatError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Illegal method implementation found")]
    #[diagnostic(
        severity(Error),
        code(deen::semantics::illegal_implementation),
        url("deen-docs.vercel.app/advanced/structures-implementations.html")
    )]
    IllegalImplementation {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("IO caused exception")]
    #[diagnostic(severity(Error), code(deen::semantics::io_error))]
    IoError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("`main` function limitations violation")]
    #[diagnostic(severity(Error), code(deen::semantics::main_error))]
    MainFunctionError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Required fields are missing")]
    #[diagnostic(severity(Error), code(deen::semantics::missing_fields))]
    MissingFields {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Operator usage caused exception")]
    #[diagnostic(severity(Error), code(deen::semantics::operator_exception))]
    OperatorException {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Value is out of allowed range")]
    #[diagnostic(severity(Error), code(deen::semantics::range_overflow))]
    RangeOverflow {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Redefinition of reserved identifier found")]
    #[diagnostic(severity(Error), code(deen::semantics::redefinition_error))]
    RedefinitionError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Main semantics rules violation")]
    #[diagnostic(severity(Error), code(deen::semantics::semantic_error))]
    SemanticalError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Expression's types mismatched")]
    #[diagnostic(severity(Error), code(deen::semantics::types_mismatch))]
    TypesMismatch {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unable to resolve provided name")]
    #[diagnostic(severity(Error), code(deen::semantics::unresolved_name))]
    UnresolvedName {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Expression is not supported in this context")]
    #[diagnostic(severity(Error), code(deen::semantics::unsupported_expression))]
    UnsupportedExpression {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Type is not supported here")]
    #[diagnostic(severity(Error), code(deen::semantics::unsupported_type))]
    UnsupportedType {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Unknown object catched")]
    #[diagnostic(severity(Error), code(deen::semantics::unknown_object))]
    UnknownObject {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Visibility rules are being violated")]
    #[diagnostic(severity(Error), code(deen::semantics::visibility_error))]
    VisibilityError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },

    #[error("Value compilation caused error")]
    #[diagnostic(severity(Error), code(deen::semantics::value_error))]
    ValueError {
        exception: String,
        #[help]
        help: Option<String>,
        #[source_code]
        src: NamedSource<String>,
        #[label("{exception}")]
        span: SourceSpan,
    },
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum SemanticWarning {
    #[error("Lexical Analyzer warning")]
    #[diagnostic(severity(Warning))]
    ModuleLexerWarning(#[from] deen_lexer::error::LexerWarning),

    #[error("Syntax Analyzer warning")]
    #[diagnostic(severity(Warning))]
    ModuleParserWarning(#[from] deen_parser::error::ParserWarning),

    #[error("Unused variable `{varname}` found")]
    #[diagnostic(
        severity(Warning),
        code(deen::semantics::unused_variable),
        help("Consider removing unused variable")
    )]
    UnusedVariable {
        varname: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("variable is defined here")]
        span: SourceSpan,
    },

    #[error("Unused expression result found")]
    #[diagnostic(
        severity(Warning),
        code(deen::semantics::unused_result),
        help("Consider assigning result to a variable")
    )]
    UnusedResult {
        message: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{message}")]
        span: SourceSpan,
    },
}
