use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
#[error("Extra zeroes at constant number")]
#[diagnostic(
        code(lexer::extra_zeroes),
        help("remove unused zeroes from number"),
        severity(Warning)
    )
]
pub struct ExtraZeroes {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}
