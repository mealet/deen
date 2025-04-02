use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

// Structs

#[derive(Error, Diagnostic, Debug)]
#[error("Undefined character found")]
#[diagnostic(
        code(lexer::undefined_char),
        help("try to replace this char with another"),
    )
]
pub struct UndefinedChar {
    #[source_code]
    pub src: NamedSource<String>,

    #[label("here")]
    pub at: SourceSpan
}
