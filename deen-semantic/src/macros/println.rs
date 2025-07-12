use super::MacroObject;
use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type};

/// **Prints formatted string to standard output with new line**
/// `println!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintlnMacro;
impl MacroObject for PrintlnMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        let _ = super::FormatMacro.verify_call(analyzer, arguments, span);
        Type::Void
    }
}
