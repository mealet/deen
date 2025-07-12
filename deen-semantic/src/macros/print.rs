use super::MacroObject;
use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type};

/// **Prints formatted string to standard output**
/// `print!(LITERAL, ...)` -> `void`
#[derive(Debug, Clone)]
pub struct PrintMacro;
impl MacroObject for PrintMacro {
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
