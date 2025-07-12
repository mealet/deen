use super::MacroObject;
use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type};

/// **Returns size of provided type / expression**
/// `sizeof!(TYPE / EXPRESSION)` -> `usize`
#[derive(Debug, Clone)]
pub struct SizeofMacro;
impl MacroObject for SizeofMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        const MINIMUM_ARGUMENTS_LEN: usize = 1;
        const RETURN_TYPE: Type = Type::USIZE;

        if arguments.len() < MINIMUM_ARGUMENTS_LEN {
            analyzer.error(
                format!(
                    "Not enough arguments: expected {}, found {}",
                    MINIMUM_ARGUMENTS_LEN,
                    arguments.len()
                ),
                *span,
            );
        }

        RETURN_TYPE
    }
}
