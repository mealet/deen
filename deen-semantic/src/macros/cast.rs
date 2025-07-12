use super::MacroObject;
use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type};

/// **Converts expression to provided type**
/// `cast!(EXPRESSION, TYPE)` -> `usize`
#[derive(Debug, Clone)]
pub struct CastMacro;
impl MacroObject for CastMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        const MINIMUM_ARGUMENTS_LEN: usize = 2;

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

        if matches!(
            arguments.first(),
            Some(Expressions::Argument {
                name: _,
                r#type: _,
                span: _
            })
        ) || !matches!(
            arguments.get(1),
            Some(Expressions::Argument {
                name: _,
                r#type: _,
                span: _
            })
        ) {
            analyzer.error(
                "Wrong macro call! Usage: cast!(EXPRESSION, TYPE)".to_string(),
                *span,
            );
        }

        let (from_type, target_type) = (
            analyzer.visit_expression(&arguments[0], None),
            analyzer.visit_expression(&arguments[1], None),
        );
        analyzer
            .verify_cast(&from_type, &target_type)
            .unwrap_or_else(|err| {
                analyzer.error(err, *span);
            });

        target_type
    }
}
