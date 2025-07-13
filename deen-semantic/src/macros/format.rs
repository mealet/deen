use super::MacroObject;
use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type, value::Value};

/// **Formats literal and args into single string**
/// `format!(LITERAL, ...)` -> `*char`
#[derive(Debug, Clone)]
pub struct FormatMacro;
impl MacroObject for FormatMacro {
    fn verify_call(
        &self,
        analyzer: &mut Analyzer,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        const DISPLAY_IMPLEMENTATION_FORMAT: &str = "fn display(&self) *char";
        const MINIMUM_ARGUMENTS_LEN: usize = 1;
        let return_type: Type = Type::Pointer(Box::new(Type::Char));

        if arguments.len() < MINIMUM_ARGUMENTS_LEN {
            analyzer.error(
                format!(
                    "Not enough arguments: expected {}, found {}",
                    MINIMUM_ARGUMENTS_LEN,
                    arguments.len()
                ),
                *span,
            );
            return return_type;
        }

        if let Some(Expressions::Value(Value::String(literal), literal_span)) = arguments.first() {
            let mut bindings: Vec<Type> = Vec::new();
            let mut cursor = 0;
            let characters = literal.chars().collect::<Vec<char>>();

            while characters.get(cursor).is_some() {
                if let Some('{') = characters.get(cursor) {
                    cursor += 1;
                    let next = characters.get(cursor);

                    match next {
                        Some('}') => bindings.push(Type::Void),
                        _ => analyzer.error(
                            String::from("Unexpected binding in string found"),
                            *literal_span,
                        ),
                    }
                }

                cursor += 1;
            }

            if arguments.len() != bindings.len() + 1 {
                analyzer.error(
                    format!(
                        "Expected {} arguments, but found {}",
                        bindings.len() + 1,
                        arguments.len()
                    ),
                    *span,
                );
                return return_type;
            }
        }

        let arguments_iterator = arguments.iter();

        arguments_iterator.skip(1).for_each(|expr| {
            let expr_type = analyzer.visit_expression(expr, None);

            match expr_type.clone() {
                int if Analyzer::is_integer(&int) => {}
                float if Analyzer::is_float(&float) => {}
                Type::Bool => {}
                Type::Enum(_, _) => {}
                Type::Char => {}

                Type::Pointer(_) => {}
                Type::Struct(_, functions) => {
                    if let Some(Type::Function(_, return_type, _)) = functions.get("display") {
                        if let Type::Pointer(ptr) = *return_type.clone()
                            && *ptr.clone() == Type::Char
                            && arguments.len() == 2
                        {
                        } else {
                            analyzer.error(
                                format!(
                                    "Implementation for DISPLAY must be: {DISPLAY_IMPLEMENTATION_FORMAT}"
                                ),
                                deen_parser::Parser::get_span_expression(expr.clone()),
                            );
                        }
                    } else {
                        analyzer.error(
                            format!(
                                "Type `{expr_type}` has no implementation for DISPLAY: {DISPLAY_IMPLEMENTATION_FORMAT}"
                            ),
                            deen_parser::Parser::get_span_expression(expr.clone()),
                        );
                    }
                }
                Type::Alias(alias) => {
                    if let Some(Type::Struct(_, functions)) = analyzer.scope.get_struct(&alias) {
                        if let Some(Type::Function(_, return_type, _)) = functions.get("display") {
                            if let Type::Pointer(ptr) = *return_type.clone()
                                && *ptr.clone() == Type::Char
                                && arguments.len() == 2
                            {
                            } else {
                                analyzer.error(
                                    format!(
                                        "Implementation for DISPLAY must be: {DISPLAY_IMPLEMENTATION_FORMAT}"
                                    ),
                                    deen_parser::Parser::get_span_expression(expr.clone()),
                                );
                            }
                        } else {
                            analyzer.error(
                                format!(
                                    "Type `{expr_type}` has no implementation for DISPLAY: {DISPLAY_IMPLEMENTATION_FORMAT}"
                                ),
                                deen_parser::Parser::get_span_expression(expr.clone()),
                            );
                        }
                    } else if analyzer.scope.get_enum(&alias).is_none() {
                        analyzer.error(
                            format!("No displayable type with name `{expr_type}` found"),
                            deen_parser::Parser::get_span_expression(expr.clone()),
                        );
                    }
                }

                _ => {
                    analyzer.error(
                        format!("Type `{expr_type}` is not supported for display"),
                        deen_parser::Parser::get_span_expression(expr.clone()),
                    );
                }
            }
        });

        return_type
    }
}
