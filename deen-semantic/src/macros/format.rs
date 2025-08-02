use super::MacroObject;
use crate::{Analyzer, error::{self, SemanticError}};
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
                SemanticError::ArgumentException {
                    exception: format!("not enough arguments: expected {}, found {}", MINIMUM_ARGUMENTS_LEN, arguments.len()),
                    help: None,
                    src: analyzer.source.clone(),
                    span: error::position_to_span(*span)
                }
            );
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
                            SemanticError::FormatError {
                                exception: format!("unknown binding in literal found"),
                                help: Some(format!("Consider using right bindings syntax with curly brackets")),
                                src: analyzer.source.clone(),
                                span: error::position_to_span(*literal_span)
                            }
                        ),
                    }
                }

                cursor += 1;
            }

            if arguments.len() != bindings.len() + 1 {
                analyzer.error(SemanticError::ArgumentException {
                    exception: format!("expected {} arguments, but found {}", bindings.len() + 1, arguments.len()),
                    help: None,
                    src: analyzer.source.clone(),
                    span: error::position_to_span(*span)
                });
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
                                SemanticError::IllegalImplementation {
                                    exception: format!("type `{expr_type}` has wrong implementation for display"),
                                    help: Some(format!("Consider using right format: {DISPLAY_IMPLEMENTATION_FORMAT}")),
                                    src: analyzer.source.clone(),
                                    span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                                }
                            );
                        }
                    } else {
                        analyzer.error(
                            SemanticError::IllegalImplementation {
                                exception: format!("type `{expr_type}` has no implementation for display"),
                                help: Some(format!("Consider implementing necessary method: {DISPLAY_IMPLEMENTATION_FORMAT}")),
                                src: analyzer.source.clone(),
                                span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                            }
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
                                    SemanticError::IllegalImplementation {
                                        exception: format!("type `{expr_type}` has wrong implementation for display"),
                                        help: Some(format!("Consider using right format: {DISPLAY_IMPLEMENTATION_FORMAT}")),
                                        src: analyzer.source.clone(),
                                        span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                                    }
                                );
                            }
                        } else {
                        analyzer.error(
                            SemanticError::IllegalImplementation {
                                exception: format!("type `{expr_type}` has no implementation for display"),
                                help: Some(format!("Consider implementing necessary method: {DISPLAY_IMPLEMENTATION_FORMAT}")),
                                src: analyzer.source.clone(),
                                span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                            }
                        );
                        }
                    } else if analyzer.scope.get_enum(&alias).is_none() {
                        analyzer.error(SemanticError::UnknownObject {
                            exception: format!("no displayable type `{expr_type}` found"),
                            help: None,
                            src: analyzer.source.clone(),
                            span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                        });
                    }
                }

                _ => {
                    analyzer.error(SemanticError::UnsupportedType {
                        exception: format!("type `{expr_type}` is not supported for display"),
                        help: None,
                        src: analyzer.source.clone(),
                        span: error::position_to_span(deen_parser::Parser::get_span_expression(&expr))
                    });
                }
            }
        });

        return_type
    }
}
