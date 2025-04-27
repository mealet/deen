#![allow(unused)]

use crate::{
    scope::Scope,
    import::Import,
    error::{SemanticError, SemanticWarning}
};
use deen_parser::{expressions::Expressions, statements::Statements, types::Type, value::Value, Parser};
use std::collections::HashMap;
use miette::NamedSource;

mod error;
mod scope;
pub mod import;

type SemanticOk = (HashMap<String, Import>, Vec<SemanticWarning>);
type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,

    imports: HashMap<String, Import>
}

impl Analyzer {
    pub fn new(src: &str, filename: &str, is_main: bool) -> Self {
        Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                scope
            },
            source: NamedSource::new(filename, src.to_owned()),

            errors: Vec::new(),
            warnings: Vec::new(),

            imports: HashMap::new(),
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        for statement in ast {
            self.visit_statement(statement);
        }

        if self.scope.get_fn("main").is_none() && self.scope.is_main {
            let err = SemanticError {
                message: String::from("Program has no entry `main` function"),
                src: self.source.clone(),
                span: 0.into()
            };

            self.errors.reverse();
            self.errors.push(err);
            self.errors.reverse();
        }

        if let Some(unused) = self.scope.check_unused_variables() {
            unused.iter().for_each(|var| {
                self.warning(
                    format!("Unused variable `{}` found", var.0),
                    var.1
                );
            });
        }


        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()))
        }

        Ok(
            (self.imports.clone(), self.warnings.clone())
        )
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });

        self.errors.push(
            error::SemanticError {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        );
    }

    #[allow(unused)]
    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });

        self.warnings.push(
            error::SemanticWarning {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        )
    }
}

impl Analyzer {
    fn visit_statement(&mut self, statement: &Statements) {
        // checking for allowed global scope statements
        if self.scope.parent.is_none() {
            match statement {
                Statements::FunctionDefineStatement { name: _, datatype: _, arguments: _, block: _, span: _ } => {},
                Statements::ImportStatement { path: _, span: _ } => {},
                Statements::StructDefineStatement { name: _, fields: _, functions: _, span: _ } => {},
                Statements::TypedefStatement { alias: _, datatype: _, span: _ } => {},
                Statements::EnumDefineStatement { name: _, fields: _, functions: _, span: _ } => {},
                _ => {
                    if let Some(err) = self.errors.last() {
                        if err.span == (255, 0).into() { return };
                    }
                    self.error(
                        String::from("In global scope only allowed: functions definitions, imports"),
                        (255, 0)
                    );
                    return;
                }
            }
        }

        match statement {
            Statements::AssignStatement { identifier, value, span } => {
                if let Some(variable) = self.scope.get_var(identifier) {
                    let value_type = self.visit_expression(value, Some(variable.datatype.clone()));

                    if variable.datatype != value_type {
                        self.error(
                            format!("Variable has type `{}`, but found `{}`", variable.datatype, value_type),
                            *span
                        );
                        return;
                    }

                    self.scope.set_init_var(identifier, true).unwrap_or_else(|err| {
                        self.error(err, *span);
                    });
                } else {
                    self.error(
                        format!("Variable \"{}\" is not defined here", identifier),
                        *span
                    );
                }
            },
            Statements::BinaryAssignStatement { identifier, operand, value, span } => {
                self.visit_statement(
                    &Statements::AssignStatement {
                        identifier: identifier.clone(),
                        span: *span,
                        value: Expressions::Binary {
                            operand: operand.clone(),
                            lhs: Box::new(
                                Expressions::Value(Value::Identifier(identifier.clone()), *span)
                            ),
                            rhs: Box::new(value.clone()),
                            span: *span,
                        },
                    }
                );
            },
            Statements::DerefAssignStatement { identifier, value, span } => {
                if let Some(variable) = self.scope.get_var(identifier) {
                    if let Type::Pointer(ptr_type) = variable.datatype {
                        let value_type = self.visit_expression(value, None);

                        if value_type != *ptr_type {
                            self.error(
                                format!("Pointer has type `{}`, but found `{}`", ptr_type, value_type),
                                *span
                            );
                            return;
                        }

                        self.scope.set_init_var(identifier, true).unwrap_or_else(|err| {
                            self.error(err, *span);
                        });
                    } else {
                        self.error(
                            format!("Unable to dereference non-pointer type `{}`", variable.datatype),
                            *span
                        );
                    }
                } else {
                    self.error(
                        format!("Variable \"{}\" is not defined here", identifier),
                        *span
                    );
                }
            },
            Statements::SliceAssignStatement { identifier, index, value, span } => {
                if let Some(variable) = self.scope.get_var(identifier) {
                    match variable.datatype {
                        Type::Array(typ, len) => {
                            // i could spent some time to implement evaluating expressions for
                            // checking index out of bounds, but it will be like in Rust: panics at
                            // the runtime

                            let index_type = self.visit_expression(index, Some(Type::USIZE));

                            if index_type != Type::USIZE {
                                self.error(
                                    format!("Expected index with type `usize`, but found `{}`", index_type),
                                    *span
                                );
                            }

                            let value_type = self.visit_expression(value, Some(*typ.clone()));

                            if value_type != *typ {
                                self.error(
                                    format!("Array has type `{}`, but found `{}`", typ, value_type),
                                    *span
                                );
                            }
                        },
                        Type::DynamicArray(typ) => {
                            let index_type = self.visit_expression(index, Some(Type::USIZE));

                            if index_type != Type::USIZE {
                                self.error(
                                    format!("Expected index with type `usize`, but found `{}`", index_type),
                                    *span
                                );
                            }

                            let value_type = self.visit_expression(value, Some(*typ.clone()));

                            if value_type != *typ {
                                self.error(
                                    format!("Array has type `{}`, but found `{}`", typ, value_type),
                                    *span
                                );
                            }
                        },
                        _ => {
                            self.error(
                                format!("Unable to apply slicing to `{}` type", variable.datatype),
                                *span
                            );
                        }
                    }
                } else {
                    self.error(
                        format!("Variable \"{}\" is not defined here", identifier),
                        *span
                    );
                }
            },
            Statements::FieldAssignStatement { object, value, span } => {
                if let Expressions::SubElement { head: _, subelements: _, span: _ } = object {} else {
                    self.error(
                        String::from("Field assign statement must have subelement expression"),
                        *span
                    );
                    return;
                }

                let object_type = self.visit_expression(object, None);
                let unwrapped_object_type = self.unwrap_alias(&object_type).unwrap_or_else(|err| {
                    self.error(err, *span);
                    return Type::Void;
                });

                if unwrapped_object_type == Type::Void { return };

                let value_type = self.visit_expression(value, Some(unwrapped_object_type.clone()));
                if unwrapped_object_type != value_type {
                    self.error(
                        format!("Field has type `{}`, but found `{}`", object_type, value_type),
                        *span
                    );
                    return;
                }
            },

            Statements::AnnotationStatement { identifier, datatype, value, span } => {
                // unwrapping type
                let mut datatype = datatype.clone().map(|tty| {
                    self.unwrap_alias(&tty).unwrap_or_else(|err| {
                        self.error(
                            err,
                            *span
                        );
                        Type::Void
                    })
                });

                if let Some(Type::Alias(alias)) = &datatype {
                    let struct_type = self.scope.get_struct(alias);
                    let enum_type = self.scope.get_enum(alias);
                    let typedef_type = self.scope.get_typedef(alias);

                    let mut staged = false;

                    if struct_type.is_some() && !staged { datatype = struct_type };
                    if enum_type.is_some() && !staged { datatype = enum_type };
                    if typedef_type.is_some() && !staged { datatype = typedef_type };
                }

                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value_type = self.visit_expression(value, Some(datatype.clone()));

                        if value_type != datatype {
                            if Self::is_integer(&value_type) && Self::is_integer(&datatype) {
                                if Self::integer_order(&value_type) > Self::integer_order(&datatype) {
                                    self.error(
                                        format!("Expected integer type `{}` (or lower), but found `{}`", datatype, value_type),
                                        *span
                                    );
                                    return;
                                }

                                return;
                            }

                            self.error(
                                format!("Expected type `{}` but found `{}`", datatype, value_type),
                                *span
                            );
                            return;
                        }

                        self.scope.add_var(identifier.clone(), datatype.clone(), true, *span);
                    },
                    (Some(datatype), None) => {
                        self.scope.add_var(identifier.clone(), datatype.clone(), false, *span);
                    },
                    (None, Some(value)) => {
                        let value_type = self.visit_expression(value, None);
                        self.scope.add_var(identifier.clone(), value_type, true, *span);
                    },
                    (None, None) => {
                        self.error(
                            format!("Variable `{}` has unknown type", identifier),
                            *span
                        );
                    }
                }
            }
            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {
                if self.scope.get_fn(name).is_some() {
                    self.error(
                        format!("Function `{}` already declared!", name),
                        *span
                    );
                    return;
                }

                let datatype = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                    self.error(
                        err,
                        *span
                    );
                    Type::Void
                });
                let mut function_scope = Scope::new();

                self.scope.add_fn(name.clone(), Type::Function(
                    arguments.iter().map(|arg| arg.1.clone()).collect::<Vec<Type>>(),
                    Box::new(datatype.clone())
                )).unwrap();

                function_scope.parent = Some(Box::new(self.scope.clone()));
                function_scope.expected = datatype.clone();

                arguments.iter().for_each(|arg| function_scope.add_var(arg.0.clone(), arg.1.clone(), true, *span));
                self.scope = function_scope;
                
                block.iter().for_each(|stmt| self.visit_statement(stmt));
                let exp = self.unwrap_alias(&self.scope.expected).unwrap_or_else(|err| {
                    self.error(err, *span);
                    Type::Void
                });
                let ret = self.unwrap_alias(&self.scope.returned).unwrap_or_else(|err| {
                    self.error(err, *span);
                    Type::Void
                });

                if exp != ret {
                    self.error(
                        format!("Function `{}` returns type `{}`, but found `{}`", name, exp, ret),
                        *span
                    );
                    return;
                }

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();
            },
            Statements::FunctionCallStatement { name, arguments, span } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(
                        format!("Function `{}` is not defined here", name),
                        *span
                    );
                    Type::Void
                });

                if func == Type::Void { return };
                if let Type::Function(func_args, func_type) = func {
                    let call_args = arguments.iter().map(|arg| self.visit_expression(arg, None)).collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        self.error(
                            format!("Function `{}` has {} arguments, but found {}", name, func_args.len(), call_args.len()),
                            *span
                        );
                        return;
                    }

                    call_args.iter().enumerate().zip(func_args).for_each(|((ind, provided), expected)| {
                        if &expected != provided {
                            self.error(
                                format!("Argument #{} must be `{}`, but found `{}`", ind + 1, expected, provided),
                                Parser::get_span_expression(arguments[ind].clone())
                            );
                        }
                    });

                    if *func_type != Type::Void {
                        self.warning(
                            format!("Unused `{}` result from function", func_type),
                            *span
                        );
                    }
                } else {
                    unreachable!()
                }
            },

            Statements::StructDefineStatement { name, functions, fields, span } => {
                let pre_type = Type::Struct(fields.clone(), HashMap::new());
                self.scope.structures.insert(name.clone(), pre_type);

                let mut structure_scope = Scope::new();
                structure_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = structure_scope;

                functions.iter().for_each(|func| {
                    self.visit_statement(func.1);
                });

                let functions_signatures = self.scope.functions.clone();
                self.scope = *self.scope.parent.clone().unwrap();

                let _ = self.scope.structures.remove(name);

                let struct_type = Type::Struct(fields.clone(), functions_signatures);
                self.scope.add_struct(name.clone(), struct_type.clone()).unwrap_or_else(|err| {
                    self.error(
                        err,
                        *span
                    );
                });
                self.scope.add_typedef(name.clone(), struct_type).unwrap_or_else(|err| {
                    self.error(err, *span);
                })
            },
            Statements::EnumDefineStatement { name, fields, functions, span } => {
                let pre_type = Type::Enum(fields.clone(), HashMap::new());
                self.scope.structures.insert(name.clone(), pre_type);

                let mut enum_scope = Scope::new();
                enum_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = enum_scope;

                functions.iter().for_each(|func| {
                    self.visit_statement(func.1);
                });

                let functions_signatures = self.scope.functions.clone();
                self.scope = *self.scope.parent.clone().unwrap();

                let _ = self.scope.structures.remove(name);

                let enum_type = Type::Enum(fields.clone(), functions_signatures);
                self.scope.add_struct(name.clone(), enum_type.clone()).unwrap_or_else(|err| {
                    self.error(
                        err,
                        *span
                    );
                });
                self.scope.add_typedef(name.clone(), enum_type.clone()).unwrap_or_else(|err| {
                    self.error(err, *span);
                })
            },
            Statements::TypedefStatement { alias, datatype, span } => {
                self.scope.add_typedef(alias.clone(), datatype.clone()).unwrap_or_else(|err| {
                    self.error(
                        err,
                        *span
                    );
                });
            },
            
            Statements::IfStatement { condition, then_block, else_block, span } => {
                let condition_type = self.visit_expression(condition, None);
                
                if condition_type != Type::Bool {
                    self.error(
                        format!("Expected `bool` type in expression, but found `{}`", condition_type),
                        *span
                    );
                    return;
                }

                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = self.scope.expected.clone();
                self.scope = new_scope;

                then_block.iter().for_each(|stmt| self.visit_statement(stmt));

                let then_block_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if then_block_type != self.scope.expected {
                    self.error(
                        format!("Expected type `{}` for scope, but found `{}`", self.scope.expected, then_block_type),
                        *span
                    );
                    return;
                }

                if let Some(else_block) = else_block {
                    let mut new_scope = Scope::new();
                    new_scope.parent = Some(Box::new(self.scope.clone()));
                    new_scope.expected = self.scope.expected.clone();
                    self.scope = new_scope;

                    else_block.iter().for_each(|stmt| self.visit_statement(stmt));

                    let else_block_type = self.scope.returned.clone();

                    if let Some(unused) = self.scope.check_unused_variables() {
                        unused.iter().for_each(|var| {
                            self.warning(
                                format!("Unused variable `{}` found", var.0),
                                var.1
                            );
                        });
                    }


                    self.scope = *self.scope.parent.clone().unwrap();

                    if then_block_type != else_block_type {
                        self.scope.returned = then_block_type.clone();
                        self.error(
                            format!("Scopes has incompatible types: `{}` and `{}`", then_block_type, else_block_type),
                            *span
                        );
                        return;
                    }

                    self.scope.returned = then_block_type;
                }
            },
            Statements::WhileStatement { condition, block, span } => {
                let condition_type = self.visit_expression(condition, None);
                
                if condition_type != Type::Bool {
                    self.error(
                        format!("Expected `bool` type in expression, but found `{}`", condition_type),
                        *span
                    );
                    return;
                }

                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = self.scope.expected.clone();
                new_scope.is_loop = true;
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));

                let block_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(
                            format!("Expected type `{}`, but found `{}`", self.scope.expected, block_type),
                            *span
                        );
                        return;
                    }

                    self.scope.returned = block_type;
                }
            },
            Statements::ForStatement { binding, iterator, block, span } => {
                const BASIC_SUPPORTED_ITERATOR_TYPES: [Type; 9] = [
                    Type::I8, Type::I16, Type::I32, Type::I64,
                    Type::U8, Type::U16, Type::U32, Type::U64,
                    Type::String
                ];

                let iterator_type = self.visit_expression(iterator, None);
                let mut binding_type = iterator_type.clone();

                match iterator_type {
                    typ if BASIC_SUPPORTED_ITERATOR_TYPES.contains(&typ) => {},
                    Type::Array(typ, _) => binding_type = *typ,
                    Type::DynamicArray(typ) => binding_type = *typ,
                    _ => {
                        self.error(
                            format!("Type `{}` is not supported for iteration", iterator_type),
                            *span
                        );
                        return;
                    }
                };

                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = self.scope.expected.clone();
                new_scope.is_loop = true;
                new_scope.add_var(binding.clone(), binding_type, true, *span);
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));

                let block_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(
                            format!("Expected type `{}`, but found `{}`", self.scope.expected, block_type),
                            *span
                        );
                        return;
                    }

                    self.scope.returned = block_type;
                }
            },
            Statements::ImportStatement { path, span } => {
                match path {
                    Expressions::Value(Value::String(path), _) => {
                        let fname = std::path::Path::new(path)
                            .file_name()
                            .map(|fname| {
                                fname.to_str().unwrap_or("$NONE")
                            });

                        match fname {
                            Some(fname) => {
                                if fname == "$NONE" {
                                    self.error(
                                        format!("Unable to get module name: `{}`", path),
                                        *span
                                    );
                                    return;
                                }

                                let src = std::fs::read_to_string(fname).unwrap_or_else(|err| {
                                    self.error(
                                        format!("Unable to read `{}`: {}", fname, err),
                                        *span
                                    );
                                    String::new()
                                });

                                if src.is_empty() { return };

                                let mut lexer = deen_lexer::Lexer::new(&src, fname);
                                let (tokens, warns) = match lexer.tokenize() {
                                    Ok(res) => res,
                                    Err((errors, warns)) => {
                                        errors.iter().for_each(|err| self.errors.push(err.clone().into()));
                                        warns.iter().for_each(|warn| self.warnings.push(warn.clone().into()));
                                        return;
                                    }
                                };
                                warns.iter().for_each(|warn| self.warnings.push(warn.clone().into()));

                                let mut parser = deen_parser::Parser::new(tokens, &src, fname);
                                let (ast, warns) = match parser.parse() {
                                    Ok(res) => res,
                                    Err((errors, warns)) => {
                                        errors.iter().for_each(|err| self.errors.push(err.clone().into()));
                                        warns.iter().for_each(|warn| self.warnings.push(warn.clone().into()));
                                        return;
                                    }
                                };
                                warns.iter().for_each(|warn| self.warnings.push(warn.clone().into()));

                                let mut mutual_import = false;
                                ast.iter().for_each(|stmt| {
                                    if let Statements::ImportStatement { path: Expressions::Value(Value::String(path), _), span } = stmt {
                                        let imp_name = std::path::Path::new(path)
                                            .file_name()
                                            .map(|fname| {
                                                fname.to_str().unwrap_or("$NONE")
                                            });

                                        if imp_name == Some(self.source.name()) {
                                            self.error(
                                                format!("Mutual import found: `{}` from `{}`", imp_name.unwrap(), fname),
                                                *span
                                            );
                                            mutual_import = true;
                                        } 
                                    }
                                });

                                if mutual_import { return };

                                let mut analyzer = Self::new(&src, fname, false);
                                let (_, warns) = match analyzer.analyze(&ast) {
                                    Ok(warns) => warns,
                                    Err((errors, warns)) => {
                                        errors.iter().for_each(|err| self.errors.push(err.clone()));
                                        warns.iter().for_each(|warn| self.warnings.push(warn.clone()));
                                        return;
                                    }
                                };
                                warns.iter().for_each(|warn| self.warnings.push(warn.clone()));

                                let module_name = fname
                                    .split(".")
                                    .nth(0)
                                    .map(|n| n.to_string())
                                    .unwrap_or(fname.replace(".dn", ""));

                                let mut import = Import::new(ast);

                                // analyzer.scope.functions.iter().for_each(|func| {
                                //     self.scope.add_fn(
                                //         format!("{}.{}", module_name, func.0),
                                //         func.1.clone()
                                //     );
                                //
                                //     import.add_fn(
                                //         format!("{}.{}", module_name, func.0), func.1.clone()
                                //     )
                                // });

                                analyzer.imports.insert(module_name, import);
                            }
                            None => {
                                self.error(
                                    format!("Unable to find: `{}`", path),
                                    *span
                                );
                            }
                        }
                    },
                    _ => {
                        self.error(
                            String::from("Import must be string constant"),
                            *span
                        );
                    }
                }
            },
            Statements::BreakStatements { span } => {
                if !self.scope.is_loop {
                    self.error(
                        String::from("Used `break` keyword outside loop"),
                        *span
                    );
                }
            },
            Statements::ReturnStatement { value, span } => {
                let value_type = self.visit_expression(value, Some(self.scope.expected.clone()));
                
                if Self::is_integer(&value_type) && Self::is_integer(&self.scope.expected) && Self::integer_order(&value_type) <= Self::integer_order(&self.scope.expected) {
                    self.scope.returned = self.scope.expected.clone();
                    return;
                }

                if Self::is_float(&value_type) && Self::is_integer(&self.scope.expected) && Self::float_order(&value_type) <= Self::float_order(&self.scope.expected) {
                    self.scope.returned = self.scope.expected.clone();
                    return;
                }

                self.scope.returned = value_type;
            },
            Statements::ScopeStatement { block, span } => {
                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();
            },
            
            Statements::Expression(expr) => {
                let expr_type = self.visit_expression(expr, None);
                if expr_type != Type::Void {
                    self.warning(
                        String::from("Unused expression result found"),
                        deen_parser::Parser::get_span_expression(expr.clone())
                    );
                }
            },
            Statements::None => unreachable!()
        }
    }

    fn visit_expression(&mut self, expr: &Expressions, expected: Option<Type>) -> Type {
        match expr {
            Expressions::Binary { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, expected);

                match (left.clone(), right.clone()) {
                    (l, r) if Self::is_integer(&l) && Self::is_integer(&r) => {
                        if Self::integer_order(&l) > Self::integer_order(&r) { l } else { r }
                    }

                    (l, r) if Self::is_float(&l) && Self::is_float(&r) => {
                        if Self::float_order(&l) > Self::float_order(&r) { l } else { r }
                    }

                    _ => {
                        self.error(
                            format!("Cannot apply binary \"{}\" to `{}` and `{}`", operand, left, right),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Unary { operand, object, span } => {
                let obj = self.visit_expression(object, expected);
                
                match (&obj, operand.as_str()) {
                    (typ, "-") if Self::is_integer(typ) => {
                        if Self::is_unsigned_integer(typ) {
                            return Self::unsigned_to_signed_integer(typ);
                        }
                        obj
                    },
                    (typ, "-") if Self::is_float(typ) => obj,
                    (typ, "!") if Self::is_integer(typ) => obj,
                    (Type::Bool, "!") => obj,

                    _ => {
                        self.error(
                            format!("Cannot apply unary \"{}\" to `{}`", operand, obj),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Boolean { operand, lhs, rhs, span } => {
                const SUPPORTED_EXTRA_TYPES: [Type; 3] = [Type::String, Type::Bool, Type::Char];

                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, expected);

                match (left.clone(), right.clone()) {
                    (l, r) if Self::is_integer(&l) && Self::is_integer(&r) => Type::Bool,
                    (l, r) if Self::is_float(&l) && Self::is_float(&r) => Type::Bool,
                    (l, r) if l == r && SUPPORTED_EXTRA_TYPES.contains(&l) => Type::Bool,
                    (Type::Pointer(l), Type::Pointer(r)) if l == r && *l == Type::Char => Type::Bool,

                    _ => {
                        self.error(
                            format!("Cannot apply boolean \"{}\" to `{}` and `{}`", operand, left, right),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Bitwise { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, Some(Type::U8));

                if !Self::is_integer(&left) || !Self::is_integer(&right) {
                    self.error(
                        format!("Cannot apply bitwise operations to `{}` and `{}`", &left, &right),
                        *span
                    );
                    return Type::Void;
                };

                if [">>", "<<"].contains(&operand.as_ref()) && !Self::is_unsigned_integer(&right) {
                    self.error(
                        "Shift index must be unsigned integer".to_string(),
                        *span
                    );
                    return Type::Void;
                }

                if Self::integer_order(&left) > Self::integer_order(&right) { left } else { right }
            },

            Expressions::Argument { name, r#type, span } => unreachable!(),
            Expressions::SubElement { head, subelements, span } => {
                let head_type = self.visit_expression(head, expected);

                let mut prev_type = self.unwrap_alias(&head_type).unwrap_or_else(|err| {
                    self.error(err, *span);
                    Type::Void
                });
                let mut prev_expr = Expressions::None;
                if prev_type == Type::Void { return Type::Void };

                subelements.iter().for_each(|sub| {
                    match sub {
                        Expressions::Value(Value::Identifier(field), field_span) => {
                            match prev_type.clone() {
                                Type::Struct(fields, _) => {
                                    let field_type = fields.get(&field.clone()).unwrap_or_else(|| {
                                        self.error(
                                            format!("Type `{}` has no field named `{}`", prev_type, field),
                                            *field_span
                                        );
                                        &Type::Void
                                    });
                                    prev_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                        self.error(err, *field_span);
                                        Type::Void
                                    });
                                    prev_expr = sub.clone();
                                }
                                Type::Enum(fields, _) => {
                                    let opt = fields.iter().find(|&x| x == field);
                                    if opt.is_none() {
                                        self.error(
                                            format!("Type `{}` has no choice named `{}`", prev_type, field),
                                            *field_span
                                        );
                                    }

                                    prev_expr = Expressions::SubElement { head: Box::new(prev_expr.clone()), subelements: vec![], span: *field_span }
                                },
                                _ => {
                                    self.error(
                                        format!("Type `{}` has no accessible fields", prev_type),
                                        *field_span
                                    );
                                }
                            };
                        }
                        Expressions::Value(Value::Integer(idx), idx_span) => {
                            match prev_type.clone() {
                                Type::Tuple(types) => {
                                    if idx >= &(types.len() as i64) {
                                        self.error(
                                            format!("Type `{}` has {} fields, but index is {}", prev_type, types.len(), idx),
                                            *idx_span
                                        );
                                        return;
                                    }

                                    let typ = types[idx.clone() as usize].clone();
                                    prev_type = self.unwrap_alias(&typ).unwrap_or_else(|err| {
                                        self.error(err, *idx_span);
                                        Type::Void
                                    });
                                    prev_expr = sub.clone();
                                },
                                _ => {
                                    self.error(
                                        format!("Type `{}` has no numbered fields", prev_type),
                                        *idx_span
                                    )
                                }
                            }
                        }
                        Expressions::FnCall { name, arguments, span } => {
                            match prev_type.clone() {
                                Type::Struct(_, functions) | Type::Enum(_, functions) => {
                                    let function_type = functions.get(name).unwrap_or_else(|| {
                                        self.error(
                                            format!("Type `{}` has no function named `{}`", prev_type, name),
                                            *span
                                        );
                                        &Type::Void
                                    });

                                    if let Type::Function(args, datatype) = function_type {
                                        let mut arguments = arguments.clone();

                                        if let Some(first_arg) = args.first() {
                                            let unwrapped_arg = self.unwrap_alias(first_arg).unwrap_or_else(|err| {
                                                self.error(err, *span);
                                                return Type::Void
                                            });

                                            if unwrapped_arg == prev_type {
                                                arguments.reverse();
                                                arguments.push(prev_expr.clone());
                                                arguments.reverse();
                                            }
                                        }

                                        prev_type = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                            self.error(err, *span);
                                            Type::Void
                                        });

                                        if arguments.len() != args.len() {
                                            self.error(
                                                format!("Function `{}` has {} arguments, but found {}", name, args.len(), arguments.len()),
                                                *span
                                            );
                                            return;
                                        }

                                        arguments.iter().enumerate().zip(args).for_each(|((index, expr), expected)| {
                                            let raw_expr_type = self.visit_expression(expr, Some(expected.clone()));
                                            let expr_type = self.unwrap_alias(&raw_expr_type).unwrap_or_else(|err| {
                                                    self.error(err, *span);
                                                    Type::Void
                                            });
                                            let expected = self.unwrap_alias(expected).unwrap_or_else(|err| {
                                                self.error(err, *span);
                                                Type::Void
                                            });

                                            if expected == Type::Void || raw_expr_type == Type::Void { return };
                                            if expr_type != expected {
                                                self.error(
                                                    format!("Argument #{} has type `{}`, but found `{}`", index + 1, expected, expr_type),
                                                    *span
                                                );
                                            }
                                        });
                                    };
                                },
                                // Type::Enum(_, functions) => {},
                                _ => {
                                    self.error(
                                        format!("Type `{}` isn't supported for function calls", prev_type),
                                        *span
                                    );
                                }
                            }
                        },
                        _ => {
                            self.error(
                                String::from("Unsupported subelement expression found"),
                                *span
                            );
                        }
                    }
                });

                prev_type
            },

            Expressions::FnCall { name, arguments, span } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(
                        format!("Function `{}` is not defined here", name),
                        *span
                    );
                    Type::Void
                });

                if func == Type::Void { return func };
                if let Type::Function(func_args, func_type) = func {
                    let call_args = arguments.iter().map(|arg| self.visit_expression(arg, None)).collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        self.error(
                            format!("Function `{}` has {} arguments, but found {}", &name, func_args.len(), call_args.len()),
                            *span
                        );
                        return Type::Void;
                    }
                    
                    call_args.iter().enumerate().zip(func_args).for_each(|((ind, provided), expected)| {
                        if &expected != provided {
                            self.error(
                                format!("Argument #{} must be `{}`, but found `{}`", ind + 1, expected, provided),
                                Parser::get_span_expression(arguments[ind].clone())
                            );
                        }
                    });

                    *func_type
                } else {
                    unreachable!()
                }
            },
            Expressions::Reference { object, span } => {
                let obj = self.visit_expression(object, expected);

                Type::Pointer(Box::new(obj))
            },
            Expressions::Dereference { object, span } => {
                let obj = self.visit_expression(object, expected);
                if let Type::Pointer(ptr_type) = obj {
                    *ptr_type
                } else {
                    self.error(
                        format!("Type {} cannot be dereferenced!", obj),
                        *span
                    );
                    Type::Void
                }
            },

            Expressions::Array { values, len, span } => {
                if *len < 1 {
                    self.error("Empty array type is unkown".to_string(), *span);
                    return Type::Void;
                }

                let arr_type = self.visit_expression(&values[0], None);

                values.iter().for_each(|val| {
                    let val_type = self.visit_expression(val, None);
                    if val_type != arr_type {
                        self.error(
                            format!("Array has type {}, but element has {}", arr_type, val_type),
                            Parser::get_span_expression(val.clone())
                        );
                    }
                });

                Type::Array(Box::new(arr_type), *len)
            },
            Expressions::Tuple { values, span } => {
                if values.len() < 1 {
                    self.error("Unknown by compilation time tuple found".to_string(), *span);
                    return Type::Void;
                }

                let mut expected_types = values.iter().map(|_| None).collect::<Vec<Option<Type>>>();
                if let Some(Type::Tuple(expectations)) = expected.clone() {
                    expected_types = expectations.into_iter().map(|exp| Some(exp)).collect();
                }

                let types = values.into_iter().zip(expected_types).map(|(val, exp)| self.visit_expression(val, exp)).collect::<Vec<Type>>();

                Type::Tuple(types)
            }
            Expressions::Slice { object, index, span } => {
                let obj = self.visit_expression(object, expected);

                match obj {
                    Type::Tuple(types) => {
                        if let Expressions::Value(Value::Integer(ind), _) = **object {
                            if ind < 0 {
                                self.error(
                                    "Tuple index must be unsigned".to_string(),
                                    Parser::get_span_expression(*index.clone())
                                );
                                return Type::Void;
                            }
                            
                            types[ind as usize].clone()
                        } else {
                            self.error(
                                "Tuple index must be known by compile-time".to_string(),
                                Parser::get_span_expression(*index.clone())
                            );
                            Type::Void
                        }
                    },
                    Type::Array(tty, _) => *tty,
                    Type::DynamicArray(tty) => *tty,

                    _ => {
                        self.error(
                            format!("Type `{}` is not supported for slice", obj),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Struct { name, fields, span } => {
                let structure = self.scope.get_struct(name).unwrap_or_else(|| {
                    self.error(
                        format!("Structure `{}` does not exist here", name),
                        *span
                    );
                    Type::Void
                });

                if structure == Type::Void { return Type::Void };
                if let Type::Struct(struct_fields, _) = structure.clone() {
                    let mut assigned_fields = HashMap::new();
                    struct_fields.iter().for_each(|x| {
                        assigned_fields.insert(x.0, false);
                    });

                    fields.iter().for_each(|field| {
                        let struct_field = struct_fields.get(field.0);
                        if let Some(field_type) = struct_field {
                            let field_type = self.unwrap_alias(&field_type).unwrap_or_else(|err| {
                                self.error(err, *span);
                                Type::Void
                            });
                            let provided_type = self.visit_expression(field.1, Some(field_type.clone()));

                            if field_type != provided_type {
                                self.error(
                                    format!("Field `{}` expected to be type `{}`, but found `{}`", field.0, field_type, provided_type),
                                    *span
                                );
                                return;
                            }

                            let _ = assigned_fields.insert(field.0, true);
                        } else {
                            self.error(
                                format!("Field `{}` doesn't exist in struct `{}`", field.0, name),
                                *span
                            );
                        }
                    });

                    let unassigned = assigned_fields.iter().filter(|x| !x.1).map(|x| x.0.to_owned().to_owned()).collect::<Vec<String>>();
                    if !unassigned.is_empty() {
                        let fmt = format!("`{}`", unassigned.join("` , `"));
                        self.error(
                            format!("Missing structure fields: {}", fmt),
                            *span
                        );
                    }

                    structure
                } else { unreachable!() }
            },
            Expressions::Scope { block, span } => {
                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = expected.unwrap_or(Type::Void);
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));
                
                let scope_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(
                            format!("Unused variable `{}` found", var.0),
                            var.1
                        );
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                scope_type
            },

            Expressions::Value(value, span) => {
                match self.visit_value(value.clone(), expected) {
                    Ok(tty) => tty,
                    Err(err) => {
                        self.error(
                            err,
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::None => Type::Void
        }
    }

    fn visit_value(&mut self, value: Value, expected: Option<Type>) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
            if expected.is_some() && expected.clone().unwrap() != Type::Void {
                    let exp = expected.unwrap();
                    match exp {
                        Type::I8 => {
                            if int < i8::MIN as i64 || int > i8::MAX as i64 {
                                return Err(String::from("Constant is out of `i8` type range"))
                            }
                        },
                        Type::I16 => {
                            if int < i16::MIN as i64 || int > i16::MAX as i64 {
                                return Err(String::from("Constant is out of `i16` type range"))
                            }
                        },
                        Type::I32 => {
                            if int < i32::MIN as i64 || int > i32::MAX as i64 {
                                return Err(String::from("Constant is out of `i32` type range"))
                            }
                        },
                        Type::I64 => {
                            if !(i64::MIN..=i64::MAX).contains(&int) {
                                return Err(String::from("Constant is out of `i64` type range"))
                            }
                        },

                        Type::U8 => {
                            if int < 0 || int > u8::MAX as i64 {
                                return Err(String::from("Constant is out of `u8` type range"))
                            }
                        },
                        Type::U16 => {
                            if int < 0 || int > u16::MAX as i64 {
                                return Err(String::from("Constant is out of `u16` type range"))
                            }
                        },
                        Type::U32 => {
                            if int < 0 || int > u32::MAX as i64 {
                                return Err(String::from("Constant is out of `u32` type range"))
                            }
                        },
                        Type::U64 => {
                            if int < 0 {
                                return Err(String::from("Constant is out of `u64` type range"))
                            }
                        },
                        Type::USIZE => {
                            if int < 0 {
                                return Err(String::from("Constant is out of `usize` type range"))
                            }
                        },

                        _ => return Err(format!("Expected `{}` but found integer constant", exp))
                    }

                    return Ok(exp);
                }

                let signed = !Self::is_unsigned_integer(&expected.unwrap_or(Type::Void));
                if int > i32::MAX as i64 {
                    if !signed {
                        if int < 0 { return Err(String::from("Expected unsigned value but found signed")) }
                        return Ok(Type::U64);
                    }
                    return Ok(Type::I64);
                }
                Ok(Type::I32)
            },
            Value::Float(float) => {
                if float > f32::MAX as f64 {
                    return Ok(Type::F64);
                }
                Ok(Type::F32)
            },
            Value::Identifier(id) => {
                // if let Some(structure) = self.scope.get_struct(&id) { return Ok(structure) }
                if let Some(typedef) = self.scope.get_typedef(&id) { return Ok(typedef) }
                if let Some(enumeration) = self.scope.get_enum(&id) { return Ok(enumeration) }

                match self.scope.get_var(&id) {
                    Some(var) => {
                        if !var.initialized { return Err(format!("Variable `{}` isn't initalized", id)) }
                        Ok(var.datatype)
                    },
                    None => Err(format!("Variable `{}` is not defined here", id))
                }
            },
            Value::String(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            Value::Char(_) => Ok(Type::Char),
            Value::Boolean(_) => Ok(Type::Bool),
            Value::Keyword(_) => Ok(Type::Void),
        }
    }
}

impl Analyzer {
    #[inline]
    pub fn is_integer(typ: &Type) -> bool {
        [
            Type::I8, Type::I16,
            Type::I32, Type::I64,
            
            Type::U8, Type::U16,
            Type::U32, Type::U64
        ].contains(typ)
    }

    #[inline]
    pub fn is_unsigned_integer(typ: &Type) -> bool {
        [Type::U8, Type::U16, Type::U32, Type::U64, Type::USIZE].contains(typ)
    }

    #[inline]
    pub fn unsigned_to_signed_integer(typ: &Type) -> Type {
        match typ {
            Type::U8 => Type::I8,
            Type::U16 => Type::I16,
            Type::U32 => Type::I32,
            Type::U64 => Type::I64,

            Type::I8 => Type::I8,
            Type::I16 => Type::I16,
            Type::I32 => Type::I32,
            Type::I64 => Type::I64,

            Type::USIZE => Type::I64,

            _ => Type::I32
        }
    }

    #[inline]
    pub fn integer_order(typ: &Type) -> usize {
        match typ {
            Type::U8 => 0,
            Type::U16 => 1,
            Type::U32 => 2,
            Type::U64 => 3,

            Type::I8 => 0,
            Type::I16 => 1,
            Type::I32 => 2,
            Type::I64 => 3,

            Type::USIZE => 4,

            _ => unreachable!()
        }
    }

    #[inline]
    pub fn is_float(typ: &Type) -> bool {
        [Type::F32, Type::F64].contains(typ)
    }

    #[inline]
    pub fn float_order(typ: &Type) -> usize {
        match typ {
            Type::F32 => 0,
            Type::F64 => 1,

            _ => unreachable!()
        }
    }

    #[inline]
    fn unwrap_alias(&self, typ: &Type) -> Result<Type, String> {
        match typ {
            Type::Alias(alias) => {
                let struct_type = self.scope.get_struct(alias);
                let enum_type = self.scope.get_enum(alias);
                let typedef_type = self.scope.get_typedef(alias);

                if let Some(struct_type) = struct_type { return Ok(struct_type) };
                if let Some(enum_type) = enum_type { return Ok(enum_type) };
                if let Some(typedef_type) = typedef_type { return Ok(typedef_type) };

                Err(format!("Type `{}` is not defined in this scope", typ))
            }
            
            Type::Pointer(ptr_type) => {
                let unwrapped_type = self.unwrap_alias(ptr_type)?;
                Ok(Type::Pointer(Box::new(unwrapped_type)))
            }
            Type::Array(typ, size) => {
                let unwrapped_type = self.unwrap_alias(typ)?;
                Ok(Type::Array(Box::new(unwrapped_type), *size))
            }
            Type::DynamicArray(typ) => {
                let unwrapped_type = self.unwrap_alias(typ)?;
                Ok(Type::DynamicArray(Box::new(unwrapped_type)))
            }
            Type::Tuple(types) => {
                let mut unwrapped_types = Vec::new();
                for typ in types {
                    let unwrapped_type = self.unwrap_alias(typ)?;
                    unwrapped_types.push(unwrapped_type)
                }

                Ok(Type::Tuple(unwrapped_types))
            }

            _ => Ok(typ.clone())
        }
    }
}
