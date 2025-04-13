#![allow(unused)]

use crate::{
    scope::Scope,
    error::{SemanticError, SemanticWarning}
};
use deen_parser::{expressions::Expressions, statements::Statements, types::Type, value::Value, Parser};
use miette::NamedSource;

mod error;
mod scope;

type SemanticOk = Vec<SemanticWarning>;
type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,
}

impl Analyzer {
    pub fn new(src: &str, filename: &str) -> Self {
        Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = true;
                scope
            },
            source: NamedSource::new(filename, src.to_owned()),

            errors: Vec::new(),
            warnings: Vec::new()
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

        return Ok(self.warnings.clone())
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
        match statement {
            Statements::AssignStatement { identifier, value, span } => {
                if let Some(variable) = self.scope.get_var(identifier) {
                    let value_type = self.visit_expression(value, false, Some(variable.datatype.clone()));

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
                    return;
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
                        let value_type = self.visit_expression(value, true, None);

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
                        return;
                    }
                } else {
                    self.error(
                        format!("Variable \"{}\" is not defined here", identifier),
                        *span
                    );
                    return;
                }
            },
            Statements::SliceAssignStatement { identifier, index, value, span } => {
                if let Some(variable) = self.scope.get_var(identifier) {
                    match variable.datatype {
                        Type::Array(typ, len) => {
                            // i could spent some time to implement evaluating expressions for
                            // checking index out of bounds, but it will be like in Rust: panics at
                            // the runtime

                            let index_type = self.visit_expression(index, true, Some(Type::USIZE));

                            if index_type != Type::USIZE {
                                self.error(
                                    format!("Expected index with type `usize`, but found `{}`", index_type),
                                    *span
                                );
                            }

                            let value_type = self.visit_expression(value, true, Some(*typ.clone()));

                            if value_type != *typ {
                                self.error(
                                    format!("Array has type `{}`, but found `{}`", typ, value_type),
                                    *span
                                );
                            }
                        },
                        Type::DynamicArray(typ) => {
                            let index_type = self.visit_expression(index, true, Some(Type::USIZE));

                            if index_type != Type::USIZE {
                                self.error(
                                    format!("Expected index with type `usize`, but found `{}`", index_type),
                                    *span
                                );
                            }

                            let value_type = self.visit_expression(value, true, Some(*typ.clone()));

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
                            return
                        }
                    }
                } else {
                    self.error(
                        format!("Variable \"{}\" is not defined here", identifier),
                        *span
                    );
                    return;
                }
            },

            Statements::AnnotationStatement { identifier, datatype, value, span } => {
                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value_type = self.visit_expression(value, !self.is_unsigned_integer(datatype), Some(datatype.clone()));

                        if value_type != *datatype {
                            if self.is_integer(&value_type) && self.is_integer(datatype) {
                                if self.integer_order(&value_type) > self.integer_order(datatype) {
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
                        let value_type = self.visit_expression(value, true, None);
                        self.scope.add_var(identifier.clone(), value_type, true, *span);
                    },
                    (None, None) => {
                        self.error(
                            format!("Variable `{}` has unknown type", identifier),
                            *span
                        );
                        return;
                    }
                }
            }
            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {
                if let Some(_) = self.scope.get_fn(name) {
                    self.error(
                        format!("Function `{}` already declared!", name),
                        *span
                    );
                    return;
                }

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
                if self.scope.expected != self.scope.returned {
                    self.error(
                        format!("Function `{}` returns type `{}`, but found `{}`", name, self.scope.expected, self.scope.returned),
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
                    let call_args = arguments.iter().map(|arg| self.visit_expression(arg, true, None)).collect::<Vec<Type>>();

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
                                format!("Expected argument with type {}, but found {}", expected, provided),
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

            
            Statements::IfStatement { condition, then_block, else_block, span } => {
                let condition_type = self.visit_expression(condition, true, None);
                
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
                let condition_type = self.visit_expression(condition, true, None);
                
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

                let iterator_type = self.visit_expression(iterator, true, None);
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

            },
            Statements::BreakStatements { span } => {
                if !self.scope.is_loop {
                    self.error(
                        String::from("Used `break` keyword outside loop"),
                        *span
                    );
                    return
                }
            },
            Statements::ReturnStatement { value, span } => {
                let value_type = self.visit_expression(value, !self.is_unsigned_integer(&self.scope.expected), Some(self.scope.expected.clone()));
                
                if self.is_integer(&value_type) && self.is_integer(&self.scope.expected) {
                    if self.integer_order(&value_type) <= self.integer_order(&self.scope.expected) {
                        self.scope.returned = self.scope.expected.clone();
                        return;
                    }
                }

                if self.is_float(&value_type) && self.is_integer(&self.scope.expected) {
                    if self.float_order(&value_type) <= self.float_order(&self.scope.expected) {
                        self.scope.returned = self.scope.expected.clone();
                        return;
                    }
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
                let _ = self.visit_expression(expr, true, None);
            },
            Statements::None => unreachable!()
        }
    }

    fn visit_expression(&mut self, expr: &Expressions, signed: bool, expected: Option<Type>) -> Type {
        match expr {
            Expressions::Binary { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs, signed, expected.clone());
                let right = self.visit_expression(rhs, signed, expected);

                match (left.clone(), right.clone()) {
                    (l, r) if self.is_integer(&l) && self.is_integer(&r) => {
                        if self.integer_order(&l) > self.integer_order(&r) { l } else { r }
                    }

                    (l, r) if self.is_float(&l) && self.is_float(&r) => {
                        if self.float_order(&l) > self.float_order(&r) { l } else { r }
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
                let obj = self.visit_expression(object, signed, expected);
                
                match (&obj, operand.as_str()) {
                    (typ, "-") if self.is_integer(typ) => {
                        if self.is_unsigned_integer(typ) {
                            return self.unsigned_to_signed_integer(typ);
                        }
                        obj
                    },
                    (typ, "-") if self.is_float(typ) => obj,
                    (typ, "!") if self.is_integer(typ) => obj,
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

                let left = self.visit_expression(lhs, signed, expected.clone());
                let right = self.visit_expression(rhs, signed, expected);

                match (left.clone(), right.clone()) {
                    (l, r) if self.is_integer(&l) && self.is_integer(&r) => Type::Bool,
                    (l, r) if self.is_float(&l) && self.is_float(&r) => Type::Bool,
                    (l, r) if l == r && SUPPORTED_EXTRA_TYPES.contains(&l) => Type::Bool,

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
                let left = self.visit_expression(lhs, signed, expected.clone());
                let right = self.visit_expression(rhs, signed, expected);

                if !self.is_integer(&left) || !self.is_integer(&right) {
                    self.error(
                        format!("Cannot apply bitwise operations to `{}` and `{}`", &left, &right),
                        *span
                    );
                    return Type::Void;
                };

                if [">>", "<<"].contains(&operand.as_ref()) && !self.is_unsigned_integer(&right) {
                    self.error(
                        "Shift index must be unsigned integer".to_string(),
                        *span
                    );
                    return Type::Void;
                }

                if self.integer_order(&left) > self.integer_order(&right) { left } else { right }
            },

            Expressions::Argument { name, r#type, span } => unreachable!(),
            Expressions::SubElement { parent, child, span } => self.visit_expression(child, signed, expected),

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
                    let call_args = arguments.iter().map(|arg| self.visit_expression(arg, signed, None)).collect::<Vec<Type>>();

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
                                format!("Expected argument with type {}, but found {}", expected, provided),
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
                let obj = self.visit_expression(object, signed, expected);

                Type::Pointer(Box::new(obj))
            },
            Expressions::Dereference { object, span } => {
                let obj = self.visit_expression(object, signed, expected);
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

                let arr_type = self.visit_expression(&values[0], signed, None);

                values.iter().for_each(|val| {
                    let val_type = self.visit_expression(val, signed, None);
                    if val_type != arr_type {
                        self.error(
                            format!("Array has type {}, but element has {}", arr_type, val_type),
                            Parser::get_span_expression(val.clone())
                        );
                    }
                });

                Type::Array(Box::new(arr_type), *len)
            },
            Expressions::Slice { object, index, span } => {
                let obj = self.visit_expression(object, signed, expected);

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
                match self.visit_value(value.clone(), signed, expected) {
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
            Expressions::None => unreachable!()
        }
    }

    fn visit_value(&mut self, value: Value, signed: bool, expected: Option<Type>) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
                if let Some(exp) = expected {
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
                            if int < i64::MIN as i64 || int > i64::MAX as i64 {
                                return Err(String::from("Constant is out of `i64` type range"))
                            }
                        },

                        Type::U8 => {
                            if int < u8::MIN as i64 || int > u8::MAX as i64 {
                                return Err(String::from("Constant is out of `u8` type range"))
                            }
                        },
                        Type::U16 => {
                            if int < u16::MIN as i64 || int > u16::MAX as i64 {
                                return Err(String::from("Constant is out of `u16` type range"))
                            }
                        },
                        Type::U32 => {
                            if int < u32::MIN as i64 || int > u32::MAX as i64 {
                                return Err(String::from("Constant is out of `u32` type range"))
                            }
                        },
                        Type::U64 => {
                            if int < u64::MIN as i64 || int > u64::MAX as i64 {
                                return Err(String::from("Constant is out of `u64` type range"))
                            }
                        },
                        Type::USIZE => {
                            if int < u64::MIN as i64 || int > u64::MAX as i64 {
                                return Err(String::from("Constant is out of `usize` type range"))
                            }
                        },

                        _ => return Err(format!("Expected `{}` but found integer constant", exp))
                    }

                    return Ok(exp);
                }

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
                match self.scope.get_var(&id) {
                    Some(var) => {
                        if !var.initialized { return Err(format!("Variable `{}` isn't initalized", id)) }
                        Ok(var.datatype)
                    },
                    None => Err(format!("Variable `{}` is not defined here", id))
                }
            },
            Value::String(_) => Ok(Type::String),
            Value::Char(_) => Ok(Type::Char),
            Value::Boolean(_) => Ok(Type::Bool),
            Value::Keyword(_) => Ok(Type::Void),
        }
    }
}

impl Analyzer {
    #[inline]
    fn is_integer(&self, typ: &Type) -> bool {
        [
            Type::I8, Type::I16,
            Type::I32, Type::I64,
            
            Type::U8, Type::U16,
            Type::U32, Type::U64
        ].contains(typ)
    }

    #[inline]
    fn is_unsigned_integer(&self, typ: &Type) -> bool {
        [Type::U8, Type::U16, Type::U32, Type::U64, Type::USIZE].contains(typ)
    }

    #[inline]
    fn unsigned_to_signed_integer(&self, typ: &Type) -> Type {
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
    fn integer_order(&self, typ: &Type) -> usize {
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
    fn is_float(&self, typ: &Type) -> bool {
        [Type::F32, Type::F64].contains(typ)
    }

    #[inline]
    fn float_order(&self, typ: &Type) -> usize {
        match typ {
            Type::F32 => 0,
            Type::F64 => 1,

            _ => unreachable!()
        }
    }
}
