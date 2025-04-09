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
            scope: Scope::new(),
            source: NamedSource::new(src, filename.to_owned()),

            errors: Vec::new(),
            warnings: Vec::new()
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        for statement in ast {
            self.visit_statement(statement);
        }

        Ok(self.warnings.clone())
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

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
        let span = (span.0, span.1.wrapping_sub(span.0));

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
            Statements::AssignStatement { identifier, value, span } => {},
            Statements::BinaryAssignStatement { identifier, operand, value, span } => {},
            Statements::DerefAssignStatement { identifier, value, span } => {},
            Statements::SliceAssignStatement { identifier, index, value, span } => {},

            Statements::AnnotationStatement { identifier, datatype, value, span } => {}
            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {},
            Statements::FunctionCallStatement { name, arguments, span } => {},
            
            Statements::IfStatement { condition, then_block, else_block, span } => {},
            Statements::WhileStatement { condition, block, span } => {},
            Statements::ForStatement { binding, iterator, block, span } => {},
            Statements::ImportStatement { path, span } => {},
            Statements::BreakStatements { span } => {},
            Statements::ReturnStatement { value, span } => {},
            
            Statements::Expression(expr) => {},
            Statements::None => unreachable!()
        }
    }

    fn visit_expression(&mut self, expr: &Expressions, signed: bool) -> Type {
        match expr {
            Expressions::Binary { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs, signed);
                let right = self.visit_expression(rhs, signed);

                match (left.clone(), right.clone()) {
                    (l, r) if self.is_integer(&l) && self.is_integer(&r) => {
                        if self.integer_order(&l) > self.integer_order(&r) { l } else { r }
                    }

                    (l, r) if self.is_float(&l) && self.is_float(&r) => {
                        if self.float_order(&l) > self.float_order(&r) { l } else { r }
                    }

                    _ => {
                        self.error(
                            format!("Cannot apply binary `{}` to {} and {}", operand, left, right),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Unary { operand, object, span } => {
                let obj = self.visit_expression(object, signed);
                
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
                            format!("Cannot apply unary `{}` to {}", operand, obj),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Boolean { operand, lhs, rhs, span } => {
                const SUPPORTED_EXTRA_TYPES: [Type; 3] = [Type::String, Type::Bool, Type::Char];

                let left = self.visit_expression(lhs, signed);
                let right = self.visit_expression(rhs, signed);

                match (left.clone(), right.clone()) {
                    (l, r) if self.is_integer(&l) && self.is_integer(&r) => Type::Bool,
                    (l, r) if self.is_float(&l) && self.is_float(&r) => Type::Bool,
                    (l, r) if l == r && SUPPORTED_EXTRA_TYPES.contains(&l) => Type::Bool,

                    _ => {
                        self.error(
                            format!("Cannot apply boolean `{}` to {} and {}", operand, left, right),
                            *span
                        );
                        Type::Void
                    }
                }
            },
            Expressions::Bitwise { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs, signed);
                let right = self.visit_expression(rhs, signed);

                if !self.is_integer(&left) || !self.is_integer(&right) {
                    self.error(
                        format!("Cannot apply bitwise operations to {} and {}", &left, &right),
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
            Expressions::SubElement { parent, child, span } => self.visit_expression(child, signed),

            Expressions::FnCall { name, arguments, span } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(
                        format!("Function `{}` is not defined", name),
                        *span
                    );
                    Type::Void
                });

                if func == Type::Void { return func };
                if let Type::Function(func_args, func_type) = func {
                    let call_args = arguments.iter().map(|arg| self.visit_expression(arg, signed)).collect::<Vec<Type>>();

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
                let obj = self.visit_expression(object, signed);

                Type::Pointer(Box::new(obj))
            },
            Expressions::Dereference { object, span } => {
                let obj = self.visit_expression(object, signed);
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

                let arr_type = self.visit_expression(&values[0], signed);

                values.iter().for_each(|val| {
                    let val_type = self.visit_expression(val, signed);
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
                let obj = self.visit_expression(object, signed);

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

            Expressions::Value(value, span) => {
                match self.visit_value(value.clone(), signed) {
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

    fn visit_value(&mut self, value: Value, signed: bool) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
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

            Type::I8 => 4,
            Type::I16 => 5,
            Type::I32 => 6,
            Type::I64 => 7,

            Type::USIZE => 8,

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
