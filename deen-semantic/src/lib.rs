use crate::{
    error::{SemanticError, SemanticWarning},
    symtable::{SymbolTable, Import},
    macros::MacrosObject,
    scope::Scope,
};
use deen_parser::{
    Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
};
use miette::NamedSource;
use std::collections::HashMap;

mod element;
mod error;
pub mod symtable;
mod macros;
mod scope;

type SemanticOk = (SymbolTable, Vec<SemanticWarning>);
type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

#[derive(Debug)]
pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,

    symtable: SymbolTable,
    macros: HashMap<String, MacrosObject>,
}

impl Analyzer {
    pub fn new(src: &str, filename: &str, is_main: bool) -> Self {
        let standart_macros = HashMap::from([
            // print!("value: {}", 15);
            (
                "print".to_string(),
                MacrosObject {
                    arguments: vec![Type::String],
                    is_first_literal: true,
                    is_var_args: true,
                    return_type: Type::Void,
                },
            ),

            // println!("value: {}", 15);
            (
                "println".to_string(),
                MacrosObject {
                    arguments: vec![Type::String],
                    is_first_literal: true,
                    is_var_args: true,
                    return_type: Type::Void,
                },
            ),

            // format!("str: {}", "hello")
            (
                "format".to_string(),
                MacrosObject {
                    arguments: vec![Type::String],
                    is_first_literal: true,
                    is_var_args: true,
                    return_type: Type::Pointer(Box::new(Type::Char)),
                },
            ),

            // panic!("number: {}", 15)
            (
                "panic".to_string(),
                MacrosObject {
                    arguments: vec![Type::String],
                    is_first_literal: true,
                    is_var_args: true,
                    return_type: Type::Void,
                },
            ),

            // panic!("number: {}", 15)
            (
                "sizeof".to_string(),
                MacrosObject {
                    arguments: vec![Type::Void],
                    is_first_literal: false,
                    is_var_args: false,
                    return_type: Type::USIZE,
                },
            ),
        ]);

        Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                scope
            },
            source: NamedSource::new(filename, src.to_owned()),

            errors: Vec::new(),
            warnings: Vec::new(),

            symtable: SymbolTable::default(),
            macros: standart_macros,
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        let pre_statements = ast.iter().filter(|stmt| {
            match stmt {
                Statements::StructDefineStatement { name: _, fields: _, functions: _, public: _, span: _ } => true,
                Statements::EnumDefineStatement { name: _, fields: _, functions: _, public: _, span: _ } => true,
                Statements::TypedefStatement { alias: _, datatype: _, span: _ } => true,
                Statements::ImportStatement { path: _, span: _ } => true,
                _ => false
            }
        }).collect::<Vec<&Statements>>();

        let after_statements = ast.iter().filter(|stmt| !pre_statements.contains(stmt));

        pre_statements.clone().into_iter().for_each(|stmt| self.visit_statement(stmt));
        after_statements.into_iter().for_each(|stmt| self.visit_statement(stmt));

        if self.scope.get_fn("main").is_none() && self.scope.is_main {
            let err = SemanticError {
                message: String::from("Program has no entry `main` function"),
                src: self.source.clone(),
                span: 0.into(),
            };

            self.errors.reverse();
            self.errors.push(err);
            self.errors.reverse();
        }

        if let Some(unused) = self.scope.check_unused_variables() {
            unused.iter().for_each(|var| {
                self.warning(format!("Unused variable `{}` found", var.0), var.1);
            });
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }

        Ok((self.symtable.clone(), self.warnings.clone()))
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });

        self.errors.push(error::SemanticError {
            message,
            span: span.into(),
            src: self.source.clone(),
        });
    }

    #[allow(unused)]
    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, if span.1 <= span.0 { 1 } else { span.1 - span.0 });

        self.warnings.push(error::SemanticWarning {
            message,
            span: span.into(),
            src: self.source.clone(),
        })
    }
}

impl Analyzer {
    fn visit_statement(&mut self, statement: &Statements) {
        // checking for allowed global scope statements
        if self.scope.parent.is_none() {
            match statement {
                Statements::FunctionDefineStatement {
                    name: _,
                    datatype: _,
                    arguments: _,
                    block: _,
                    public: _,
                    span: _,
                    header_span: _,
                } => {}
                Statements::ImportStatement { path: _, span: _ } => {}
                Statements::StructDefineStatement {
                    name: _,
                    fields: _,
                    functions: _,
                    public: _,
                    span: _,
                } => {}
                Statements::TypedefStatement {
                    alias: _,
                    datatype: _,
                    span: _,
                } => {}
                Statements::EnumDefineStatement {
                    name: _,
                    fields: _,
                    functions: _,
                    public: _,
                    span: _,
                } => {}
                Statements::ExternStatement { identifier: _, arguments: _, return_type: _, extern_type: _, public: _, is_var_args: _, span: _ } => {}
                _ => {
                    if let Some(err) = self.errors.last() {
                        if err.span == (255, 0).into() {
                            return;
                        };
                    }
                    self.error(
                        String::from(
                            "In global scope only allowed: functions definitions, imports",
                        ),
                        (255, 0),
                    );
                    return;
                }
            }
        }

        match statement {
            Statements::AssignStatement {
                object,
                value,
                span,
            } => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    if let Some(variable) = self.scope.get_var(identifier) {
                        let value_type = self.visit_expression(value, Some(variable.datatype.clone()));

                        if variable.datatype != value_type {
                            self.error(
                                format!(
                                    "Variable has type `{}`, but found `{}`",
                                    variable.datatype, value_type
                                ),
                                *span,
                            );
                            return;
                        }

                        self.scope
                            .set_init_var(identifier, true)
                            .unwrap_or_else(|err| {
                                self.error(err, *span);
                            });
                    } else {
                        self.error(
                            format!("Variable \"{}\" is not defined here", identifier),
                            *span,
                        );
                    }
                }
            }
            Statements::BinaryAssignStatement {
                object,
                operand,
                value,
                span,
            } => {
                self.visit_statement(&Statements::AssignStatement {
                    object: object.clone(),
                    span: *span,
                    value: Expressions::Binary {
                        operand: operand.clone(),
                        lhs: Box::new(object.clone()),
                        rhs: Box::new(value.clone()),
                        span: *span,
                    },
                });
            }
            Statements::DerefAssignStatement {
                object,
                value,
                span,
            } => {
                let instance = self.visit_expression(object, Some(Type::Pointer(Box::new(Type::Void))));
                if let Type::Pointer(ptr_type) = instance {
                    let value_type = self.visit_expression(value, Some(*ptr_type.clone()));

                    if value_type != *ptr_type {
                        self.error(
                            format!("Pointer expected type `{}`, but found `{}`", ptr_type, value_type),
                            *span
                        );
                        return;
                    }
                } else {
                    self.error(
                        format!("Type `{}` cannot be deref-assigned", instance),
                        *span
                    );
                    return;
                }
            }
            Statements::SliceAssignStatement {
                object,
                index,
                value,
                span,
            } => {
                let instance = self.visit_expression(object, None);
                match instance {
                    Type::Array(typ, _) => {
                        // i could spent some time to implement evaluating expressions for
                        // checking index out of bounds, but it will be like in Rust: panics at
                        // the runtime

                        let index_type = self.visit_expression(index, Some(Type::USIZE));

                        if index_type != Type::USIZE {
                            self.error(
                                format!(
                                    "Expected index with type `usize`, but found `{}`",
                                    index_type
                                ),
                                *span,
                            );
                        }

                        let value_type = self.visit_expression(value, Some(*typ.clone()));

                        if value_type != *typ {
                            self.error(
                                format!("Array has type `{}`, but found `{}`", typ, value_type),
                                *span,
                            );
                        }
                    }
                    Type::DynamicArray(typ) => {
                        let index_type = self.visit_expression(index, Some(Type::USIZE));

                        if index_type != Type::USIZE {
                            self.error(
                                format!(
                                    "Expected index with type `usize`, but found `{}`",
                                    index_type
                                ),
                                *span,
                            );
                        }

                        let value_type = self.visit_expression(value, Some(*typ.clone()));

                        if value_type != *typ {
                            self.error(
                                format!("Array has type `{}`, but found `{}`", typ, value_type),
                                *span,
                            );
                        }
                    }
                    _ => {
                        self.error(
                            format!("Unable to apply slicing to `{}` type", instance),
                            *span,
                        );
                    }
                }
            }
            Statements::FieldAssignStatement {
                object,
                value,
                span,
            } => {
                if let Expressions::SubElement {
                    head: _,
                    subelements: _,
                    span: _,
                } = object
                {
                } else {
                    self.error(
                        String::from("Field assign statement must have subelement expression"),
                        *span,
                    );
                    return;
                }

                let object_type = self.visit_expression(object, None);
                let unwrapped_object_type = self.unwrap_alias(&object_type).unwrap_or_else(|err| {
                    self.error(err, *span);
                    Type::Void
                });

                if unwrapped_object_type == Type::Void {
                    return;
                };

                let value_type = self.visit_expression(value, Some(unwrapped_object_type.clone()));
                if unwrapped_object_type != value_type {
                    self.error(
                        format!(
                            "Field has type `{}`, but found `{}`",
                            object_type, value_type
                        ),
                        *span,
                    );
                }
            }

            Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span,
            } => {
                // unwrapping type
                let display_type = datatype.clone();

                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value_type = self.visit_expression(value, Some(datatype.clone()));

                        if &value_type != datatype {
                            let unwrapped_datatype = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                self.error(err, *span);
                                Type::Void
                            });

                            if unwrapped_datatype != value_type {
                                if Self::is_integer(&value_type) && Self::is_integer(&unwrapped_datatype) {
                                    if Self::integer_order(&value_type) > Self::integer_order(&unwrapped_datatype)
                                    {
                                        self.error(
                                            format!(
                                                "Expected integer type `{}`, but found `{}`",
                                                display_type.unwrap(),
                                                value_type
                                            ),
                                            *span,
                                        );
                                    }
                                } else {
                                    self.error(
                                        format!(
                                            "Expected type `{}` but found `{}`",
                                            display_type.unwrap(),
                                            value_type
                                        ),
                                        *span,
                                    );
                                }
                            }
                        }

                        self.scope
                            .add_var(identifier.clone(), datatype.clone(), true, *span);
                    }
                    (Some(datatype), None) => {
                        self.scope
                            .add_var(identifier.clone(), datatype.clone(), false, *span);
                    }
                    (None, Some(value)) => {
                        let value_type = self.visit_expression(value, None);
                        self.scope
                            .add_var(identifier.clone(), value_type, true, *span);
                    }
                    (None, None) => {
                        self.error(format!("Variable `{}` has unknown type", identifier), *span);
                    }
                }
            }
            Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments,
                block,
                public,
                span,
                header_span,
            } => {
                if !self.scope.is_main && name == "main" {
                    self.error(
                        String::from("Function `main()` is not allowed not in main module"),
                        *span,
                    );
                    return;
                }

                if self.scope.get_fn(name).is_some() {
                    self.error(
                        format!("Function `{}` already declared!", name),
                        *header_span,
                    );
                    return;
                }

                let mut function_scope = Scope::new();

                self.scope
                    .add_fn(
                        name.clone(),
                        Type::Function(
                            arguments
                                .iter()
                                .map(|arg| arg.1.clone())
                                .collect::<Vec<Type>>(),
                            Box::new(datatype.clone()),
                            false
                        ),
                        *public,
                    )
                    .unwrap();

                function_scope.parent = Some(Box::new(self.scope.clone()));
                function_scope.expected = datatype.clone();

                arguments.iter().for_each(|arg| {
                    function_scope.add_var(arg.0.clone(), arg.1.clone(), true, *header_span)
                });
                self.scope = function_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));
                let exp = self
                    .unwrap_alias(&self.scope.expected)
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                        Type::Void
                    });
                let ret = self
                    .unwrap_alias(&self.scope.returned)
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                        Type::Void
                    });

                if exp != ret {
                    self.error(
                        format!(
                            "Function `{}` returns type `{}`, but found `{}`",
                            name, datatype, ret
                        ),
                        *header_span,
                    );
                }

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if *public && name == "main" {
                    self.error(
                        String::from("Function `main()` is not allowed to be public"),
                        *header_span,
                    );
                }
            }
            Statements::FunctionCallStatement {
                name,
                arguments,
                span,
            } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(format!("Function `{}` is not defined here", name), *span);
                    Type::Void
                });

                if func == Type::Void {
                    return;
                };
                if let Type::Function(func_args, func_type, is_var_args) = func {
                    let call_args = arguments
                        .iter()
                        .zip(func_args.clone())
                        .map(|(arg, exp)| self.visit_expression(arg, Some(exp)))
                        .collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        if is_var_args && call_args.len() >= func_args.len() {} else {
                            self.error(
                                format!(
                                    "Function `{}` has {} arguments, but found {}",
                                    name,
                                    func_args.len(),
                                    call_args.len()
                                ),
                                *span,
                            );
                            return;
                        }
                    }

                    call_args.iter().enumerate().zip(func_args).for_each(
                        |((ind, provided), expected)| {
                            if &expected != provided {
                                self.error(
                                    format!(
                                        "Argument #{} must be `{}`, but found `{}`",
                                        ind + 1,
                                        expected,
                                        provided
                                    ),
                                    Parser::get_span_expression(arguments[ind].clone()),
                                );
                            }
                        },
                    );

                    if *func_type != Type::Void {
                        self.warning(
                            format!("Unused `{}` result from function", func_type),
                            *span,
                        );
                    }
                } else {
                    unreachable!()
                }
            }

            Statements::MacroCallStatement {
                name,
                arguments,
                span,
            } => {
                let _ = self.verify_macrocall(name, arguments, span);
            }

            Statements::StructDefineStatement {
                name,
                functions,
                fields,
                public,
                span,
            } => {
                let pre_type = Type::Struct(fields.clone(), HashMap::new());
                self.scope.structures.insert(
                    name.clone(),
                    element::ScopeElement {
                        datatype: pre_type,
                        public: *public,
                    },
                );

                let mut structure_scope = Scope::new();
                structure_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = structure_scope;

                functions.iter().for_each(|func| {
                    let mut wrapped_statement = func.1.clone();
                    
                    if let Statements::FunctionDefineStatement { name: function_name, datatype, arguments, block, public, span, header_span } = wrapped_statement.clone() {
                        if let Some(_) = arguments.iter().find(|arg| arg.0 == "self" && arg.1 == Type::SelfRef) {
                            let mut arguments = arguments.clone();
                            *arguments.first_mut().unwrap() = (
                                String::from("self"),
                                Type::Alias(name.clone())
                            );

                            wrapped_statement = Statements::FunctionDefineStatement { name: function_name, datatype, arguments, block, public, span, header_span };
                        }
                    }

                    self.visit_statement(&wrapped_statement);
                });

                let functions_signatures = self.scope.functions.clone();
                self.scope = *self.scope.parent.clone().unwrap();

                let _ = self.scope.structures.remove(name);

                let struct_type = Type::Struct(
                    fields.clone(),
                    functions_signatures
                        .into_iter()
                        .map(|x| (x.0, x.1.datatype))
                        .collect(),
                );
                self.scope
                    .add_struct(name.clone(), struct_type.clone(), *public)
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                    });
                self.scope
                    .add_typedef(name.clone(), struct_type)
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                    })
            }
            Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public,
                span,
            } => {
                let pre_type = Type::Enum(fields.clone(), HashMap::new());
                self.scope.enums.insert(
                    name.clone(),
                    element::ScopeElement {
                        datatype: pre_type,
                        public: *public,
                    },
                );

                let mut enum_scope = Scope::new();
                enum_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = enum_scope;

                functions.iter().for_each(|func| {
                    self.visit_statement(func.1);
                });

                let functions_signatures = self.scope.functions.clone();
                self.scope = *self.scope.parent.clone().unwrap();

                let _ = self.scope.enums.remove(name);

                let enum_type = Type::Enum(
                    fields.clone(),
                    functions_signatures
                        .into_iter()
                        .map(|x| (x.0, x.1.datatype))
                        .collect(),
                );
                self.scope
                    .add_enum(name.clone(), enum_type.clone(), *public)
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                    });
                self.scope
                    .add_typedef(name.clone(), enum_type.clone())
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                    })
            }
            Statements::TypedefStatement {
                alias,
                datatype,
                span,
            } => {
                self.scope
                    .add_typedef(alias.clone(), datatype.clone())
                    .unwrap_or_else(|err| {
                        self.error(err, *span);
                    });
            }

            Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let condition_type = self.visit_expression(condition, None);

                if condition_type != Type::Bool {
                    self.error(
                        format!(
                            "Expected `bool` type in expression, but found `{}`",
                            condition_type
                        ),
                        *span,
                    );
                    return;
                }

                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = self.scope.expected.clone();
                self.scope = new_scope;

                then_block
                    .iter()
                    .for_each(|stmt| self.visit_statement(stmt));

                let then_block_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if then_block_type != self.scope.expected {
                    self.error(
                        format!(
                            "Expected type `{}` for scope, but found `{}`",
                            self.scope.expected, then_block_type
                        ),
                        *span,
                    );
                    return;
                }

                self.scope.returned = then_block_type.clone();
                if let Some(else_block) = else_block {
                    let mut new_scope = Scope::new();
                    new_scope.parent = Some(Box::new(self.scope.clone()));
                    new_scope.expected = self.scope.expected.clone();
                    self.scope = new_scope;

                    else_block
                        .iter()
                        .for_each(|stmt| self.visit_statement(stmt));

                    let else_block_type = self.scope.returned.clone();

                    if let Some(unused) = self.scope.check_unused_variables() {
                        unused.iter().for_each(|var| {
                            self.warning(format!("Unused variable `{}` found", var.0), var.1);
                        });
                    }

                    self.scope = *self.scope.parent.clone().unwrap();

                    if then_block_type != else_block_type {
                        self.error(
                            format!(
                                "Scopes has incompatible types: `{}` and `{}`",
                                then_block_type, else_block_type
                            ),
                            *span,
                        );
                    }
                }
            }
            Statements::WhileStatement {
                condition,
                block,
                span,
            } => {
                let condition_type = self.visit_expression(condition, None);

                if condition_type != Type::Bool {
                    self.error(
                        format!(
                            "Expected `bool` type in expression, but found `{}`",
                            condition_type
                        ),
                        *span,
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
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(
                            format!(
                                "Expected type `{}`, but found `{}`",
                                self.scope.expected, block_type
                            ),
                            *span,
                        );
                        return;
                    }

                    self.scope.returned = block_type;
                }
            }
            Statements::ForStatement {
                binding,
                iterator,
                block,
                span,
            } => {
                const BASIC_SUPPORTED_ITERATOR_TYPES: [Type; 9] = [
                    Type::I8,
                    Type::I16,
                    Type::I32,
                    Type::I64,
                    Type::U8,
                    Type::U16,
                    Type::U32,
                    Type::U64,
                    Type::String,
                ];

                let iterator_type = self.visit_expression(iterator, None);
                let mut binding_type = iterator_type.clone();

                match iterator_type {
                    typ if BASIC_SUPPORTED_ITERATOR_TYPES.contains(&typ) => {}
                    Type::Array(typ, _) => binding_type = *typ,
                    Type::DynamicArray(typ) => binding_type = *typ,
                    _ => {
                        self.error(
                            format!("Type `{}` is not supported for iteration", iterator_type),
                            *span,
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
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(
                            format!(
                                "Expected type `{}`, but found `{}`",
                                self.scope.expected, block_type
                            ),
                            *span,
                        );
                        return;
                    }

                    self.scope.returned = block_type;
                }
            }
            Statements::ImportStatement { path, span } => match path {
                Expressions::Value(Value::String(path), _) => {
                    let fname = std::path::Path::new(path)
                        .file_name()
                        .map(|fname| fname.to_str().unwrap_or("$NONE"));

                    match fname {
                        Some(fname) => {
                            if fname == "$NONE" {
                                self.error(format!("Unable to get module name: `{}`", path), *span);
                                return;
                            }

                            let src = std::fs::read_to_string(fname).unwrap_or_else(|err| {
                                self.error(format!("Unable to read `{}`: {}", fname, err), *span);
                                String::new()
                            });

                            if src.is_empty() {
                                return;
                            };

                            let mut lexer = deen_lexer::Lexer::new(&src, fname);
                            let (tokens, warns) = match lexer.tokenize() {
                                Ok(res) => res,
                                Err((errors, warns)) => {
                                    errors
                                        .iter()
                                        .for_each(|err| self.errors.push(err.clone().into()));
                                    warns
                                        .iter()
                                        .for_each(|warn| self.warnings.push(warn.clone().into()));
                                    return;
                                }
                            };
                            warns
                                .iter()
                                .for_each(|warn| self.warnings.push(warn.clone().into()));

                            let mut parser = deen_parser::Parser::new(tokens, &src, fname);
                            let (ast, warns) = match parser.parse() {
                                Ok(res) => res,
                                Err((errors, warns)) => {
                                    errors
                                        .iter()
                                        .for_each(|err| self.errors.push(err.clone().into()));
                                    warns
                                        .iter()
                                        .for_each(|warn| self.warnings.push(warn.clone().into()));
                                    return;
                                }
                            };
                            warns
                                .iter()
                                .for_each(|warn| self.warnings.push(warn.clone().into()));

                            let mut mutual_import = false;
                            ast.iter().for_each(|stmt| {
                                if let Statements::ImportStatement {
                                    path: Expressions::Value(Value::String(path), _),
                                    span,
                                } = stmt
                                {
                                    let imp_name = std::path::Path::new(path)
                                        .file_name()
                                        .map(|fname| fname.to_str().unwrap_or("$NONE"));

                                    if imp_name == Some(self.source.name()) {
                                        self.error(
                                            format!(
                                                "Mutual import found: `{}` from `{}`",
                                                imp_name.unwrap(),
                                                fname
                                            ),
                                            *span,
                                        );
                                        mutual_import = true;
                                    }
                                }
                            });

                            if mutual_import {
                                return;
                            };

                            let mut analyzer = Self::new(&src, fname, false);
                            let (embedded_symtable, warns) = match analyzer.analyze(&ast) {
                                Ok(warns) => warns,
                                Err((errors, warns)) => {
                                    errors.iter().for_each(|err| self.errors.push(err.clone()));
                                    warns
                                        .iter()
                                        .for_each(|warn| self.warnings.push(warn.clone()));
                                    return;
                                }
                            };
                            warns
                                .iter()
                                .for_each(|warn| self.warnings.push(warn.clone()));

                            let module_name = fname
                                .split(".")
                                .nth(0)
                                .map(|n| n.to_string())
                                .unwrap_or(fname.replace(".dn", ""));

                            let mut import = Import::new(ast, &src);

                            analyzer.scope.functions.into_iter().for_each(|func| {
                                if func.1.public {
                                    import.add_fn(
                                        format!("{}", func.0),
                                        func.1.datatype,
                                    );
                                }
                            });

                            analyzer.scope.structures.into_iter().for_each(|structure| {
                                if structure.1.public {
                                    import.add_struct(
                                        format!("{}", structure.0),
                                        structure.1.datatype,
                                    );
                                }
                            });

                            analyzer.scope.enums.into_iter().for_each(|enumeration| {
                                if enumeration.1.public {
                                    import.add_enum(
                                        format!("{}", enumeration.0),
                                        enumeration.1.datatype,
                                    );
                                }
                            });

                            import.embedded_symtable = embedded_symtable;
                            self.symtable.imports.insert(module_name, import);
                        }
                        None => {
                            self.error(format!("Unable to find: `{}`", path), *span);
                        }
                    }
                }
                _ => {
                    self.error(String::from("Import must be string constant"), *span);
                }
            },

            Statements::ExternStatement { identifier, arguments, return_type, public, extern_type, is_var_args, span } => {
                const SUPPORTED_EXTERN_TYPES: [&'static str; 1] = ["C"];

                if !SUPPORTED_EXTERN_TYPES.contains(&extern_type.as_str()) {
                    self.error(
                        format!("Unsupported extern type found. Currently supported are: \"{}\"", SUPPORTED_EXTERN_TYPES.join("\", ")),
                        *span
                    )
                }

                if identifier == "main" {
                    self.error(
                        String::from("Function `main()` cannot be external declared"),
                        *span
                    );
                }
                if self.scope.get_fn(&identifier).is_some() {
                    self.error(
                        format!("Function `{}()` is already declared", &identifier),
                        *span
                    );
                    return;
                }

                self.scope.add_fn(
                    identifier.clone(),
                    Type::Function(
                        arguments.clone(),
                        Box::new(return_type.clone()),
                        *is_var_args
                    ),
                    *public
                ).unwrap_or_else(|err| {
                    self.error(err, *span);
                });
            },

            Statements::BreakStatements { span } => {
                if !self.scope.is_loop() {
                    self.error(String::from("Used `break` keyword outside loop"), *span);
                }
            }
            Statements::ReturnStatement { value, span: _ } => {
                let value_type = self.visit_expression(value, Some(self.scope.expected.clone()));

                if Self::is_integer(&value_type)
                    && Self::is_integer(&self.scope.expected)
                    && Self::integer_order(&value_type) <= Self::integer_order(&self.scope.expected)
                {
                    self.scope.returned = self.scope.expected.clone();
                    return;
                }

                if Self::is_float(&value_type)
                    && Self::is_integer(&self.scope.expected)
                    && Self::float_order(&value_type) <= Self::float_order(&self.scope.expected)
                {
                    self.scope.returned = self.scope.expected.clone();
                    return;
                }

                self.scope.returned = value_type;
            }
            Statements::ScopeStatement { block, span: _ } => {
                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();
            }

            Statements::Expression(expr) => {
                let expr_type = self.visit_expression(expr, None);
                if expr_type != Type::Void {
                    self.warning(
                        String::from("Unused expression result found"),
                        deen_parser::Parser::get_span_expression(expr.clone()),
                    );
                }
            }
            Statements::None => unreachable!(),
        }
    }

    fn visit_expression(&mut self, expr: &Expressions, expected: Option<Type>) -> Type {
        match expr {
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span,
            } => {
                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, Some(left.clone()));

                match (left.clone(), right.clone()) {
                    (l, r) if Self::is_integer(&l) && Self::is_integer(&r) => {
                        if Self::is_unsigned_integer(&l) && !Self::is_unsigned_integer(&r) {
                            return r;
                        };
                        if Self::is_unsigned_integer(&r) && !Self::is_unsigned_integer(&l) {
                            return l;
                        };

                        if Self::integer_order(&l) > Self::integer_order(&r) {
                            l
                        } else {
                            r
                        }
                    }

                    (l, r) if Self::is_float(&l) && Self::is_float(&r) => {
                        if Self::float_order(&l) > Self::float_order(&r) {
                            l
                        } else {
                            r
                        }
                    }

                    _ => {
                        self.error(
                            format!(
                                "Cannot apply binary \"{}\" to `{}` and `{}`",
                                operand, left, right
                            ),
                            *span,
                        );
                        left
                    }
                }
            }
            Expressions::Unary {
                operand,
                object,
                span,
            } => {
                let obj = self.visit_expression(object, expected);

                match (&obj, operand.as_str()) {
                    (typ, "-") if Self::is_integer(typ) => {
                        if Self::is_unsigned_integer(typ) {
                            return Self::unsigned_to_signed_integer(typ);
                        }
                        obj
                    }
                    (typ, "-") if Self::is_float(typ) => obj,
                    (typ, "!") if Self::is_integer(typ) => obj,
                    (Type::Bool, "!") => obj,

                    _ => {
                        self.error(
                            format!("Cannot apply unary \"{}\" to `{}`", operand, obj),
                            *span,
                        );
                        obj
                    }
                }
            }
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span,
            } => {
                const SUPPORTED_EXTRA_TYPES: [Type; 3] = [Type::String, Type::Bool, Type::Char];

                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, Some(left.clone()));

                match (left.clone(), right.clone()) {
                    (l, r) if Self::is_integer(&l) && Self::is_integer(&r) => Type::Bool,
                    (l, r) if Self::is_float(&l) && Self::is_float(&r) => Type::Bool,
                    (l, r) if l == r && SUPPORTED_EXTRA_TYPES.contains(&l) => Type::Bool,
                    (Type::Pointer(l), Type::Pointer(r)) if l == r && *l == Type::Char => {
                        Type::Bool
                    }

                    _ => {
                        self.error(
                            format!(
                                "Cannot apply boolean \"{}\" to `{}` and `{}`",
                                operand, left, right
                            ),
                            *span,
                        );
                        Type::Bool
                    }
                }
            }
            Expressions::Bitwise {
                operand,
                lhs,
                rhs,
                span,
            } => {
                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, Some(Type::U8));

                if !Self::is_integer(&left) || !Self::is_integer(&right) {
                    self.error(
                        format!(
                            "Cannot apply bitwise operations to `{}` and `{}`",
                            &left, &right
                        ),
                        *span,
                    );
                    return left;
                };

                if [">>", "<<"].contains(&operand.as_ref()) && !Self::is_unsigned_integer(&right) {
                    self.error("Shift index must be unsigned integer".to_string(), *span);
                    return left;
                }

                if Self::integer_order(&left) > Self::integer_order(&right) {
                    left
                } else {
                    right
                }
            }

            Expressions::Argument {
                name,
                r#type,
                span,
            } => {
                if name == "@deen_type" {
                    return Type::Void;
                }

                self.error(
                    String::from("Argument expressions isn't supported in global code"),
                    *span
                );
                r#type.clone()
            },
            Expressions::SubElement {
                head,
                subelements,
                span,
            } => {
                let head_type = self.visit_expression(head, expected.clone());

                let mut prev_type_display = head_type.clone();
                let mut prev_type = self.unwrap_alias(&head_type).unwrap_or_else(|err| {
                    self.error(err, *span);
                    Type::Void
                });
                let mut prev_expr = *head.clone();
                if prev_type == Type::Void {
                    return head_type;
                };

                subelements.iter().for_each(|sub| {
                    let mut is_ptr = false;
                    if let Type::Pointer(ptr_type) = prev_type.clone() {
                        is_ptr = true;
                        prev_type = *ptr_type;
                    }
                    match sub {
                        Expressions::Value(Value::Identifier(field), field_span) => {
                            match prev_type.clone() {
                                Type::Struct(fields, _) => {
                                    let field_type = fields.get(&field.clone()).unwrap_or_else(|| {
                                        self.error(
                                            format!("Type `{}` has no field named `{}`", prev_type_display, field),
                                            *field_span
                                        );
                                        &Type::Void
                                    });

                                    prev_type_display = field_type.clone();
                                    prev_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                        self.error(err, *field_span);
                                        Type::Void
                                    });

                                    if is_ptr {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    } else {
                                        if let Some(Type::Pointer(_)) = expected.clone() {
                                            prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                        }
                                    }

                                    prev_expr = sub.clone();
                                }
                                Type::Enum(fields, _) => {
                                    let opt = fields.iter().find(|&x| x == field);
                                    if opt.is_none() {
                                        self.error(
                                            format!("Type `{}` has no choice named `{}`", prev_type_display, field),
                                            *field_span
                                        );
                                    }

                                    prev_expr = Expressions::SubElement { head: Box::new(prev_expr.clone()), subelements: vec![], span: *field_span }
                                },
                                _ => {
                                    self.error(
                                        format!("Type `{}` has no accessible fields", prev_type_display),
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
                                            format!("Type `{}` has {} fields, but index is {}", prev_type_display, types.len(), idx),
                                            *idx_span
                                        );
                                        return;
                                    }

                                    let typ = types[*idx as usize].clone();
                                    prev_type_display = typ.clone();
                                    prev_type = self.unwrap_alias(&typ).unwrap_or_else(|err| {
                                        self.error(err, *idx_span);
                                        Type::Void
                                    });

                                    if is_ptr {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    } else {
                                        if let Some(Type::Pointer(_)) = expected.clone() {
                                            prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                        }
                                    }
                                    prev_expr = sub.clone();
                                },
                                _ => {
                                    self.error(
                                        format!("Type `{}` has no numbered fields", prev_type_display),
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
                                            format!("Type `{}` has no function named `{}`", prev_type_display, name),
                                            *span
                                        );
                                        &Type::Void
                                    });

                                    if let Type::Function(args, datatype, is_var_args) = function_type {
                                        let mut arguments = arguments.clone();

                                        if let Some(Type::Alias(alias)) = args.first() {
                                            if Type::Alias(alias.clone()) == prev_type_display {
                                                arguments.reverse();
                                                arguments.push(
                                                    Expressions::Reference { object: Box::new(prev_expr.clone()), span: (deen_parser::Parser::get_span_expression(prev_expr.clone())) },
                                                );
                                                arguments.reverse();
                                            }
                                        }

                                        prev_type_display = *datatype.clone();
                                        prev_type = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                            self.error(err, *span);
                                            Type::Void
                                        });

                                        if arguments.len() != args.len() {
                                            if *is_var_args && arguments.len() >= args.len() {} else {
                                                self.error(
                                                    format!("Function `{}` has {} arguments, but found {}", name, args.len(), arguments.len()),
                                                    *span
                                                );
                                                return;
                                            }
                                        }

                                        arguments.iter().enumerate().zip(args).for_each(|((index, expr), expected)| {
                                            let raw_expr_type = self.visit_expression(expr, Some(expected.clone()));
                                            let expr_type = self.unwrap_alias(&raw_expr_type).unwrap_or_else(|err| {
                                                    self.error(err, *span);
                                                    Type::Void
                                            });
                                            let raw_expected = expected;
                                            let expected = self.unwrap_alias(expected).unwrap_or_else(|err| {
                                                self.error(err, *span);
                                                Type::Void
                                            });

                                            if expected == Type::Void || raw_expr_type == Type::Void { return };
                                            if let Type::Pointer(ptr_type) = raw_expr_type.clone() {
                                                if *ptr_type.clone() == *raw_expected { return };
                                            }
                                            if expr_type != expected {
                                                self.error(
                                                    format!("Argument #{} has type `{}`, but found `{}`", index + 1, raw_expected, raw_expr_type),
                                                    deen_parser::Parser::get_span_expression(expr.clone())
                                                );
                                            }
                                        });
                                    };
                                },
                                Type::ImportObject(imp) => {
                                    let import = self.symtable.imports.get(&imp).unwrap().clone();
                                    let name = format!("{}.{}", imp, name);

                                    if let Some(Type::Function(args, datatype, is_var_args)) = import.functions.get(&name) {
                                        prev_type_display = *datatype.clone();
                                        prev_type = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                            self.error(err, *span);
                                            Type::Void
                                        });

                                        if arguments.len() != args.len() {
                                            if *is_var_args && arguments.len() >= args.len() {} else {
                                                self.error(
                                                    format!("Function `{}()` has {} arguments, but found {}", name, args.len(), arguments.len()),
                                                    *span
                                                );
                                                return;
                                            }
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
                                                    deen_parser::Parser::get_span_expression(expr.clone())
                                                );
                                            }
                                        });
                                    } else {
                                        self.error(
                                            format!("Import `{}` has no functions named `{}()`", imp, name),
                                            *span
                                        );
                                    };
                                }
                                // Type::Enum(_, functions) => {},
                                _ => {
                                    self.error(
                                        format!("Type `{}` isn't supported for function calls", prev_type),
                                        *span
                                    );
                                }
                            }
                        },
                        Expressions::Struct { name, fields, span } => {
                            match prev_type.clone() {
                                Type::ImportObject(imp) => {
                                    let import = self.symtable.imports.get(&imp).unwrap().clone();

                                    let name = format!("{}.{}", imp, name); 
                                    if let Some(Type::Struct(struct_fields, _)) = import.structs.get(&name) {

                                        let mut assigned_fields = HashMap::new();
                                        struct_fields.iter().for_each(|x| {
                                            assigned_fields.insert(x.0, false);
                                        });

                                        fields.iter().for_each(|field| {
                                            let struct_field = struct_fields.get(field.0);
                                            if let Some(field_type) = struct_field {
                                                let field_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                                    self.error(err, *span);
                                                    Type::Void
                                                });
                                                let provided_type = self.visit_expression(field.1, Some(field_type.clone()));

                                                if field_type != provided_type && field_type != Type::Void {
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

                                        prev_type_display = Type::Alias(name.clone());
                                        prev_type = import.structs.get(&name).unwrap().clone();
                                    }
                                }
                                _ => {
                                    self.error(
                                        String::from("Unsupported structure init found"),
                                        *span
                                    );
                                }
                            }
                        }
                        _ => {
                            self.error(
                                String::from("Unsupported subelement expression found"),
                                *span
                            );
                        }
                    }
                });

                prev_type_display
            }

            Expressions::FnCall {
                name,
                arguments,
                span,
            } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(format!("Function `{}` is not defined here", name), *span);
                    Type::Void
                });

                if func == Type::Void {
                    return func;
                };
                if let Type::Function(func_args, func_type, is_var_args) = func {
                    let call_args = arguments
                        .iter()
                        .zip(func_args.clone())
                        .map(|(arg, exp)| self.visit_expression(arg, Some(exp)))
                        .collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        if is_var_args && call_args.len() >= func_args.len() {} else {
                            self.error(
                                format!(
                                    "Function `{}` has {} arguments, but found {}",
                                    &name,
                                    func_args.len(),
                                    call_args.len()
                                ),
                                *span,
                            );
                            return *func_type;
                        }
                    }

                    call_args.iter().enumerate().zip(func_args).for_each(
                        |((ind, provided), expected)| {
                            if &expected != provided {
                                self.error(
                                    format!(
                                        "Argument #{} must be `{}`, but found `{}`",
                                        ind + 1,
                                        expected,
                                        provided
                                    ),
                                    Parser::get_span_expression(arguments[ind].clone()),
                                );
                            }
                        },
                    );

                    *func_type
                } else {
                    unreachable!()
                }
            }

            Expressions::MacroCall {
                name,
                arguments,
                span,
            } => self.verify_macrocall(name, arguments, span),

            Expressions::Reference { object, span: _ } => {
                let obj = self.visit_expression(object, expected);

                Type::Pointer(Box::new(obj))
            }
            Expressions::Dereference { object, span } => {
                let obj = self.visit_expression(object, expected);
                if let Type::Pointer(ptr_type) = obj {
                    *ptr_type
                } else {
                    self.error(format!("Type {} cannot be dereferenced!", obj), *span);
                    obj
                }
            }

            Expressions::Array { values, len, span } => {
                if *len < 1 {
                    self.error("Empty array type is unknown".to_string(), *span);
                    return Type::Void;
                }

                let arr_type = self.visit_expression(&values[0], None);

                values.iter().for_each(|val| {
                    let val_type = self.visit_expression(val, None);
                    if val_type != arr_type {
                        self.error(
                            format!("Array has type {}, but element has {}", arr_type, val_type),
                            Parser::get_span_expression(val.clone()),
                        );
                    }
                });

                Type::Array(Box::new(arr_type), *len)
            }
            Expressions::Tuple { values, span } => {
                if values.is_empty() {
                    self.error("Unknown by compilation time tuple found".to_string(), *span);
                    return Type::Void;
                }

                let mut expected_types = values.iter().map(|_| None).collect::<Vec<Option<Type>>>();
                if let Some(Type::Tuple(expectations)) = expected.clone() {
                    expected_types = expectations.into_iter().map(Some).collect();
                }

                let types = values
                    .iter()
                    .zip(expected_types)
                    .map(|(val, exp)| self.visit_expression(val, exp))
                    .collect::<Vec<Type>>();

                Type::Tuple(types)
            }
            Expressions::Slice {
                object,
                index,
                span,
            } => {
                let obj = self.visit_expression(object, expected);

                match obj {
                    Type::Tuple(types) => {
                        if let Expressions::Value(Value::Integer(ind), _) = **object {
                            if ind < 0 {
                                self.error(
                                    "Tuple index must be unsigned".to_string(),
                                    Parser::get_span_expression(*index.clone()),
                                );
                                return Type::Void;
                            }

                            types[ind as usize].clone()
                        } else {
                            self.error(
                                "Tuple index must be known by compile-time".to_string(),
                                Parser::get_span_expression(*index.clone()),
                            );
                            Type::Void
                        }
                    }
                    Type::Array(tty, _) => *tty,
                    Type::DynamicArray(tty) => *tty,

                    _ => {
                        self.error(format!("Type `{}` is not supported for slice", obj), *span);
                        Type::Void
                    }
                }
            }
            Expressions::Struct { name, fields, span } => {
                let structure = self.scope.get_struct(name).unwrap_or_else(|| {
                    self.error(format!("Structure `{}` does not exist here", name), *span);
                    Type::Void
                });

                if structure == Type::Void {
                    return Type::Void;
                };
                if let Type::Struct(struct_fields, _) = structure.clone() {
                    let mut assigned_fields = HashMap::new();
                    struct_fields.iter().for_each(|x| {
                        assigned_fields.insert(x.0, false);
                    });

                    fields.iter().for_each(|field| {
                        let struct_field = struct_fields.get(field.0);
                        if let Some(field_type) = struct_field {
                            let field_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                self.error(err, *span);
                                Type::Void
                            });
                            let provided_type =
                                self.visit_expression(field.1, Some(field_type.clone()));

                            if field_type != provided_type && field_type != Type::Void {
                                self.error(
                                    format!(
                                        "Field `{}` expected to be type `{}`, but found `{}`",
                                        field.0, field_type, provided_type
                                    ),
                                    *span,
                                );
                                return;
                            }

                            let _ = assigned_fields.insert(field.0, true);
                        } else {
                            self.error(
                                format!("Field `{}` doesn't exist in struct `{}`", field.0, name),
                                *span,
                            );
                        }
                    });

                    let unassigned = assigned_fields
                        .iter()
                        .filter(|x| !x.1)
                        .map(|x| x.0.to_owned().to_owned())
                        .collect::<Vec<String>>();
                    if !unassigned.is_empty() {
                        let fmt = format!("`{}`", unassigned.join("` , `"));
                        self.error(format!("Missing structure fields: {}", fmt), *span);
                    }

                    Type::Alias(name.clone())
                } else {
                    unreachable!()
                }
            }
            Expressions::Scope { block, span: _ } => {
                let mut new_scope = Scope::new();
                new_scope.parent = Some(Box::new(self.scope.clone()));
                new_scope.expected = expected.unwrap_or(Type::Void);
                self.scope = new_scope;

                block.iter().for_each(|stmt| self.visit_statement(stmt));

                let scope_type = self.scope.returned.clone();

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(format!("Unused variable `{}` found", var.0), var.1);
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                scope_type
            }

            Expressions::Value(value, span) => match self.visit_value(value.clone(), expected) {
                Ok(tty) => tty,
                Err(err) => {
                    self.error(err, *span);
                    Type::Void
                }
            },
            Expressions::None => Type::Void,
        }
    }

    fn visit_value(&mut self, value: Value, expected: Option<Type>) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
                if expected.is_some() && Self::is_integer(&expected.clone().unwrap()) {
                    let exp = expected.unwrap();
                    match exp {
                        Type::I8 => {
                            if int < i8::MIN as i64 || int > i8::MAX as i64 {
                                return Err(String::from("Constant is out of `i8` type range"));
                            }
                        }
                        Type::I16 => {
                            if int < i16::MIN as i64 || int > i16::MAX as i64 {
                                return Err(String::from("Constant is out of `i16` type range"));
                            }
                        }
                        Type::I32 => {
                            if int < i32::MIN as i64 || int > i32::MAX as i64 {
                                return Err(String::from("Constant is out of `i32` type range"));
                            }
                        }
                        Type::I64 => {
                            if !(i64::MIN..=i64::MAX).contains(&int) {
                                return Err(String::from("Constant is out of `i64` type range"));
                            }
                        }

                        Type::U8 => {
                            if int < 0 || int > u8::MAX as i64 {
                                return Err(String::from("Constant is out of `u8` type range"));
                            }
                        }
                        Type::U16 => {
                            if int < 0 || int > u16::MAX as i64 {
                                return Err(String::from("Constant is out of `u16` type range"));
                            }
                        }
                        Type::U32 => {
                            if int < 0 || int > u32::MAX as i64 {
                                return Err(String::from("Constant is out of `u32` type range"));
                            }
                        }
                        Type::U64 => {
                            if int < 0 {
                                return Err(String::from("Constant is out of `u64` type range"));
                            }
                        }
                        Type::USIZE => {
                            if int < 0 {
                                return Err(String::from("Constant is out of `usize` type range"));
                            }
                        }

                        _ => return Err(format!("Expected `{}` but found integer constant", exp)),
                    }

                    return Ok(exp);
                }

                let signed = !Self::is_unsigned_integer(&expected.unwrap_or(Type::Void));
                if int > i32::MAX as i64 {
                    if !signed {
                        if int < 0 {
                            return Err(String::from("Expected unsigned value but found signed"));
                        }
                        return Ok(Type::U64);
                    }
                    return Ok(Type::I64);
                }
                Ok(Type::I32)
            }
            Value::Float(float) => {
                if float > f32::MAX as f64 {
                    return Ok(Type::F64);
                }
                Ok(Type::F32)
            }
            Value::Identifier(id) => {
                if self.symtable.imports.contains_key(&id) {
                    return Ok(Type::ImportObject(id));
                }

                if let Some(_) = self.scope.get_struct(&id) {
                    return Ok(Type::Alias(id));
                }
                if let Some(typedef) = self.scope.get_typedef(&id) {
                    return Ok(typedef);
                }
                if let Some(enumeration) = self.scope.get_enum(&id) {
                    return Ok(enumeration);
                }

                match self.scope.get_var(&id) {
                    Some(var) => {
                        if !var.initialized {
                            return Err(format!("Variable `{}` isn't initalized", id));
                        }
                        Ok(var.datatype)
                    }
                    None => Err(format!("Variable `{}` is not defined here", id)),
                }
            }
            Value::String(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            Value::Char(_) => Ok(Type::Char),
            Value::Boolean(_) => Ok(Type::Bool),
            Value::Keyword(_) => Ok(Type::Void),
            Value::Void => Ok(Type::Void),
        }
    }
}

impl Analyzer {
    pub fn verify_macrocall(
        &mut self,
        name: &String,
        arguments: &Vec<Expressions>,
        span: &(usize, usize),
    ) -> Type {
        if let Some(macro_object) = self.macros.get(name).cloned() {
            if arguments.len() < macro_object.arguments.len() {
                self.error(
                    format!(
                        "Not enough arguments. Expected {}",
                        macro_object.arguments.len()
                    ),
                    *span,
                );
                return macro_object.return_type;
            }

            if macro_object.is_first_literal {
                if let Some(Expressions::Value(Value::String(literal), literal_span)) =
                    arguments.first()
                {
                    let mut bindings: Vec<Type> = Vec::new();

                    let mut cursor = 0;
                    let characters = literal.chars().collect::<Vec<char>>();

                    while characters.get(cursor).is_some() {
                        if let Some('{') = characters.get(cursor) {
                            cursor += 1;
                            let next = characters.get(cursor);

                            match next {
                                Some('}') => bindings.push(Type::Void),
                                _ => self.error(
                                    String::from("Unexpected binding in string found"),
                                    *literal_span,
                                ),
                            }
                        }

                        cursor += 1;
                    }

                    if arguments.len() != bindings.len() + 1 {
                        self.error(
                            format!(
                                "Expected {} arguments, but found {}",
                                bindings.len() + 1,
                                arguments.len()
                            ),
                            *span,
                        );
                        return macro_object.return_type;
                    }

                    let mut arguments_iterator = arguments.iter();
                    let _ = arguments_iterator.next();

                    arguments_iterator.for_each(|expr| {
                        let expr_type = self.visit_expression(expr, None);

                        match expr_type.clone() {
                            int if Self::is_integer(&int) => {}
                            float if Self::is_float(&float) => {},
                            Type::Bool => {},
                            Type::Pointer(ptr) => {
                                match *ptr {
                                    Type::Char => {},
                                    _ => {
                                        self.error(
                                            format!("Type `{}` must be dereferenced to be displayed", expr_type),
                                            deen_parser::Parser::get_span_expression(expr.clone())
                                        )
                                    }
                                }
                            }
                            Type::Struct(_, functions) => {
                                if let Some(Type::Function(_, return_type, _)) = functions.get("display") {
                                    if let Type::Pointer(ptr) = *return_type.clone() {
                                        if *ptr.clone() == Type::Char {} else {
                                            self.error(
                                                "Implementation for display must be `display(&self) *char`".to_string(),
                                                deen_parser::Parser::get_span_expression(expr.clone())
                                            );
                                        }
                                    } else {
                                        self.error(
                                            "Implementation for display must be `display(&self) *char`".to_string(),
                                            deen_parser::Parser::get_span_expression(expr.clone())
                                        );
                                    }
                                } else {
                                    self.error(
                                        format!("Type `{}` has no implementation for display: `display(&self) *char", expr_type),
                                        deen_parser::Parser::get_span_expression(expr.clone())
                                    );
                                }
                            }
                            Type::Alias(alias) => {
                                if let Some(Type::Struct(_, functions)) = self.scope.get_struct(&alias) {
                                    if let Some(Type::Function(_, return_type, _)) = functions.get("display") {
                                        if let Type::Pointer(ptr) = *return_type.clone() {
                                            if *ptr.clone() == Type::Char {} else {
                                                self.error(
                                                    "Implementation for display must be `display(&self) *char`".to_string(),
                                                    deen_parser::Parser::get_span_expression(expr.clone())
                                                );
                                            }
                                        } else {
                                            self.error(
                                                "Implementation for display must be `display(&self) *char`".to_string(),
                                                deen_parser::Parser::get_span_expression(expr.clone())
                                            );
                                        }
                                    } else {
                                        self.error(
                                            format!("Type `{}` has no implementation for display: `display(&self) *char", expr_type),
                                            deen_parser::Parser::get_span_expression(expr.clone())
                                        );
                                    }
                                } else {
                                    self.error(
                                        format!("No displayable type with name `{}` found", expr_type),
                                        deen_parser::Parser::get_span_expression(expr.clone())
                                    );
                                }
                            }
                            Type::Enum(_, _) => {},
                            Type::Char => {},
                            _ => {
                                self.error(
                                    format!("Type `{}` is not supported for display", expr_type),
                                    deen_parser::Parser::get_span_expression(expr.clone())
                                );
                            }
                        }
                    });

                    return macro_object.return_type;
                } else {
                    self.error(
                        String::from("Macro requires string literal as first argument"),
                        *span,
                    );
                    return macro_object.return_type;
                }
            }

            macro_object
                .arguments
                .iter()
                .enumerate()
                .zip(arguments)
                .for_each(|((index, expected), expression)| {
                    let provided = self.visit_expression(expression, Some(expected.clone()));
                    if &provided != expected && expected != &Type::Void {
                        self.error(
                            format!(
                                "Argument #{} expected to be `{}`, but found `{}`",
                                index, expected, provided
                            ),
                            deen_parser::Parser::get_span_expression(expression.clone()),
                        );
                    };
                });

            if macro_object.arguments.len() < arguments.len() && !macro_object.is_var_args {
                self.error(
                    format!(
                        "Too much arguments! Expected {} but found {} args",
                        macro_object.arguments.len(),
                        arguments.len()
                    ),
                    *span,
                );
            }

            macro_object.return_type
        } else {
            self.error(format!("There's no macros called `{}!`", name), *span);
            Type::Void
        }
    }
}

impl Analyzer {
    #[inline]
    pub fn is_integer(typ: &Type) -> bool {
        [
            Type::I8,
            Type::I16,
            Type::I32,
            Type::I64,
            Type::U8,
            Type::U16,
            Type::U32,
            Type::U64,
            Type::USIZE,
        ]
        .contains(typ)
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

            _ => Type::I32,
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

            _ => unreachable!(),
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

            _ => unreachable!(),
        }
    }

    #[inline]
    fn unwrap_alias(&self, typ: &Type) -> Result<Type, String> {
        match typ {
            Type::Alias(alias) => {
                let struct_type = self.scope.get_struct(alias);
                let enum_type = self.scope.get_enum(alias);
                let typedef_type = self.scope.get_typedef(alias);

                if let Some(struct_type) = struct_type {
                    return Ok(struct_type);
                };
                if let Some(enum_type) = enum_type {
                    return Ok(enum_type);
                };
                if let Some(typedef_type) = typedef_type {
                    return Ok(typedef_type);
                };
                
                if alias.contains(".") {
                    let splitted_alias = alias.split(".").collect::<Vec<&str>>();
                    let module_name = splitted_alias[0];

                    if let Some(import_object) = self.symtable.imports.get(module_name) {
                        let struct_type = import_object.get_struct(alias);
                        let enum_type = import_object.get_enum(alias);

                        if let Some(struct_type) = struct_type {
                            return Ok(struct_type);
                        }

                        if let Some(enum_type) = enum_type {
                            return Ok(enum_type);
                        }
                    } else {
                        return Err(format!("Import `{}` is not declared here", module_name))
                    }
                }

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

            _ => Ok(typ.clone()),
        }
    }
}
