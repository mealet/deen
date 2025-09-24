//! # Deen Semantical Analyzer
//! Toolkit for analyzing and checking statements/expressions from [`deen_parser`]. <br/>
//! Wikipedia Explanation: <https://en.wikipedia.org/wiki/Semantic_analysis_(compilers)>
//!
//! Main tool is the [`Analyzer`] structure
//!
//! ## Usage
//! ```ignore
//! use deen_semantic::Analyzer
//!
//! let ast = {
//!     // ...
//! };
//!
//! let mut analyzer = Analyzer::new("source code", "source name", true); // if module is main - true, otherwise - false
//! match analyzer.analyze(&ast) {
//!     Ok((symbol_table, warnings)) => {},
//!     Err((errors, warnings)) => {},
//! }
//! ```

use crate::{
    error::{SemanticError, SemanticWarning},
    macros::{CastMacro, FormatMacro, PanicMacro, PrintMacro, PrintlnMacro, SizeofMacro},
    macros::{CompilerMacros, MacroObject},
    scope::Scope,
    symtable::{Include, SymbolTable},
};
use deen_parser::{
    Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
};
use indexmap::IndexMap;
use miette::NamedSource;
use std::{
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

mod element;
mod error;
mod macros;
mod scope;
/// Semantic Analyzer Symbol Table
pub mod symtable;

pub type SemanticOk = (SymbolTable, Vec<SemanticWarning>);
pub type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

const STANDARD_LIBRARY_VAR: &str = "DEEN_LIB";

/// Main Analyzer Struct
#[derive(Debug)]
pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,
    source_path: PathBuf,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,

    symtable: SymbolTable,
    compiler_macros: HashMap<String, CompilerMacros>,
}

impl Analyzer {
    pub fn new(src: &str, filename: &str, source_path: PathBuf, is_main: bool) -> Self {
        let compiler_macros = HashMap::from([
            (
                String::from("print"),
                CompilerMacros::PrintMacro(PrintMacro),
            ),
            (
                String::from("println"),
                CompilerMacros::PrintlnMacro(PrintlnMacro),
            ),
            (
                String::from("format"),
                CompilerMacros::FormatMacro(FormatMacro),
            ),
            (
                String::from("panic"),
                CompilerMacros::PanicMacro(PanicMacro),
            ),
            (
                String::from("sizeof"),
                CompilerMacros::SizeofMacro(SizeofMacro),
            ),
            (String::from("cast"), CompilerMacros::CastMacro(CastMacro)),
        ]);

        Analyzer {
            scope: {
                let mut scope = Scope::new();
                scope.is_main = is_main;
                scope
            },
            source: NamedSource::new(filename, src.to_owned()),
            source_path,

            errors: Vec::new(),
            warnings: Vec::new(),

            symtable: SymbolTable::default(),
            compiler_macros,
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        // let pre_statements = ast
        //     .iter()
        //     .filter(|stmt| {
        //         matches!(
        //             stmt,
        //             Statements::StructDefineStatement {
        //                 name: _,
        //                 fields: _,
        //                 functions: _,
        //                 public: _,
        //                 span: _
        //             } | Statements::EnumDefineStatement {
        //                 name: _,
        //                 fields: _,
        //                 functions: _,
        //                 public: _,
        //                 span: _
        //             } | Statements::TypedefStatement {
        //                 alias: _,
        //                 datatype: _,
        //                 span: _
        //             } | Statements::ImportStatement { path: _, span: _ }
        //                 | Statements::ExternStatement {
        //                     identifier: _,
        //                     arguments: _,
        //                     return_type: _,
        //                     extern_type: _,
        //                     is_var_args: _,
        //                     public: _,
        //                     span: _
        //                 }
        //         )
        //     })
        //     .collect::<Vec<&Statements>>();
        //
        // let after_statements = ast.iter().filter(|stmt| !pre_statements.contains(stmt));
        //
        // pre_statements
        //     .clone()
        //     .into_iter()
        //     .for_each(|stmt| self.visit_statement(stmt));
        // after_statements
        //     .into_iter()
        //     .for_each(|stmt| self.visit_statement(stmt));

        ast.iter().for_each(|stmt| self.visit_statement(stmt));

        if self.scope.get_fn("main").is_none() && self.scope.is_main {
            let err = SemanticError::GlobalError {
                message: "Program has no entry `main` function".to_string(),
                help: Some("Consider creating main function: `fn main() {}`".to_string()),
                src: self.source.clone(),
            };

            self.errors.reverse();
            self.errors.push(err);
            self.errors.reverse();
        }

        if let Some(unused) = self.scope.check_unused_variables() {
            unused.iter().for_each(|var| {
                self.warning(SemanticWarning::UnusedVariable {
                    varname: var.0.clone(),
                    src: self.source.clone(),
                    span: error::position_to_span(var.1),
                });
            });
        }

        if !self.errors.is_empty() {
            return Err((self.errors.clone(), self.warnings.clone()));
        }

        Ok((self.symtable.clone(), self.warnings.clone()))
    }

    fn error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    #[allow(unused)]
    fn warning(&mut self, warning: SemanticWarning) {
        self.warnings.push(warning)
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
                Statements::IncludeStatement { path: _, span: _ } => {}
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
                Statements::ExternStatement {
                    identifier: _,
                    arguments: _,
                    return_type: _,
                    extern_type: _,
                    public: _,
                    is_var_args: _,
                    span: _,
                } => {}
                Statements::ExternDeclareStatement {
                    identifier: _,
                    datatype: _,
                    span: _,
                } => {}
                Statements::LinkCStatement { path: _, span: _ } => {}
                _ => {
                    // if let Some(err) = self.errors.last() {
                    //     if err.span == (255, 0).into() {
                    //         return;
                    //     };
                    // }

                    self.error(SemanticError::SemanticalError {
                        exception: "This item is not allowed in global scope".to_string(),
                        help: Some("Consider removing this item from global scope".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(Parser::get_span_statement(statement)),
                    });

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
                        let value_type =
                            self.visit_expression(value, Some(variable.datatype.clone()));

                        if variable.datatype != value_type {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "variable has type `{}`, but found `{}`",
                                    variable.datatype, value_type
                                ),
                                help: Some(
                                    "Consider changing value, or variable datatype".to_string(),
                                ),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });

                            return;
                        }

                        self.scope
                            .set_init_var(identifier, true)
                            .unwrap_or_else(|_| {
                                self.error(SemanticError::UnresolvedName {
                                    exception: format!(
                                        "variable `{identifier}` is not defined here"
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                            });
                    } else {
                        self.error(SemanticError::UnresolvedName {
                            exception: format!("variable \"{identifier}\" is not defined here"),
                            help: Some("Verify provided identifier".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                let instance =
                    self.visit_expression(object, Some(Type::Pointer(Box::new(Type::Void))));

                match instance {
                    Type::Pointer(ptr_type) => {
                        let value_type = self.visit_expression(value, Some(*ptr_type.clone()));

                        if value_type != *ptr_type {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "pointer has type `{ptr_type}`, but found `{value_type}`"
                                ),
                                help: Some(
                                    "Consider changing provided value, or pointer type".to_string(),
                                ),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }
                    }

                    Type::Alias(alias) => {
                        const IMPLEMENTATION_FORMAT: &str = "fn deref_assign(&self, value: _)";

                        let struct_type = self.scope.get_struct(&alias).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be derefence-assigned"),
                                help: Some("Verify provided type, or change it".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });

                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return;
                        };

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, false)) =
                                functions.get("deref_assign")
                            {
                                if !(args.first().unwrap_or(&Type::Undefined)
                                    == &Type::Alias(alias.clone())
                                    && args.get(1).unwrap_or(&Type::Undefined) != &Type::Undefined
                                    && *datatype.clone() == Type::Void)
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("type `{alias}` has WRONG implementation for deref-assign"),
                                        help: Some(format!("Consider using right format: {IMPLEMENTATION_FORMAT}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });

                                    return;
                                }

                                let expected_value = args.get(1).unwrap();
                                let value_type =
                                    self.visit_expression(value, Some(expected_value.clone()));

                                if *expected_value != value_type {
                                    self.error(
                                        SemanticError::TypesMismatch {
                                            exception: format!("expected type `{expected_value}`, but found `{value_type}`"),
                                            help: Some("Consider changing value, or implementation format".to_string()),
                                            src: self.source.clone(),
                                            span: error::position_to_span(*span)
                                        }
                                    );
                                }
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!(
                                        "type `{alias}` has no implementation for deref-assign"
                                    ),
                                    help: Some(
                                        "Consider implementing necessary method".to_string(),
                                    ),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    _ => {
                        self.error(SemanticError::UnsupportedType {
                            exception: format!("type `{instance}` cannot be deref-assigned"),
                            help: Some("no help".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                    }
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
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "expected index with `usize` type, but found `{index_type}`"
                                ),
                                help: Some(
                                    "You can cast integer types to `usize`: cast!(VALUE, usize))"
                                        .to_string(),
                                ),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }

                        let value_type = self.visit_expression(value, Some(*typ.clone()));

                        if value_type != *typ {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "array's elements has type `{typ}`, but found `{value_type}`"
                                ),
                                help: Some("Consider changing provided value".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }
                    }
                    Type::DynamicArray(_) => {
                        // TODO: Remove dynamic array type

                        unreachable!()

                        // let index_type = self.visit_expression(index, Some(Type::USIZE));
                        //
                        // if index_type != Type::USIZE {
                        //     self.error(
                        //         format!(
                        //             "Expected index with type `usize`, but found `{index_type}`"
                        //         ),
                        //         *span,
                        //     );
                        // }
                        //
                        // let value_type = self.visit_expression(value, Some(*typ.clone()));
                        //
                        // if value_type != *typ {
                        //     self.error(
                        //         format!("Array has type `{typ}`, but found `{value_type}`"),
                        //         *span,
                        //     );
                        // }
                    }
                    Type::Pointer(ptr_type) => {
                        let value_type = self.visit_expression(value, Some(*ptr_type.clone()));

                        if value_type != *ptr_type {
                            self.error(SemanticError::TypesMismatch {
                                exception: format!(
                                    "pointer has type `{ptr_type}`, but found `{value_type}"
                                ),
                                help: Some("Consider changing value, or pointer type".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }
                    }
                    Type::Alias(alias) => {
                        const IMPLEMENTATION_FORMAT: &str =
                            "fn slice_assign(&self, index: usize, value: _)";

                        let struct_type = self.scope.get_struct(&alias).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be slice-assigned"),
                                help: Some("Consider changing provided type".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });

                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return;
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) =
                                functions.get("slice_assign")
                            {
                                if !(args.first().unwrap_or(&Type::Undefined)
                                    == &Type::Alias(alias.clone())
                                    && args.get(1).unwrap_or(&Type::Undefined) == &Type::USIZE
                                    && args.get(2).unwrap_or(&Type::Undefined) != &Type::Undefined
                                    && *datatype.clone() == Type::Void)
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("type `{alias}` has wrong implementation for slice-assign"),
                                        help: Some(format!("Consider using right format: {IMPLEMENTATION_FORMAT}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    })
                                }
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!("type `{alias}` has no implementation for slice-assign"),
                                    help: Some(format!("Consider implementing necessary method: {IMPLEMENTATION_FORMAT}")),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span)
                                });
                            }
                        } else {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be slice-assigned"),
                                help: Some("Consider using another supported type".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }
                    }
                    _ => {
                        self.error(SemanticError::UnsupportedType {
                            exception: format!("type `{instance}` cannot be slice-assigned"),
                            help: Some("Consider using another supported type".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                    self.error(SemanticError::UnsupportedExpression {
                        exception: "field assign is supported only for subelement expression"
                            .to_string(),
                        help: Some("Verify provided expression".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });

                    return;
                }

                let object_type = self.visit_expression(object, None);
                let unwrapped_object_type = self.unwrap_alias(&object_type).unwrap_or_else(|err| {
                    self.error(SemanticError::UnresolvedName {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    Type::Void
                });

                if unwrapped_object_type == Type::Void {
                    return;
                };

                let value_type = self.visit_expression(value, Some(unwrapped_object_type.clone()));
                if object_type != value_type {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "field has type `{object_type}`, but found `{value_type}`"
                        ),
                        help: Some(
                            "Consider using another value, or changing field type".to_string(),
                        ),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
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
                        let value_span = Parser::get_span_expression(value);
                        let value_type = self.visit_expression(value, Some(datatype.clone()));

                        if &value_type != datatype {
                            let unwrapped_datatype =
                                self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                    self.error(SemanticError::UnresolvedName {
                                        exception: err,
                                        help: None,
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span),
                                    });
                                    Type::Void
                                });

                            if unwrapped_datatype != value_type {
                                if Self::is_integer(&value_type)
                                    && Self::is_integer(&unwrapped_datatype)
                                {
                                    if Self::integer_order(&value_type)
                                        > Self::integer_order(&unwrapped_datatype)
                                    {
                                        self.error(SemanticError::TypesMismatch {
                                            exception: format!("expected integer type `{}`, but found `{}`", display_type.unwrap(), value_type),
                                            help: Some("Consider using value with smaller range, or change explicit type".to_string()),
                                            src: self.source.clone(),
                                            span: error::position_to_span(value_span)
                                        });
                                    }
                                } else {
                                    // allowing assignation if both types are pointers and one of them is `*void`
                                    if let Type::Pointer(ref expected_ptr) = unwrapped_datatype
                                        && let Type::Pointer(ref value_ptr) = value_type
                                        && (*expected_ptr.clone() == Type::Void
                                            || *value_ptr.clone() == Type::Void)
                                    {
                                    } else {
                                        self.error(SemanticError::TypesMismatch {
                                            exception: format!("expected type `{}`, but found `{}`", display_type.unwrap(), value_type),
                                            help: Some("Consider using another value type, or change explicit type".to_string()),
                                            src: self.source.clone(),
                                            span: error::position_to_span(value_span)
                                        });
                                    }
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
                        self.error(SemanticError::UnknownObject {
                            exception: format!("variable `{identifier}` has unknown type"),
                            help: Some(
                                "Provide explicit or implicit type for annotation".to_string(),
                            ),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                    self.error(SemanticError::MainFunctionError {
                        exception: "`main()` function is not allowed in non-global scope"
                            .to_string(),
                        help: Some("Move it to the global scope".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return;
                }

                if self.scope.get_fn(name).is_some() {
                    self.error(SemanticError::RedefinitionError {
                        exception: format!(
                            "function `{}` already declared",
                            name.replace("@!", "")
                        ),
                        help: Some("Consider using another function name".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*header_span),
                    });

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
                            false,
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
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        Type::Void
                    });
                let ret = self
                    .unwrap_alias(&self.scope.returned)
                    .unwrap_or_else(|err| {
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        Type::Void
                    });

                if exp != ret && ret != Type::Undefined {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "function `{}` returns type `{}`, but found `{}`",
                            name.replace("@!", ""),
                            datatype,
                            ret
                        ),
                        help: Some(
                            "Consider verifying returned value, or change function signature"
                                .to_string(),
                        ),
                        src: self.source.clone(),
                        span: error::position_to_span(*header_span),
                    });
                }

                if let Some(unused) = self.scope.check_unused_variables() {
                    unused.iter().for_each(|var| {
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if *public && name == "main" {
                    self.error(SemanticError::VisibilityError {
                        exception: "`main()` function is not allowed to be public".to_string(),
                        help: Some("Consider removing `pub` keyword".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*header_span),
                    });
                }
            }
            Statements::FunctionCallStatement {
                name,
                arguments,
                span,
            } => {
                let func = self.scope.get_fn(name).unwrap_or_else(|| {
                    self.error(SemanticError::UnresolvedName {
                        exception: format!("function `{name}` is not defined here"),
                        help: Some("Verify provided function name".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    Type::Void
                });

                if func == Type::Void {
                    return;
                };
                if let Type::Function(func_args, func_type, is_var_args) = func {
                    let mut expected_args = func_args.clone();
                    if arguments.len() > func_args.len() {
                        expected_args.resize(arguments.len(), Type::Void);
                    }

                    let call_args = arguments
                        .iter()
                        .zip(expected_args)
                        .map(|(arg, exp)| self.visit_expression(arg, Some(exp)))
                        .collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        if is_var_args && call_args.len() >= func_args.len() {
                        } else {
                            self.error(SemanticError::ArgumentException {
                                exception: format!(
                                    "function `{}` has {} arguments, but found {}",
                                    name,
                                    func_args.len(),
                                    call_args.len()
                                ),
                                help: Some("Verify provided call arguments".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            return;
                        }
                    }

                    call_args.iter().enumerate().zip(func_args).for_each(
                        |((ind, provided), expected)| {
                            let is_void_ptr = {
                                if let Type::Pointer(expected_ptr_type) = expected.clone() {
                                    matches!(
                                        (provided, *expected_ptr_type == Type::Void),
                                        (Type::Pointer(_), true)
                                    )
                                } else {
                                    false
                                }
                            };

                            if &expected != provided
                                && !is_void_ptr
                                && expected != Type::Void
                                && *provided != Type::Null
                            {
                                self.error(SemanticError::ArgumentException {
                                    exception: format!(
                                        "argument #{} must be `{}`, but found `{}`",
                                        ind + 1,
                                        expected,
                                        provided
                                    ),
                                    help: Some("Consider verifying provided argument".to_string()),
                                    src: self.source.clone(),
                                    span: error::position_to_span(Parser::get_span_expression(
                                        &arguments[ind],
                                    )),
                                });
                            }
                        },
                    );

                    if *func_type != Type::Void {
                        self.warning(SemanticWarning::UnusedResult {
                            message: format!("unused `{func_type}` result from function"),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                let pre_type = Type::Struct(fields.clone(), IndexMap::new());
                if self
                    .scope
                    .structures
                    .insert(
                        name.clone(),
                        element::ScopeElement {
                            datatype: pre_type,
                            public: *public,
                        },
                    )
                    .is_some()
                {
                    self.error(SemanticError::RedefinitionError {
                        exception: format!("structure `{name}` already declared"),
                        help: Some("Consider changing structure name".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return;
                }

                let mut structure_scope = Scope::new();
                structure_scope.parent = Some(Box::new(self.scope.clone()));
                self.scope = structure_scope;

                functions.iter().for_each(|func| {
                    let mut wrapped_statement = func.1.clone();
                    let mut fn_name = String::new();

                    if let Statements::FunctionDefineStatement {
                        name: mut function_name,
                        datatype,
                        arguments,
                        block,
                        public,
                        span,
                        header_span,
                    } = wrapped_statement.clone()
                    {
                        function_name = format!("@!{function_name}");
                        fn_name = function_name.clone();
                        if arguments
                            .iter()
                            .any(|arg| arg.0 == "self" && arg.1 == Type::SelfRef)
                        {
                            let mut arguments = arguments.clone();
                            *arguments.first_mut().unwrap() =
                                (String::from("self"), Type::Alias(name.clone()));

                            wrapped_statement = Statements::FunctionDefineStatement {
                                name: function_name,
                                datatype,
                                arguments,
                                block,
                                public,
                                span,
                                header_span,
                            };
                        } else {
                            wrapped_statement = Statements::FunctionDefineStatement {
                                name: function_name,
                                datatype,
                                arguments,
                                block,
                                public,
                                span,
                                header_span,
                            };
                        }
                    }

                    self.visit_statement(&wrapped_statement);
                    let signature = self.scope.get_fn(&fn_name).unwrap();
                    let struct_ptr = self.scope.get_mut_struct(name).unwrap();

                    if let Type::Struct(fields, mut functions) = struct_ptr.datatype.clone() {
                        let fn_name = fn_name.replace("@!", "");
                        functions.insert(fn_name.clone(), signature);
                        struct_ptr.datatype = Type::Struct(fields, functions);
                    }
                });

                // let functions_signatures = self.scope.functions.clone();
                self.scope = *self.scope.parent.clone().unwrap();

                // let _ = self.scope.structures.remove(name);

                // let struct_type = Type::Struct(
                //     fields.clone(),
                //     functions_signatures
                //         .into_iter()
                //         .map(|x| (
                //             x.0.replace("@!", ""),
                //             x.1.datatype
                //         ))
                //         .collect(),
                // );
                // self.scope
                //     .add_struct(name.clone(), struct_type.clone(), *public)
                //     .unwrap_or_else(|err| {
                //         self.error(err, *span);
                //     });
                // self.scope
                //     .add_typedef(name.clone(), struct_type)
                //     .unwrap_or_else(|err| {
                //         self.error(err, *span);
                //     })
            }
            Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public,
                span,
            } => {
                let pre_type = Type::Enum(fields.clone(), IndexMap::new());
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

                // WARN: Methods in enums are currently disabled
                if !functions.is_empty() {
                    self.error(SemanticError::DisabledFeature {
                        exception: "methods in enums are currently disabled".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                }

                // functions.iter().for_each(|func| {
                //     self.visit_statement(func.1);
                // });
                //
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
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                    });

                self.scope
                    .add_typedef(name.clone(), enum_type.clone())
                    .unwrap_or_else(|err| {
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                    self.error(SemanticError::TypesMismatch {
                        exception: format!("expected `bool` type, but found `{condition_type}`"),
                        help: Some("Consider returning `bool` type in expression".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
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
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if then_block_type != self.scope.expected
                    && then_block_type != Type::Void
                    && then_block_type != Type::Undefined
                {
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "scope expected `{}`, but found `{}`",
                            self.scope.expected, then_block_type
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return;
                }

                if then_block_type != Type::Void {
                    self.scope.returned = then_block_type.clone();
                }

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
                            self.warning(SemanticWarning::UnusedVariable {
                                varname: var.0.clone(),
                                src: self.source.clone(),
                                span: error::position_to_span(var.1),
                            });
                        });
                    }

                    self.scope = *self.scope.parent.clone().unwrap();

                    if then_block_type != else_block_type
                        && (then_block_type != Type::Undefined
                            && else_block_type != Type::Undefined)
                    {
                        self.error(SemanticError::TypesMismatch {
                            exception: format!("scopes has incompatible types: `{then_block_type}` and `{else_block_type}`"),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span)
                        });
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
                    self.error(SemanticError::TypesMismatch {
                        exception: format!(
                            "expected `bool` in expression, but found `{condition_type}`"
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
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
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(SemanticError::TypesMismatch {
                            exception: format!(
                                "expected `{}`, but found `{}`",
                                self.scope.expected, block_type
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                    Type::USIZE,
                ];

                let iterator_type = self.visit_expression(iterator, None);
                let mut binding_type = iterator_type.clone();

                match iterator_type.clone() {
                    typ if BASIC_SUPPORTED_ITERATOR_TYPES.contains(&typ) => {}
                    Type::Array(typ, _) => binding_type = *typ,
                    Type::DynamicArray(typ) => binding_type = *typ,
                    Type::Alias(alias) => {
                        let alias_type = self.unwrap_alias(&iterator_type).unwrap_or_else(|err| {
                            self.error(SemanticError::UnresolvedName {
                                exception: err,
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if alias_type == Type::Void {
                            return;
                        }

                        match alias_type {
                            Type::Struct(_, functions) => {
                                const IMPLEMENTATION_FORMAT: &str = "fn iterate(&self) (_, bool)";

                                if let Some(Type::Function(args, return_type, _)) =
                                    functions.get("iterate")
                                {
                                    let mut error_flag = args.len() > 1;
                                    if let Some(Type::Alias(alias_arg)) = args.first() {
                                        if alias_arg != &alias {
                                            error_flag = true
                                        }
                                    }

                                    if let Type::Tuple(fields) = *return_type.clone() {
                                        if fields.len() != 2 {
                                            error_flag = true
                                        } else {
                                            match (fields[0].clone(), fields[1].clone()) {
                                                (left_type, Type::Bool) => binding_type = left_type,
                                                _ => error_flag = true,
                                            }
                                        }
                                    }

                                    if error_flag {
                                        self.error(SemanticError::IllegalImplementation {
                                            exception: format!("type `{alias}` has WRONG implementation for iteration"),
                                            help: Some(format!("Use right implementation syntax: {IMPLEMENTATION_FORMAT}")),
                                            src: self.source.clone(),
                                            span: error::position_to_span(*span)
                                        });

                                        return;
                                    }
                                } else {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("structure `{alias}` has no implementation for iteration"),
                                        help: Some(format!("Implement necessary method: {IMPLEMENTATION_FORMAT}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }
                            }
                            _ => {
                                self.error(SemanticError::UnsupportedType {
                                    exception: format!(
                                        "type `{iterator_type}` isn't supported for iteration"
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                                return;
                            }
                        }
                    }
                    _ => {
                        self.error(SemanticError::UnsupportedType {
                            exception: format!(
                                "type `{iterator_type}` isn't supported for iteration"
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                if block_type != Type::Void {
                    if block_type != self.scope.expected {
                        self.error(SemanticError::TypesMismatch {
                            exception: format!(
                                "expected `{}`, but found `{}`",
                                self.scope.expected, block_type
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        return;
                    }

                    self.scope.returned = block_type;
                }
            }
            Statements::ImportStatement { path: _, span } => {
                self.error(SemanticError::DisabledFeature {
                    exception: "imports are currently disabled".to_string(),
                    help: None,
                    src: self.source.clone(),
                    span: error::position_to_span(*span),
                });

                // match path {
                //     Expressions::Value(Value::String(path), _) => {
                //         let fname = std::path::Path::new(path)
                //             .file_name()
                //             .map(|fname| fname.to_str().unwrap_or("$NONE"));
                //
                //         match fname {
                //             Some(fname) => {
                //                 if fname == "$NONE" {
                //                     self.error(format!("Unable to get module name: `{}`", path), *span);
                //                     return;
                //                 }
                //
                //                 let src = std::fs::read_to_string(fname).unwrap_or_else(|err| {
                //                     self.error(format!("Unable to read `{}`: {}", fname, err), *span);
                //                     String::new()
                //                 });
                //
                //                 if src.is_empty() {
                //                     return;
                //                 };
                //
                //                 let mut lexer = deen_lexer::Lexer::new(&src, fname);
                //                 let (tokens, warns) = match lexer.tokenize() {
                //                     Ok(res) => res,
                //                     Err((errors, warns)) => {
                //                         errors
                //                             .iter()
                //                             .for_each(|err| self.errors.push(err.clone().into()));
                //                         warns
                //                             .iter()
                //                             .for_each(|warn| self.warnings.push(warn.clone().into()));
                //                         return;
                //                     }
                //                 };
                //                 warns
                //                     .iter()
                //                     .for_each(|warn| self.warnings.push(warn.clone().into()));
                //
                //                 let mut parser = deen_parser::Parser::new(tokens, &src, fname);
                //                 let (ast, warns) = match parser.parse() {
                //                     Ok(res) => res,
                //                     Err((errors, warns)) => {
                //                         errors
                //                             .iter()
                //                             .for_each(|err| self.errors.push(err.clone().into()));
                //                         warns
                //                             .iter()
                //                             .for_each(|warn| self.warnings.push(warn.clone().into()));
                //                         return;
                //                     }
                //                 };
                //                 warns
                //                     .iter()
                //                     .for_each(|warn| self.warnings.push(warn.clone().into()));
                //
                //                 let mut mutual_import = false;
                //                 ast.iter().for_each(|stmt| {
                //                     if let Statements::ImportStatement {
                //                         path: Expressions::Value(Value::String(path), _),
                //                         span,
                //                     } = stmt
                //                     {
                //                         let imp_name = std::path::Path::new(path)
                //                             .file_name()
                //                             .map(|fname| fname.to_str().unwrap_or("$NONE"));
                //
                //                         if imp_name == Some(self.source.name()) {
                //                             self.error(
                //                                 format!(
                //                                     "Mutual import found: `{}` from `{}`",
                //                                     imp_name.unwrap(),
                //                                     fname
                //                                 ),
                //                                 *span,
                //                             );
                //                             mutual_import = true;
                //                         }
                //                     }
                //                 });
                //
                //                 if mutual_import {
                //                     return;
                //                 };
                //
                //                 let mut analyzer = Self::new(&src, fname, false);
                //                 let (embedded_symtable, warns) = match analyzer.analyze(&ast) {
                //                     Ok(warns) => warns,
                //                     Err((errors, warns)) => {
                //                         errors.iter().for_each(|err| self.errors.push(err.clone()));
                //                         warns
                //                             .iter()
                //                             .for_each(|warn| self.warnings.push(warn.clone()));
                //                         return;
                //                     }
                //                 };
                //                 warns
                //                     .iter()
                //                     .for_each(|warn| self.warnings.push(warn.clone()));
                //
                //                 let module_name = fname
                //                     .split(".")
                //                     .nth(0)
                //                     .map(|n| n.to_string())
                //                     .unwrap_or(fname.replace(".dn", ""));
                //
                //                 let mut import = Import::new(ast, &src);
                //
                //                 analyzer.scope.functions.into_iter().for_each(|func| {
                //                     if func.1.public {
                //                         import.add_fn(func.0.to_string(), func.1.datatype);
                //                     }
                //                 });
                //
                //                 analyzer.scope.structures.into_iter().for_each(|structure| {
                //                     if structure.1.public {
                //                         import
                //                             .add_struct(structure.0.to_string(), structure.1.datatype);
                //                     }
                //                 });
                //
                //                 analyzer.scope.enums.into_iter().for_each(|enumeration| {
                //                     if enumeration.1.public {
                //                         import.add_enum(
                //                             enumeration.0.to_string(),
                //                             enumeration.1.datatype,
                //                         );
                //                     }
                //                 });
                //
                //                 import.embedded_symtable = embedded_symtable;
                //                 self.symtable.imports.insert(module_name, import);
                //             }
                //             None => {
                //                 self.error(format!("Unable to find: `{}`", path), *span);
                //             }
                //         }
                //     }
                //     _ => {
                //         self.error(String::from("Import must be string constant"), *span);
                //     }
                // }
            }

            Statements::IncludeStatement { path, span } => {
                let (import_path, path_span) =
                    if let Expressions::Value(Value::String(path), span) = path {
                        (path, span)
                    } else {
                        unreachable!()
                    };

                if import_path.is_empty() {
                    self.error(SemanticError::FormatError {
                        exception: "empty include path provided".to_string(),
                        help: Some("Provide right path to your module".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*path_span),
                    });

                    return;
                }

                let included_path =
                    self.expand_library_path(import_path, true)
                        .unwrap_or_else(|err| {
                            self.error(SemanticError::FormatError {
                                exception: format!("unable to resolve provided path: {err}"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*path_span),
                            });

                            PathBuf::new()
                        });

                if included_path.as_os_str().is_empty() {
                    return;
                };

                // Reading source code
                let src = std::fs::read_to_string(&included_path).unwrap_or_else(|err| {
                    self.error(SemanticError::IoError {
                        exception: format!("unable to read source code: {err}"),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*path_span),
                    });

                    String::new()
                });

                // Getting file name
                let fname = included_path
                    .file_name()
                    .unwrap_or_else(|| {
                        self.error(SemanticError::IoError {
                            exception: "unable to resolve provided path".to_string(),
                            help: Some("PathBuf returned None value on `file_name()`".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*path_span),
                        });
                        OsStr::new("")
                    })
                    .to_str()
                    .unwrap_or_else(|| {
                        self.error(SemanticError::IoError {
                            exception: "unable to resolve provided path".to_string(),
                            help: Some("OsStr returned None value on `to_str()`".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*path_span),
                        });
                        ""
                    });

                if fname.is_empty() {
                    return;
                };
                if src.is_empty() {
                    return;
                };

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                if self.symtable.included.contains_key(&module_name) {
                    return;
                }

                // Lexical Analyzer
                let mut lexer = deen_lexer::Lexer::new(&src, fname);
                let (tokens, _) = match lexer.tokenize() {
                    Ok(result) => result,
                    Err((errors, _)) => {
                        errors
                            .into_iter()
                            .for_each(|err| self.errors.push(err.into()));
                        return;
                    }
                };

                // Syntax Analyzer
                let mut parser = Parser::new(tokens, &src, fname);
                let (ast, _) = match parser.parse() {
                    Ok(ast) => ast,
                    Err((errors, _)) => {
                        errors
                            .into_iter()
                            .for_each(|err| self.errors.push(err.into()));
                        return;
                    }
                };

                // Semantical Analyzer
                let mut analyzer = Analyzer::new(&src, fname, included_path.clone(), false);
                let (symtable, _) = match analyzer.analyze(&ast) {
                    Ok(res) => res,
                    Err((errors, _)) => {
                        errors.into_iter().for_each(|err| self.errors.push(err));
                        return;
                    }
                };

                analyzer.scope.functions.into_iter().for_each(|func| {
                    if func.1.public && self.scope.get_fn(&func.0).is_none() {
                        self.scope
                            .add_fn(func.0, func.1.datatype, true)
                            .unwrap_or_else(|err| {
                                self.error(SemanticError::UnresolvedName {
                                    exception: err,
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                            });
                    }
                });

                analyzer.scope.structures.into_iter().for_each(|structure| {
                    if structure.1.public && self.scope.get_struct(&structure.0).is_none() {
                        self.scope
                            .add_struct(structure.0, structure.1.datatype, true)
                            .unwrap_or_else(|err| {
                                self.error(SemanticError::UnresolvedName {
                                    exception: err,
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                            });
                    }
                });

                analyzer.scope.enums.into_iter().for_each(|enumeration| {
                    if enumeration.1.public && self.scope.get_enum(&enumeration.0).is_none() {
                        self.scope
                            .add_enum(enumeration.0, enumeration.1.datatype, true)
                            .unwrap_or_else(|err| {
                                self.error(SemanticError::UnresolvedName {
                                    exception: err,
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                            });
                    }
                });

                let include = Include { ast };
                self.symtable.included.insert(module_name, include);

                symtable.included.into_iter().for_each(|inc| {
                    let _ = self.symtable.included.insert(inc.0, inc.1);
                });

                symtable.linked.into_iter().for_each(|link| {
                    let _ = self.symtable.linked.insert(link);
                })
            }

            Statements::ExternDeclareStatement {
                identifier,
                datatype,
                span,
            } => {
                if self.scope.get_var(identifier).is_some() {
                    self.error(SemanticError::RedefinitionError {
                        exception: format!("identifier `{identifier}` already taken"),
                        help: Some(
                            "Consider using another identifier, or rename taken".to_string(),
                        ),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                }

                self.scope
                    .add_var(identifier.to_string(), datatype.clone(), true, (0, 0));

                // making variable `used`
                let _ = self.scope.get_var(identifier);
            }

            Statements::LinkCStatement { path, span: _ } => {
                let (link_path, path_span) =
                    if let Expressions::Value(Value::String(path), span) = path {
                        (path, span)
                    } else {
                        unreachable!()
                    };

                let formatted_path =
                    self.expand_library_path(link_path, false)
                        .unwrap_or_else(|err| {
                            self.error(SemanticError::FormatError {
                                exception: format!("unable to resolve linkage path: {err}"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*path_span),
                            });
                            PathBuf::new()
                        });

                if formatted_path.as_os_str().is_empty() {
                    return;
                };
                if !formatted_path.exists() {
                    self.error(SemanticError::IoError {
                        exception: format!(
                            "provided linkage file doesn't exists: `{}`",
                            formatted_path.as_os_str().to_str().unwrap_or("none")
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*path_span),
                    });

                    return;
                }
                if !formatted_path.is_file() {
                    self.error(SemanticError::IoError {
                        exception: format!(
                            "provided path is not a file: `{}`",
                            formatted_path.as_os_str().to_str().unwrap_or("none")
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*path_span),
                    });
                    return;
                }

                let _ = self.symtable.linked.insert(formatted_path);
            }

            Statements::ExternStatement {
                identifier,
                arguments,
                return_type,
                public,
                extern_type,
                is_var_args,
                span,
            } => {
                const SUPPORTED_EXTERN_TYPES: [&str; 1] = ["C"];

                if !SUPPORTED_EXTERN_TYPES.contains(&extern_type.as_str()) {
                    self.error(SemanticError::UnsupportedType {
                        exception: "unsupported extern type found".to_string(),
                        help: Some(format!(
                            "Currently supported: \"{}\"",
                            SUPPORTED_EXTERN_TYPES.join("\", ")
                        )),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                }

                if identifier == "main" {
                    self.error(SemanticError::MainFunctionError {
                        exception: "function `main() cannot be external declared".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                }
                if self.scope.get_fn(identifier).is_some() {
                    self.error(SemanticError::RedefinitionError {
                        exception: format!("function `{}()` is already declared", &identifier),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return;
                }

                if let Type::Alias(_) = return_type {
                    let _ = self.unwrap_alias(return_type).unwrap_or_else(|err| {
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        Type::Void
                    });
                }

                self.scope
                    .add_fn(
                        identifier.clone(),
                        Type::Function(
                            arguments.clone(),
                            Box::new(return_type.clone()),
                            *is_var_args,
                        ),
                        *public,
                    )
                    .unwrap_or_else(|err| {
                        self.error(SemanticError::UnresolvedName {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                    });
            }

            Statements::BreakStatements { span } => {
                if !self.scope.is_loop() {
                    self.error(SemanticError::SemanticalError {
                        exception: "use of `break` keyword outside loop".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
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
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();
            }

            Statements::Expression(expr) => {
                let expr_type = self.visit_expression(expr, None);
                if expr_type != Type::Void {
                    self.warning(SemanticWarning::UnusedResult {
                        message: format!("unused result with type `{expr_type}`"),
                        src: self.source.clone(),
                        span: error::position_to_span(Parser::get_span_expression(expr)),
                    });
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

                    (Type::Pointer(_), r) if Self::is_integer(&r) => {
                        if operand != "+" && operand != "-" {
                            self.error(SemanticError::OperatorException {
                                exception: "unsupported binary operator for pointer".to_string(),
                                help: Some(
                                    "Pointers arithemics supports only: `+` / `-`".to_string(),
                                ),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }

                        left
                    }

                    // (Type::Pointer(_), Type::Pointer(_)) => {
                    //     if operand != "+" && operand != "-" {
                    //         self.error(SemanticError::OperatorException {
                    //             exception: "unsupported binary operator for pointer".to_string(),
                    //             help: Some(
                    //                 "Pointers arithemics supports only: `+` / `-`".to_string(),
                    //             ),
                    //             src: self.source.clone(),
                    //             span: error::position_to_span(*span),
                    //         });
                    //     }
                    //
                    //     Type::USIZE
                    // }
                    (Type::Alias(left), Type::Alias(right)) => {
                        let implementation_format: String =
                            format!("fn binary(&self, other: *{left}, operand: *char) {left}");

                        let struct_type = self.scope.get_struct(&left).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!(
                                    "type `{}` isn't avaible for binary operations",
                                    &left
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return expected.unwrap_or(Type::Alias(left));
                        }

                        if left != right {
                            self.error(SemanticError::OperatorException {
                                exception: format!(
                                    "cannot apply binary \"{operand}\" to `{left}` and `{right}`"
                                ),
                                help: Some("Consider using compatible types".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            return Type::Alias(left);
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) = functions.get("binary")
                            {
                                if !(*args
                                    == vec![
                                        Type::Alias(left.clone()),
                                        Type::Pointer(Box::new(Type::Alias(left.clone()))),
                                        Type::Pointer(Box::new(Type::Char)),
                                    ]
                                    && *datatype.clone() == Type::Alias(left.clone()))
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("type `{left}` has wrong implementation for binary operations"),
                                        help: Some(format!("Consider using right format: {implementation_format}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }

                                *datatype.clone()
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!(
                                        "type `{left}` has no implementation for binary operations"
                                    ),
                                    help: Some(format!(
                                        "Implement necessary method: {implementation_format}"
                                    )),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                                expected.unwrap_or(Type::Alias(left))
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    _ => {
                        self.error(SemanticError::OperatorException {
                            exception: format!(
                                "cannot apply binary \"{operand}\" to `{left}` and `{right}`"
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        expected.unwrap_or(left)
                    }
                }
            }
            Expressions::Unary {
                operand,
                object,
                span,
            } => {
                let obj = self.visit_expression(object, expected.clone());

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

                    (Type::Alias(alias), _) => {
                        let implementation_format =
                            format!("fn unary(&self, operand: *char) {alias}");

                        let struct_type = self.scope.get_struct(alias).unwrap_or_else(|| {
                            self.error(SemanticError::OperatorException {
                                exception: format!(
                                    "type `{}` isn't avaible for unary operations",
                                    &alias
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return expected.unwrap_or(Type::Alias(alias.clone()));
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) = functions.get("unary")
                            {
                                if !(*args
                                    == vec![
                                        Type::Alias(alias.clone()),
                                        Type::Pointer(Box::new(Type::Char)),
                                    ]
                                    && *datatype.clone() == Type::Alias(alias.clone()))
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("type `{alias}` has wrong implementation for unary operations"),
                                        help: Some(format!("Consider using right format: {implementation_format}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }

                                *datatype.clone()
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!("type `{alias}` has wrong implementation for unary operations"),
                                    help: Some(format!("Consider using right format: {implementation_format}")),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span)
                                });
                                Type::Alias(alias.clone())
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    _ => {
                        self.error(SemanticError::OperatorException {
                            exception: format!("cannot apply unary \"{operand}\" to `{obj}`"),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                const SUPPORTED_EXTRA_TYPES: [Type; 3] = [Type::Bool, Type::Char, Type::Null];

                let left = self.visit_expression(lhs, expected.clone());
                let right = self.visit_expression(rhs, Some(left.clone()));

                match (left.clone(), right.clone()) {
                    (l, r)
                        if (matches!(l, Type::Pointer(_)) && r == Type::Null)
                            || (matches!(r, Type::Pointer(_)) && l == Type::Null) =>
                    {
                        if !matches!(operand.as_str(), "==" | "!=") {
                            self.error(SemanticError::OperatorException {
                                exception: "null checker only supports: `==` / `!=`".to_string(),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                        }

                        Type::Bool
                    }

                    (l, r) if Self::is_integer(&l) && Self::is_integer(&r) => Type::Bool,

                    (l, r)
                        if (Self::is_integer(&l) && r == Type::Char)
                            || (Self::is_integer(&r) && l == Type::Char) =>
                    {
                        Type::Bool
                    }

                    (l, r) if Self::is_float(&l) && Self::is_float(&r) => Type::Bool,
                    (l, r) if l == r && SUPPORTED_EXTRA_TYPES.contains(&l) => Type::Bool,

                    (Type::Pointer(l), Type::Pointer(r)) if l == r && *l == Type::Char => {
                        Type::Bool
                    }

                    (Type::Alias(left), Type::Alias(right)) => {
                        let implementation_format =
                            format!("fn compare(&self, other: *{left}) i32");

                        if self.scope.get_enum(&left).is_some()
                            && self.scope.get_enum(&right).is_some()
                            && left == right
                        {
                            return Type::Bool;
                        }

                        let struct_type = self.scope.get_struct(&left).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!(
                                    "type `{}` isn't avaible for boolean operations",
                                    &left
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return Type::Alias(left);
                        }

                        if left != right {
                            self.error(SemanticError::OperatorException {
                                exception: format!(
                                    "cannot apply boolean \"{operand}\" to `{left}` and `{right}`"
                                ),
                                help: Some("Consider using compatible types".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            return Type::Bool;
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) =
                                functions.get("compare")
                            {
                                if !(*args
                                    == vec![
                                        Type::Alias(left.clone()),
                                        Type::Pointer(Box::new(Type::Alias(left.clone()))),
                                    ]
                                    && *datatype.clone() == Type::I32)
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!(
                                            "type `{left}` has wrong implementation for comparison"
                                        ),
                                        help: Some(format!(
                                            "Consider using right format: {implementation_format}"
                                        )),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span),
                                    });
                                }

                                Type::Bool
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!("type `{left}` has no implementation for comparison"),
                                    help: Some(format!("Consider implementing necessary method: {implementation_format}")),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span)
                                });

                                Type::Bool
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    _ => {
                        self.error(SemanticError::OperatorException {
                            exception: format!(
                                "cannot apply boolean \"{operand}\" to `{left}` and `{right}`"
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                    self.error(SemanticError::OperatorException {
                        exception: format!(
                            "cannot apply bitwise operations to `{}` and `{}`",
                            &left, &right
                        ),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return expected.unwrap_or(left);
                };

                if [">>", "<<"].contains(&operand.as_ref()) && !Self::is_unsigned_integer(&right) {
                    self.error(SemanticError::TypesMismatch {
                        exception: "shift index must be unsigned integer".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return expected.unwrap_or(left);
                }

                if Self::integer_order(&left) > Self::integer_order(&right) {
                    left
                } else {
                    right
                }
            }

            Expressions::Argument { name, r#type, span } => {
                if name != "@deen_type" {
                    self.error(SemanticError::ArgumentException {
                        exception: "arguments are not allowed in global code".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                }

                r#type.clone()
            }
            Expressions::SubElement {
                head,
                subelements,
                span,
            } => {
                let head_type = self.visit_expression(head, expected.clone());

                let mut prev_type_display = head_type.clone();
                let mut prev_type = self.unwrap_alias(&head_type).unwrap_or_else(|err| {
                    self.error(SemanticError::UnresolvedName {
                        exception: err,
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    Type::Void
                });
                let mut prev_expr = *head.clone();
                if prev_type == Type::Void {
                    return expected.unwrap_or(head_type);
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
                                        self.error(SemanticError::UnknownObject {
                                            exception: format!("type `{prev_type_display}` has no fields named `{field}`"),
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*field_span)
                                        });

                                        &Type::Void
                                    });

                                    prev_type_display = field_type.clone();
                                    prev_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                        self.error(SemanticError::UnresolvedName {
                                            exception: err,
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*span)
                                        });
                                        Type::Void
                                    });

                                    if is_ptr {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    } else if let Some(Type::Pointer(_)) = expected.clone() {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    }

                                    prev_expr = sub.clone();
                                }
                                Type::Enum(fields, _) => {
                                    let opt = fields.iter().find(|&x| x == field);
                                    if opt.is_none() {
                                        self.error(SemanticError::UnknownObject {
                                            exception: format!("type `{prev_type_display}` has no option named `{field}`"),
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*field_span)
                                        });
                                    }

                                    prev_expr = Expressions::SubElement { head: Box::new(prev_expr.clone()), subelements: vec![], span: *field_span }
                                },
                                _ => {
                                    self.error(SemanticError::UnsupportedType {
                                        exception: format!("type `{prev_type_display}` has no accessible fields"),
                                        help: None,
                                        src: self.source.clone(),
                                        span: error::position_to_span(*field_span)
                                    });
                                }
                            };
                        }
                        Expressions::Value(Value::Integer(idx), idx_span) => {
                            match prev_type.clone() {
                                Type::Tuple(types) => {
                                    if idx >= &(types.len() as i64) {
                                        self.error(SemanticError::RangeOverflow {
                                            exception: format!("type `{}` has {} fields, but index is {}", prev_type_display, types.len(), idx),
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*idx_span)
                                        });
                                        return;
                                    }

                                    let typ = types[*idx as usize].clone();
                                    prev_type_display = typ.clone();
                                    prev_type = self.unwrap_alias(&typ).unwrap_or_else(|err| {
                                        self.error(SemanticError::UnresolvedName {
                                            exception: err,
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*span)
                                        });
                                        Type::Void
                                    });

                                    if is_ptr {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    } else if let Some(Type::Pointer(_)) = expected.clone() {
                                        prev_type = Type::Pointer(Box::new(prev_type.clone()));
                                    }
                                    prev_expr = sub.clone();
                                },
                                _ => {
                                    self.error(SemanticError::UnknownObject {
                                        exception: format!("type `{prev_type_display}` has no numbered fields"),
                                        help: None,
                                        src: self.source.clone(),
                                        span: error::position_to_span(*idx_span)
                                    });
                                }
                            }
                        }
                        Expressions::FnCall { name, arguments, span } => {
                            match prev_type.clone() {
                                Type::Struct(_, functions) | Type::Enum(_, functions) => {
                                    let function_type = functions.get(name).unwrap_or_else(|| {
                                        self.error(SemanticError::UnresolvedName {
                                            exception: format!("type `{prev_type_display}` has no function `{name}`"),
                                            help: None,
                                            src: self.source.clone(),
                                            span: error::position_to_span(*span)
                                        });
                                        &Type::Void
                                    });

                                    if let Type::Function(args, datatype, is_var_args) = function_type {
                                        let mut arguments = arguments.clone();

                                        if let Some(Type::Alias(alias)) = args.first() {
                                            let is_pointed_struct = {
                                                if let Type::Pointer(ptr_type) = prev_type_display.clone() {
                                                    prev_type_display = *ptr_type;
                                                    true
                                                } else {
                                                    false
                                                }
                                            };

                                            if Type::Alias(alias.clone()) == prev_type_display {
                                                arguments.reverse();

                                                let self_arg = if is_pointed_struct {
                                                    prev_expr.clone()
                                                } else {
                                                    Expressions::Reference { object: Box::new(prev_expr.clone()), span: (Parser::get_span_expression(&prev_expr)) }
                                                };

                                                arguments.push(self_arg);
                                                arguments.reverse();
                                            }
                                        }

                                        prev_type_display = *datatype.clone();
                                        prev_type = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                            self.error(SemanticError::UnresolvedName {
                                                exception: err,
                                                help: None,
                                                src: self.source.clone(),
                                                span: error::position_to_span(*span)
                                            });
                                            Type::Void
                                        });

                                        if arguments.len() != args.len() {
                                            if *is_var_args && arguments.len() >= args.len() {} else {
                                                self.error(SemanticError::ArgumentException {
                                                    exception: format!("function `{}` has {} arguments, but found {}", name, args.len(), arguments.len()),
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                return;
                                            }
                                        }

                                        arguments.iter().enumerate().zip(args).for_each(|((index, expr), expected)| {
                                            let raw_expr_type = self.visit_expression(expr, Some(expected.clone()));
                                            let expr_type = self.unwrap_alias(&raw_expr_type).unwrap_or_else(|err| {
                                                self.error(SemanticError::UnresolvedName {
                                                    exception: err,
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                Type::Void
                                            });
                                            let raw_expected = expected;
                                            let expected = self.unwrap_alias(expected).unwrap_or_else(|err| {
                                                self.error(SemanticError::UnresolvedName {
                                                    exception: err,
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                Type::Void
                                            });

                                            if expected == Type::Void || raw_expr_type == Type::Void { return };
                                            if let Type::Pointer(ptr_type) = raw_expr_type.clone() {
                                                if *ptr_type.clone() == *raw_expected { return };
                                            }
                                            if expr_type != expected {
                                                if let Type::Pointer(ref expr_ptr) = expr_type
                                                && let Type::Pointer(ref expected_ptr) = expected
                                                && (*expr_ptr.clone() == Type::Void || *expected_ptr.clone() == Type::Void)
                                                {} else {
                                                    self.error(SemanticError::TypesMismatch {
                                                        exception: format!("argument #{} has type `{}`, but found `{}`", index + 1, raw_expected, raw_expr_type),
                                                        help: None,
                                                        src: self.source.clone(),
                                                        span: error::position_to_span(Parser::get_span_expression(expr))
                                                    });
                                                }
                                            }
                                        });
                                    };
                                },
                                Type::ImportObject(imp) => {
                                    let import = self.symtable.imports.get(&imp).unwrap().clone();
                                    let name = format!("{imp}.{name}");

                                    if let Some(Type::Function(args, datatype, is_var_args)) = import.functions.get(&name) {
                                        prev_type_display = *datatype.clone();
                                        prev_type = self.unwrap_alias(datatype).unwrap_or_else(|err| {
                                            self.error(SemanticError::UnresolvedName {
                                                exception: err,
                                                help: None,
                                                src: self.source.clone(),
                                                span: error::position_to_span(*span)
                                            });
                                            Type::Void
                                        });

                                        if arguments.len() != args.len() {
                                            if *is_var_args && arguments.len() >= args.len() {} else {
                                                self.error(SemanticError::ArgumentException {
                                                    exception: format!("function `{}()` has {} arguments, but found {}", name, args.len(), arguments.len()),
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                return;
                                            }
                                        }

                                        arguments.iter().enumerate().zip(args).for_each(|((index, expr), expected)| {
                                            let raw_expr_type = self.visit_expression(expr, Some(expected.clone()));
                                            let expr_type = self.unwrap_alias(&raw_expr_type).unwrap_or_else(|err| {
                                                self.error(SemanticError::UnresolvedName {
                                                    exception: err,
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                Type::Void
                                            });
                                            let expected = self.unwrap_alias(expected).unwrap_or_else(|err| {
                                                self.error(SemanticError::UnresolvedName {
                                                    exception: err,
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                                Type::Void

                                            });

                                            if expected == Type::Void || raw_expr_type == Type::Void { return };
                                            if expr_type != expected {
                                                self.error(SemanticError::TypesMismatch {
                                                    exception: format!("argument #{} has type `{}`, but found `{}`", index + 1, expected, expr_type),
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(Parser::get_span_expression(expr))
                                                });
                                            }
                                        });
                                    } else {
                                        unreachable!()
                                        // self.error(
                                        //     format!("Import `{imp}` has no functions named `{name}()`"),
                                        //     *span
                                        // );
                                    };
                                }
                                // Type::Enum(_, functions) => {},
                                _ => {
                                    self.error(SemanticError::UnsupportedType {
                                        exception: format!("type `{prev_type}` isn't supported for method calls"),
                                        help: None,
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }
                            }
                        },
                        Expressions::Struct { name, fields, span } => {
                            match prev_type.clone() {
                                Type::ImportObject(imp) => {
                                    let import = self.symtable.imports.get(&imp).unwrap().clone();

                                    let name = format!("{imp}.{name}"); 
                                    if let Some(Type::Struct(struct_fields, _)) = import.structs.get(&name) {

                                        let mut assigned_fields = HashMap::new();
                                        struct_fields.iter().for_each(|x| {
                                            assigned_fields.insert(x.0, false);
                                        });

                                        fields.iter().for_each(|field| {
                                            let struct_field = struct_fields.get(field.0);
                                            if let Some(field_type) = struct_field {
                                                let field_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                                                    self.error(SemanticError::UnresolvedName {
                                                        exception: err,
                                                        help: None,
                                                        src: self.source.clone(),
                                                        span: error::position_to_span(*span)
                                                    });
                                                    Type::Void
                                                });
                                                let provided_type = self.visit_expression(field.1, Some(field_type.clone()));

                                                if field_type != provided_type && field_type != Type::Void {
                                                    self.error(SemanticError::TypesMismatch {
                                                        exception: format!("field `{}` expected to be `{}`, but found `{}`", field.0, field_type, provided_type),
                                                        help: None,
                                                        src: self.source.clone(),
                                                        span: error::position_to_span(*span)
                                                    });
                                                }

                                                let _ = assigned_fields.insert(field.0, true);
                                            } else {
                                                self.error(SemanticError::UnresolvedName {
                                                    exception: format!("field `{}` doesn't exists in `{}`", field.0, name),
                                                    help: None,
                                                    src: self.source.clone(),
                                                    span: error::position_to_span(*span)
                                                });
                                            }
                                        });

                                        // let unassigned = assigned_fields.iter().filter(|x| !x.1).map(|x| x.0.to_owned().to_owned()).collect::<Vec<String>>();
                                        // if !unassigned.is_empty() {
                                        //     let fmt = format!("`{}`", unassigned.join("` , `"));
                                        //     self.error(
                                        //         format!("Missing structure fields: {fmt}"),
                                        //         *span
                                        //     );
                                        // }

                                        prev_type_display = Type::Alias(name.clone());
                                        prev_type = import.structs.get(&name).unwrap().clone();
                                    }
                                }
                                _ => {
                                    self.error(SemanticError::UnsupportedExpression {
                                        exception: "unsupported structure initialization found".to_string(),
                                        help: None,
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }
                            }
                        }
                        _ => {
                            self.error(SemanticError::UnsupportedExpression {
                                exception: "unsupported subelement expression".to_string(),
                                help: Some("You can open issue on Github Repository to fix that".to_string()),
                                src: self.source.clone(),
                                span: error::position_to_span(*span)
                            });
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
                    self.error(SemanticError::UnresolvedName {
                        exception: format!("function `{name}` is not defined here"),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    Type::Void
                });
                if func == Type::Void {
                    return expected.unwrap_or(Type::Void);
                };
                if let Type::Function(func_args, mut func_type, is_var_args) = func {
                    let call_args = arguments
                        .iter()
                        .zip(func_args.clone())
                        .map(|(arg, exp)| self.visit_expression(arg, Some(exp)))
                        .collect::<Vec<Type>>();

                    if call_args.len() != func_args.len() {
                        if is_var_args && call_args.len() >= func_args.len() {
                        } else {
                            self.error(SemanticError::ArgumentException {
                                exception: format!(
                                    "function `{}` has {} arguments, but found {}",
                                    &name,
                                    func_args.len(),
                                    call_args.len()
                                ),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            return *func_type;
                        }
                    }

                    call_args.iter().enumerate().zip(func_args).for_each(
                        |((ind, provided), expected)| {
                            let is_void_ptr = {
                                if let Type::Pointer(expected_ptr_type) = expected.clone() {
                                    matches!(
                                        (provided, *expected_ptr_type == Type::Void),
                                        (Type::Pointer(_) | Type::Null, true)
                                    )
                                } else {
                                    false
                                }
                            };

                            if &expected != provided && !is_void_ptr {
                                self.error(SemanticError::ArgumentException {
                                    exception: format!(
                                        "argument #{} must be `{}`, but found `{}`",
                                        ind + 1,
                                        expected,
                                        provided
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(Parser::get_span_expression(
                                        &arguments[ind].clone(),
                                    )),
                                });
                            }
                        },
                    );

                    // yeah, i know this looks like a shit, but i need it
                    if let Type::Pointer(func_ptr_type) = *func_type.clone() {
                        if let (Some(Type::Pointer(expected_ptr_type)), true) =
                            (expected.clone(), *func_ptr_type == Type::Void)
                        {
                            *func_type = Type::Pointer(expected_ptr_type);
                        }
                    }

                    let mut return_type = *func_type;
                    if Self::is_integer(&return_type)
                        && (Self::is_integer(expected.as_ref().unwrap_or(&Type::Void))
                            || matches!(expected.as_ref().unwrap_or(&Type::Void), Type::Char))
                        && Self::integer_order(expected.as_ref().unwrap())
                            <= Self::integer_order(&return_type)
                    {
                        return_type = expected.unwrap();
                    }

                    return_type
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
                let obj = self.visit_expression(object, expected.clone());

                match obj {
                    Type::Pointer(ptr_type) => *ptr_type,
                    Type::Alias(alias) => {
                        const IMPLEMENTATION_FORMAT: &str = "fn deref(&self) _";

                        let struct_type = self.scope.get_struct(&alias).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be dereferenced"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return expected.unwrap_or(Type::Void);
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) = functions.get("deref")
                            {
                                if *args != vec![Type::Alias(alias.clone())] {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!("type `{alias}` has wrong implementation for dereference"),
                                        help: Some(format!("Consider using right format: {IMPLEMENTATION_FORMAT}")),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span)
                                    });
                                }
                                *datatype.clone()
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!("type `{alias}` has no implementation for dereference"),
                                    help: Some(format!("Consider implementing necessary method: {IMPLEMENTATION_FORMAT}")),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span)
                                });
                                expected.unwrap_or(Type::Alias(alias))
                            }
                        } else {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be dereferenced"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            expected.unwrap_or(struct_type)
                        }
                    }

                    _ => {
                        self.error(SemanticError::UnsupportedType {
                            exception: format!("type `{obj}` cannot be dereferenced"),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        expected.unwrap_or(obj)
                    }
                }
            }

            Expressions::Array { values, len, span } => {
                if *len < 1 {
                    self.error(SemanticError::UnknownObject {
                        exception: "empty array is unknown".to_string(),
                        help: Some("Add some elements in array to make it type-known".to_string()),
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return Type::Void;
                }

                let arr_type = self.visit_expression(&values[0], None);

                values.iter().for_each(|val| {
                    let val_type = self.visit_expression(val, None);
                    if val_type != arr_type {
                        self.error(SemanticError::TypesMismatch {
                            exception: format!(
                                "array has type `{arr_type}`, but value is `{val_type}`"
                            ),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(Parser::get_span_expression(val)),
                        });
                    }
                });

                Type::Array(Box::new(arr_type), *len)
            }
            Expressions::Tuple { values, span } => {
                if values.is_empty() {
                    self.error(SemanticError::UnknownObject {
                        exception: "empty tuple is unknown".to_string(),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    return expected.unwrap_or(Type::Void);
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
                let obj = self.visit_expression(object, expected.clone());

                match obj {
                    Type::Tuple(types) => {
                        if let Expressions::Value(Value::Integer(ind), _) = **object {
                            if ind < 0 {
                                self.error(SemanticError::TypesMismatch {
                                    exception: "tuple index must be unsigned".to_string(),
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(Parser::get_span_expression(
                                        index,
                                    )),
                                });

                                return expected.unwrap_or(Type::Void);
                            }

                            types[ind as usize].clone()
                        } else {
                            self.error(SemanticError::UnknownObject {
                                exception: "tuple index must be a known constant".to_string(),
                                help: Some(
                                    "Replace provided index with unsigned integer constant"
                                        .to_string(),
                                ),
                                src: self.source.clone(),
                                span: error::position_to_span(Parser::get_span_expression(index)),
                            });

                            expected.unwrap_or(Type::Void)
                        }
                    }
                    Type::Array(tty, _) => *tty,
                    Type::Pointer(ptr_type) => *ptr_type,
                    Type::DynamicArray(tty) => *tty,

                    Type::Alias(alias) => {
                        const IMPLEMENTATION_FORMAT: &str = "fn slice(&self, index: usize) _";

                        let struct_type = self.scope.get_struct(&alias).unwrap_or_else(|| {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be sliced"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            Type::Void
                        });

                        if struct_type == Type::Void {
                            return expected.unwrap_or(Type::Void);
                        }

                        if let Type::Struct(_, functions) = struct_type {
                            if let Some(Type::Function(args, datatype, _)) = functions.get("slice")
                            {
                                if !(*args == vec![Type::Alias(alias.clone()), Type::USIZE]
                                    && *datatype.clone() != Type::Void)
                                {
                                    self.error(SemanticError::IllegalImplementation {
                                        exception: format!(
                                            "type `{alias}` has wrong implementation for slice"
                                        ),
                                        help: Some(format!(
                                            "Consider using right format: {IMPLEMENTATION_FORMAT}"
                                        )),
                                        src: self.source.clone(),
                                        span: error::position_to_span(*span),
                                    });
                                }

                                *datatype.clone()
                            } else {
                                self.error(SemanticError::IllegalImplementation {
                                    exception: format!("type `{alias}` has no implementation for slice"),
                                    help: Some(format!("Consider implementing necessary method: {IMPLEMENTATION_FORMAT}")),
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span)
                                });
                                expected.unwrap_or(Type::Alias(alias))
                            }
                        } else {
                            self.error(SemanticError::UnsupportedType {
                                exception: format!("type `{alias}` cannot be sliced"),
                                help: None,
                                src: self.source.clone(),
                                span: error::position_to_span(*span),
                            });
                            expected.unwrap_or(struct_type)
                        }
                    }

                    _ => {
                        self.error(SemanticError::UnsupportedType {
                            exception: format!("type `{obj}` isn't supported for slice"),
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        expected.unwrap_or(Type::Void)
                    }
                }
            }
            Expressions::Struct { name, fields, span } => {
                let structure = self.scope.get_struct(name).unwrap_or_else(|| {
                    self.error(SemanticError::UnresolvedName {
                        exception: format!("structure `{name}` doesn't exist here"),
                        help: None,
                        src: self.source.clone(),
                        span: error::position_to_span(*span),
                    });
                    Type::Void
                });

                if structure == Type::Void {
                    return expected.unwrap_or(Type::Void);
                };
                if let Type::Struct(struct_fields, _) = structure.clone() {
                    let mut assigned_fields = HashMap::new();
                    let mut none_fields = Vec::new();

                    struct_fields.iter().for_each(|x| {
                        assigned_fields.insert(x.0, false);
                    });

                    fields.iter().for_each(|field| {
                        let struct_field = struct_fields.get(field.0);
                        if let Some(field_type) = struct_field {
                            // let field_type = self.unwrap_alias(field_type).unwrap_or_else(|err| {
                            //     self.error(err, *span);
                            //     Type::Void
                            // });

                            let field_type = field_type.clone();
                            let provided_type =
                                self.visit_expression(field.1, Some(field_type.clone()));

                            if field_type != provided_type && field_type != Type::Void {
                                self.error(SemanticError::TypesMismatch {
                                    exception: format!(
                                        "field `{}` expected to be `{}`, but found `{}`",
                                        field.0, field_type, provided_type
                                    ),
                                    help: None,
                                    src: self.source.clone(),
                                    span: error::position_to_span(*span),
                                });
                                return;
                            }

                            let _ = assigned_fields.insert(field.0, true);
                        } else {
                            none_fields.push(field.0.as_str());
                        }
                    });

                    if !none_fields.is_empty() {
                        self.error(SemanticError::UnresolvedName {
                            exception: format!("non-existent fields: {}", none_fields.join(", ")),
                            help: Some("Consider removing mentioned fields".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                    }

                    let unassigned = assigned_fields
                        .iter()
                        .filter(|x| !x.1 && !x.0.starts_with("__"))
                        .map(|x| x.0.to_owned().to_owned())
                        .collect::<Vec<String>>();
                    if !unassigned.is_empty() && none_fields.is_empty() {
                        let fmt = format!("`{}`", unassigned.join("` , `"));
                        self.error(SemanticError::MissingFields {
                            exception: format!("missing structure fields: {fmt}"),
                            help: Some("Consider initializing mentioned fields".to_string()),
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
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
                        self.warning(SemanticWarning::UnusedVariable {
                            varname: var.0.clone(),
                            src: self.source.clone(),
                            span: error::position_to_span(var.1),
                        });
                    });
                }

                self.scope = *self.scope.parent.clone().unwrap();

                scope_type
            }

            Expressions::Value(value, span) => {
                match self.visit_value(value.clone(), expected.clone()) {
                    Ok(tty) => tty,
                    Err(err) => {
                        self.error(SemanticError::ValueError {
                            exception: err,
                            help: None,
                            src: self.source.clone(),
                            span: error::position_to_span(*span),
                        });
                        expected.unwrap_or(Type::Void)
                    }
                }
            }
            Expressions::None => Type::Void,
        }
    }

    fn visit_value(&mut self, value: Value, expected: Option<Type>) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
                if expected.is_some()
                    && (Self::is_integer(&expected.clone().unwrap())
                        || expected.as_ref().unwrap() == &Type::Char)
                {
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

                        Type::Char => {
                            if !(0..=255).contains(&int) {
                                return Err(String::from("Constant is out of `char` type range"));
                            }
                        }

                        _ => return Err(format!("Expected `{exp}` but found integer constant")),
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
                if let Some(exp) = expected
                    && Self::is_float(&exp)
                {
                    return Ok(exp);
                }

                if float > f32::MAX as f64 {
                    return Ok(Type::F64);
                }
                Ok(Type::F32)
            }
            Value::Identifier(id) => {
                if self.symtable.imports.contains_key(&id) {
                    return Ok(Type::ImportObject(id));
                }

                if self.scope.get_struct(&id).is_some() {
                    return Ok(Type::Alias(id));
                }
                if self.scope.get_typedef(&id).is_some() {
                    return Ok(Type::Alias(id));
                }
                if self.scope.get_enum(&id).is_some() {
                    return Ok(Type::Alias(id));
                }

                match self.scope.get_var(&id) {
                    Some(var) => {
                        if !var.initialized {
                            return Err(format!("Variable `{id}` isn't initalized"));
                        }

                        if expected.unwrap_or(Type::Undefined) == Type::Char
                            && Self::is_integer(&var.datatype)
                        {
                            return Ok(Type::Char);
                        }
                        Ok(var.datatype)
                    }
                    None => Err(format!("Identifier `{id}` is not defined here")),
                }
            }
            Value::String(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            Value::Char(_) => Ok(Type::Char),
            Value::Boolean(_) => Ok(Type::Bool),
            Value::Keyword(_) => Ok(Type::Void),
            Value::Null => Ok(Type::Null),
            Value::Void => Ok(Type::Void),
        }
    }
}

impl Analyzer {
    pub fn verify_macrocall(
        &mut self,
        name: &String,
        arguments: &[Expressions],
        span: &(usize, usize),
    ) -> Type {
        let macro_object = self.compiler_macros.get(name).cloned().unwrap_or_else(|| {
            self.error(SemanticError::UnresolvedName {
                exception: format!("there's no macro called `{name}!()`"),
                help: Some("Check official compiler's macros list in docs: https://deen-docs.vercel.app/advanced/compiler-macros.html".to_string()),
                src: self.source.clone(),
                span: error::position_to_span(*span)
            });

            CompilerMacros::None
        });

        macro_object.verify_call(self, arguments, span)
    }

    fn verify_cast(&self, from: &Type, to: &Type) -> Result<(), String> {
        match (from, to) {
            // integers types casts
            _ if Self::is_integer(from) && Self::is_integer(to) => Ok(()),

            // float types casts
            _ if Self::is_float(from) && Self::is_float(to) => Ok(()),

            // integers && float casts
            _ if (Self::is_integer(from) && Self::is_float(to))
                || (Self::is_float(from) && Self::is_integer(to)) =>
            {
                Ok(())
            }

            // integer and `char` casts
            _ if (Self::is_integer(from) && to == &Type::Char)
                || (from == &Type::Char && Self::is_integer(to)) =>
            {
                Ok(())
            }

            // boolean and integer casts
            _ if (from == &Type::Bool && Self::is_integer(to))
                || (Self::is_integer(from) && to == &Type::Bool) =>
            {
                Ok(())
            }

            // pointer to integer cast
            _ if matches!(from, &Type::Pointer(_)) && Self::is_integer(to) => Ok(()),

            // integer to pointer cast
            _ if Self::is_integer(from) && matches!(to, &Type::Pointer(_)) => Ok(()),

            // pointers types casts
            _ if matches!(from, &Type::Pointer(_)) && matches!(to, &Type::Pointer(_)) => Ok(()),

            _ if from == to => Ok(()),
            _ => Err(format!("Cast `{from}` -> `{to}` is unavaible")),
        }
    }
}

impl Analyzer {
    /// Returns true if provided type is integer
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

    /// Returns true if provided type is **unsigned** integer
    #[inline]
    pub fn is_unsigned_integer(typ: &Type) -> bool {
        [Type::U8, Type::U16, Type::U32, Type::U64, Type::USIZE].contains(typ)
    }

    /// Converts unsigned integer type to its signed analogue
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

    /// Returns integer order position
    #[inline]
    pub fn integer_order(typ: &Type) -> usize {
        match typ {
            Type::Bool => 0,
            Type::Char => 0,
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

    /// Returns float order position
    #[inline]
    pub fn float_order(typ: &Type) -> usize {
        match typ {
            Type::F32 => 1,
            Type::F64 => 2,

            _ => 0,
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
                        return Err(format!("import `{module_name}` is not declared here"));
                    }
                }

                Err(format!("type `{typ}` is not defined in this scope"))
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

    fn expand_library_path(
        &self,
        path: impl AsRef<str>,
        is_deen_module: bool,
    ) -> Result<PathBuf, Box<dyn std::error::Error>> {
        let path = path.as_ref();

        if path.starts_with('@') {
            // standard library path

            let deenlib_env = std::env::var(STANDARD_LIBRARY_VAR)?;
            let expanded_stdlib_path = shellexpand::full(&deenlib_env)?;
            let mut path_buffer = PathBuf::from(expanded_stdlib_path.as_ref());

            let module_path = format!(
                "{}{}",
                path.replace("@", ""),
                if !path.contains(".dn") && is_deen_module {
                    ".dn"
                } else {
                    ""
                }
            );

            path_buffer.push(module_path);
            Ok(path_buffer)
        } else {
            // relative library path (relative from current module)
            let relative_dir = self
                .source_path
                .parent()
                .unwrap_or(Path::new(""))
                .to_path_buf();

            if relative_dir.to_str().unwrap_or("").is_empty()
                || !relative_dir.exists()
                || !relative_dir.is_dir()
            {
                return Ok(PathBuf::from(path));
            }

            Ok(relative_dir.join(path))
        }
    }
}
