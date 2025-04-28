#![allow(unused)]

use deen_parser::{
    statements::Statements,
    expressions::Expressions,
    value::Value,
    types::Type
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue
    },
    AddressSpace
};
use crate::{
    variable::Variable,
    function::Function,
    enumeration::Enumeration,
    structure::{Structure, Field},
};

use std::collections::HashMap;
use deen_semantic::import::Import;

mod variable;
mod function;
mod structure;
mod enumeration;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    function: Option<FunctionValue<'ctx>>,

    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, Function<'ctx>>,
    structures: HashMap<String, Structure<'ctx>>,
    enumerations: HashMap<String, Enumeration<'ctx>>,
    typedefs: HashMap<String, Type>,

    imports: HashMap<String, Import>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn create_context() -> Context {
        inkwell::targets::Target::initialize_all(
            &inkwell::targets::InitializationConfig::default()
        );
        inkwell::context::Context::create()
    }

    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        imports: HashMap<String, Import>
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        module.set_source_file_name(&format!("{}.dn", module_name));
        module.set_triple(&inkwell::targets::TargetMachine::get_default_triple());

        module.add_global_metadata(
            "ident",
            &context.metadata_node(
                &[
                    context.metadata_string(
                        &format!("deen compiler version {}", env!("CARGO_PKG_VERSION"))
                    ).into()
                ]
            )
        ).unwrap();

        Self {
            context,
            builder,
            module,

            function: None,

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enumerations: HashMap::new(),
            typedefs: HashMap::new(),

            imports,
        }
    }

    pub fn compile(&mut self, statements: Vec<Statements>, prefix: Option<String>) -> &Module<'ctx> {
        for statement in statements {
            self.compile_statement(statement, prefix.clone());
        }

        let main_fn = self.functions.get("main");

        if let Some(main_fn) = main_fn {
            let verified = main_fn.value.verify(false);
            if main_fn.datatype == Type::Void && !verified {
                self.builder.position_at_end(main_fn.value.get_last_basic_block().unwrap());
                self.builder.build_return(
                    Some(
                        &self.context.i8_type().const_int(0, false)
                    )
                ).unwrap();
            }
        }

        &self.module
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn compile_statement(&mut self, statement: Statements, prefix: Option<String>) {
        match statement {
            Statements::AssignStatement { identifier, value, span } => {
                let var = self.variables.get(&identifier).unwrap().clone();
                let compiled_value = self.compile_expression(value, Some(var.datatype));

                self.builder.build_store(var.ptr, compiled_value.1);
            },
            Statements::BinaryAssignStatement { identifier, operand, value, span } => {
                let stmt = Statements::AssignStatement {
                    identifier: identifier.clone(),
                    value: Expressions::Binary {
                        operand,
                        lhs: Box::new(
                            Expressions::Value(
                                Value::Identifier(identifier), (0, 0)
                            )
                        ),
                        rhs: Box::new(
                            value
                        ),
                        span: (0, 0)
                    },
                    span
                };

                self.compile_statement(stmt, prefix);
            },
            Statements::DerefAssignStatement { identifier, value, span } => {
                let var = self.variables.get(&identifier).unwrap().clone();
                let compiled_value = self.compile_expression(value, Some(var.datatype));

                let dereferenced_ptr = self.builder.build_load(self.context.ptr_type(AddressSpace::default()), var.ptr, "").unwrap();
                self.builder.build_store(dereferenced_ptr.into_pointer_value(), compiled_value.1);
            },
            Statements::SliceAssignStatement { identifier, index, value, span } => {
                let var = self.variables.get(&identifier).unwrap().clone();
                let (item_type, len) = match var.datatype.clone() {
                    Type::Array(tty, len) => (*tty, len),

                    _ => unreachable!()
                };

                let compiled_value = self.compile_expression(value, Some(item_type.clone()));
                let compiled_idx = self.compile_expression(index, Some(Type::USIZE));

                // checking for the right index
                let checker_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb"); // idxcb - index checker block
                let error_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb_err");
                let ok_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb_ok");

                self.builder.build_unconditional_branch(checker_block).unwrap();
                self.builder.position_at_end(checker_block);

                let expected_basic_value = self.context.i64_type().const_int((len + 1) as u64, false);
                let provided_basic_value = compiled_idx.1.into_int_value();

                let cmp_value = self.builder.build_int_compare(inkwell::IntPredicate::SLT, provided_basic_value, expected_basic_value, "").unwrap();
                self.builder.build_conditional_branch(cmp_value, ok_block, error_block).unwrap();

                self.builder.position_at_end(error_block);

                let panic_message = self.builder.build_global_string_ptr("Array has len %ld, but index is %ld", "panic_msg").unwrap();
                self.build_panic(
                    panic_message.as_basic_value_enum(),
                    vec![
                        expected_basic_value.into(),
                        provided_basic_value.into()
                    ]
                );
                self.builder.build_unconditional_branch(ok_block).unwrap();
                self.builder.position_at_end(ok_block);

                // getting ptr

                let array_ptr = self.builder.build_load(self.context.ptr_type(AddressSpace::default()), var.ptr, "").unwrap();
                let ptr = unsafe {
                    self.builder.build_gep(
                        self.get_basic_type(item_type),
                        array_ptr.into_pointer_value(),
                        &[
                            compiled_idx.1.into_int_value()
                        ],
                        ""
                    ).unwrap()
                };

                // storing value
                self.builder.build_store(ptr, compiled_value.1).unwrap();
            },
            Statements::FieldAssignStatement { object, value, span } => {
                let compiled_object = self.compile_expression(object, Some(Type::Pointer(Box::new(Type::Void))));
                let compiled_value = self.compile_expression(value, Some(compiled_object.0));

                self.builder.build_store(compiled_object.1.into_pointer_value(), compiled_value.1).unwrap();
            },

            Statements::AnnotationStatement { identifier, datatype, value, span } => {
                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value = self.compile_expression(value, Some(datatype));
                        let alloca = self.builder.build_alloca(value.1.get_type(), &identifier).unwrap();

                        self.variables.insert(identifier, Variable { datatype: value.0, llvm_type: value.1.get_type(), ptr: alloca });
                        let _ = self.builder.build_store(alloca, value.1).unwrap();
                    },
                    (Some(datatype), _) => {
                        let basic_type = self.get_basic_type(datatype.clone());
                        let alloca = self.builder.build_alloca(basic_type, &identifier).unwrap();

                        self.variables.insert(identifier, Variable { datatype, llvm_type: basic_type, ptr: alloca });
                    },
                    (_, Some(value)) => {
                        let compiled_value = self.compile_expression(value, None);
                        let alloca = self.builder.build_alloca(compiled_value.1.get_type(), &identifier).unwrap();

                        self.variables.insert(identifier, Variable { datatype: compiled_value.0, llvm_type: compiled_value.1.get_type(), ptr: alloca });
                        let _ = self.builder.build_store(alloca, compiled_value.1).unwrap();
                    },
                    _ => unreachable!()
                }
            },

            Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span } => {
                let name = format!("{}{}", prefix.unwrap_or_default(), name);

                let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                arguments.iter().for_each(|arg| {
                    args.push(
                        self.get_basic_type(arg.1.clone()).into()
                    );
                });

                let fn_type = self.get_fn_type(datatype.clone(), &args, false);
                let function = self.module.add_function(&name, fn_type, Some(inkwell::module::Linkage::External));
                let entry = self.context.append_basic_block(function, "entry");

                let old_function = self.function.clone();
                let old_position = self.builder.get_insert_block();
                self.builder.position_at_end(entry);
                self.function = Some(function);

                let mut old_variables = HashMap::new();

                arguments.iter().enumerate().for_each(|(index, arg)| {
                    let arg_name = arg.0.clone();
                    let arg_value = function.get_nth_param(index as u32).unwrap();
                    old_variables.insert(arg_name.clone(), self.variables.remove(&arg_name));

                    let param_type = self.get_basic_type(arg.1.clone());
                    let param_alloca = self.builder.build_alloca(param_type, "").unwrap();

                    let _ = self.builder.build_store(param_alloca, arg_value);
                    
                    self.variables.insert(arg_name, Variable { datatype: arg.1.clone(), llvm_type: param_type, ptr: param_alloca });
                });

                let typed_args = arguments.iter().map(|x| x.1.clone()).collect();
                self.functions.insert(name.clone(), Function { name, datatype, value: function, arguments: typed_args });
                block.iter().for_each(|stmt| self.compile_statement(stmt.clone(), None));

                if let Some(basic_block) = old_position {
                    self.builder.position_at_end(basic_block);
                }
                
                self.function = old_function;
                old_variables.iter().for_each(|var| {
                    if let Some(value) = var.1 {
                        self.variables.insert(var.0.to_owned(), value.to_owned());
                    }
                })
            }
            Statements::FunctionCallStatement { name, arguments, span } => {
                let function = self.functions.get(&name).unwrap().clone();
                let mut basic_args: Vec<BasicMetadataValueEnum> = Vec::new();

                arguments.into_iter().zip(function.arguments.clone()).for_each(|(expr, expected)| {
                    basic_args.push(
                        self.compile_expression(expr, Some(expected)).1.into()
                    );
                });

                self.builder.build_call(function.value, &basic_args, "");
            },

            Statements::StructDefineStatement { name, fields, functions, public, span } => {
                let name = format!("{}{}", prefix.unwrap_or_default(), name);
                let struct_type = self.context.opaque_struct_type(&name);
                let mut compiled_fields = Vec::new();

                fields.iter().for_each(|field| {
                    compiled_fields.push(
                        Field {
                            name: field.0.to_owned(),
                            nth: compiled_fields.len() as u32,
                            datatype: field.1.to_owned(),
                            llvm_type: self.get_basic_type(field.1.to_owned())
                        }
                    );
                });

                let basic_fields_types = compiled_fields.iter().map(|field| field.llvm_type).collect::<Vec<BasicTypeEnum>>();
                struct_type.set_body(&basic_fields_types, false);

                let mut fields_hashmap = HashMap::new();
                compiled_fields.into_iter().for_each(|field| {
                    fields_hashmap.insert(field.name.clone(), field);
                });

                self.structures.insert(name.clone(), Structure { name: name.clone(), fields: fields_hashmap, llvm_type: struct_type.into() });
                
                functions.iter().for_each(|(_, function_statement)| {
                    self.compile_statement(function_statement.to_owned(), Some(format!("struct_{}__", name)));
                });
            },
            Statements::EnumDefineStatement { name, fields, functions, public, span } => {
                let name = format!("{}{}", prefix.unwrap_or_default(), name);
                self.enumerations.insert(name.clone(), Enumeration { name: name.clone(), fields, llvm_type: self.context.i8_type().into() });

                functions.iter().for_each(|(_, function_statement)| {
                    self.compile_statement(function_statement.to_owned(), Some(format!("enum_{}__", name)))
                });
            },
            Statements::TypedefStatement { alias, datatype, span } => {
                self.typedefs.insert(alias, datatype);
            },

            Statements::IfStatement { condition, then_block, else_block, span } => {
                let condition = self.compile_expression(condition, None);

                let then_basic_block = self.context.append_basic_block(self.function.unwrap(), "__if_then");
                let else_basic_block = if else_block.is_some() { Some(self.context.append_basic_block(self.function.unwrap(), "__if_else")) } else { None };
                let after_basic_block = self.context.append_basic_block(self.function.unwrap(), "__if_after");

                self.builder.build_conditional_branch(condition.1.into_int_value(), then_basic_block, else_basic_block.unwrap_or(after_basic_block)).unwrap();
                self.builder.position_at_end(then_basic_block);

                then_block.into_iter().for_each(|stmt| self.compile_statement(stmt, None));
                self.builder.build_unconditional_branch(after_basic_block).unwrap();

                if let Some(else_basic_block) = else_basic_block {
                    self.builder.position_at_end(else_basic_block);
                    else_block.unwrap().into_iter().for_each(|stmt| self.compile_statement(stmt, None));
                    self.builder.build_unconditional_branch(after_basic_block).unwrap();
                }

                self.builder.position_at_end(after_basic_block);
            },
            Statements::WhileStatement { condition, block, span } => todo!(),
            Statements::ForStatement { binding, iterator, block, span } => todo!(),

            Statements::BreakStatements { span } => todo!(),
            Statements::ReturnStatement { value, span } => {
                let compiled_value = self.compile_expression(value, None);
                self.builder.build_return(Some(&compiled_value.1)).unwrap();
            },
            Statements::ImportStatement { path, span } => {
                let path = if let Expressions::Value(Value::String(path), _) = path { path } else { String::default() };
                let fname = std::path::Path::new(&path)
                    .file_name()
                    .map(|fname| {
                        fname.to_str().unwrap_or("$NONE")
                    })
                    .unwrap();

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                let import = self.imports.get(&module_name).unwrap();
                let mut codegen = Self::new(&self.context, &module_name, import.embedded_imports.clone());

                let prefix = format!("__{}_", module_name);
                let module = codegen.compile(import.ast.clone(), Some(prefix));

                self.module.link_in_module(module.to_owned());
            },
            Statements::ScopeStatement { block, span } => {
                block.iter().for_each(|stmt| self.compile_statement(stmt.clone(), None));
            }

            Statements::Expression(expr) => {
                let _ = self.compile_expression(expr, None);
            }
            Statements::None => unreachable!()
        }
    }

    fn compile_expression(&mut self, expression: Expressions, expected: Option<Type>) -> (Type, BasicValueEnum<'ctx>) {
        match expression {
            Expressions::Value(val, _) => self.compile_value(val, expected),
            Expressions::FnCall { name, arguments, span } => {
                let function = self.functions.get(&name).unwrap().clone();
                let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                arguments.iter().zip(function.arguments).for_each(|(arg, fn_expected)| {
                    args.push(self.compile_expression(arg.clone(), Some(fn_expected)).1.into())
                });

                (function.datatype, self.builder.build_call(function.value, &args, "").unwrap().try_as_basic_value().left().unwrap())
            }

            Expressions::Reference { object, span } => {
                match *object {
                    Expressions::Value(Value::Identifier(id), _) => {
                        let var = self.variables.get(&id).unwrap();
                        (var.datatype.clone(), var.ptr.into())
                    },
                    _ => {
                        let value = self.compile_expression(*object, expected);
                        let alloca = self.builder.build_alloca(value.1.get_type(), "").unwrap();
                        let _ = self.builder.build_store(alloca, value.1);

                        (Type::Pointer(Box::new(value.0)), alloca.into())
                    }
                }
            },
            Expressions::Dereference { object, span } => {
                let (ptr_type, ptr) = self.compile_expression(*object, expected);
                let basic_type = self.get_basic_type(ptr_type.clone());

                let value = self.builder.build_load(basic_type, ptr.into_pointer_value(), "").unwrap();
                (ptr_type, value)
            },

            Expressions::Unary { operand, object, span } => {
                let object_value = self.compile_expression(*object, expected);
                
                match operand.as_str() {
                    "-" => {
                        match object_value.0 {
                            Type::I8 | Type::I16 | Type::I32 | Type::I64 |
                            Type::U8 | Type::U16 | Type::U32 | Type::U64 |
                            Type::USIZE => {
                                (
                                    deen_semantic::Analyzer::unsigned_to_signed_integer(&object_value.0),
                                    self.builder.build_int_neg(object_value.1.into_int_value(), "").unwrap().into()
                                )
                            }

                            Type::F32 | Type::F64 => {
                                (
                                    object_value.0,
                                    self.builder.build_float_neg(object_value.1.into_float_value(), "").unwrap().into()
                                )
                            }

                            _ => unreachable!()
                        }
                    }

                    "!" => {
                        (
                            object_value.0,
                            self.builder.build_not(object_value.1.into_int_value(), "").unwrap().into()
                        )
                    }

                    _ => unreachable!()
                }
            },
            Expressions::Binary { operand, lhs, rhs, span } => {
                let lhs_value = self.compile_expression(*lhs, expected.clone());
                let rhs_value = self.compile_expression(*rhs, expected);

                let senior_type = match lhs_value.0.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        if deen_semantic::Analyzer::integer_order(&lhs_value.0) > deen_semantic::Analyzer::integer_order(&rhs_value.0) {
                            lhs_value.0
                        } else {
                            rhs_value.0
                        }
                    }
                    typ if deen_semantic::Analyzer::is_float(&typ) => {
                        if deen_semantic::Analyzer::float_order(&lhs_value.0) > deen_semantic::Analyzer::float_order(&rhs_value.0) {
                            lhs_value.0
                        } else {
                            rhs_value.0
                        }
                    }

                    _ => unreachable!()
                };

                let output = match senior_type.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        match operand.as_str() {
                            "+" => {
                                if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                    self.builder.build_int_nsw_add(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                } else {
                                    self.builder.build_int_add(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                }
                            }
                            "-" => {
                                if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                    self.builder.build_int_nsw_sub(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                } else {
                                    self.builder.build_int_sub(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                }
                            }
                            "*" => {
                                if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                    self.builder.build_int_nsw_mul(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                } else {
                                    self.builder.build_int_mul(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                }
                            }
                            "/" => {
                                if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                    self.builder.build_int_unsigned_div(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                } else {
                                    self.builder.build_int_signed_div(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                                }
                            }

                            _ => unreachable!()
                        }
                    },
                    typ if deen_semantic::Analyzer::is_float(&typ) => {
                        match operand.as_str() {
                            "+" => self.builder.build_float_add(lhs_value.1.into_float_value(), rhs_value.1.into_float_value(), "").unwrap().as_basic_value_enum(),
                            "-" => self.builder.build_float_sub(lhs_value.1.into_float_value(), rhs_value.1.into_float_value(), "").unwrap().as_basic_value_enum(),
                            "*" => self.builder.build_float_mul(lhs_value.1.into_float_value(), rhs_value.1.into_float_value(), "").unwrap().as_basic_value_enum(),
                            "/" => self.builder.build_float_div(lhs_value.1.into_float_value(), rhs_value.1.into_float_value(), "").unwrap().as_basic_value_enum(),

                            _ => unreachable!()
                        }
                    },
                    _ => unreachable!()
                };

                (senior_type, output)
            },
            Expressions::Boolean { operand, lhs, rhs, span } => {
                let lhs_value = self.compile_expression(*lhs, expected.clone());
                let rhs_value = self.compile_expression(*rhs, expected);

                match operand.as_str() {
                    "&&" => {
                        return (
                            Type::Bool,
                            self.builder.build_and(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                        )
                    }
                    "||" => {
                        return (
                            Type::Bool,
                            self.builder.build_or(lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                        )
                    }
                    _ => {}
                }

                match lhs_value.0 {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        let predicate = match operand.as_str() {
                            ">" => inkwell::IntPredicate::SGT,
                            "<" => inkwell::IntPredicate::SLT,
                            "<=" | "=<" => inkwell::IntPredicate::SLE,
                            ">=" | "=>" => inkwell::IntPredicate::SGE,
                            "==" => inkwell::IntPredicate::EQ,
                            "!=" => inkwell::IntPredicate::NE,
                            _ => unreachable!()
                        };

                        (
                            Type::Bool,
                            self.builder.build_int_compare(predicate, lhs_value.1.into_int_value(), rhs_value.1.into_int_value(), "").unwrap().as_basic_value_enum()
                        )
                    }

                    typ if deen_semantic::Analyzer::is_float(&typ) => {
                        let predicate = match operand.as_str() {
                            ">" => inkwell::FloatPredicate::OGT,
                            "<" => inkwell::FloatPredicate::OLT,
                            "<=" | "=<" => inkwell::FloatPredicate::OLE,
                            ">=" | "=>" => inkwell::FloatPredicate::OGE,
                            "==" => inkwell::FloatPredicate::OEQ,
                            "!=" => inkwell::FloatPredicate::ONE,
                            _ => unreachable!()
                        };


                        (
                            Type::Bool,
                            self.builder.build_float_compare(predicate, lhs_value.1.into_float_value(), rhs_value.1.into_float_value(), "").unwrap().as_basic_value_enum()
                        )
                    }
                    Type::Pointer(ptr_type) if *ptr_type == Type::Char => {
                        todo!()
                    }

                    _ => unreachable!()
                }
            },
            Expressions::Bitwise { operand, lhs, rhs, span } => {
                let left = self.compile_expression(*lhs, expected.clone());
                let right = self.compile_expression(*rhs, expected.clone());

                let sign_extend = deen_semantic::Analyzer::is_unsigned_integer(&left.0);
                let basic_value = match operand.as_str() {
                    "<<" => self.builder.build_left_shift(left.1.into_int_value(), right.1.into_int_value(), "").unwrap().as_basic_value_enum(),
                    ">>" => self.builder.build_right_shift(left.1.into_int_value(), right.1.into_int_value(), sign_extend, "").unwrap().as_basic_value_enum(),
                    "&" => self.builder.build_and(left.1.into_int_value(), right.1.into_int_value(), "").unwrap().as_basic_value_enum(),
                    "|" => self.builder.build_or(left.1.into_int_value(), right.1.into_int_value(), "").unwrap().as_basic_value_enum(),
                    "^" => self.builder.build_xor(left.1.into_int_value(), right.1.into_int_value(), "").unwrap().as_basic_value_enum(),

                    _ => unreachable!()
                };

                (left.0, basic_value)
            },

            Expressions::SubElement { head, subelements, span } => {
                let compiled_head = self.compile_expression(*head, Some(Type::Pointer(Box::new(Type::Void))));

                let mut prev_val = compiled_head.1;
                let mut prev_type = compiled_head.0;
                let mut prev_expr = Expressions::None;

                subelements.iter().for_each(|sub| {
                    match sub {
                        Expressions::Value(Value::Identifier(field), _) => {
                            if let Type::Alias(alias) = prev_type.clone() {
                                let alias_type = self.get_alias_type(prev_type.clone()).unwrap();

                                match alias_type {
                                    "struct" => {
                                        let structure = self.structures.get(&alias).unwrap();
                                        let field = structure.fields.get(field).unwrap();

                                        let ptr = self.builder.build_struct_gep(structure.llvm_type, prev_val.into_pointer_value(), field.nth, "").unwrap();
                                        
                                        let value = if let Some(Type::Pointer(_)) = expected {
                                            ptr.as_basic_value_enum()
                                        } else {
                                            self.builder.build_load(field.llvm_type, ptr, "").unwrap()
                                        };

                                        prev_type = field.datatype.clone();
                                        prev_val = value;
                                    },
                                    "enum" => {
                                        let enumeration = self.enumerations.get(&alias).unwrap();
                                        let idx = enumeration.fields.iter().position(|f| f == field).unwrap();
                                        let idx_value = self.context.i8_type().const_int(idx as u64, false);

                                        prev_val = idx_value.into();
                                    },

                                    _ => unreachable!()
                                }
                            }
                        },

                        Expressions::Value(Value::Integer(idx), _) => {
                            match prev_type.clone() {
                                Type::Tuple(types) => {
                                    let field_type = types[idx.clone() as usize].clone();
                                    let field_basic_type = self.get_basic_type(field_type.clone());

                                    let tuple_type = self.context.struct_type(
                                        &types.into_iter().map(|typ| self.get_basic_type(typ)).collect::<Vec<BasicTypeEnum>>(),
                                        false
                                    );

                                    let ptr = self.builder.build_struct_gep(tuple_type, prev_val.into_pointer_value(), idx.clone() as u32, "").unwrap();


                                    let value = if let Some(Type::Pointer(_)) = expected {
                                        ptr.as_basic_value_enum()
                                    } else {
                                        self.builder.build_load(field_basic_type, ptr, "").unwrap()
                                    };

                                    prev_type = field_type;
                                    prev_val = value;
                                }
                                _ => unreachable!()
                            }
                        },

                        Expressions::FnCall { name, arguments, span: _ } => {
                            match prev_type.clone() {
                                Type::Alias(alias) => {
                                    let alias_type = self.get_alias_type(prev_type.clone()).unwrap();

                                    match alias_type {
                                        "struct" | "enum" => {
                                            let function = self.functions.get(&format!("{}_{}__{}", alias_type, alias, name)).unwrap().clone();
                                            let mut arguments = arguments
                                                .into_iter()
                                                .zip(function.arguments.clone())
                                                .map(|(arg, exp)| self.compile_expression(arg.clone(), Some(exp)).1.into())
                                                .collect::<Vec<BasicMetadataValueEnum>>();
                                            
                                            if let Some(Type::Alias(first_arg)) = function.arguments.get(0) {
                                                if *first_arg == alias {
                                                    let self_val: BasicMetadataValueEnum = if prev_val.is_pointer_value() {
                                                        self.builder.build_load(
                                                            self.get_basic_type(prev_type.clone()),
                                                            prev_val.into_pointer_value(),
                                                            ""
                                                        ).unwrap().into()
                                                    } else { prev_val.into() };

                                                    arguments.reverse();
                                                    arguments.push(self_val);
                                                    arguments.reverse();
                                                }
                                            }
                                            
                                            prev_type = function.datatype;
                                            prev_val = self.builder.build_call(function.value, &arguments, "").unwrap().try_as_basic_value().left().unwrap();
                                        },
                                        _ => unreachable!()
                                    }
                                },
                                _ => unreachable!()
                            }
                        }

                        _ => unreachable!()
                    }
                });

                (prev_type, prev_val)
            },
            Expressions::Scope { block, span } => {
                let fn_type = self.get_fn_type(expected.clone().unwrap_or(Type::Void), &[], false);
                let scope_fn_value = self.module.add_function("__scope_wrap", fn_type, Some(inkwell::module::Linkage::Private));
                let entry = self.context.append_basic_block(scope_fn_value, "entry");
                let current_position = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(entry);
                block.iter().for_each(|stmt| self.compile_statement(stmt.to_owned(), None));

                self.builder.position_at_end(current_position);
                let scope_result = self.builder.build_call(scope_fn_value, &[], "").unwrap().try_as_basic_value().left().unwrap();

                (expected.unwrap_or(Type::Void), scope_result)
            },

            Expressions::Array { values, len, span } => {
                let expected_items_type = match expected {
                    Some(Type::Array(typ, _)) => Some(*typ),
                    _ => None
                };

                let compiled_values = values.into_iter().map(|val| self.compile_expression(val, expected_items_type.clone())).collect::<Vec<(Type, BasicValueEnum)>>();

                let arr_type = compiled_values[0].0.clone();
                let arr_basic_type = compiled_values[0].1.get_type();

                let arr_alloca = self.builder.build_array_alloca(arr_basic_type, self.context.i64_type().const_int(len as u64, false), "").unwrap();

                compiled_values.into_iter().enumerate().for_each(|(ind, (_, basic_value))| {
                    let ptr = unsafe {
                        self.builder.build_gep(
                            arr_basic_type,
                            arr_alloca,
                            &[
                                self.context.i64_type().const_int(ind as u64, false)
                            ],
                            ""
                        ).unwrap()
                    };
                    self.builder.build_store(ptr, basic_value).unwrap();
                });

                (Type::Array(Box::new(arr_type), len), arr_alloca.into())
            },
            Expressions::Tuple { values, span } => {
                let mut expected_types = values.iter().map(|_| None).collect::<Vec<Option<Type>>>();
                if let Some(Type::Tuple(expectations)) = expected.clone() {
                    expected_types = expectations.into_iter().map(|exp| Some(exp)).collect();
                }

                let compiled_values = values.into_iter().zip(expected_types).map(|(val, exp)| self.compile_expression(val, exp)).collect::<Vec<(Type, BasicValueEnum)>>();
                let tuple_type = self.context.struct_type(
                    &compiled_values.iter().map(|val| val.1.get_type()).collect::<Vec<BasicTypeEnum>>(),
                    false
                );

                let compiled_types = compiled_values.iter().map(|(typ, _)| typ.clone()).collect::<Vec<Type>>();
                let alloca = self.builder.build_alloca(
                    tuple_type,
                    &format!(
                        "tuple__{}",
                        compiled_types.iter().map(|typ| typ.to_string()).collect::<Vec<String>>().join("_")
                    )
                ).unwrap();

                compiled_values.into_iter().enumerate().for_each(|(idx, (_, basic_val))| {
                    let ptr = self.builder.build_struct_gep(tuple_type, alloca, idx as u32, "").unwrap();
                    self.builder.build_store(ptr, basic_val);
                });

                let value = match expected {
                    Some(Type::Pointer(_)) => alloca.into(),
                    _ => self.builder.build_load(tuple_type, alloca, "").unwrap()
                };
                let tuple_datatype = Type::Tuple(compiled_types);

                (tuple_datatype, value)
            },
            Expressions::Slice { object, index, span } => {
                let obj = self.compile_expression(*object, None);
                let idx = self.compile_expression(*index, Some(Type::USIZE));

                match obj.0 {
                    Type::Array(ret_type, len) => {
                        // checking for the right index
                        let checker_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb"); // idxcb - index checker block
                        let error_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb_err");
                        let ok_block = self.context.append_basic_block(self.function.unwrap(), "__idxcb_ok");

                        self.builder.build_unconditional_branch(checker_block).unwrap();
                        self.builder.position_at_end(checker_block);

                        let expected_basic_value = self.context.i64_type().const_int((len + 1) as u64, false);
                        let provided_basic_value = idx.1.into_int_value();

                        let cmp_value = self.builder.build_int_compare(inkwell::IntPredicate::SLT, provided_basic_value, expected_basic_value, "").unwrap();
                        self.builder.build_conditional_branch(cmp_value, ok_block, error_block).unwrap();

                        self.builder.position_at_end(error_block);


                        let panic_message = self.builder.build_global_string_ptr("Array has len %ld, but index is %ld", "panic_msg").unwrap();
                        self.build_panic(
                            panic_message.as_basic_value_enum(),
                            vec![
                                expected_basic_value.into(),
                                provided_basic_value.into()
                            ]
                        );
                        self.builder.build_unconditional_branch(ok_block).unwrap();

                        self.builder.position_at_end(ok_block);

                        // getting value

                        let basic_ret_type = self.get_basic_type(*ret_type.clone());
                        let ptr = unsafe {
                            self.builder.build_gep(
                                basic_ret_type,
                                obj.1.into_pointer_value(),
                                &[
                                    idx.1.into_int_value()
                                ],
                                ""
                            ).unwrap()
                        };

                        let ret_value = self.builder.build_load(basic_ret_type, ptr, "").unwrap();
                        return (*ret_type, ret_value);
                    }
                    _ => unreachable!()
                }
            },
            Expressions::Struct { name, fields, span } => {
                let name = if self.is_main() { name } else { format!("__{}_{}", self.module.get_name().to_str().unwrap(), name) };
                let structure = self.structures.get(&name).unwrap().clone();
                let struct_alloca = self.builder.build_alloca(structure.llvm_type, &format!("struct.{}.init", name)).unwrap();

                for (field_name, field_expr) in fields {
                    let struct_field = structure.fields.get(&field_name).unwrap();
                    let field_value = self.compile_expression(field_expr, Some(struct_field.datatype.clone()));

                    let ordered_index = self.context.i64_type().const_int(struct_field.nth as u64, false);
                    let field_ptr = unsafe {
                        self.builder.build_gep(structure.llvm_type, struct_alloca, &[ordered_index], "").unwrap()
                    };

                    let _ = self.builder.build_store(field_ptr, field_value.1).unwrap();
                }
                
                let value = match expected {
                    Some(Type::Pointer(_)) => struct_alloca.into(),
                    _ => self.builder.build_load(structure.llvm_type, struct_alloca, "").unwrap()
                };

                (Type::Alias(name), value)
            },

            Expressions::Argument { name, r#type, span } => unreachable!(),
            Expressions::None => unreachable!()
        }
    }

    fn compile_value(&mut self, value: Value, expected: Option<Type>) -> (Type, BasicValueEnum<'ctx>) {
        match value {
            Value::Integer(int) => {
                if let Some(exp) = expected {
                    let (expected_type, signed) = match exp {
                        Type::I8 => (self.context.i8_type(), true),
                        Type::I16 => (self.context.i16_type(), true),
                        Type::I32 => (self.context.i32_type(), true),
                        Type::I64 => (self.context.i64_type(), true),

                        Type::U8 => (self.context.i8_type(), false),
                        Type::U16 => (self.context.i16_type(), false),
                        Type::U32 => (self.context.i32_type(), false),
                        Type::U64 => (self.context.i64_type(), false),
                        Type::USIZE => (self.context.i64_type(), false),

                        _ => unreachable!()
                    };

                    return (exp, expected_type.const_int(int as u64, signed).as_basic_value_enum());
                }

                match int {
                    -2_147_483_648..=2_147_483_647 => (Type::I32, self.context.i32_type().const_int(int as u64, true).into()),
                    -9_223_372_036_854_775_808..=9_223_372_036_854_775_807 => (Type::I64, self.context.i64_type().const_int(int as u64, true).into()),
                }
            }
            Value::Float(float) => {
                if let Some(exp) = expected {
                    return match exp {
                        Type::F32 => (Type::F32, self.context.f32_type().const_float(float).into()),
                        Type::F64 => (Type::F64, self.context.f64_type().const_float(float).into()),
                        _ => unreachable!()
                    };
                }

                (Type::F32, self.context.f32_type().const_float(float).into())
            },
            
            Value::Char(ch) => (Type::Char, self.context.i8_type().const_int(ch as u64, false).into()),
            Value::String(str) => {
                let global_value = self.builder.build_global_string_ptr(&str, "const_str").unwrap();
                global_value.set_constant(false);
                (Type::Pointer(Box::new(Type::Char)), global_value.as_pointer_value().into())
            },

            Value::Boolean(bool) => (Type::Bool, self.context.bool_type().const_int(bool as u64, false).into()),
            Value::Identifier(id) => {
                let externed_id = if self.is_main() { id.clone() } else { format!("__{}_{}", self.module.get_name().to_str().unwrap(), id) };
                if let Some(typedef) = self.typedefs.get(&externed_id) { return (typedef.clone(), self.context.i8_type().const_zero().into()) }
                if let Some(enumeration) = self.enumerations.get(&externed_id) { return (Type::Alias(externed_id), self.context.i8_type().const_zero().into()) }
                
                let variable = self.variables.get(&id).unwrap(); // already checked by semantic analyzer
                let value = match expected {
                    Some(Type::Pointer(_)) => variable.ptr.into(),
                    _ => self.builder.build_load(variable.llvm_type, variable.ptr, "").unwrap()
                };

                (variable.datatype.clone(), value)
            },

            Value::Keyword(key) => unreachable!()
        }
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn build_panic(&mut self, message: BasicValueEnum<'ctx>, specifiers: Vec<BasicMetadataValueEnum<'ctx>>) {
        let panic_fn = self.module.get_function("__deen_panic").unwrap_or_else(|| {
            self.create_panic_function()
        });

        let mut args: Vec<BasicMetadataValueEnum> = [vec![message.into()], specifiers].concat();
        self.builder.build_call(panic_fn, &args, "");
    }

    fn create_panic_function(&mut self) -> FunctionValue<'ctx> {
        let fn_type = self.context.void_type().fn_type(&[
            self.context.ptr_type(AddressSpace::default()).into()
        ], true);
        let fn_value = self.module.add_function("__deen_panic", fn_type, Some(inkwell::module::Linkage::Private));
        let entry = self.context.append_basic_block(fn_value, "entry");
        let old_position = self.builder.get_insert_block().unwrap();
        self.builder.position_at_end(entry);
        
        let printf_fn = self.module.get_function("printf").unwrap_or_else(|| {
            self.module.add_function(
                "printf",
                self.context.void_type().fn_type(&[
                    self.context.ptr_type(AddressSpace::default()).into()
                ], true),
                None
            )
        });
        let exit_fn = self.module.get_function("exit").unwrap_or_else(|| {
            self.module.add_function(
                "exit",
                self.context.void_type().fn_type(&[
                    self.context.i32_type().into()
                ], false),
                None
            )
        });

        let args = fn_value.get_params().into_iter().map(|x| x.into()).collect::<Vec<BasicMetadataValueEnum>>();

        self.builder.build_call(printf_fn, &args, "");
        self.builder.build_call(exit_fn, &[self.context.i32_type().const_int(1, false).into()], "");
        self.builder.build_return(None);

        self.builder.position_at_end(old_position);

        fn_value
    }
}

impl<'ctx> CodeGen<'ctx> {
    #[inline]
    fn get_basic_type(&self, datatype: Type) -> BasicTypeEnum<'ctx> {
        match datatype {
            Type::I8 => self.context.i8_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),

            Type::U8 => self.context.i8_type().into(),
            Type::U16 => self.context.i16_type().into(),
            Type::U32 => self.context.i32_type().into(),
            Type::U64 => self.context.i32_type().into(),
            Type::USIZE => self.context.custom_width_int_type(64).into(),

            Type::F32 => self.context.f32_type().into(),
            Type::F64 => self.context.f64_type().into(),

            Type::Void => self.context.i8_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Char => self.context.custom_width_int_type(8).into(),
            Type::Bool => self.context.bool_type().into(),

            Type::Pointer(_) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Array(datatype, len) => self.get_basic_type(*datatype).array_type(len as u32).into(),
            Type::DynamicArray(datatype) => todo!(),
            
            Type::Tuple(types) => unreachable!(),
            Type::Alias(alias) => {
                let alias = if self.is_main() { alias } else { format!("__{}_{}", self.module.get_name().to_str().unwrap(), alias) };
                let struct_type = self.structures.get(&alias);
                let enum_type = self.enumerations.get(&alias);
                let typedef_type = self.typedefs.get(&alias);

                if let Some(struct_type) = struct_type { return struct_type.llvm_type };
                if let Some(enum_type) = enum_type { return enum_type.llvm_type };
                if let Some(typedef_type) = typedef_type { return self.get_basic_type(typedef_type.to_owned()) };

                unreachable!()
            },
            
            Type::Function(_, _) => unreachable!(),
            Type::ImportObject(_) => unreachable!(),
            Type::Struct(fields, _) => self.context.struct_type(
                &fields.iter().map(|field| 
                    self.get_basic_type(field.1.clone())
                ).collect::<Vec<BasicTypeEnum>>(),
                false
            ).into(),
            Type::Enum(fields, _) => self.context.i16_type().into(),

            // Type::Function(args, datatype) => self.get_basic_type(*datatype).fn_type(
            //     &args.iter().map(|arg| self.get_basic_type(arg.clone()).into()).collect::<Vec<BasicMetadataTypeEnum>>(),
            //     false
            // ).into()
        }
    }

    fn get_fn_type(&self, datatype: Type, arguments: &[BasicMetadataTypeEnum<'ctx>], is_var_args: bool) -> FunctionType<'ctx> {
        self.get_basic_type(datatype).fn_type(arguments, false)
    }

    fn get_alias_type(&self, alias_type: Type) -> Option<&str> {
        if let Type::Alias(alias) = alias_type {
            let struct_type = self.structures.get(&alias);
            let enum_type = self.enumerations.get(&alias);
            let typedef_type = self.typedefs.get(&alias);

            if let Some(struct_type) = struct_type { return Some("struct") };
            if let Some(enum_type) = enum_type { return Some("enum") };
            if let Some(typedef_type) = typedef_type { return Some("typedef") };

            unreachable!()
        } else {
            None
        }
    }

    fn is_main(&self) -> bool {
        self.module.get_function("main").is_some()
    }
}
