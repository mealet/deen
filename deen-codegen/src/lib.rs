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

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enumerations: HashMap::new(),
            typedefs: HashMap::new(),

            imports,
        }
    }

    pub fn compile(&mut self, statements: Vec<Statements>) -> &Module<'ctx> {
        for statement in statements {
            self.compile_statement(statement, None);
        }

        let main_fn = self.functions.get("main").unwrap();
        let verified = main_fn.value.verify(false);
        if main_fn.datatype == Type::Void && !verified {
            self.builder.position_at_end(main_fn.value.get_last_basic_block().unwrap());
            self.builder.build_return(
                Some(
                    &self.context.i8_type().const_int(0, false)
                )
            ).unwrap();
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
            Statements::SliceAssignStatement { identifier, index, value, span } => todo!(),
            Statements::FieldAssignStatement { object, subelements, span } => todo!(),

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

            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {
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

                let old_position = self.builder.get_insert_block();
                self.builder.position_at_end(entry);

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

            Statements::StructDefineStatement { name, fields, functions, span } => {
                let struct_type = self.context.opaque_struct_type(&name);
                let mut compiled_fields = Vec::new();

                fields.iter().for_each(|field| {
                    compiled_fields.push(
                        Field {
                            name: field.0.to_owned(),
                            nth: compiled_fields.len(),
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
            Statements::EnumDefineStatement { name, fields, functions, span } => {
                self.enumerations.insert(name.clone(), Enumeration { name: name.clone(), fields, llvm_type: self.context.i8_type().into() });

                functions.iter().for_each(|(_, function_statement)| {
                    self.compile_statement(function_statement.to_owned(), Some(format!("enum_{}__", name)))
                });
            },
            Statements::TypedefStatement { alias, datatype, span } => {
                self.typedefs.insert(alias, datatype);
            },

            Statements::IfStatement { condition, then_block, else_block, span } => todo!(),
            Statements::WhileStatement { condition, block, span } => todo!(),
            Statements::ForStatement { binding, iterator, block, span } => todo!(),

            Statements::BreakStatements { span } => todo!(),
            Statements::ReturnStatement { value, span } => {
                let compiled_value = self.compile_expression(value, None);
                self.builder.build_return(Some(&compiled_value.1)).unwrap();
            },
            Statements::ImportStatement { path, span } => todo!(),
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

            Expressions::Unary { operand, object, span } => todo!(),
            Expressions::Binary { operand, lhs, rhs, span } => todo!(),
            Expressions::Boolean { operand, lhs, rhs, span } => todo!(),
            Expressions::Bitwise { operand, lhs, rhs, span } => todo!(),

            Expressions::SubElement { head, subelements, span } => todo!(),
            Expressions::Scope { block, span } => todo!(),

            Expressions::Array { values, len, span } => todo!(),
            Expressions::Slice { object, index, span } => todo!(),
            Expressions::Struct { name, fields, span } => {
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
                (Type::String, global_value.as_pointer_value().into())
            },

            Value::Boolean(bool) => (Type::Bool, self.context.bool_type().const_int(bool as u64, false).into()),
            Value::Identifier(id) => {
                let variable = self.variables.get(&id).unwrap(); // already checked by semantic analyzer
                (variable.datatype.clone(), self.builder.build_load(variable.llvm_type, variable.ptr, "").unwrap())
            },

            Value::Keyword(key) => unreachable!()
        }
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
                let struct_type = self.structures.get(&alias);
                let enum_type = self.enumerations.get(&alias);
                let typedef_type = self.typedefs.get(&alias);

                if let Some(struct_type) = struct_type { return struct_type.llvm_type };
                if let Some(enum_type) = enum_type { return enum_type.llvm_type };
                if let Some(typedef_type) = typedef_type { return self.get_basic_type(typedef_type.to_owned()) };

                unreachable!()
            },
            
            Type::Function(_, _) => unreachable!(),
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
}
