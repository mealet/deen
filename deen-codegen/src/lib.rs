use crate::{
    enumeration::Enumeration,
    function::Function,
    macros::StandartMacros,
    scope::Scope,
    structure::{Field, Structure},
    variable::Variable,
};
use deen_parser::{expressions::Expressions, statements::Statements, types::Type, value::Value};
use inkwell::{
    AddressSpace,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};

use deen_semantic::symtable::SymbolTable;
use std::collections::HashMap;

mod enumeration;
mod function;
mod macros;
mod scope;
mod structure;
mod variable;

pub struct CodeGen<'ctx> {
    source: String,

    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    scope: Box<Scope<'ctx>>,
    function: Option<FunctionValue<'ctx>>,
    breaks: Vec<BasicBlock<'ctx>>,
    booleans_strings: Option<(PointerValue<'ctx>, PointerValue<'ctx>)>,

    symtable: SymbolTable,
    imports: HashMap<String, ModuleContent<'ctx>>,
    includes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ModuleContent<'ctx> {
    pub functions: HashMap<String, Function<'ctx>>,
    pub structures: HashMap<String, Structure<'ctx>>,
    pub enumerations: HashMap<String, Enumeration<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn create_context() -> Context {
        inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
        inkwell::context::Context::create()
    }

    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        module_source: &str,
        symtable: SymbolTable,
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        module.set_source_file_name(&format!("{module_name}.dn"));
        module.set_triple(&inkwell::targets::TargetMachine::get_default_triple());

        module
            .add_global_metadata(
                "ident",
                &context.metadata_node(&[context
                    .metadata_string(&format!(
                        "deen compiler v{} {}",
                        env!("CARGO_PKG_VERSION"),
                        env!("GIT_HASH").chars().take(8).collect::<String>()
                    ))
                    .into()]),
            )
            .unwrap();

        Self {
            source: module_source.to_owned(),

            context,
            builder,
            module,

            scope: Box::new(Scope::new()),
            function: None,
            breaks: vec![],
            booleans_strings: None,

            symtable,
            imports: HashMap::new(),
            includes: Vec::new(),
        }
    }

    pub fn compile(
        &mut self,
        statements: Vec<Statements>,
        prefix: Option<String>,
    ) -> (&Module<'ctx>, ModuleContent<'ctx>) {
        // let pre_statements = statements
        //     .iter()
        //     .filter(|stmt| match stmt {
        //         Statements::StructDefineStatement {
        //             name: _,
        //             fields: _,
        //             functions: _,
        //             public: _,
        //             span: _,
        //         } => true,
        //         Statements::EnumDefineStatement {
        //             name: _,
        //             fields: _,
        //             functions: _,
        //             public: _,
        //             span: _,
        //         } => true,
        //         Statements::TypedefStatement {
        //             alias: _,
        //             datatype: _,
        //             span: _,
        //         } => true,
        //         Statements::ImportStatement { path: _, span: _ } => true,
        //         Statements::FunctionDefineStatement {
        //             name,
        //             datatype: _,
        //             arguments: _,
        //             block: _,
        //             public: _,
        //             span: _,
        //             header_span: _,
        //         } => name != "main",
        //         _ => false,
        //     })
        //     .collect::<Vec<&Statements>>();
        //
        // let after_statements = statements
        //     .iter()
        //     .filter(|stmt| !pre_statements.contains(stmt));
        //
        // pre_statements
        //     .clone()
        //     .into_iter()
        //     .for_each(|stmt| self.compile_statement(stmt.clone(), prefix.clone()));
        // after_statements
        //     .into_iter()
        //     .for_each(|stmt| self.compile_statement(stmt.clone(), prefix.clone()));

        statements
            .into_iter()
            .for_each(|stmt| self.compile_statement(stmt, prefix.clone()));

        let module_content = {
            let functions = self.scope.stricted_functions();
            let structures = self.scope.stricted_structs();
            let enumerations = self.scope.stricted_enums();

            ModuleContent {
                functions,
                structures,
                enumerations,
            }
        };

        (&self.module, module_content)
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn compile_statement(&mut self, statement: Statements, prefix: Option<String>) {
        match statement {
            Statements::AssignStatement {
                object,
                value,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(identifier), _) = object {
                    let var = self.scope.get_variable(&identifier).unwrap();
                    let compiled_value = self.compile_expression(value, Some(var.datatype));

                    self.builder.build_store(var.ptr, compiled_value.1).unwrap();
                } else {
                    let ptr = self
                        .compile_expression(object, Some(Type::Pointer(Box::new(Type::Undefined))));
                    let value = self.compile_expression(value, None);

                    let _ = self
                        .builder
                        .build_store(ptr.1.into_pointer_value(), value.1);
                }
            }
            Statements::BinaryAssignStatement {
                object,
                operand,
                value,
                span,
            } => {
                let stmt = Statements::AssignStatement {
                    object: object.clone(),
                    value: Expressions::Binary {
                        operand,
                        lhs: Box::new(object),
                        rhs: Box::new(value),
                        span: (0, 0),
                    },
                    span,
                };

                self.compile_statement(stmt, prefix);
            }
            Statements::DerefAssignStatement {
                object,
                value,
                span: _,
            } => {
                let (instance_type, instance_ptr) = self.compile_expression(object.clone(), None);

                match instance_type {
                    Type::Pointer(ptr_type) => {
                        let compiled_value = self.compile_expression(value, Some(*ptr_type));

                        self.builder
                            .build_store(instance_ptr.into_pointer_value(), compiled_value.1)
                            .unwrap();
                    }

                    Type::Alias(alias) => {
                        let instance_ptr = self
                            .compile_expression(
                                object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let deref_assign_fn = struct_type.functions.get("deref_assign").unwrap();

                        let compiled_value = self
                            .compile_expression(value, Some(deref_assign_fn.arguments[1].clone()));
                        self.builder
                            .build_call(
                                deref_assign_fn.value,
                                &[instance_ptr.into(), compiled_value.1.into()],
                                "@deen_deref_assign_call",
                            )
                            .unwrap();
                    }

                    _ => {
                        panic!("Non-pointer type handled: `{instance_type}`")
                    }
                }
            }
            Statements::SliceAssignStatement {
                object,
                index,
                value,
                span,
            } => {
                let obj = self.compile_expression(object.clone(), None);
                let idx = self.compile_expression(index, Some(Type::USIZE));

                match obj.0 {
                    Type::Array(item_type, len) => {
                        let obj_ptr = if obj.1.is_pointer_value() {
                            obj.1.into_pointer_value()
                        } else {
                            let recompiled = self
                                .compile_expression(
                                    object,
                                    Some(Type::Pointer(Box::new(Type::Undefined))),
                                )
                                .1;
                            recompiled.into_pointer_value()
                        };

                        let value = self.compile_expression(value, Some(*item_type.clone()));

                        // checking for the right index
                        let checker_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb"); // idxcb - index checker block
                        let error_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb_err");
                        let ok_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb_ok");

                        self.builder
                            .build_unconditional_branch(checker_block)
                            .unwrap();
                        self.builder.position_at_end(checker_block);

                        let expected_basic_value =
                            self.context.i64_type().const_int(len as u64, false);
                        let provided_basic_value = idx.1.into_int_value();

                        let cmp_value = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                provided_basic_value,
                                expected_basic_value,
                                "",
                            )
                            .unwrap();
                        self.builder
                            .build_conditional_branch(cmp_value, ok_block, error_block)
                            .unwrap();

                        self.builder.position_at_end(error_block);

                        self.build_panic(
                            "Array has len %ld, but index is %ld",
                            vec![expected_basic_value.into(), provided_basic_value.into()],
                            self.get_source_line(span.0),
                        );
                        self.build_branch(ok_block);
                        self.builder.position_at_end(ok_block);

                        // getting ptr

                        let ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    self.get_basic_type(*item_type),
                                    obj_ptr,
                                    &[idx.1.into_int_value()],
                                    "",
                                )
                                .unwrap()
                        };

                        // storing value
                        self.builder.build_store(ptr, value.1).unwrap();
                    }
                    Type::Pointer(ptr_type) => {
                        // compiling value and ptr
                        let value = self.compile_expression(value, Some(*ptr_type.clone()));
                        let ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    self.get_basic_type(*ptr_type),
                                    obj.1.into_pointer_value(),
                                    &[idx.1.into_int_value()],
                                    "",
                                )
                                .unwrap()
                        };

                        // storing value
                        self.builder.build_store(ptr, value.1).unwrap();
                    }

                    Type::Alias(alias) => {
                        let instance_ptr = self
                            .compile_expression(
                                object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let slice_assign_fn = struct_type.functions.get("slice_assign").unwrap();
                        let compiled_value = self
                            .compile_expression(value, Some(slice_assign_fn.arguments[2].clone()));

                        self.builder
                            .build_call(
                                slice_assign_fn.value,
                                &[instance_ptr.into(), idx.1.into(), compiled_value.1.into()],
                                "@deen_slice_assign_call",
                            )
                            .unwrap();
                    }

                    _ => unreachable!(),
                }
            }
            Statements::FieldAssignStatement {
                object,
                value,
                span: _,
            } => {
                let compiled_object =
                    self.compile_expression(object, Some(Type::Pointer(Box::new(Type::Undefined))));
                let compiled_value = self.compile_expression(value, Some(compiled_object.0));

                self.builder
                    .build_store(compiled_object.1.into_pointer_value(), compiled_value.1)
                    .unwrap();
            }

            Statements::AnnotationStatement {
                identifier,
                datatype,
                value,
                span: _,
            } => {
                let empty_binding = identifier == "_";

                match (datatype, value) {
                    (Some(datatype), Some(value)) => {
                        let value = self.compile_expression(value, Some(datatype.clone()));

                        if !empty_binding {
                            let alloca = self
                                .builder
                                .build_alloca(value.1.get_type(), &identifier)
                                .unwrap();

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype,
                                    llvm_type: value.1.get_type(),
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                            let _ = self.builder.build_store(alloca, value.1).unwrap();
                        }
                    }
                    (Some(datatype), _) => {
                        let basic_type = self.get_basic_type(datatype.clone());

                        if !empty_binding {
                            let alloca =
                                self.builder.build_alloca(basic_type, &identifier).unwrap();

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype,
                                    llvm_type: basic_type,
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                        }
                    }
                    (_, Some(value)) => {
                        let compiled_value = self.compile_expression(value, None);

                        if !empty_binding {
                            let alloca = self
                                .builder
                                .build_alloca(compiled_value.1.get_type(), &identifier)
                                .unwrap();

                            self.scope.set_variable(
                                identifier,
                                Variable {
                                    datatype: compiled_value.0,
                                    llvm_type: compiled_value.1.get_type(),
                                    ptr: alloca,
                                    no_drop: false,
                                    global: false,
                                },
                            );
                            let _ = self.builder.build_store(alloca, compiled_value.1).unwrap();
                        }
                    }
                    _ => unreachable!(),
                };
            }

            Statements::FunctionDefineStatement {
                name: raw_name,
                datatype,
                arguments,
                block,
                public: _,
                span: _,
                header_span: _,
            } => {
                let module_name = self.module.get_name().to_str().unwrap();
                let name = format!("{}{}", prefix.clone().unwrap_or_default(), raw_name,);

                let llvm_ir_name = format!(
                    "{}{}{}({})",
                    if module_name == "main" {
                        "".to_owned()
                    } else {
                        format!("{module_name}.")
                    },
                    prefix.clone().unwrap_or_default(),
                    raw_name,
                    arguments
                        .iter()
                        .map(|arg| arg.1.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                arguments.iter().for_each(|arg| {
                    args.push(self.get_basic_type(arg.1.clone()).into());
                });

                let fn_type = self.get_fn_type(datatype.clone(), &args, false);
                let function = self.module.add_function(
                    if name == "main" {
                        "main"
                    } else {
                        &llvm_ir_name
                    },
                    fn_type,
                    None,
                );
                let entry = self.context.append_basic_block(function, "entry");

                let old_position = self.builder.get_insert_block();
                let old_function = self.function;
                self.builder.position_at_end(entry);
                self.function = Some(function);

                self.enter_new_scope();

                arguments.iter().enumerate().for_each(|(index, arg)| {
                    let arg_name = arg.0.clone();
                    let arg_value = function.get_nth_param(index as u32).unwrap();

                    let param_type = self.get_basic_type(arg.1.clone());
                    let param_alloca = self.builder.build_alloca(param_type, "").unwrap();

                    let _ = self.builder.build_store(param_alloca, arg_value);

                    let arg_datatype = match arg.1 {
                        Type::SelfRef => {
                            let prefix = prefix.clone().unwrap();
                            let alias = prefix.replace("struct_", "").replace("enum_", "");
                            let alias = alias.split("__").collect::<Vec<&str>>()[0];

                            Type::Alias(alias.to_owned())
                        }
                        _ => arg.1.clone(),
                    };

                    self.scope.set_variable(
                        arg_name,
                        Variable {
                            datatype: arg_datatype,
                            llvm_type: param_type,
                            ptr: param_alloca,
                            no_drop: false,
                            global: false,
                        },
                    );
                });

                let typed_args = arguments.iter().map(|x| x.1.clone()).collect::<Vec<Type>>();
                self.scope.set_function(
                    name.clone(),
                    Function {
                        datatype: datatype.clone(),
                        value: function,
                        arguments: typed_args.clone(),
                        called: false,
                    },
                );
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));

                if datatype == Type::Void {
                    self.builder.build_return(None).unwrap();
                }

                if !function.verify(false) {
                    let latest_block = function.get_last_basic_block().unwrap();
                    let prev_pos = self.builder.get_insert_block().unwrap();

                    self.builder.position_at_end(latest_block);

                    if let Some(instruction) = latest_block.get_last_instruction() {
                        if !instruction.is_terminator() {
                            self.builder.build_return(None).unwrap();
                        }
                    } else {
                        self.builder.build_return(None).unwrap();
                    }

                    self.builder.position_at_end(prev_pos);
                }

                self.exit_scope();
                if let Some(basic_block) = old_position {
                    self.builder.position_at_end(basic_block);
                }

                self.function = old_function;

                self.scope.set_function(
                    name.clone(),
                    Function {
                        datatype: datatype.clone(),
                        value: function,
                        arguments: typed_args,
                        called: false,
                    },
                );
            }
            Statements::FunctionCallStatement {
                name,
                arguments,
                span: _,
            } => {
                {
                    let mut_fn = self.scope.get_mut_function(&name).unwrap();
                    mut_fn.called = true
                }

                let function = self.scope.get_function(&name).unwrap();
                let mut basic_args: Vec<BasicMetadataValueEnum> = Vec::new();

                let mut function_args = function.arguments.clone();

                if function_args.len() >= 2
                    && function_args[function_args.len() - 1] == Type::Void
                    && function_args[function_args.len() - 2] == Type::Void
                {
                    function_args.resize(arguments.len(), Type::Void);
                }

                arguments
                    .into_iter()
                    .zip(function_args)
                    .for_each(|(expr, expected)| {
                        basic_args.push(self.compile_expression(expr, Some(expected)).1.into());
                    });

                self.builder
                    .build_call(function.value, &basic_args, "")
                    .unwrap();
            }

            Statements::MacroCallStatement {
                name,
                arguments,
                span: _,
            } => {
                self.build_macro_call(&name, arguments);
            }

            Statements::StructDefineStatement {
                name: raw_name,
                fields,
                functions,
                public: _,
                span: _,
            } => {
                let name = format!("{}{}", prefix.clone().unwrap_or_default(), raw_name);
                let llvm_name = format!(
                    "{}.{}{}",
                    self.module.get_name().to_str().unwrap(),
                    prefix.unwrap_or_default(),
                    raw_name
                );

                let struct_type = self.context.opaque_struct_type(&llvm_name);
                let mut compiled_fields = Vec::new();

                fields.iter().for_each(|field| {
                    compiled_fields.push(Field {
                        name: field.0.to_owned(),
                        nth: compiled_fields.len() as u32,
                        datatype: field.1.to_owned(),
                        llvm_type: self.get_basic_type(field.1.to_owned()),
                    });
                });

                let basic_fields_types = compiled_fields
                    .iter()
                    .map(|field| field.llvm_type)
                    .collect::<Vec<BasicTypeEnum>>();
                struct_type.set_body(&basic_fields_types, false);

                let mut fields_hashmap = HashMap::new();
                compiled_fields.into_iter().for_each(|field| {
                    fields_hashmap.insert(field.name.clone(), field);
                });

                self.scope.set_struct(
                    name.clone(),
                    Structure {
                        fields: fields_hashmap,
                        functions: HashMap::new(),
                        llvm_type: struct_type.into(),
                    },
                );

                functions.iter().for_each(|(_, function_statement)| {
                    self.enter_new_scope();

                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("struct_{name}__")),
                    );

                    let (mut function_id, mut function_value) =
                        self.scope.stricted_functions().into_iter().last().unwrap();
                    self.exit_scope_raw();

                    if let Some(Type::SelfRef) = function_value.arguments.first() {
                        *function_value.arguments.first_mut().unwrap() =
                            Type::Pointer(Box::new(Type::Alias(name.clone())));
                    }
                    self.scope
                        .set_function(function_id.clone(), function_value.clone());

                    function_id = function_id.replace(&format!("struct_{name}__"), "");
                    self.scope
                        .get_mut_struct(&name)
                        .unwrap()
                        .functions
                        .insert(function_id, function_value);
                });
            }
            Statements::EnumDefineStatement {
                name,
                fields,
                functions,
                public: _,
                span: _,
            } => {
                let name = format!("{}{}", prefix.unwrap_or_default(), name);
                self.scope.set_enum(
                    name.clone(),
                    Enumeration {
                        fields,
                        functions: HashMap::new(),
                        llvm_type: self.context.i8_type().into(),
                    },
                );

                functions.iter().for_each(|(_, function_statement)| {
                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("enum_{name}__")),
                    );

                    self.enter_new_scope();

                    self.compile_statement(
                        function_statement.to_owned(),
                        Some(format!("enum_{name}__")),
                    );

                    let (mut function_id, function_value) =
                        self.scope.stricted_functions().into_iter().last().unwrap();
                    self.exit_scope_raw();

                    function_id = function_id.replace(&format!("struct_{name}__"), "");
                    self.scope
                        .get_mut_enum(&name)
                        .unwrap()
                        .functions
                        .insert(function_id, function_value);
                });
            }
            Statements::TypedefStatement {
                alias,
                datatype,
                span: _,
            } => {
                self.scope.set_typedef(alias, datatype);
            }

            Statements::IfStatement {
                condition,
                then_block,
                else_block,
                span: _,
            } => {
                let condition = self.compile_expression(condition, None);

                let then_basic_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "__if_then");
                let else_basic_block = if else_block.is_some() {
                    Some(
                        self.context
                            .append_basic_block(self.function.unwrap(), "__if_else"),
                    )
                } else {
                    None
                };
                let after_basic_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "__if_after");

                self.builder
                    .build_conditional_branch(
                        condition.1.into_int_value(),
                        then_basic_block,
                        else_basic_block.unwrap_or(after_basic_block),
                    )
                    .unwrap();
                self.builder.position_at_end(then_basic_block);

                self.enter_new_scope();

                then_block
                    .into_iter()
                    .for_each(|stmt| self.compile_statement(stmt, None));

                self.exit_scope();
                self.build_branch(after_basic_block);

                if let Some(else_basic_block) = else_basic_block {
                    self.enter_new_scope();

                    self.builder.position_at_end(else_basic_block);
                    else_block
                        .unwrap()
                        .into_iter()
                        .for_each(|stmt| self.compile_statement(stmt, None));

                    self.exit_scope();
                    self.build_branch(after_basic_block);
                }

                self.builder.position_at_end(after_basic_block);
            }
            Statements::WhileStatement {
                condition,
                block,
                span: _,
            } => {
                let condition_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "__while_condition");
                let statements_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "__while_block");
                let after_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "__while_after");

                let _ = self
                    .builder
                    .build_unconditional_branch(condition_block)
                    .unwrap();
                self.builder.position_at_end(condition_block);
                self.breaks.push(after_block);

                let compiled_condition = self.compile_expression(condition, None);

                let _ = self.builder.build_conditional_branch(
                    compiled_condition.1.into_int_value(),
                    statements_block,
                    after_block,
                );

                self.enter_new_scope();

                self.builder.position_at_end(statements_block);
                block
                    .into_iter()
                    .for_each(|statement| self.compile_statement(statement, prefix.clone()));

                self.exit_scope();

                let _ = self
                    .builder
                    .build_unconditional_branch(condition_block)
                    .unwrap();
                self.builder.position_at_end(after_block);
                let _ = self.breaks.pop();
            }
            Statements::ForStatement {
                binding,
                iterator,
                block,
                span,
            } => {
                // blocks definition
                let iterator_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "for_iterator");
                let statements_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "for_block");
                let after_block = self
                    .context
                    .append_basic_block(self.function.unwrap(), "for_after");

                // binding initialization

                let mut compiled_iterator = self
                    .compile_expression(iterator, Some(Type::Pointer(Box::new(Type::Undefined))));

                if let Type::Pointer(ptr_type) = compiled_iterator.0 {
                    compiled_iterator.0 = *ptr_type.clone();

                    if let Type::Array(_, _) = *ptr_type {
                        compiled_iterator.1 = self
                            .builder
                            .build_load(
                                self.context.ptr_type(AddressSpace::default()),
                                compiled_iterator.1.into_pointer_value(),
                                "",
                            )
                            .unwrap();
                    }
                }

                let binding_type = match compiled_iterator.0.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => typ,

                    Type::Array(typ, _) => *typ,
                    Type::DynamicArray(typ) => *typ,
                    Type::Alias(alias) => {
                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        if let Type::Tuple(types) = iterator_fn.datatype {
                            types[0].clone()
                        } else {
                            unreachable!()
                        }
                    }

                    _ => unreachable!(),
                };

                let basic_binding_type = self.get_basic_type(binding_type.clone());
                let binding_ptr = self
                    .builder
                    .build_alloca(basic_binding_type, &binding)
                    .unwrap();

                self.enter_new_scope();

                self.scope.set_variable(
                    binding.clone(),
                    Variable {
                        datatype: binding_type,
                        llvm_type: basic_binding_type,
                        ptr: binding_ptr,
                        no_drop: false,
                        global: false,
                    },
                );
                self.breaks.push(after_block);

                match compiled_iterator.0.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        // runtime checker for negative number
                        if !deen_semantic::Analyzer::is_unsigned_integer(&typ)
                            && compiled_iterator
                                .1
                                .into_int_value()
                                .get_sign_extended_constant()
                                .unwrap_or(-1)
                                < 0
                        {
                            let checker_block = self.context.insert_basic_block_after(
                                self.builder.get_insert_block().unwrap(),
                                "int_checker",
                            );
                            let err_block = self
                                .context
                                .insert_basic_block_after(checker_block, "int_integer_panic");
                            let ok_block = self
                                .context
                                .insert_basic_block_after(err_block, "int_checker_ok");

                            self.builder
                                .build_unconditional_branch(checker_block)
                                .unwrap();
                            self.builder.position_at_end(checker_block);

                            let zero = self.context.i64_type().const_zero();
                            let cmp = self
                                .builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SGE,
                                    compiled_iterator.1.into_int_value(),
                                    zero,
                                    "",
                                )
                                .unwrap();

                            self.builder
                                .build_conditional_branch(cmp, ok_block, err_block)
                                .unwrap();
                            self.builder.position_at_end(err_block);

                            let specifier = self.type_specifier(&typ);
                            self.build_panic(
                                format!("Loop `for` handled negative number: {specifier}"),
                                vec![compiled_iterator.1.into()],
                                self.get_source_line(span.0),
                            );

                            self.builder.position_at_end(ok_block);
                        }

                        // initial binding value

                        self.builder
                            .build_store(binding_ptr, self.get_basic_type(typ.clone()).const_zero())
                            .unwrap();

                        // condition block

                        let _ = self
                            .builder
                            .build_unconditional_branch(iterator_block)
                            .unwrap();
                        self.builder.position_at_end(iterator_block);

                        let binding_value = self
                            .builder
                            .build_load(self.get_basic_type(typ), binding_ptr, "")
                            .unwrap();
                        let iter_cmp = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                binding_value.into_int_value(),
                                compiled_iterator.1.into_int_value(),
                                "",
                            )
                            .unwrap();

                        self.builder
                            .build_conditional_branch(iter_cmp, statements_block, after_block)
                            .unwrap();
                    }

                    Type::Alias(alias) => {
                        // allocating iterator status
                        let status_varname = format!("@deen_iterator_status_{}", &binding);

                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        let iter_status_alloca = self
                            .builder
                            .build_alloca(self.context.bool_type(), &status_varname)
                            .unwrap();

                        self.scope.set_variable(
                            status_varname,
                            Variable {
                                datatype: Type::Bool,
                                llvm_type: self.context.bool_type().into(),
                                ptr: iter_status_alloca,
                                no_drop: false,
                                global: false,
                            },
                        );

                        let iterator_struct: BasicMetadataValueEnum =
                            if compiled_iterator.1.is_pointer_value() {
                                compiled_iterator.1.into()
                            } else {
                                let alloca = self
                                    .builder
                                    .build_alloca(compiled_iterator.1.get_type(), "")
                                    .unwrap();
                                let _ = self
                                    .builder
                                    .build_store(alloca, compiled_iterator.1)
                                    .unwrap();
                                alloca.into()
                            };

                        // calling iterator function

                        let output = self
                            .builder
                            .build_call(iterator_fn.value, &[iterator_struct], "")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // allocating result

                        let tuple_alloca =
                            self.builder.build_alloca(output.get_type(), "").unwrap();
                        self.builder.build_store(tuple_alloca, output).unwrap();

                        // storing iterator status

                        let status_field_ptr = self
                            .builder
                            .build_struct_gep(output.get_type(), tuple_alloca, 1, "")
                            .unwrap();
                        let status = self
                            .builder
                            .build_load(self.context.bool_type(), status_field_ptr, "")
                            .unwrap();

                        self.builder
                            .build_store(iter_status_alloca, status)
                            .unwrap();

                        // storing binding value

                        let binding_field_ptr = self
                            .builder
                            .build_struct_gep(output.get_type(), tuple_alloca, 0, "")
                            .unwrap();
                        let iter_value = self
                            .builder
                            .build_load(basic_binding_type, binding_field_ptr, "")
                            .unwrap();

                        self.builder.build_store(binding_ptr, iter_value).unwrap();

                        // condition block

                        let _ = self
                            .builder
                            .build_unconditional_branch(iterator_block)
                            .unwrap();
                        self.builder.position_at_end(iterator_block);

                        let status = self
                            .builder
                            .build_load(self.context.bool_type(), iter_status_alloca, "")
                            .unwrap();
                        self.builder
                            .build_conditional_branch(
                                status.into_int_value(),
                                statements_block,
                                after_block,
                            )
                            .unwrap();
                    }
                    Type::Array(arrtype, len) => {
                        // allocating iterator position
                        let iterator_position_varname =
                            format!("@deen_iterator_position_{}", &binding);
                        let iterator_position_alloca = self
                            .builder
                            .build_alloca(self.context.i64_type(), &iterator_position_varname)
                            .unwrap();

                        self.scope.set_variable(
                            iterator_position_varname,
                            Variable {
                                datatype: Type::USIZE,
                                llvm_type: self.context.i64_type().into(),
                                ptr: iterator_position_alloca,
                                no_drop: false,
                                global: false,
                            },
                        );

                        // assigning first element

                        let basic_type = self.get_basic_type(*arrtype);
                        let ptr = unsafe {
                            self.builder
                                .build_gep(
                                    basic_type,
                                    compiled_iterator.1.into_pointer_value(),
                                    &[self.context.i64_type().const_zero()],
                                    "",
                                )
                                .unwrap()
                        };

                        let value = self.builder.build_load(basic_type, ptr, "").unwrap();
                        self.builder.build_store(binding_ptr, value).unwrap();

                        // condition block

                        let _ = self
                            .builder
                            .build_unconditional_branch(iterator_block)
                            .unwrap();
                        self.builder.position_at_end(iterator_block);

                        let iterator_position = self
                            .builder
                            .build_load(self.context.i64_type(), iterator_position_alloca, "")
                            .unwrap();
                        let array_len_value = self.context.i64_type().const_int(len as u64, false);

                        let cmp = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::ULT,
                                iterator_position.into_int_value(),
                                array_len_value,
                                "",
                            )
                            .unwrap();

                        self.builder
                            .build_conditional_branch(cmp, statements_block, after_block)
                            .unwrap();
                    }
                    Type::DynamicArray(_) => {}

                    _ => unreachable!(),
                }

                // statements block

                self.enter_new_scope();

                self.builder.position_at_end(statements_block);
                block
                    .into_iter()
                    .for_each(|statement| self.compile_statement(statement, prefix.clone()));

                self.exit_scope();

                // making iteration

                match compiled_iterator.0 {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        let current_value = self
                            .builder
                            .build_load(basic_binding_type, binding_ptr, "itertmp")
                            .unwrap();
                        let incremented_value = self
                            .builder
                            .build_int_add(
                                current_value.into_int_value(),
                                basic_binding_type.into_int_type().const_int(1, false),
                                "iterinc",
                            )
                            .unwrap();

                        self.builder
                            .build_store(binding_ptr, incremented_value)
                            .unwrap();
                    }

                    Type::Alias(alias) => {
                        let status_varname = format!("@deen_iterator_status_{}", &binding);
                        let status_alloca = self.scope.get_variable(status_varname).unwrap().ptr;

                        let iterator_fn = self
                            .scope
                            .get_function(format!("struct_{}__{}", alias, "iterate"))
                            .unwrap();
                        let iterator_struct: BasicMetadataValueEnum =
                            if compiled_iterator.1.is_pointer_value() {
                                compiled_iterator.1.into()
                            } else {
                                let alloca = self
                                    .builder
                                    .build_alloca(compiled_iterator.1.get_type(), "")
                                    .unwrap();
                                let _ = self
                                    .builder
                                    .build_store(alloca, compiled_iterator.1)
                                    .unwrap();
                                alloca.into()
                            };

                        // calling iterator function

                        let output = self
                            .builder
                            .build_call(iterator_fn.value, &[iterator_struct], "")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // allocating result

                        let tuple_alloca =
                            self.builder.build_alloca(output.get_type(), "").unwrap();
                        self.builder.build_store(tuple_alloca, output).unwrap();

                        // storing iterator status

                        let status_field_ptr = self
                            .builder
                            .build_struct_gep(output.get_type(), tuple_alloca, 1, "")
                            .unwrap();
                        let status = self
                            .builder
                            .build_load(self.context.bool_type(), status_field_ptr, "")
                            .unwrap();

                        self.builder.build_store(status_alloca, status).unwrap();

                        // storing binding value

                        let binding_field_ptr = self
                            .builder
                            .build_struct_gep(output.get_type(), tuple_alloca, 0, "")
                            .unwrap();
                        let iter_value = self
                            .builder
                            .build_load(basic_binding_type, binding_field_ptr, "")
                            .unwrap();

                        self.builder.build_store(binding_ptr, iter_value).unwrap();
                    }
                    Type::Array(arrtype, _) => {
                        // incrementing iterator position
                        let iterator_position_varname =
                            format!("@deen_iterator_position_{}", &binding);
                        let iterator_position_ptr = self
                            .scope
                            .get_variable(iterator_position_varname)
                            .unwrap()
                            .ptr;

                        let value = self
                            .builder
                            .build_load(self.context.i64_type(), iterator_position_ptr, "")
                            .unwrap();
                        let incremented_value = self
                            .builder
                            .build_int_add(
                                value.into_int_value(),
                                self.context.i64_type().const_int(1, false),
                                "",
                            )
                            .unwrap();

                        self.builder
                            .build_store(iterator_position_ptr, incremented_value)
                            .unwrap();

                        // storing value to the binding

                        let basic_type = self.get_basic_type(*arrtype);
                        let ptr = unsafe {
                            self.builder
                                .build_gep(
                                    basic_type,
                                    compiled_iterator.1.into_pointer_value(),
                                    &[incremented_value],
                                    "",
                                )
                                .unwrap()
                        };

                        let value = self.builder.build_load(basic_type, ptr, "").unwrap();
                        self.builder.build_store(binding_ptr, value).unwrap();
                    }
                    Type::DynamicArray(_) => {}

                    _ => unreachable!(),
                }

                self.build_branch(iterator_block);

                // exit

                self.builder.position_at_end(after_block);
                let _ = self.breaks.pop();

                self.exit_scope();
            }

            Statements::BreakStatements { span: _ } => {
                let break_block = self.breaks.last().cloned().unwrap();

                let _ = self
                    .builder
                    .build_unconditional_branch(break_block)
                    .unwrap();
            }
            Statements::ReturnStatement { value, span: _ } => {
                let compiled_value = self.compile_expression(value, Some(Type::NoDrop));
                if compiled_value.0 != Type::Void {
                    self.builder.build_return(Some(&compiled_value.1)).unwrap();
                }
            }

            Statements::LinkCStatement { path: _, span: _ } => {}

            Statements::ExternDeclareStatement {
                identifier,
                datatype,
                span: _,
            } => {
                let basic_type = self.get_basic_type(datatype.clone());
                let global = self.module.add_global(basic_type, None, &identifier);
                global.set_linkage(Linkage::External);
                global.set_alignment(8);

                self.scope.set_variable(
                    identifier,
                    Variable {
                        datatype,
                        llvm_type: basic_type,
                        ptr: self.context.ptr_type(AddressSpace::default()).const_null(),
                        no_drop: true,
                        global: true,
                    },
                );
            }

            Statements::ExternStatement {
                identifier,
                arguments,
                return_type,
                extern_type: _,
                is_var_args,
                public: _,
                span: _,
            } => {
                if self.scope.get_function(&identifier).is_some() {
                    return;
                }

                let basic_arguments = arguments
                    .iter()
                    .map(|arg| self.get_basic_type(arg.clone()).into())
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                let fn_type = self.get_fn_type(return_type.clone(), &basic_arguments, is_var_args);
                let fn_value = self.module.get_function(&identifier).unwrap_or_else(|| {
                    self.module
                        .add_function(&identifier, fn_type, Some(Linkage::External))
                });

                // little hack cuz i dont wanna add `is_var_args` argument to function structure
                // and fix the whole code for the only one usage

                let mut arguments = arguments;
                arguments.push(Type::Void);
                arguments.push(Type::Void);

                // so if we have 2 void arguments at the end (which is impossible in basic code) - this is var args

                self.scope.set_function(
                    identifier,
                    Function {
                        datatype: return_type,
                        value: fn_value,
                        arguments,
                        called: false,
                    },
                )
            }

            Statements::ImportStatement { path, span: _ } => {
                let path = if let Expressions::Value(Value::String(path), _) = path {
                    path
                } else {
                    String::default()
                };
                let fname = std::path::Path::new(&path)
                    .file_name()
                    .map(|fname| fname.to_str().unwrap_or("$NONE"))
                    .unwrap();

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                let import = self
                    .symtable
                    .imports
                    .get(&module_name)
                    .cloned()
                    .unwrap_or_default();
                let mut codegen = Self::new(
                    self.context,
                    &module_name,
                    &import.source,
                    import.embedded_symtable.clone(),
                );

                let (_module, mut module_content) = codegen.compile(import.ast.clone(), None);

                module_content.functions.iter_mut().for_each(|func| {
                    let args_fmt = func
                        .1
                        .arguments
                        .iter()
                        .map(|typ| typ.to_string())
                        .collect::<Vec<String>>();
                    let args = func
                        .1
                        .arguments
                        .iter()
                        .map(|typ| self.get_basic_type(typ.clone()).into())
                        .collect::<Vec<BasicMetadataTypeEnum<'ctx>>>();
                    let fn_type = self.get_fn_type(func.1.datatype.clone(), &args, false);

                    let declared_fn = self.module.add_function(
                        &format!("{}.{}({})", &module_name, func.0, args_fmt.join(", ")),
                        fn_type,
                        Some(Linkage::External),
                    );
                    func.1.value = declared_fn;
                });

                self.imports.insert(module_name, module_content);
            }

            Statements::IncludeStatement { path, span: _ } => {
                let path = if let Expressions::Value(Value::String(path), _) = path {
                    path.replace("@", "")
                } else {
                    String::default()
                };

                let fname = std::path::Path::new(&path)
                    .file_name()
                    .map(|fname| fname.to_str().unwrap_or("$NONE"))
                    .unwrap();

                let module_name = fname
                    .split(".")
                    .nth(0)
                    .map(|n| n.to_string())
                    .unwrap_or(fname.replace(".dn", ""));

                if self.includes.contains(&module_name) {
                    return;
                };
                self.includes.push(module_name.clone());

                let include = self.symtable.included.get(&module_name).unwrap().clone();
                include
                    .ast
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));
            }

            Statements::ScopeStatement { block, span: _ } => {
                self.enter_new_scope();
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.clone(), None));

                self.exit_scope();
            }

            Statements::Expression(expr) => {
                let _ = self.compile_expression(expr, None);
            }
            Statements::None => unreachable!(),
        }
    }

    fn compile_expression(
        &mut self,
        expression: Expressions,
        expected: Option<Type>,
    ) -> (Type, BasicValueEnum<'ctx>) {
        match expression {
            Expressions::Value(val, _) => self.compile_value(val, expected),
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                {
                    let mut_fn = self.scope.get_mut_function(&name).unwrap();
                    mut_fn.called = true
                }

                let function = self.scope.get_function(&name).unwrap();
                let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                arguments
                    .iter()
                    .zip(function.arguments)
                    .for_each(|(arg, fn_expected)| {
                        args.push(
                            self.compile_expression(arg.clone(), Some(fn_expected))
                                .1
                                .into(),
                        )
                    });

                let mut return_type = function.datatype;

                if let Type::Pointer(ref return_ptr_type) = return_type {
                    if let (Some(Type::Pointer(expected_ptr_type)), true) =
                        (expected.clone(), **return_ptr_type == Type::Void)
                    {
                        return_type = Type::Pointer(expected_ptr_type);
                    }
                }

                if deen_semantic::Analyzer::is_integer(&return_type)
                    && (deen_semantic::Analyzer::is_integer(
                        expected.as_ref().unwrap_or(&Type::Void),
                    ) || matches!(expected.as_ref().unwrap_or(&Type::Void), Type::Char))
                    && deen_semantic::Analyzer::integer_order(expected.as_ref().unwrap())
                        <= deen_semantic::Analyzer::integer_order(&return_type)
                {
                    return_type = expected.unwrap();
                }

                (
                    return_type,
                    self.builder
                        .build_call(function.value, &args, "")
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                        .unwrap(),
                )
            }

            Expressions::MacroCall {
                name,
                arguments,
                span: _,
            } => self.build_macro_call(&name, arguments),

            Expressions::Reference { object, span: _ } => match *object {
                Expressions::Value(Value::Identifier(id), _) => {
                    let var = self.scope.get_variable(&id).unwrap();

                    (
                        Type::Pointer(Box::new(var.datatype.clone())),
                        var.ptr.into(),
                    )
                }
                Expressions::Slice {
                    object: _,
                    index: _,
                    span: _,
                } => {
                    self.compile_expression(*object, Some(Type::Pointer(Box::new(Type::Undefined))))
                }
                Expressions::SubElement {
                    head: _,
                    subelements: _,
                    span: _,
                } => {
                    let (result_type, ptr) = self.compile_expression(
                        *object,
                        Some(Type::Pointer(Box::new(Type::Undefined))),
                    );
                    (Type::Pointer(Box::new(result_type)), ptr)
                }
                _ => {
                    let value = self.compile_expression(
                        *object,
                        Some(Type::Pointer(Box::new(Type::Undefined))),
                    );
                    let alloca = self.builder.build_alloca(value.1.get_type(), "").unwrap();
                    let _ = self.builder.build_store(alloca, value.1);

                    (Type::Pointer(Box::new(value.0)), alloca.into())
                }
            },
            Expressions::Dereference { object, span: _ } => {
                let (datatype, ptr) = self.compile_expression(*object.clone(), None);

                match datatype {
                    Type::Pointer(ptr_type) => {
                        let basic_type = self.get_basic_type(*ptr_type.clone());
                        let value = self
                            .builder
                            .build_load(basic_type, ptr.into_pointer_value(), "")
                            .unwrap();

                        (*ptr_type, value)
                    }

                    Type::Alias(alias) => {
                        let ptr = self
                            .compile_expression(
                                *object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let deref_fn = struct_type.functions.get("deref").unwrap();

                        // calling deref struct

                        let call_result = self
                            .builder
                            .build_call(deref_fn.value, &[ptr.into()], "@deen_deref_call")
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left();

                        (deref_fn.datatype.clone(), call_result)
                    }

                    _ => {
                        panic!(
                            "Semantical Analyzer missed dereference expression bug:\n- `{datatype}`:\n{ptr:#?}"
                        )
                    }
                }
            }

            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                let mut object_value = self.compile_expression(*object, expected);

                if let Type::Pointer(ref ptr_type) = object_value.0
                    && let Type::Alias(_) = *ptr_type.clone()
                {
                    object_value.0 = *ptr_type.clone();
                }

                if let Type::Alias(alias) = &object_value.0 {
                    let structure = self.scope.get_struct(alias).unwrap();
                    let unary_function = structure.functions.get("unary").unwrap();

                    let object_ptr = if object_value.1.is_pointer_value() {
                        object_value.1.into_pointer_value()
                    } else {
                        let alloca = self
                            .builder
                            .build_alloca(object_value.1.get_type(), "")
                            .unwrap();
                        self.builder.build_store(alloca, object_value.1).unwrap();

                        alloca
                    };

                    let object_ptr: BasicMetadataValueEnum = object_ptr.into();
                    let operand: BasicMetadataValueEnum = self
                        .builder
                        .build_global_string_ptr(&operand, "@deen_operand")
                        .unwrap()
                        .as_basic_value_enum()
                        .into();

                    let function_output = self
                        .builder
                        .build_call(
                            unary_function.value,
                            &[object_ptr, operand],
                            "@deen_unop_call",
                        )
                        .unwrap()
                        .try_as_basic_value()
                        .left()
                        .unwrap();

                    return (unary_function.datatype.clone(), function_output);
                }

                match operand.as_str() {
                    "-" => match object_value.0 {
                        Type::I8
                        | Type::I16
                        | Type::I32
                        | Type::I64
                        | Type::U8
                        | Type::U16
                        | Type::U32
                        | Type::U64
                        | Type::USIZE => (
                            deen_semantic::Analyzer::unsigned_to_signed_integer(&object_value.0),
                            self.builder
                                .build_int_neg(object_value.1.into_int_value(), "")
                                .unwrap()
                                .into(),
                        ),

                        Type::F32 | Type::F64 => (
                            object_value.0,
                            self.builder
                                .build_float_neg(object_value.1.into_float_value(), "")
                                .unwrap()
                                .into(),
                        ),

                        _ => unreachable!(),
                    },

                    "!" => (
                        object_value.0,
                        self.builder
                            .build_not(object_value.1.into_int_value(), "")
                            .unwrap()
                            .into(),
                    ),

                    _ => unreachable!(),
                }
            }
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let expected = expected.and_then(|typ| {
                    if let Type::Pointer(_) = typ {
                        None
                    } else {
                        Some(typ)
                    }
                });

                let lhs_value = self.compile_expression(*lhs.clone(), expected.clone());
                let rhs_value = self.compile_expression(*rhs.clone(), expected.clone());

                let senior_type = match lhs_value.0.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => {
                        if deen_semantic::Analyzer::integer_order(&lhs_value.0)
                            > deen_semantic::Analyzer::integer_order(&rhs_value.0)
                        {
                            lhs_value.0
                        } else {
                            rhs_value.0.clone()
                        }
                    }
                    typ if deen_semantic::Analyzer::is_float(&typ) => {
                        if deen_semantic::Analyzer::float_order(&lhs_value.0)
                            > deen_semantic::Analyzer::float_order(&rhs_value.0)
                        {
                            lhs_value.0
                        } else {
                            rhs_value.0.clone()
                        }
                    }

                    typ if matches!((&typ, &rhs_value.0), (Type::Pointer(_), Type::Pointer(_))) => {
                        Type::USIZE
                    }
                    Type::Pointer(_) => lhs_value.0,

                    Type::Alias(alias) => Type::Alias(alias),

                    _ => panic!("Unreachable type found: {}", lhs_value.0.clone()),
                };

                let output = match senior_type.clone() {
                    typ if deen_semantic::Analyzer::is_integer(&typ) => match operand.as_str() {
                        "+" => {
                            if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                self.builder
                                    .build_int_nsw_add(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            } else {
                                self.builder
                                    .build_int_add(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }
                        "-" => {
                            if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                self.builder
                                    .build_int_nsw_sub(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            } else {
                                self.builder
                                    .build_int_sub(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }
                        "*" => {
                            if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                self.builder
                                    .build_int_nsw_mul(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            } else {
                                self.builder
                                    .build_int_mul(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }
                        "/" => {
                            if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                self.builder
                                    .build_int_unsigned_div(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            } else {
                                self.builder
                                    .build_int_signed_div(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }
                        "%" => {
                            if deen_semantic::Analyzer::is_unsigned_integer(&typ) {
                                self.builder
                                    .build_int_unsigned_rem(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            } else {
                                self.builder
                                    .build_int_signed_rem(
                                        lhs_value.1.into_int_value(),
                                        rhs_value.1.into_int_value(),
                                        "",
                                    )
                                    .unwrap()
                                    .as_basic_value_enum()
                            }
                        }

                        _ => panic!(
                            "Unsupported for codegen operator found! Please open issue on Github!"
                        ),
                    },
                    typ if deen_semantic::Analyzer::is_float(&typ) => match operand.as_str() {
                        "+" => self
                            .builder
                            .build_float_add(
                                lhs_value.1.into_float_value(),
                                rhs_value.1.into_float_value(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                        "-" => self
                            .builder
                            .build_float_sub(
                                lhs_value.1.into_float_value(),
                                rhs_value.1.into_float_value(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                        "*" => self
                            .builder
                            .build_float_mul(
                                lhs_value.1.into_float_value(),
                                rhs_value.1.into_float_value(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                        "/" => self
                            .builder
                            .build_float_div(
                                lhs_value.1.into_float_value(),
                                rhs_value.1.into_float_value(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),

                        _ => unreachable!(),
                    },

                    Type::Pointer(ptr_type) => {
                        if matches!(rhs_value.0, Type::Pointer(_)) {
                            let lhs_int = self
                                .builder
                                .build_ptr_to_int(
                                    lhs_value.1.into_pointer_value(),
                                    self.context.i64_type(),
                                    "",
                                )
                                .unwrap();
                            let rhs_int = self
                                .builder
                                .build_ptr_to_int(
                                    rhs_value.1.into_pointer_value(),
                                    self.context.i64_type(),
                                    "",
                                )
                                .unwrap();

                            let mut value = match operand.as_str() {
                                "+" => self.builder.build_int_add(lhs_int, rhs_int, "").unwrap(),
                                "-" => self.builder.build_int_sub(lhs_int, rhs_int, "").unwrap(),
                                _ => unreachable!(),
                            }
                            .as_basic_value_enum();

                            if matches!(expected, Some(Type::Pointer(_))) {
                                value = self
                                    .builder
                                    .build_int_to_ptr(
                                        value.into_int_value(),
                                        lhs_value.1.get_type().into_pointer_type(),
                                        "",
                                    )
                                    .unwrap()
                                    .into();
                            }

                            value
                        } else {
                            unsafe {
                                self.builder
                                    .build_in_bounds_gep(
                                        self.get_basic_type(*ptr_type.clone()),
                                        lhs_value.1.into_pointer_value(),
                                        &[rhs_value.1.into_int_value()],
                                        "",
                                    )
                                    .unwrap()
                            }
                            .as_basic_value_enum()
                        }
                    }

                    Type::Alias(alias) => {
                        // let exp = Some(Type::Pointer(Box::new(Type::Undefined)));
                        // let lhs_value = self.compile_expression(*lhs, exp.clone());
                        // let rhs_value = self.compile_expression(*rhs, exp);

                        let structure = self.scope.get_struct(alias).unwrap();
                        let binary_function = structure.functions.get("binary").unwrap();

                        let left_ptr = self
                            .compile_expression(
                                *lhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;
                        let right_ptr = self
                            .compile_expression(
                                *rhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let left_ptr: BasicMetadataValueEnum = left_ptr.into();
                        let right_ptr: BasicMetadataValueEnum = right_ptr.into();
                        let operand: BasicMetadataValueEnum = self
                            .builder
                            .build_global_string_ptr(&operand, "@deen_operand")
                            .unwrap()
                            .as_basic_value_enum()
                            .into();

                        self.builder
                            .build_call(
                                binary_function.value,
                                &[left_ptr, right_ptr, operand],
                                "@deen_binop_call",
                            )
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }

                    _ => unreachable!(),
                };

                (senior_type, output)
            }
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let mut lhs_value = self.compile_expression(*lhs.clone(), expected.clone());
                let mut rhs_value =
                    self.compile_expression(*rhs.clone(), Some(lhs_value.0.clone()));

                if let Type::Alias(left) = &lhs_value.0
                    && let Type::Alias(right) = &rhs_value.0
                    && self.scope.get_enum(left).is_some()
                    && self.scope.get_enum(right).is_some()
                {
                    lhs_value.0 = Type::U8;
                    rhs_value.0 = Type::U8;
                }

                match operand.as_str() {
                    "&&" => {
                        return (
                            Type::Bool,
                            self.builder
                                .build_and(
                                    lhs_value.1.into_int_value(),
                                    rhs_value.1.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                    "||" => {
                        return (
                            Type::Bool,
                            self.builder
                                .build_or(
                                    lhs_value.1.into_int_value(),
                                    rhs_value.1.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                    _ => {}
                }

                match lhs_value.0 {
                    typ if typ == Type::Null || rhs_value.0 == Type::Null => {
                        let leading_value = if typ == Type::Null {
                            rhs_value.1
                        } else {
                            lhs_value.1
                        };
                        let result_value = if operand == "==" {
                            self.builder
                                .build_is_null(leading_value.into_pointer_value(), "")
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_is_not_null(leading_value.into_pointer_value(), "")
                                .unwrap()
                                .into()
                        };

                        (Type::Bool, result_value)
                    }

                    typ if deen_semantic::Analyzer::is_integer(&typ) || typ == Type::Char => {
                        let predicate = match operand.as_str() {
                            ">" => inkwell::IntPredicate::SGT,
                            "<" => inkwell::IntPredicate::SLT,
                            "<=" | "=<" => inkwell::IntPredicate::SLE,
                            ">=" | "=>" => inkwell::IntPredicate::SGE,
                            "==" => inkwell::IntPredicate::EQ,
                            "!=" => inkwell::IntPredicate::NE,
                            _ => unreachable!(),
                        };

                        (
                            Type::Bool,
                            self.builder
                                .build_int_compare(
                                    predicate,
                                    lhs_value.1.into_int_value(),
                                    rhs_value.1.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
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
                            _ => unreachable!(),
                        };

                        (
                            Type::Bool,
                            self.builder
                                .build_float_compare(
                                    predicate,
                                    lhs_value.1.into_float_value(),
                                    rhs_value.1.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        )
                    }
                    Type::Pointer(ptr_type) if *ptr_type == Type::Char => {
                        let strcmp_fn = self.module.get_function("strcmp").unwrap_or_else(|| {
                            let fn_type = self.context.i32_type().fn_type(
                                &[
                                    self.context.ptr_type(AddressSpace::default()).into(),
                                    self.context.ptr_type(AddressSpace::default()).into(),
                                ],
                                false,
                            );

                            self.module.add_function("strcmp", fn_type, None)
                        });

                        let cmp_value = self
                            .builder
                            .build_call(strcmp_fn, &[lhs_value.1.into(), rhs_value.1.into()], "")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        let int_predicate = match operand.as_str() {
                            ">" => inkwell::IntPredicate::SGT,
                            "<" => inkwell::IntPredicate::SLT,
                            "<=" | "=<" => inkwell::IntPredicate::SLE,
                            ">=" | "=>" => inkwell::IntPredicate::SGE,
                            "==" => inkwell::IntPredicate::EQ,
                            "!=" => inkwell::IntPredicate::NE,
                            _ => unreachable!(),
                        };

                        (
                            Type::Bool,
                            self.builder
                                .build_int_compare(
                                    int_predicate,
                                    cmp_value.into_int_value(),
                                    self.context.i32_type().const_zero(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        )
                    }

                    Type::Alias(alias) => {
                        // calling compare function
                        let structure = self.scope.get_struct(alias).unwrap();
                        let compare_function = structure.functions.get("compare").unwrap();

                        let left_ptr = self
                            .compile_expression(
                                *lhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;
                        let right_ptr = self
                            .compile_expression(
                                *rhs.clone(),
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let left_ptr: BasicMetadataValueEnum = left_ptr.into();
                        let right_ptr: BasicMetadataValueEnum = right_ptr.into();

                        let function_output = self
                            .builder
                            .build_call(
                                compare_function.value,
                                &[left_ptr, right_ptr],
                                "@deen_cmp_call",
                            )
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap();

                        // comparing

                        let int_predicate = match operand.as_str() {
                            ">" => inkwell::IntPredicate::SGT,
                            "<" => inkwell::IntPredicate::SLT,
                            "<=" | "=<" => inkwell::IntPredicate::SLE,
                            ">=" | "=>" => inkwell::IntPredicate::SGE,
                            "==" => inkwell::IntPredicate::EQ,
                            "!=" => inkwell::IntPredicate::NE,
                            _ => unreachable!(),
                        };

                        (
                            Type::Bool,
                            self.builder
                                .build_int_compare(
                                    int_predicate,
                                    self.context.i32_type().const_zero(),
                                    function_output.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        )
                    }

                    Type::Bool => (
                        Type::Bool,
                        self.builder
                            .build_and(
                                lhs_value.1.into_int_value(),
                                rhs_value.1.into_int_value(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                    ),

                    _ => panic!("Boolean catched: {} ? {}", lhs_value.0, rhs_value.0),
                }
            }
            Expressions::Bitwise {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                let left = self.compile_expression(*lhs, expected.clone());
                let right = self.compile_expression(*rhs, expected.clone());

                let sign_extend = deen_semantic::Analyzer::is_unsigned_integer(&left.0);
                let basic_value = match operand.as_str() {
                    "<<" => self
                        .builder
                        .build_left_shift(left.1.into_int_value(), right.1.into_int_value(), "")
                        .unwrap()
                        .as_basic_value_enum(),
                    ">>" => self
                        .builder
                        .build_right_shift(
                            left.1.into_int_value(),
                            right.1.into_int_value(),
                            sign_extend,
                            "",
                        )
                        .unwrap()
                        .as_basic_value_enum(),
                    "&" => self
                        .builder
                        .build_and(left.1.into_int_value(), right.1.into_int_value(), "")
                        .unwrap()
                        .as_basic_value_enum(),
                    "|" => self
                        .builder
                        .build_or(left.1.into_int_value(), right.1.into_int_value(), "")
                        .unwrap()
                        .as_basic_value_enum(),
                    "^" => self
                        .builder
                        .build_xor(left.1.into_int_value(), right.1.into_int_value(), "")
                        .unwrap()
                        .as_basic_value_enum(),

                    _ => unreachable!(),
                };

                (left.0, basic_value)
            }

            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {
                let compiled_head =
                    self.compile_expression(*head, Some(Type::Pointer(Box::new(Type::Undefined))));

                let mut prev_val = compiled_head.1;
                let mut prev_type = compiled_head.0;

                // I know it looks kinda awful and terrible, but it works.
                // There's no way you can get double pointer to a struct in sub-element, so i just
                // placed self pointers into this shit.
                if let Type::Pointer(ptr_type) = prev_type.clone() {
                    if let Type::Pointer(ptr_type) = *ptr_type.clone() {
                        prev_type = *ptr_type;
                        prev_val = self
                            .builder
                            .build_load(
                                self.context.ptr_type(AddressSpace::default()),
                                prev_val.into_pointer_value(),
                                "",
                            )
                            .unwrap();
                    }
                }

                subelements.iter().for_each(|sub| match sub {
                    Expressions::Value(Value::Identifier(field), _) => {
                        if let Type::Pointer(ptr_type) = prev_type.clone() {
                            prev_type = *ptr_type;
                        }

                        if let Type::Alias(alias) = prev_type.clone() {
                            let alias_type = self.get_alias_type(prev_type.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let structure = self.scope.get_struct(&alias).unwrap();
                                    let field = structure.fields.get(field).unwrap();

                                    let ptr = self
                                        .builder
                                        .build_struct_gep(
                                            structure.llvm_type,
                                            prev_val.into_pointer_value(),
                                            field.nth,
                                            "",
                                        )
                                        .unwrap();

                                    let value = if let Some(Type::Pointer(ptr_type)) =
                                        expected.clone()
                                    {
                                        if *ptr_type == Type::Undefined {
                                            ptr.as_basic_value_enum()
                                        } else {
                                            self.builder
                                                .build_load(field.llvm_type, ptr, "")
                                                .unwrap()
                                        }
                                    } else {
                                        self.builder.build_load(field.llvm_type, ptr, "").unwrap()
                                    };

                                    prev_type = field.datatype.clone();
                                    prev_val = value;
                                }
                                "enum" => {
                                    let enumeration = self.scope.get_enum(&alias).unwrap();
                                    let idx =
                                        enumeration.fields.iter().position(|f| f == field).unwrap();
                                    let idx_value =
                                        self.context.i8_type().const_int(idx as u64, false);

                                    prev_val = idx_value.into();
                                }

                                _ => unreachable!(),
                            }
                        }
                    }

                    Expressions::Value(Value::Integer(idx), _) => match prev_type.clone() {
                        Type::Tuple(types) => {
                            let field_type = types[*idx as usize].clone();
                            let field_basic_type = self.get_basic_type(field_type.clone());

                            let tuple_type = self.context.struct_type(
                                &types
                                    .into_iter()
                                    .map(|typ| self.get_basic_type(typ))
                                    .collect::<Vec<BasicTypeEnum>>(),
                                false,
                            );

                            let ptr = self
                                .builder
                                .build_struct_gep(
                                    tuple_type,
                                    prev_val.into_pointer_value(),
                                    *idx as u32,
                                    "",
                                )
                                .unwrap();

                            let value = if let Some(Type::Pointer(ptr_type)) = expected.clone() {
                                if *ptr_type == Type::Undefined {
                                    ptr.as_basic_value_enum()
                                } else {
                                    self.builder.build_load(field_basic_type, ptr, "").unwrap()
                                }
                            } else {
                                self.builder.build_load(field_basic_type, ptr, "").unwrap()
                            };

                            prev_type = field_type;
                            prev_val = value;
                        }
                        _ => unreachable!(),
                    },

                    Expressions::FnCall {
                        name,
                        arguments,
                        span: _,
                    } => {
                        if let Type::Pointer(ptr_type) = prev_type.clone() {
                            prev_type = *ptr_type;
                        }

                        match prev_type.clone() {
                            Type::Alias(alias) => {
                                let alias_type =
                                    self.get_alias_type(prev_type.clone(), None).unwrap();

                                match alias_type {
                                    "struct" | "enum" => {
                                        let function = self
                                            .scope
                                            .get_function(format!("{alias_type}_{alias}__{name}"))
                                            .unwrap();

                                        {
                                            let mut_fn = self
                                                .scope
                                                .get_mut_function(format!(
                                                    "{alias_type}_{alias}__{name}"
                                                ))
                                                .unwrap();
                                            mut_fn.called = true;
                                        }

                                        let mut arguments = arguments
                                            .iter()
                                            .zip(function.arguments.clone())
                                            .map(|(arg, exp)| {
                                                self.compile_expression(arg.clone(), Some(exp))
                                                    .1
                                                    .into()
                                            })
                                            .collect::<Vec<BasicMetadataValueEnum>>();

                                        if let Some(Type::Pointer(ptr_type)) =
                                            function.arguments.first()
                                        {
                                            if let Type::Alias(arg_alias) = *ptr_type.clone() {
                                                if arg_alias == alias {
                                                    arguments.reverse();
                                                    arguments.push(prev_val.into());
                                                    arguments.reverse();
                                                }
                                            }
                                        }

                                        prev_type = function.datatype;
                                        prev_val = self
                                            .builder
                                            .build_call(function.value, &arguments, "")
                                            .unwrap()
                                            .try_as_basic_value()
                                            .left()
                                            .unwrap_or(self.context.i8_type().const_zero().into());
                                    }
                                    _ => unreachable!(),
                                }
                            }
                            Type::ImportObject(import_name) => {
                                let module_content =
                                    self.imports.get(&import_name).unwrap().clone();
                                let function = module_content.functions.get(name).unwrap();

                                let arguments = arguments
                                    .iter()
                                    .zip(function.arguments.clone())
                                    .map(|(arg, exp)| {
                                        self.compile_expression(arg.clone(), Some(exp)).1.into()
                                    })
                                    .collect::<Vec<BasicMetadataValueEnum>>();

                                prev_type = function.datatype.to_owned();
                                prev_val = self
                                    .builder
                                    .build_call(function.value, &arguments, "")
                                    .unwrap()
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap_or(self.context.i8_type().const_zero().into());
                            }
                            _ => {
                                panic!(
                                    "FnCall `{}()` unreachable type got: `{}`",
                                    name, &prev_type
                                );
                            }
                        }
                    }

                    Expressions::Struct {
                        name,
                        fields,
                        span: _,
                    } => {
                        if let Type::ImportObject(import_name) = prev_type.clone() {
                            let structure = self
                                .imports
                                .get(&import_name)
                                .unwrap()
                                .structures
                                .get(name)
                                .unwrap()
                                .clone();
                            let struct_alloca = self
                                .builder
                                .build_alloca(structure.llvm_type, &format!("struct.{name}.init"))
                                .unwrap();

                            for (field_name, field_expr) in fields {
                                let struct_field = structure.fields.get(field_name).unwrap();
                                let field_value = self.compile_expression(
                                    field_expr.clone(),
                                    Some(struct_field.datatype.clone()),
                                );

                                let field_ptr = self
                                    .builder
                                    .build_struct_gep(
                                        structure.llvm_type,
                                        struct_alloca,
                                        struct_field.nth,
                                        "",
                                    )
                                    .unwrap();
                                let _ = self.builder.build_store(field_ptr, field_value.1).unwrap();
                            }

                            let value = match expected {
                                Some(Type::Pointer(_)) => struct_alloca.into(),
                                _ => self
                                    .builder
                                    .build_load(structure.llvm_type, struct_alloca, "")
                                    .unwrap(),
                            };

                            prev_type = Type::Alias(format!("{import_name}.{name}"));
                            prev_val = value;
                        } else {
                            unreachable!()
                        }
                    }
                    _ => {
                        panic!("Unreachable expression found: {sub:?}")
                    }
                });

                (prev_type, prev_val)
            }
            Expressions::Scope { block, span: _ } => {
                let fn_type = self.get_fn_type(expected.clone().unwrap_or(Type::Void), &[], false);
                let scope_fn_value = self.module.add_function(
                    "__scope_wrap",
                    fn_type,
                    Some(inkwell::module::Linkage::Private),
                );
                let entry = self.context.append_basic_block(scope_fn_value, "entry");
                let current_position = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(entry);
                block
                    .iter()
                    .for_each(|stmt| self.compile_statement(stmt.to_owned(), None));

                self.builder.position_at_end(current_position);
                let scope_result = self
                    .builder
                    .build_call(scope_fn_value, &[], "")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                (expected.unwrap_or(Type::Void), scope_result)
            }

            Expressions::Array {
                values,
                len,
                span: _,
            } => {
                let expected_items_type = match expected {
                    Some(Type::Array(typ, _)) => Some(*typ),
                    _ => None,
                };

                let compiled_values = values
                    .into_iter()
                    .map(|val| self.compile_expression(val, expected_items_type.clone()))
                    .collect::<Vec<(Type, BasicValueEnum)>>();

                let arr_type = compiled_values[0].0.clone();
                let arr_basic_type = compiled_values[0].1.get_type();

                let arr_alloca = self
                    .builder
                    .build_array_alloca(
                        arr_basic_type,
                        self.context.i64_type().const_int(len as u64, false),
                        "",
                    )
                    .unwrap();

                compiled_values
                    .into_iter()
                    .enumerate()
                    .for_each(|(ind, (_, basic_value))| {
                        let ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    arr_basic_type,
                                    arr_alloca,
                                    &[self.context.i64_type().const_int(ind as u64, false)],
                                    "",
                                )
                                .unwrap()
                        };
                        self.builder.build_store(ptr, basic_value).unwrap();
                    });

                (Type::Array(Box::new(arr_type), len), arr_alloca.into())
            }
            Expressions::Tuple { values, span: _ } => {
                let mut expected_types = values.iter().map(|_| None).collect::<Vec<Option<Type>>>();
                if let Some(Type::Tuple(expectations)) = expected.clone() {
                    expected_types = expectations.into_iter().map(Some).collect();
                }

                let compiled_values = values
                    .into_iter()
                    .zip(expected_types)
                    .map(|(val, exp)| self.compile_expression(val, exp))
                    .collect::<Vec<(Type, BasicValueEnum)>>();
                let tuple_type = self.context.struct_type(
                    &compiled_values
                        .iter()
                        .map(|val| val.1.get_type())
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                );

                let compiled_types = compiled_values
                    .iter()
                    .map(|(typ, _)| typ.clone())
                    .collect::<Vec<Type>>();
                let alloca = self
                    .builder
                    .build_alloca(
                        tuple_type,
                        &format!(
                            "tuple__{}",
                            compiled_types
                                .iter()
                                .map(|typ| typ.to_string())
                                .collect::<Vec<String>>()
                                .join("_")
                        ),
                    )
                    .unwrap();

                compiled_values
                    .into_iter()
                    .enumerate()
                    .for_each(|(idx, (_, basic_val))| {
                        let ptr = self
                            .builder
                            .build_struct_gep(tuple_type, alloca, idx as u32, "")
                            .unwrap();
                        self.builder.build_store(ptr, basic_val).unwrap();
                    });

                let value = match expected {
                    Some(Type::Pointer(_)) => alloca.into(),
                    _ => self.builder.build_load(tuple_type, alloca, "").unwrap(),
                };
                let tuple_datatype = Type::Tuple(compiled_types);

                (tuple_datatype, value)
            }
            Expressions::Slice {
                object,
                index,
                span,
            } => {
                let obj = self.compile_expression(*object.clone(), None);
                let idx = self.compile_expression(*index, Some(Type::USIZE));

                match obj.0 {
                    Type::Array(ret_type, len) => {
                        let obj_ptr = if obj.1.is_pointer_value() {
                            obj.1.into_pointer_value()
                        } else {
                            let recompiled = self
                                .compile_expression(
                                    *object.clone(),
                                    Some(Type::Pointer(Box::new(Type::Undefined))),
                                )
                                .1;
                            recompiled.into_pointer_value()
                        };

                        // checking for the right index
                        let checker_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb"); // idxcb - index checker block
                        let error_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb_err");
                        let ok_block = self
                            .context
                            .append_basic_block(self.function.unwrap(), "__idxcb_ok");

                        self.builder
                            .build_unconditional_branch(checker_block)
                            .unwrap();
                        self.builder.position_at_end(checker_block);

                        let expected_basic_value =
                            self.context.i64_type().const_int(len as u64, false);
                        let provided_basic_value = idx.1.into_int_value();

                        let cmp_value = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::SLT,
                                provided_basic_value,
                                expected_basic_value,
                                "",
                            )
                            .unwrap();
                        self.builder
                            .build_conditional_branch(cmp_value, ok_block, error_block)
                            .unwrap();

                        self.builder.position_at_end(error_block);

                        self.build_panic(
                            "Array has len %ld, but index is %ld",
                            vec![expected_basic_value.into(), provided_basic_value.into()],
                            self.get_source_line(span.0),
                        );
                        self.build_branch(ok_block);

                        self.builder.position_at_end(ok_block);

                        // getting value

                        let basic_ret_type = self.get_basic_type(*ret_type.clone());
                        let ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    basic_ret_type,
                                    obj_ptr,
                                    &[idx.1.into_int_value()],
                                    "",
                                )
                                .unwrap()
                        };

                        if let Some(Type::Pointer(expected_ptr)) = expected
                            && *expected_ptr == Type::Undefined
                        {
                            return (Type::Pointer(Box::new(*ret_type)), ptr.into());
                        }

                        let ret_value = self.builder.build_load(basic_ret_type, ptr, "").unwrap();
                        (*ret_type, ret_value)
                    }
                    Type::Pointer(ptr_type) => {
                        let basic_ret_type = self.get_basic_type(*ptr_type.clone());
                        let ptr = unsafe {
                            self.builder
                                .build_in_bounds_gep(
                                    basic_ret_type,
                                    obj.1.into_pointer_value(),
                                    &[idx.1.into_int_value()],
                                    "",
                                )
                                .unwrap()
                        };

                        let ret_value = self.builder.build_load(basic_ret_type, ptr, "").unwrap();
                        (*ptr_type, ret_value)
                    }

                    Type::Alias(alias) => {
                        // getting struct ptr and type
                        let ptr = self
                            .compile_expression(
                                *object,
                                Some(Type::Pointer(Box::new(Type::Undefined))),
                            )
                            .1;

                        let struct_type = self.scope.get_struct(alias).unwrap();
                        let slice_fn = struct_type.functions.get("slice").unwrap();

                        // calling slice function
                        let call_result = self
                            .builder
                            .build_call(
                                slice_fn.value,
                                &[ptr.into(), idx.1.into()],
                                "@deen_slice_call",
                            )
                            .unwrap()
                            .try_as_basic_value()
                            .unwrap_left();

                        (slice_fn.datatype.clone(), call_result)
                    }

                    _ => unreachable!(),
                }
            }
            Expressions::Struct {
                name,
                fields,
                span: _,
            } => {
                let structure = self.scope.get_struct(&name).unwrap();
                let struct_alloca = self
                    .builder
                    .build_alloca(structure.llvm_type, &format!("struct.{name}.init"))
                    .unwrap();

                for (field_name, field_expr) in fields {
                    let struct_field = structure.fields.get(&field_name).unwrap();
                    let field_value =
                        self.compile_expression(field_expr, Some(struct_field.datatype.clone()));

                    // let ordered_index = self
                    //     .context
                    //     .i64_type()
                    //     .const_int(struct_field.nth as u64, false);

                    let field_ptr = self
                        .builder
                        .build_struct_gep(structure.llvm_type, struct_alloca, struct_field.nth, "")
                        .unwrap();

                    let _ = self.builder.build_store(field_ptr, field_value.1).unwrap();
                }

                let value = match expected {
                    Some(Type::Pointer(_)) => struct_alloca.into(),
                    _ => self
                        .builder
                        .build_load(structure.llvm_type, struct_alloca, "")
                        .unwrap(),
                };

                (Type::Alias(name), value)
            }

            Expressions::Argument {
                name: _,
                r#type,
                span: _,
            } => (r#type.clone(), self.get_basic_type(r#type).const_zero()),
            Expressions::None => unreachable!(),
        }
    }

    fn compile_value(
        &mut self,
        value: Value,
        expected: Option<Type>,
    ) -> (Type, BasicValueEnum<'ctx>) {
        match value {
            Value::Integer(int) => {
                if deen_semantic::Analyzer::is_integer(&expected.clone().unwrap_or(Type::Void))
                    || expected == Some(Type::Char)
                {
                    let exp = if let Some(exp) = expected.clone() {
                        exp
                    } else {
                        unreachable!()
                    };

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

                        Type::Char => (self.context.i8_type(), false),

                        _ => {
                            panic!("Unreachable type expected: {exp}");
                        }
                    };

                    return (
                        exp,
                        expected_type
                            .const_int(int as u64, signed)
                            .as_basic_value_enum(),
                    );
                }

                match int {
                    -2_147_483_648..=2_147_483_647 => (
                        Type::I32,
                        self.context.i32_type().const_int(int as u64, true).into(),
                    ),
                    -9_223_372_036_854_775_808..=9_223_372_036_854_775_807 => (
                        Type::I64,
                        self.context.i64_type().const_int(int as u64, true).into(),
                    ),
                }
            }
            Value::Float(float) => {
                if let Some(exp) = expected {
                    return match exp {
                        Type::F32 => (Type::F32, self.context.f32_type().const_float(float).into()),
                        Type::F64 => (Type::F64, self.context.f64_type().const_float(float).into()),
                        _ => (Type::F64, self.context.f64_type().const_float(float).into()),
                    };
                }

                (Type::F64, self.context.f64_type().const_float(float).into())
            }

            Value::Char(ch) => (
                Type::Char,
                self.context.i8_type().const_int(ch as u64, false).into(),
            ),
            Value::String(str) => {
                let global_value = self
                    .builder
                    .build_global_string_ptr(&str, "const_str")
                    .unwrap();
                global_value.set_constant(false);
                (
                    Type::Pointer(Box::new(Type::Char)),
                    global_value.as_pointer_value().into(),
                )
            }

            Value::Boolean(bool) => (
                Type::Bool,
                self.context
                    .bool_type()
                    .const_int(bool as u64, false)
                    .into(),
            ),
            Value::Identifier(id) => {
                // current module objects
                if self.scope.get_struct(&id).is_some() {
                    return (Type::Alias(id), self.context.i8_type().const_zero().into());
                }
                if let Some(typedef) = self.scope.get_typedef(&id) {
                    return (typedef.clone(), self.context.i8_type().const_zero().into());
                }
                if self.scope.get_enum(&id).is_some() {
                    return (Type::Alias(id), self.context.i8_type().const_zero().into());
                }

                // seeking through imports
                if self.imports.contains_key(&id) {
                    return (
                        Type::ImportObject(id),
                        self.context.i8_type().const_zero().into(),
                    );
                }

                let variable = self.scope.get_mut_variable(&id).unwrap(); // already checked by semantic analyzer

                if variable.global {
                    let global_value = self.module.get_global(&id).unwrap().as_basic_value_enum();
                    return (variable.datatype.clone(), global_value);
                }

                if expected == Some(Type::NoDrop) {
                    variable.no_drop = true;
                }

                let value = match expected.clone() {
                    Some(Type::Pointer(ptr_type)) => {
                        if *ptr_type == Type::Undefined {
                            variable.ptr.into()
                        } else {
                            self.builder
                                .build_load(variable.llvm_type, variable.ptr, "")
                                .unwrap()
                        }
                    }
                    _ => self
                        .builder
                        .build_load(variable.llvm_type, variable.ptr, "")
                        .unwrap(),
                };
                let datatype = match expected {
                    Some(Type::Pointer(ptr_type)) => {
                        if id == "self" {
                            Type::Pointer(Box::new(Type::Pointer(Box::new(
                                variable.datatype.clone(),
                            ))))
                        } else if *ptr_type == Type::Undefined {
                            Type::Pointer(Box::new(variable.datatype.clone()))
                        } else {
                            variable.datatype.clone()
                        }
                    }
                    _ => variable.datatype.clone(),
                };

                (datatype, value)
            }

            Value::Void => (Type::Void, self.context.bool_type().const_zero().into()),
            Value::Null => (
                Type::Null,
                self.context
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into(),
            ),
            Value::Keyword(_) => unreachable!(),
        }
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn build_panic(
        &mut self,
        message: impl std::convert::AsRef<str>,
        specifiers: Vec<BasicMetadataValueEnum<'ctx>>,
        call_line: usize,
    ) {
        let mut message = message.as_ref().to_owned();
        let panic_fn = self
            .module
            .get_function("__deen_panic")
            .unwrap_or_else(|| self.create_panic_function());

        if message.chars().last().unwrap_or(' ') != '\n' {
            message.push('\n')
        }

        let message_ptr = self
            .builder
            .build_global_string_ptr(
                &format!(
                    "\n* Runtime Panic at `{}.dn` <line {}>\n{}",
                    self.module.get_name().to_str().unwrap(),
                    call_line,
                    message
                ),
                "panic_formatter",
            )
            .unwrap()
            .as_pointer_value();

        let args: Vec<BasicMetadataValueEnum> = [vec![message_ptr.into()], specifiers].concat();
        self.builder.build_call(panic_fn, &args, "").unwrap();

        // let _ = self.builder.build_return(None);
    }

    fn create_panic_function(&mut self) -> FunctionValue<'ctx> {
        let fn_type = self.context.void_type().fn_type(
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true,
        );

        let fn_value = self.module.add_function(
            "__deen_panic",
            fn_type,
            Some(inkwell::module::Linkage::Private),
        );
        let entry = self.context.append_basic_block(fn_value, "entry");
        let old_position = self.builder.get_insert_block().unwrap();
        self.builder.position_at_end(entry);

        let printf_fn = self.module.get_function("printf").unwrap_or_else(|| {
            self.module.add_function(
                "printf",
                self.context.void_type().fn_type(
                    &[self.context.ptr_type(AddressSpace::default()).into()],
                    true,
                ),
                None,
            )
        });
        let exit_fn = self.module.get_function("exit").unwrap_or_else(|| {
            self.module.add_function(
                "exit",
                self.context
                    .void_type()
                    .fn_type(&[self.context.i32_type().into()], false),
                None,
            )
        });

        let args = fn_value
            .get_params()
            .into_iter()
            .map(|x| x.into())
            .collect::<Vec<BasicMetadataValueEnum>>();

        self.builder.build_call(printf_fn, &args, "").unwrap();
        self.builder
            .build_call(
                exit_fn,
                &[self.context.i32_type().const_int(1, false).into()],
                "",
            )
            .unwrap();
        self.builder.build_return(None).unwrap();

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
            Type::Char => self.context.i8_type().into(),
            Type::Bool => self.context.bool_type().into(),

            Type::Pointer(_) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Array(datatype, len) => {
                self.get_basic_type(*datatype).array_type(len as u32).into()
            }
            Type::DynamicArray(_) => todo!(),

            Type::Tuple(types) => {
                let basic_types = types
                    .into_iter()
                    .map(|typ| self.get_basic_type(typ))
                    .collect::<Vec<BasicTypeEnum>>();
                self.context
                    .struct_type(&basic_types, false)
                    .as_basic_type_enum()
            }
            Type::Alias(alias) => {
                if alias == "_" {
                    return self.context.bool_type().as_basic_type_enum();
                }

                let struct_type = self.scope.get_struct(&alias);
                let enum_type = self.scope.get_enum(&alias);
                let typedef_type = self.scope.get_typedef(&alias);

                if let Some(struct_type) = struct_type {
                    return struct_type.llvm_type;
                };
                if let Some(enum_type) = enum_type {
                    return enum_type.llvm_type;
                };
                if let Some(typedef_type) = typedef_type {
                    return self.get_basic_type(typedef_type.to_owned());
                };

                panic!("Compiler's semantic didn't catch undefined alias: `{alias}`")
            }

            Type::Function(_, _, _) => unreachable!(),
            Type::ImportObject(_) => unreachable!(),
            Type::Undefined => unreachable!(),
            Type::NoDrop => unreachable!(),
            Type::Struct(fields, _) => self
                .context
                .struct_type(
                    &fields
                        .iter()
                        .map(|field| self.get_basic_type(field.1.clone()))
                        .collect::<Vec<BasicTypeEnum>>(),
                    false,
                )
                .into(),
            Type::Enum(_, _) => self.context.i16_type().into(),
            Type::SelfRef => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Null => self.context.bool_type().into(),
            // Type::Function(args, datatype) => self.get_basic_type(*datatype).fn_type(
            //     &args.iter().map(|arg| self.get_basic_type(arg.clone()).into()).collect::<Vec<BasicMetadataTypeEnum>>(),
            //     false
            // ).into()
        }
    }

    fn get_fn_type(
        &self,
        datatype: Type,
        arguments: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        match datatype {
            Type::Void => self.context.void_type().fn_type(arguments, is_var_args),
            _ => self
                .get_basic_type(datatype)
                .fn_type(arguments, is_var_args),
        }
    }

    fn get_alias_type(&self, alias_type: Type, import_name: Option<&str>) -> Option<&str> {
        if let Type::Alias(alias) = alias_type {
            let struct_type = self.scope.get_struct(&alias);
            let enum_type = self.scope.get_enum(&alias);
            let typedef_type = self.scope.get_typedef(&alias);

            if struct_type.is_some() {
                return Some("struct");
            };
            if enum_type.is_some() {
                return Some("enum");
            };
            if typedef_type.is_some() {
                return Some("typedef");
            };

            if let Some(import_name) = import_name {
                let import = self.imports.get(import_name).unwrap();

                let struct_type = import.structures.get(&alias);
                let enum_type = import.enumerations.get(&alias);

                if struct_type.is_some() {
                    return Some("struct");
                };
                if enum_type.is_some() {
                    return Some("enum");
                };

                None
            } else {
                *self
                    .imports
                    .values()
                    .map(|import| {
                        let struct_type = import.structures.get(&alias);
                        let enum_type = import.enumerations.get(&alias);

                        if struct_type.is_some() {
                            return Some("struct");
                        };
                        if enum_type.is_some() {
                            return Some("enum");
                        };

                        None
                    })
                    .collect::<Vec<Option<&str>>>()
                    .first()
                    .unwrap()
            }
        } else {
            None
        }
    }

    fn get_source_line(&self, position: usize) -> usize {
        self.source
            .char_indices()
            .take_while(|&(pos, _)| pos < position)
            .filter(|&(_, chr)| chr == '\n')
            .count()
            + 1
    }

    fn build_branch(&mut self, block: BasicBlock<'ctx>) {
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder.build_unconditional_branch(block).unwrap();
        }
    }

    fn type_specifier(&self, datatype: &Type) -> String {
        match datatype {
            Type::I8 => "%hhd",
            Type::I16 => "%hd",
            Type::I32 => "%d",
            Type::I64 => "%lld",

            Type::U8 => "%hhu",
            Type::U16 => "%hu",
            Type::U32 => "%u",
            Type::U64 => "%llu",

            Type::USIZE => "%zu",

            Type::F32 => "%f",
            Type::F64 => "%lf",

            Type::Char => "%c",
            Type::Pointer(ptr) => match **ptr {
                Type::Char => "%s",
                _ => "%p",
            },

            Type::Bool => "%s",
            Type::Enum(_, _) => "%d",

            Type::Alias(_) => {
                let alias_type = self.get_alias_type(datatype.clone(), None).unwrap();
                match alias_type {
                    "struct" => "%s",
                    "enum" => "%d",
                    _ => unreachable!(),
                }
            }
            _ => "%s",
        }
        .to_string()
    }

    fn booleans_strings(&mut self) -> (PointerValue<'ctx>, PointerValue<'ctx>) {
        if let Some(allocated_strings) = self.booleans_strings {
            return allocated_strings;
        }

        let strings = (
            self.builder
                .build_global_string_ptr("true", "@true")
                .unwrap()
                .as_pointer_value(),
            self.builder
                .build_global_string_ptr("false", "@false")
                .unwrap()
                .as_pointer_value(),
        );

        self.booleans_strings = Some(strings);
        strings
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn panic_function_test() {
        let ctx = CodeGen::create_context();
        let mut codegen = CodeGen::new(
            &ctx,
            "",
            "",
            deen_semantic::symtable::SymbolTable::default(),
        );

        let main_fn = codegen.module.add_function(
            "main",
            codegen.context.void_type().fn_type(&[], false),
            None,
        );
        let entry_block = codegen.context.append_basic_block(main_fn, "entry");
        codegen.builder.position_at_end(entry_block);

        let panic_fn = codegen.create_panic_function();

        assert_eq!(panic_fn.get_linkage(), Linkage::Private);
        assert!(!panic_fn.is_null());
        assert!(!panic_fn.is_undef());
        assert!(panic_fn.verify(false));
    }

    #[test]
    fn boolean_strings_test() {
        let ctx = CodeGen::create_context();
        let mut codegen = CodeGen::new(
            &ctx,
            "",
            "",
            deen_semantic::symtable::SymbolTable::default(),
        );

        let main_fn = codegen.module.add_function(
            "main",
            codegen.context.void_type().fn_type(&[], false),
            None,
        );
        let entry_block = codegen.context.append_basic_block(main_fn, "entry");
        codegen.builder.position_at_end(entry_block);

        let (true_str, false_str) = codegen.booleans_strings();
        let (true_str_2, false_str_2) = codegen.booleans_strings();

        assert_eq!(true_str, true_str_2);
        assert_eq!(false_str, false_str_2);

        assert_eq!(true_str.get_name().to_str().unwrap(), "@true");
        assert_eq!(false_str.get_name().to_str().unwrap(), "@false");
    }
}
