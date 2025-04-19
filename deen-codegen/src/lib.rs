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
use std::collections::HashMap;
use variable::Variable;

mod variable;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    variables: HashMap<String, Variable<'ctx>>
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            builder,
            module,

            variables: HashMap::new()
        }
    }

    pub fn compile(&mut self, statements: Vec<Statements>) -> Module<'ctx> {
        for statement in statements {
            self.compile_statement(statement);
        }

        self.module.clone()
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn compile_statement(&mut self, statement: Statements) {
        match statement {
            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {
                let mut args: Vec<BasicMetadataTypeEnum<'ctx>> = Vec::new();
                arguments.iter().for_each(|arg| {
                    args.push(
                        self.get_basic_type(arg.1.clone()).into()
                    );
                });

                let fn_type = self.get_fn_type(datatype, &args, false);
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

                block.iter().for_each(|stmt| self.compile_statement(stmt.clone()));

                if let Some(basic_block) = old_position {
                    self.builder.position_at_end(basic_block);
                }
                
                old_variables.iter().for_each(|var| {
                    if let Some(value) = var.1 {
                        self.variables.insert(var.0.to_owned(), value.to_owned());
                    }
                })
            }
            _ => todo!()
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

            Type::Void => self.context.bool_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Char => self.context.custom_width_int_type(8).into(),
            Type::Bool => self.context.bool_type().into(),

            Type::Pointer(_) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Array(datatype, len) => self.get_basic_type(*datatype).array_type(len as u32).into(),
            Type::DynamicArray(datatype) => todo!(),
            
            Type::Tuple(types) => unreachable!(),
            Type::Alias(alias) => todo!(),
            
            Type::Function(_, _) => unreachable!(),
            Type::Struct(fields, _) => self.context.struct_type(
                &fields.iter().map(|field| 
                    self.get_basic_type(field.1.clone())
                ).collect::<Vec<BasicTypeEnum>>(),
                false
            ).into(),
            Type::Enum(fields, _) => unreachable!()

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
