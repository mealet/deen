use crate::function::Function;
use deen_parser::types::Type;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Structure<'ctx> {
    pub fields: HashMap<String, Field<'ctx>>,
    pub functions: HashMap<String, Function<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}

#[derive(Debug, Clone)]
pub struct Field<'ctx> {
    pub name: String,
    pub nth: u32,
    pub datatype: Type,
    pub llvm_type: BasicTypeEnum<'ctx>,
}
