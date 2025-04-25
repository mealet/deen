use std::collections::HashMap;
use deen_parser::types::Type;
use inkwell::types::BasicTypeEnum;
use crate::function::Function;

#[derive(Debug, Clone)]
pub struct Structure<'ctx> {
    pub name: String,
    pub fields: HashMap<String, Field<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>
}

#[derive(Debug, Clone)]
pub struct Field<'ctx> {
    pub name: String,
    pub nth: usize,
    pub datatype: Type,
    pub llvm_type: BasicTypeEnum<'ctx>
}
