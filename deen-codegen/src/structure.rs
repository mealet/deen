use std::collections::HashMap;
use deen_parser::types::Type;
use inkwell::types::BasicTypeEnum;
use crate::function::Function;

pub struct Structure<'ctx> {
    pub name: String,
    pub fields: HashMap<String, Field<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>
}

pub struct Field<'ctx> {
    pub name: String,
    pub nth: usize,
    pub llvm_type: BasicTypeEnum<'ctx>
}
