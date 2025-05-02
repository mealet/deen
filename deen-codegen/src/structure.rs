use std::collections::HashMap;
use deen_parser::types::Type;
use inkwell::types::BasicTypeEnum;

#[derive(Debug, Clone)]
pub struct Structure<'ctx> {
    pub fields: HashMap<String, Field<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>
}

#[derive(Debug, Clone)]
pub struct Field<'ctx> {
    pub name: String,
    pub nth: u32,
    pub datatype: Type,
    pub llvm_type: BasicTypeEnum<'ctx>
}
