use crate::function::Function;
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Enumeration<'ctx> {
    pub fields: Vec<String>,
    pub functions: HashMap<String, Function<'ctx>>,
    pub llvm_type: BasicTypeEnum<'ctx>,
}
