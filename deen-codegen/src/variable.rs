use deen_parser::types::Type;
use inkwell::{types::BasicTypeEnum, values::PointerValue};

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
    pub datatype: Type,
    pub llvm_type: BasicTypeEnum<'ctx>,
    pub ptr: PointerValue<'ctx>,
    pub no_drop: bool,
    pub global: bool,
}
