use inkwell::types::BasicTypeEnum;

#[derive(Debug, Clone)]
pub struct Enumeration<'ctx> {
    pub fields: Vec<String>,
    pub llvm_type: BasicTypeEnum<'ctx>
}
