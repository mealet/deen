use inkwell::types::BasicTypeEnum;

pub struct Enumeration<'ctx> {
    pub name: String,
    pub fields: Vec<String>,
    pub llvm_type: BasicTypeEnum<'ctx>
}
