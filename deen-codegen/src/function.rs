use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Function<'ctx> {
    pub datatype: Type,
    pub value: inkwell::values::FunctionValue<'ctx>,
    pub arguments: Vec<Type>,
    pub called: bool,
}
