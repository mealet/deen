use deen_parser::types::Type;

#[derive(Debug, Clone)]
#[allow(unused)]
pub struct MacrosObject {
    pub arguments: Vec<Type>,
    pub is_first_literal: bool,
    pub is_var_args: bool,
    pub return_type: Type,
}
