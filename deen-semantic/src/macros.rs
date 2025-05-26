use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct MacrosObject {
    pub arguments: Vec<Type>,
    pub settings: Vec<MacrosOption>,
    pub return_type: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MacrosOption {
    FirstLiteral,
    VarArgs,
    // MatchingTypes,
}
