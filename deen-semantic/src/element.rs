use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct ScopeElement {
    pub datatype: Type,
    pub public: bool,
}
