use crate::types::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Integer(i64),
    String(String),
    Char(char),
    Boolean(bool),
    Identifier(String),
    Keyword(String),
    Type(Type)
}
