//! # Values
//! **Values** here is the simplest basic units. They contains constants, identifiers, keywords and etc.<br/>
//! Values is always an internal elemenets of expressions.

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Char(char),
    Boolean(bool),
    Identifier(String),
    Keyword(String),
    Null,
    Void,
}
