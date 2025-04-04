#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,

    U8,
    U16,
    U32,
    U64,

    String,
    Char,

    Bool,
    Void,

    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    DynamicArray(Box<Type>),

    Struct(String),
    Tuple(Vec<Type>),
    Alias(String, Box<Type>),

    Function {
        args: Vec<Type>,
        r#type: Box<Type>
    }
}
