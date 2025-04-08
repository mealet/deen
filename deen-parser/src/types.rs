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
    USIZE,

    String,
    Char,

    Bool,
    Void,

    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    DynamicArray(Box<Type>),

    Tuple(Vec<Type>),
    Alias(String),
    
    // will be used for semantical analyzer
    Function(Vec<Type>, Box<Type>), // fn foo(a: i32, b: u32) string  --->  Function([I32, U32], String)
    Struct(Vec<Type>), // struct Abc { a: i32, b: bool, c: *u64 }  ---> Struct([I32, Bool, Pointer(U64)])
}
