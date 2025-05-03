use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,

    F32,
    F64,

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
    Struct(HashMap<String, Type>, HashMap<String, Type>), // struct Abc { a: i32, b: bool, c: *u64 }  ---> Struct([I32, Bool, Pointer(U64)])
    Enum(Vec<String>, HashMap<String, Type>),             // enum Abc { A, B, C } -> Enum([A, B, C])

    ImportObject(String),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),

            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),

            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::USIZE => write!(f, "usize"),

            Type::String => write!(f, "string"),
            Type::Char => write!(f, "char"),

            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),

            Type::Pointer(inner) => write!(f, "*{inner}"),
            Type::Array(inner, size) => write!(f, "[{inner}; {size}]"),
            Type::DynamicArray(inner) => write!(f, "[]{inner}"),

            Type::Tuple(elements) => {
                write!(f, "(")?;
                for (i, ty) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{ty}")?;
                }
                write!(f, ")")
            }

            Type::Alias(alias) => write!(f, "{alias}"),
            Type::Function(args, functype) => write!(
                f,
                "{functype} ({})",
                args.iter()
                    .map(|a| format!("{a}"))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Struct(args, _) => write!(
                f,
                "struct {{ {} }}",
                args.iter()
                    .map(|a| format!("{}", a.1))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Enum(args, _) => write!(
                f,
                "enum {{ {} }}",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),

            Type::ImportObject(imp) => write!(f, "<{imp}>"),
        }
    }
}
