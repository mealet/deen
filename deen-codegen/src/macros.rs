use deen_parser::{expressions::Expressions, types::Type, value::Value};
use inkwell::values::BasicValueEnum;

use crate::CodeGen;

pub trait StandartMacros<'ctx> { 
    fn build_macro_call(&mut self, id: &str, arguments: Vec<Expressions>) -> (Type, BasicValueEnum<'ctx>);
}

impl<'ctx> StandartMacros<'ctx> for CodeGen<'ctx> {
    fn build_macro_call(&mut self, id: &str, arguments: Vec<Expressions>) -> (Type, BasicValueEnum<'ctx>) {
        match id {
            "print" | "println" => {
                let literal = if let Some(Expressions::Value(Value::String(str), _)) = arguments.get(0) { str.clone() } else { String::default() };
                let compiled_args = arguments.iter().skip(1).map(|expr| {
                    self.compile_expression(expr.clone(), None)
                }).collect::<Vec<(Type, BasicValueEnum)>>();

                let format_specifiers = compiled_args.into_iter().map(|(typ, _)| {
                    match typ {
                        Type::I8 => "%hhd",
                        Type::I16 => "%hd",
                        Type::I32 => "%d",
                        Type::I64 => "%lld",
                        
                        Type::U8 => "%hhu",
                        Type::U16 => "%hu",
                        Type::U32 => "%u",
                        Type::U64 => "%llu",

                        Type::USIZE => "%zu",
                        
                        Type::F32 => "%f",
                        Type::F64 => "%lf",

                        Type::String => "%s",
                        Type::Char => "%c",
                        Type::Pointer(ptr) => {
                            match *ptr {
                                Type::Char => "%s",
                                _ => unreachable!()
                            }
                        }

                        Type::Bool => "%d",
                        _ => todo!()
                    }
                });

                todo!()
            },
            "drop" => todo!(),
            "format" => todo!(),
            _ => unreachable!()
        }
    }
}
