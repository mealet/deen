use deen_parser::{expressions::Expressions, types::Type, value::Value};
use inkwell::{
    AddressSpace,
    module::Linkage,
    values::{BasicMetadataValueEnum, BasicValueEnum},
};

use crate::CodeGen;

pub trait StandartMacros<'ctx> {
    fn build_macro_call(
        &mut self,
        id: &str,
        arguments: Vec<Expressions>,
    ) -> (Type, BasicValueEnum<'ctx>);
}

impl<'ctx> StandartMacros<'ctx> for CodeGen<'ctx> {
    fn build_macro_call(
        &mut self,
        id: &str,
        arguments: Vec<Expressions>,
    ) -> (Type, BasicValueEnum<'ctx>) {
        match id {
            "print" | "println" => {
                let mut literal =
                    if let Some(Expressions::Value(Value::String(str), _)) = arguments.first() {
                        str.clone()
                    } else {
                        String::default()
                    };
                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, BasicValueEnum)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.into_iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }
                let printf_fn = self.module.get_function("printf").unwrap_or_else(|| {
                    self.module.add_function(
                        "printf",
                        self.context.i32_type().fn_type(
                            &[self.context.ptr_type(AddressSpace::default()).into()],
                            true,
                        ),
                        Some(Linkage::External),
                    )
                });

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            self.builder
                                .build_select(arg.1.into_int_value(), _true, _false, "")
                                .unwrap()
                                .into()
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone()).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            self.builder
                                                .build_load(
                                                    self.get_basic_type(arg.0),
                                                    arg.1.into_pointer_value(),
                                                    "",
                                                )
                                                .unwrap()
                                                .into()
                                        } else {
                                            arg.1.into()
                                        };

                                    let output: BasicMetadataValueEnum = self
                                        .builder
                                        .build_call(display_function.value, &[self_val], "")
                                        .unwrap()
                                        .try_as_basic_value()
                                        .left()
                                        .unwrap()
                                        .into();
                                    output
                                }
                                "enum" => arg.1.into(),
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1.into(),
                    })
                    .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

                let global_literal: BasicMetadataValueEnum<'ctx> = self
                    .builder
                    .build_global_string_ptr(&literal, "")
                    .unwrap()
                    .as_pointer_value()
                    .into();
                let call_arguments = [vec![global_literal], format_values].concat();

                let _ = self.builder.build_call(printf_fn, &call_arguments, "");

                (Type::Void, self.context.bool_type().const_zero().into())
            }
            "format" => {
                let mut literal =
                    if let Some(Expressions::Value(Value::String(str), _)) = arguments.first() {
                        str.clone()
                    } else {
                        String::default()
                    };
                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, BasicValueEnum)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.into_iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }
                let sprintf_fn = self.module.get_function("printf").unwrap_or_else(|| {
                    self.module.add_function(
                        "sprintf",
                        self.context.i32_type().fn_type(
                            &[
                                self.context.ptr_type(AddressSpace::default()).into(),
                                self.context.ptr_type(AddressSpace::default()).into(),
                            ],
                            true,
                        ),
                        Some(Linkage::External),
                    )
                });

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            self.builder
                                .build_select(arg.1.into_int_value(), _true, _false, "")
                                .unwrap()
                                .into()
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone()).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            self.builder
                                                .build_load(
                                                    self.get_basic_type(arg.0),
                                                    arg.1.into_pointer_value(),
                                                    "",
                                                )
                                                .unwrap()
                                                .into()
                                        } else {
                                            arg.1.into()
                                        };

                                    let output: BasicMetadataValueEnum = self
                                        .builder
                                        .build_call(display_function.value, &[self_val], "")
                                        .unwrap()
                                        .try_as_basic_value()
                                        .left()
                                        .unwrap()
                                        .into();
                                    output
                                }
                                "enum" => arg.1.into(),
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1.into(),
                    })
                    .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

                let global_literal: BasicMetadataValueEnum<'ctx> = self
                    .builder
                    .build_global_string_ptr(&literal, "")
                    .unwrap()
                    .as_pointer_value()
                    .into();

                let result_alloca = self.builder.build_alloca(self.context.ptr_type(AddressSpace::default()), "").unwrap();
                let call_arguments = [vec![result_alloca.into(), global_literal], format_values].concat();

                let _ = self.builder.build_call(sprintf_fn, &call_arguments, "");

                (Type::Pointer(Box::new(Type::Char)), result_alloca.into())

                // BUG: Formatting and storing unkonwn sized string into stack allocated buffer
                // with stricted size causes segmentation fault. There's only 2 ways I've found how
                // to fix it:
                // 1. Find the string length and then allocate memory for that
                // 2. Implement dynamic `string` datatype with allocation on heap and make macro
                //    return this type.
            }
            "panic" => {
                let (mut literal, call_line) =
                    if let Some(Expressions::Value(Value::String(str), span)) = arguments.first() {
                        (
                            str.clone(),
                            self.get_source_line(span.0)
                        )
                    } else {
                        (String::default(), 0)
                    };

                let compiled_args = arguments
                    .iter()
                    .skip(1)
                    .map(|expr| self.compile_expression(expr.clone(), None))
                    .collect::<Vec<(Type, BasicValueEnum)>>();

                let format_specifiers = compiled_args
                    .iter()
                    .map(|(typ, _)| self.type_specifier(typ))
                    .collect::<Vec<String>>();

                format_specifiers.into_iter().for_each(|spec| {
                    if let Some(position) = literal.find("{}") {
                        literal.replace_range(position..position + 2, &spec);
                    }
                });

                if id.contains("ln") {
                    literal.push('\n')
                }

                let format_values = compiled_args
                    .into_iter()
                    .map(|arg| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            self.builder
                                .build_select(arg.1.into_int_value(), _true, _false, "")
                                .unwrap()
                                .into()
                        }
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone()).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            self.builder
                                                .build_load(
                                                    self.get_basic_type(arg.0),
                                                    arg.1.into_pointer_value(),
                                                    "",
                                                )
                                                .unwrap()
                                                .into()
                                        } else {
                                            arg.1.into()
                                        };

                                    let output: BasicMetadataValueEnum = self
                                        .builder
                                        .build_call(display_function.value, &[self_val], "")
                                        .unwrap()
                                        .try_as_basic_value()
                                        .left()
                                        .unwrap()
                                        .into();
                                    output
                                }
                                "enum" => arg.1.into(),
                                _ => unreachable!(),
                            }
                        }
                        _ => arg.1.into(),
                    })
                    .collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
                
                self.build_panic(literal, format_values, call_line);
                (Type::Void, self.context.bool_type().const_zero().into())

            },
            "drop" => todo!(),
            _ => {
                panic!("Provided macros is under development stage: `{}!()`", id)
            },
        }
    }
}
