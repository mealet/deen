use deen_parser::{expressions::Expressions, types::Type, value::Value};
use inkwell::{
    AddressSpace,
    module::Linkage,
    values::{BasicMetadataValueEnum, BasicValueEnum},
    types::BasicType
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
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    // NOTE: Method `display(&self) *char` won`t give mutability of
                                    // structure, it will only provide tool to use struct in basic
                                    // output.
                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            arg.1.into()
                                        } else {
                                            let alloca = self.builder.build_alloca(arg.1.get_type(), "").unwrap();
                                            let _ = self.builder.build_store(alloca, arg.1).unwrap();
                                            alloca.into()
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
                let snprintf_fn = self.module.get_function("snprintf").unwrap_or_else(|| {
                    self.module.add_function(
                        "snprintf",
                        self.context.i32_type().fn_type(
                            &[
                                self.context.ptr_type(AddressSpace::default()).into(),
                                self.context.i64_type().into(),
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
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            arg.1.into()
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

                // first call for output size

                let first_call_args = [
                    vec![
                        self.context.ptr_type(AddressSpace::default()).const_null().into(),
                        self.context.i64_type().const_zero().into(),
                        global_literal,
                    ],
                    format_values.clone()
                ].concat();

                let length = self.builder.build_call(
                    snprintf_fn,
                    &first_call_args,
                    ""
                ).unwrap().try_as_basic_value().left().unwrap();

                let buffer = {
                    let buffer_size = self.builder.build_int_add(
                        length.into_int_value(),
                        self.context.i32_type().const_int(1, false),
                        ""
                    ).unwrap();

                    self.builder.build_array_alloca(self.context.i8_type(), buffer_size, "").unwrap()
                };

                // second call for the final format

                let sprintf_fn = self.module.get_function("sprintf").unwrap_or_else(|| {
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

                let call_arguments = [vec![buffer.into(), global_literal], format_values].concat();

                let _ = self.builder.build_call(sprintf_fn, &call_arguments, "");

                (Type::Pointer(Box::new(Type::Char)), buffer.into())
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
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!("{}_{}__{}", alias_type, alias, "display"))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum =
                                        if arg.1.is_pointer_value() {
                                            arg.1.into()
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
            "sizeof" => {
                let instance = arguments.first().unwrap();
                let basic_type = {
                    if let Expressions::Argument { name: _, r#type, span: _ } = instance {
                        self.get_basic_type(r#type.clone())
                    } else {
                        self.compile_expression(instance.clone(), None).1.get_type()
                    }
                };

                (Type::USIZE, basic_type.size_of().unwrap().into())
            },
            _ => {
                panic!("Provided macros is under development stage: `{}!()`", id)
            },
        }
    }
}
