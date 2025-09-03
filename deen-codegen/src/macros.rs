use deen_parser::{expressions::Expressions, types::Type, value::Value};
use inkwell::{
    AddressSpace,
    module::Linkage,
    types::BasicType,
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
                    .enumerate()
                    .map(|(index, arg)| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            self.builder
                                .build_select(arg.1.into_int_value(), _true, _false, "")
                                .unwrap()
                                .into()
                        }
                        Type::F32 => self
                            .builder
                            .build_float_ext(arg.1.into_float_value(), self.context.f64_type(), "")
                            .unwrap()
                            .into(),
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum = if arg
                                        .1
                                        .is_pointer_value()
                                    {
                                        arg.1.into()
                                    } else {
                                        let self_expression =
                                            arguments.iter().skip(1).nth(index).unwrap().clone();

                                        let mut recompiled = self
                                            .compile_expression(
                                                self_expression,
                                                Some(Type::Pointer(Box::new(Type::Undefined))),
                                            )
                                            .1;

                                        if !recompiled.is_pointer_value() {
                                            let alloca = self
                                                .builder
                                                .build_alloca(arg.1.get_type(), "")
                                                .unwrap();
                                            let _ =
                                                self.builder.build_store(alloca, arg.1).unwrap();

                                            recompiled = alloca.into();
                                        }

                                        assert!(recompiled.is_pointer_value());
                                        recompiled.into()
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
                    .enumerate()
                    .map(|(index, arg)| match arg.0.clone() {
                        Type::Bool => {
                            let (_true, _false) = self.booleans_strings();
                            self.builder
                                .build_select(arg.1.into_int_value(), _true, _false, "")
                                .unwrap()
                                .into()
                        }
                        Type::F32 => self
                            .builder
                            .build_float_ext(arg.1.into_float_value(), self.context.f64_type(), "")
                            .unwrap()
                            .into(),
                        Type::Alias(alias) => {
                            let alias_type = self.get_alias_type(arg.0.clone(), None).unwrap();

                            match alias_type {
                                "struct" => {
                                    let display_function = self
                                        .scope
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum = if arg
                                        .1
                                        .is_pointer_value()
                                    {
                                        arg.1.into()
                                    } else {
                                        let mut recompiled = self
                                            .compile_expression(
                                                arguments
                                                    .iter()
                                                    .skip(1)
                                                    .nth(index)
                                                    .unwrap()
                                                    .clone(),
                                                Some(Type::Pointer(Box::new(Type::Undefined))),
                                            )
                                            .1;

                                        if !recompiled.is_pointer_value() {
                                            let alloca = self
                                                .builder
                                                .build_alloca(arg.1.get_type(), "")
                                                .unwrap();
                                            let _ =
                                                self.builder.build_store(alloca, arg.1).unwrap();

                                            recompiled = alloca.into();
                                        }

                                        assert!(recompiled.is_pointer_value());

                                        recompiled.into()
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
                        self.context
                            .ptr_type(AddressSpace::default())
                            .const_null()
                            .into(),
                        self.context.i64_type().const_zero().into(),
                        global_literal,
                    ],
                    format_values.clone(),
                ]
                .concat();

                let length = self
                    .builder
                    .build_call(snprintf_fn, &first_call_args, "")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                let buffer = {
                    let buffer_size = self
                        .builder
                        .build_int_add(
                            length.into_int_value(),
                            self.context.i32_type().const_int(1, false),
                            "",
                        )
                        .unwrap();

                    self.builder
                        .build_array_alloca(self.context.i8_type(), buffer_size, "")
                        .unwrap()
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
                        (str.clone(), self.get_source_line(span.0))
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
                                        .get_function(format!(
                                            "{}_{}__{}",
                                            alias_type, alias, "display"
                                        ))
                                        .unwrap();

                                    let self_val: BasicMetadataValueEnum = arg.1.into();

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
            }
            "sizeof" => {
                let instance = arguments.first().unwrap();
                let basic_type = {
                    if let Expressions::Argument {
                        name: _,
                        r#type,
                        span: _,
                    } = instance
                    {
                        self.get_basic_type(r#type.clone())
                    } else {
                        self.compile_expression(instance.clone(), None).1.get_type()
                    }
                };

                (Type::USIZE, basic_type.size_of().unwrap().into())
            }
            "cast" => {
                let from = arguments.first().unwrap().clone();
                let to = arguments.get(1).unwrap().clone();

                let from_value = self.compile_expression(from, None);
                let to_type = self.compile_expression(to, None);
                let target_basic_type = self.get_basic_type(to_type.0.clone());

                match (&from_value.0, &to_type.0) {
                    (from, to) if from == to => from_value,

                    // integer types cast
                    (from, to)
                        if deen_semantic::Analyzer::is_integer(from)
                            && deen_semantic::Analyzer::is_integer(to)
                            || from == &Type::Char
                            || to == &Type::Char
                            || from == &Type::Bool
                            || to == &Type::Bool =>
                    {
                        let unsigned = deen_semantic::Analyzer::is_unsigned_integer(to);

                        let from_order = deen_semantic::Analyzer::integer_order(from);
                        let to_order = deen_semantic::Analyzer::integer_order(to);

                        let value = if from_order > to_order {
                            // truncating
                            self.builder
                                .build_int_truncate(
                                    from_value.1.into_int_value(),
                                    target_basic_type.into_int_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        } else if from_order < to_order {
                            // extending
                            if unsigned {
                                self.builder
                                    .build_int_z_extend(
                                        from_value.1.into_int_value(),
                                        target_basic_type.into_int_type(),
                                        "",
                                    )
                                    .unwrap()
                                    .into()
                            } else {
                                self.builder
                                    .build_int_s_extend(
                                        from_value.1.into_int_value(),
                                        target_basic_type.into_int_type(),
                                        "",
                                    )
                                    .unwrap()
                                    .into()
                            }
                        } else {
                            // casting
                            self.builder
                                .build_bit_cast(from_value.1, target_basic_type.into_int_type(), "")
                                .unwrap()
                        };

                        (to_type.0, value)
                    }

                    // float types casts
                    (from, to)
                        if deen_semantic::Analyzer::is_float(from)
                            && deen_semantic::Analyzer::is_float(to) =>
                    {
                        let from_order = deen_semantic::Analyzer::float_order(from);
                        let to_order = deen_semantic::Analyzer::float_order(to);

                        let value = if from_order > to_order {
                            // truncating
                            self.builder
                                .build_float_trunc(
                                    from_value.1.into_float_value(),
                                    target_basic_type.into_float_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        } else {
                            // extending
                            self.builder
                                .build_float_ext(
                                    from_value.1.into_float_value(),
                                    target_basic_type.into_float_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        };

                        (to_type.0, value)
                    }

                    // `float -> integer` cast
                    (from, to)
                        if deen_semantic::Analyzer::is_float(from)
                            && deen_semantic::Analyzer::is_integer(to) =>
                    {
                        let unsigned = deen_semantic::Analyzer::is_unsigned_integer(to);

                        let value = if unsigned {
                            self.builder
                                .build_float_to_unsigned_int(
                                    from_value.1.into_float_value(),
                                    target_basic_type.into_int_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_float_to_signed_int(
                                    from_value.1.into_float_value(),
                                    target_basic_type.into_int_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        };

                        (to_type.0, value)
                    }

                    // `integer -> float` cast
                    (from, to)
                        if deen_semantic::Analyzer::is_integer(from)
                            && deen_semantic::Analyzer::is_float(to) =>
                    {
                        let unsigned = deen_semantic::Analyzer::is_unsigned_integer(from);

                        let value = if unsigned {
                            self.builder
                                .build_unsigned_int_to_float(
                                    from_value.1.into_int_value(),
                                    target_basic_type.into_float_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_signed_int_to_float(
                                    from_value.1.into_int_value(),
                                    target_basic_type.into_float_type(),
                                    "",
                                )
                                .unwrap()
                                .into()
                        };

                        (to_type.0, value)
                    }

                    // `pointer -> integer` cast
                    (from, to)
                        if matches!(from, &Type::Pointer(_))
                            && deen_semantic::Analyzer::is_integer(to) =>
                    {
                        let value = self
                            .builder
                            .build_ptr_to_int(
                                from_value.1.into_pointer_value(),
                                target_basic_type.into_int_type(),
                                "",
                            )
                            .unwrap();

                        (to_type.0, value.into())
                    }

                    // `integer -> pointer` cast
                    (from, to)
                        if deen_semantic::Analyzer::is_integer(from)
                            && matches!(to, &Type::Pointer(_)) =>
                    {
                        let value = self
                            .builder
                            .build_int_to_ptr(
                                from_value.1.into_int_value(),
                                target_basic_type.into_pointer_type(),
                                "",
                            )
                            .unwrap();

                        (to_type.0, value.into())
                    }

                    // pointers types cast
                    (from, to)
                        if matches!(from, &Type::Pointer(_)) && matches!(to, &Type::Pointer(_)) =>
                    {
                        let value = self
                            .builder
                            .build_pointer_cast(
                                from_value.1.into_pointer_value(),
                                target_basic_type.into_pointer_type(),
                                "",
                            )
                            .unwrap();

                        (to_type.0, value.into())
                    }

                    _ => panic!(
                        "Unimplemented cast type catched: `{}` -> `{}`",
                        from_value.0, to_type.0
                    ),
                }
            }
            _ => {
                panic!("Provided macros is under development stage: `{id}!()`")
            }
        }
    }
}
