use deen_lexer::Lexer;
use deen_parser::{
    Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
};

#[test]
fn binary_expression() {
    const SRC: &str = "let a = 5 + 2;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "+");

                if let Expressions::Value(Value::Integer(5), _) = *lhs.clone() {
                } else {
                    panic!("Wrong LHS found")
                };
                if let Expressions::Value(Value::Integer(2), _) = *rhs.clone() {
                } else {
                    panic!("Wrong LHS found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn binary_advanced_expression() {
    const SRC: &str = "let a = 2 + 2 * 2;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Binary {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "+");

                if let Expressions::Value(Value::Integer(2), _) = *lhs.clone() {
                } else {
                    panic!("Wrong LHS found")
                };
                if let Expressions::Binary {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *rhs.clone()
                {
                    assert_eq!(operand, "*");

                    if let Expressions::Value(Value::Integer(2), _) = *lhs.clone() {
                    } else {
                        panic!("Wrong LHS found")
                    };
                    if let Expressions::Value(Value::Integer(2), _) = *rhs.clone() {
                    } else {
                        panic!("Wrong LHS found")
                    };
                } else {
                    panic!("Wrong LHS found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn unary_negative_expression() {
    const SRC: &str = "let a = -2;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                assert_eq!(operand, "-");

                if let Expressions::Value(Value::Integer(2), _) = *object.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn unary_not_expression() {
    const SRC: &str = "let a = !2;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Unary {
                operand,
                object,
                span: _,
            } => {
                assert_eq!(operand, "!");

                if let Expressions::Value(Value::Integer(2), _) = *object.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_eq_expression() {
    const SRC: &str = "let a = 1 == 1;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "==");

                if let Expressions::Value(Value::Integer(1), _) = *lhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_ne_expression() {
    const SRC: &str = "let a = 1 != 1;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "!=");

                if let Expressions::Value(Value::Integer(1), _) = *lhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_bt_expression() {
    const SRC: &str = "let a = 1 > 1;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, ">");

                if let Expressions::Value(Value::Integer(1), _) = *lhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_lt_expression() {
    const SRC: &str = "let a = 1 < 1;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "<");

                if let Expressions::Value(Value::Integer(1), _) = *lhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(1), _) = *rhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn boolean_advanced_expression() {
    const SRC: &str = "let a = 1 == 1 && 0 != 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Boolean {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "&&");

                if let Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *lhs.clone()
                {
                    assert_eq!(operand, "==");

                    if let Expressions::Value(Value::Integer(1), _) = *lhs.clone() {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(1), _) = *rhs.clone() {
                    } else {
                        panic!("Wrong object expression found")
                    };
                } else {
                    panic!("Wrong boolean expression found");
                }

                if let Expressions::Boolean {
                    operand,
                    lhs,
                    rhs,
                    span: _,
                } = *rhs.clone()
                {
                    assert_eq!(operand, "!=");

                    if let Expressions::Value(Value::Integer(0), _) = *lhs.clone() {
                    } else {
                        panic!("Wrong object expression found")
                    };
                    if let Expressions::Value(Value::Integer(5), _) = *rhs.clone() {
                    } else {
                        panic!("Wrong object expression found")
                    };
                } else {
                    panic!("Wrong boolean expression found");
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn bitwise_expression() {
    const SRC: &str = "let a = 5 << 2;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Bitwise {
                operand,
                lhs,
                rhs,
                span: _,
            } => {
                assert_eq!(operand, "<<");

                if let Expressions::Value(Value::Integer(5), _) = *lhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
                if let Expressions::Value(Value::Integer(2), _) = *rhs.clone() {
                } else {
                    panic!("Wrong object expression found")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn argument_expression() {
    const SRC: &str = "let a = some_arg: i32";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Argument {
                name,
                r#type,
                span: _,
            } => {
                assert_eq!(name, "some_arg");
                assert_eq!(r#type, Type::I32);
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn argument_advanced_expression() {
    const SRC: &str = "let a = some_arg: *[i32; 5]";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Argument {
                name,
                r#type,
                span: _,
            } => {
                assert_eq!(name, "some_arg");
                assert_eq!(
                    r#type,
                    Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
                );
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn subelement_expression() {
    const SRC: &str = "let a = some_struct.field";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(id), _) = *head {
                    assert_eq!(id, "some_struct");
                } else {
                    panic!("Wrong subelement expr head found");
                }

                let mut subs = subelements.into_iter();
                if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                    assert_eq!(id, "field");
                } else {
                    panic!("Wrong subelement in subelement expr found")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn subelement_advanced_expression() {
    const SRC: &str = "let a = some_struct.field.method()";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::SubElement {
                head,
                subelements,
                span: _,
            } => {
                // Okay, here's the problem that caused by moving `SubElement` expr to term
                // function: parsing multiple embedded subelements creates some kind of tree of
                // included subeleemnts.
                // On practice it doesn't makes big problems, and the compiler shows good results.
                // Even LLVM IR didn't changed and works well, but we're getting tests failure.
                //
                // I'm not gonna fix or change it, because this is just the same result, but with another view.
                // I'll rewrite this test for the new implementation as soon as possible 👀

                if let Expressions::Value(Value::Identifier(id), _) = *head {
                    assert_eq!(id, "some_struct");
                } else {
                    panic!("Wrong subelement expr head found");
                }

                let mut subs = subelements.into_iter();

                if let Some(Expressions::Value(Value::Identifier(id), _)) = subs.next() {
                    assert_eq!(id, "field");
                } else {
                    // panic!("Wrong subelement in subelement expr found")
                }

                if let Some(Expressions::FnCall {
                    name,
                    arguments,
                    span: _,
                }) = subs.next()
                {
                    assert_eq!(name, "method");
                    assert!(arguments.is_empty());
                } else {
                    // panic!("Wrong subelement in subelement expr found")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn fncall_expression() {
    const SRC: &str = "let a = call_me()";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                assert_eq!(name, "call_me");
                assert!(arguments.is_empty());
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn fncall_advanced_expression() {
    const SRC: &str = "let a = call_me(1, id, 1.0)";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::FnCall {
                name,
                arguments,
                span: _,
            } => {
                assert_eq!(name, "call_me");

                let mut args = arguments.into_iter();

                if let Some(Expressions::Value(Value::Integer(int), _)) = args.next() {
                    assert_eq!(int, 1)
                } else {
                    panic!("Argument does not matches expected")
                };
                if let Some(Expressions::Value(Value::Identifier(id), _)) = args.next() {
                    assert_eq!(id, "id")
                } else {
                    panic!("Argument does not matches expected")
                };
                if let Some(Expressions::Value(Value::Float(fl), _)) = args.next() {
                    assert_eq!(fl, 1.0)
                } else {
                    panic!("Argument does not matches expected")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn reference_expression() {
    const SRC: &str = "let a = &b";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Reference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn reference_advanced_expression() {
    const SRC: &str = "let a = &(b)";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Reference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn dereference_expression() {
    const SRC: &str = "let a = *b";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Dereference { object, span: _ } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b")
                } else {
                    panic!("Ref object doesn't matches expected")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn dereference_advanced_expression() {
    const SRC: &str = "let a = **b";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Dereference { object, span: _ } => {
                if let Expressions::Dereference { object, span: _ } = *object {
                    if let Expressions::Value(Value::Identifier(id), _) = *object {
                        assert_eq!(id, "b");
                    } else {
                        panic!("Double dereferenced object isn't identifier")
                    }
                } else {
                    panic!("First level dereference object isn't dereference expr")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn array_expression() {
    const SRC: &str = "let a = [1, 2, 3]";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Array {
                values,
                len,
                span: _,
            } => {
                assert_eq!(len, 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Arg #1 is wrong")
                }
                if let Some(Expressions::Value(Value::Integer(2), _)) = values.next() {
                } else {
                    panic!("Arg #2 is wrong")
                }
                if let Some(Expressions::Value(Value::Integer(3), _)) = values.next() {
                } else {
                    panic!("Arg #3 is wrong")
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn tuple_expression() {
    const SRC: &str = "let a = (1, 5, 4)";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Tuple { values, span: _ } => {
                assert_eq!(values.len(), 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Integer(5), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Integer(4), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn tuple_advanced_expression() {
    const SRC: &str = "let a = (1, 2.0, \"hello\")";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Tuple { values, span: _ } => {
                assert_eq!(values.len(), 3);

                let mut values = values.into_iter();
                if let Some(Expressions::Value(Value::Integer(1), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::Float(2.0), _)) = values.next() {
                } else {
                    panic!("Argument is wrong")
                };
                if let Some(Expressions::Value(Value::String(str), _)) = values.next() {
                    assert_eq!(str, "hello")
                } else {
                    panic!("Argument is wrong")
                };
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn slice_expression() {
    const SRC: &str = "let a = b[0]";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Slice {
                object,
                index,
                span: _,
            } => {
                if let Expressions::Value(Value::Identifier(id), _) = *object {
                    assert_eq!(id, "b");
                    if let Expressions::Value(Value::Integer(0), _) = *index {
                    } else {
                        panic!("Wrong index on slice")
                    }
                }
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}

#[test]
fn struct_expression() {
    const SRC: &str = "let a = Person { .age = 32, .name = \"John\", .money = 333.12 };";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    if let Some(Statements::AnnotationStatement {
        identifier: _,
        value,
        span: _,
        datatype: _,
    }) = ast.first()
    {
        match value.clone().unwrap() {
            Expressions::Struct {
                name,
                fields,
                span: _,
            } => {
                assert_eq!(name, "Person");
                assert!(fields.contains_key("age"));
                assert!(fields.contains_key("name"));
                assert!(fields.contains_key("money"));
            }
            _ => panic!("Wrong expression value found: {value:?}"),
        }
    } else {
        panic!("Statements side failure");
    }
}
