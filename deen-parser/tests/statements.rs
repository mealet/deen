use deen_lexer::Lexer;
use deen_parser::{
    Parser, expressions::Expressions, statements::Statements, types::Type, value::Value,
};

#[test]
fn assign_statement() {
    const SRC: &str = "some_var = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(identifier, "some_var");
            } else {
                panic!("Wrong obj expr parsed")
            }

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn binary_assign_statement() {
    const SRC: &str = "some_var += 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::BinaryAssignStatement {
            object,
            operand,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(identifier, "some_var");
            }

            assert_eq!(operand, "+");

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn deref_assign_statement() {
    const SRC: &str = "*ptr = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::DerefAssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(identifier, "ptr");
            }

            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr found")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn slice_assign_statement() {
    const SRC: &str = "list[0] = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::SliceAssignStatement {
            object,
            index,
            value,
            span: _,
        }) => {
            if let Expressions::Value(Value::Identifier(identifier), _) = object {
                assert_eq!(identifier, "list");
            }

            if let Expressions::Value(Value::Integer(0), _) = index {
            } else {
                panic!("Wrong index expr parsed")
            }
            if let Expressions::Value(Value::Integer(5), _) = value {
            } else {
                panic!("Wrong value expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn field_assign_statement() {
    const SRC: &str = "some_struct.field = 12";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FieldAssignStatement {
            object,
            value,
            span: _,
        }) => {
            if let Expressions::SubElement {
                head,
                subelements,
                span: _,
            } = object
            {
                if let Expressions::Value(Value::Identifier(id), _) = *head.clone() {
                    assert_eq!(id, "some_struct")
                } else {
                    panic!("Wrong head expr found")
                };
                if let Some(Expressions::Value(Value::Identifier(field), _)) = subelements.first() {
                    assert_eq!(field, "field");
                } else {
                    panic!("Wrong subelement expr found")
                }
            } else {
                panic!("Wrong value expr parsed")
            };
            if let Expressions::Value(Value::Integer(12), _) = value {
            } else {
                panic!("Wrong value expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement() {
    const SRC: &str = "let var;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_none());
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_with_type() {
    const SRC: &str = "let var: i32;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_none());

            assert_eq!(datatype.clone().unwrap(), Type::I32);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_with_value() {
    const SRC: &str = "let var = 15;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_some());

            if let Some(Expressions::Value(Value::Integer(15), _)) = value {
            } else {
                panic!("Wrong value expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn annotation_statement_full() {
    const SRC: &str = "let var: usize = 15;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement {
            identifier,
            datatype,
            value,
            span: _,
        }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_some());

            assert_eq!(datatype.clone().unwrap(), Type::USIZE);
            if let Some(Expressions::Value(Value::Integer(15), _)) = value {
            } else {
                panic!("Wrong value expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement() {
    const SRC: &str = "fn foo() {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_type() {
    const SRC: &str = "fn foo() usize {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_args() {
    const SRC: &str = "fn foo(a: i32, b: u64) usize {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_with_block() {
    const SRC: &str = "fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {
                } else {
                    panic!("Wrong value in statement block parsed")
                }
            } else {
                panic!("Wrong statement parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_define_statement_public() {
    const SRC: &str = "pub fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement {
            name,
            datatype,
            arguments,
            block,
            public,
            span: _,
            header_span: _,
        }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else {
                panic!("Wrong argument expr parsed")
            }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {
                } else {
                    panic!("Wrong value in statement block parsed")
                }
            } else {
                panic!("Wrong statement parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_call_statement() {
    const SRC: &str = "foo()";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionCallStatement {
            name,
            arguments,
            span: _,
        }) => {
            assert_eq!(name, "foo");
            assert!(arguments.is_empty());
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn function_call_advanced_statement() {
    const SRC: &str = "foo(1, 2)";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionCallStatement {
            name,
            arguments,
            span: _,
        }) => {
            assert_eq!(name, "foo");
            assert!(!arguments.is_empty());

            if let Some(Expressions::Value(Value::Integer(1), _)) = arguments.first() {
            } else {
                panic!("Wrong #1 argument parsed")
            }
            if let Some(Expressions::Value(Value::Integer(2), _)) = arguments.get(1) {
            } else {
                panic!("Wrong #2 argument parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_statement() {
    const SRC: &str = "struct Person { name: *char, age: u8 }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(functions.is_empty());

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_with_fn_statement() {
    const SRC: &str = "struct Person { name: *char, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(!functions.is_empty());

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn struct_define_public_statement() {
    const SRC: &str = "pub struct Person { name: *char, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!functions.is_empty());
            assert!(public);

            if let Some(Type::Pointer(_)) = fields.get("name") {
            } else {
                panic!("Wrong argument parsed")
            }
            if let Some(Type::U8) = fields.get("age") {
            } else {
                panic!("Wrong argument parsed")
            }

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
            } else {
                panic!("Wrong function define stmt parsed!")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_statement() {
    const SRC: &str = "enum ABC { A, B, C }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "ABC");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(functions.is_empty());

            if let Some("A") = fields.first().map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("B") = fields.get(1).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("C") = fields.get(2).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_with_fn_statement() {
    const SRC: &str = "enum ABC { A, B, C, fn foo() {} }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "ABC");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(!functions.is_empty());

            if let Some("A") = fields.first().map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("B") = fields.get(1).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("C") = fields.get(2).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };

            if let Some(Statements::FunctionDefineStatement {
                name,
                datatype,
                arguments: _,
                block: _,
                public: _,
                span: _,
                header_span: _,
            }) = functions.get("foo")
            {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
            } else {
                panic!("Wrong function define stmt parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn enum_define_pub_statement() {
    const SRC: &str = "pub enum ABC { A, B, C }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::EnumDefineStatement {
            name,
            fields,
            functions,
            public,
            span: _,
        }) => {
            assert_eq!(name, "ABC");
            assert!(!fields.is_empty());
            assert!(public);
            assert!(functions.is_empty());

            if let Some("A") = fields.first().map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("B") = fields.get(1).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
            if let Some("C") = fields.get(2).map(|x| x.as_str()) {
            } else {
                panic!("Wrong field parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn typedef_statement() {
    const SRC: &str = "typedef int i32";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::TypedefStatement {
            alias,
            datatype,
            span: _,
        }) => {
            assert_eq!(alias, "int");
            assert_eq!(datatype, &Type::I32);
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn typedef_advanced_statement() {
    const SRC: &str = "typedef array_ptr *[i32; 5]";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::TypedefStatement {
            alias,
            datatype,
            span: _,
        }) => {
            assert_eq!(alias, "array_ptr");
            assert_eq!(
                datatype,
                &Type::Pointer(Box::new(Type::Array(Box::new(Type::I32), 5)))
            );
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn if_statement() {
    const SRC: &str = "if true {};";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::IfStatement {
            condition,
            then_block,
            else_block,
            span: _,
        }) => {
            assert!(else_block.is_none());
            assert!(then_block.is_empty());

            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn if_else_statement() {
    const SRC: &str = "if true {} else {};";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::IfStatement {
            condition,
            then_block,
            else_block,
            span: _,
        }) => {
            assert!(else_block.is_some());
            assert!(then_block.is_empty());

            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn while_statement() {
    const SRC: &str = "while true {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::WhileStatement {
            condition,
            block: _,
            span: _,
        }) => {
            if let Expressions::Value(Value::Boolean(true), _) = condition {
            } else {
                panic!("Wrong condition expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn for_statement() {
    const SRC: &str = "for i = 5 {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::ForStatement {
            binding,
            iterator,
            block: _,
            span: _,
        }) => {
            assert_eq!(binding, "i");
            if let Expressions::Value(Value::Integer(5), _) = iterator {
            } else {
                panic!("Wrong iterator obj parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn import_statement() {
    const SRC: &str = "import \"module.dn\"";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::ImportStatement { path, span: _ }) => {
            if let Expressions::Value(Value::String(str), _) = path {
                assert_eq!(str, "module.dn")
            } else {
                panic!("Wrong import object expr parsed")
            };
        }
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn break_statement() {
    const SRC: &str = "break";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::BreakStatements { span: _ }) => {}
        _ => panic!("Wrong statement parsed"),
    }
}

#[test]
fn return_statement() {
    const SRC: &str = "return 15;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::ReturnStatement { value, span: _ }) => {
            if let Expressions::Value(Value::Integer(15), _) = value {
            } else {
                panic!("Wrong return expr parsed")
            }
        }
        _ => panic!("Wrong statement parsed"),
    }
}
