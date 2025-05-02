use deen_lexer::Lexer;
use deen_parser::{
    Parser,
    statements::Statements,
    expressions::Expressions,
    value::Value,
    types::Type
};


#[test]
fn assign_statement_test() {
    const SRC: &str = "some_var = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AssignStatement { identifier, value, span: _ }) => {
            assert_eq!(identifier, "some_var");
            if let Expressions::Value(Value::Integer(5), _) = value {} else { panic!("Wrong value expr found") };
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn binary_assign_statement_test() {
    const SRC: &str = "some_var += 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::BinaryAssignStatement { identifier, operand, value, span: _ }) => {
            assert_eq!(identifier, "some_var");
            assert_eq!(operand, "+");

            if let Expressions::Value(Value::Integer(5), _) = value {} else { panic!("Wrong value expr found") };
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn deref_assign_statement_test() {
    const SRC: &str = "*ptr = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::DerefAssignStatement { identifier, value, span: _ }) => {
            assert_eq!(identifier, "ptr");

            if let Expressions::Value(Value::Integer(5), _) = value {} else { panic!("Wrong value expr found") };
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn slice_assign_statement_test() {
    const SRC: &str = "list[0] = 5;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::SliceAssignStatement { identifier, index, value, span: _ }) => {
            assert_eq!(identifier, "list");

            if let Expressions::Value(Value::Integer(0), _) = index {} else { panic!("Wrong index expr parsed") }
            if let Expressions::Value(Value::Integer(5), _) = value {} else { panic!("Wrong value expr parsed") };
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn field_assign_statement_test() {
    const SRC: &str = "some_struct.field = 12";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FieldAssignStatement { object, value, span: _ }) => {
            if let Expressions::SubElement { head, subelements, span: _ } = object {
                if let Expressions::Value(Value::Identifier(id), _) = *head.clone() { assert_eq!(id, "some_struct") } else { panic!("Wrong head expr found") };
                if let Some(Expressions::Value(Value::Identifier(field), _)) = subelements.first() {
                    assert_eq!(field, "field");
                } else { panic!("Wrong subelement expr found") }
            } else { panic!("Wrong value expr parsed") };
            if let Expressions::Value(Value::Integer(12), _) = value {} else { panic!("Wrong value expr parsed") };
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn annotation_statement_test() {
    const SRC: &str = "let var;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement { identifier, datatype, value, span: _ }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_none());
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn annotation_statement_with_type_test() {
    const SRC: &str = "let var: i32;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement { identifier, datatype, value, span: _ }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_none());

            assert_eq!(datatype.clone().unwrap(), Type::I32);
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn annotation_statement_with_value_test() {
    const SRC: &str = "let var = 15;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement { identifier, datatype, value, span: _ }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_none());
            assert!(value.is_some());

            if let Some(Expressions::Value(Value::Integer(15), _)) = value {} else { panic!("Wrong value expr parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn annotation_statement_full_test() {
    const SRC: &str = "let var: usize = 15;";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::AnnotationStatement { identifier, datatype, value, span: _ }) => {
            assert_eq!(identifier, "var");
            assert!(datatype.is_some());
            assert!(value.is_some());

            assert_eq!(datatype.clone().unwrap(), Type::USIZE);
            if let Some(Expressions::Value(Value::Integer(15), _)) = value {} else { panic!("Wrong value expr parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn function_define_statement_test() {
    const SRC: &str = "fn foo() {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span: _ }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn function_define_statement_with_type_test() {
    const SRC: &str = "fn foo() usize {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span: _ }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(arguments.is_empty());
            assert!(block.is_empty());
            assert!(!public);
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn function_define_statement_with_args_test() {
    const SRC: &str = "fn foo(a: i32, b: u64) usize {}";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span: _ }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::USIZE);
            assert!(block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else { panic!("Wrong argument expr parsed") }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else { panic!("Wrong argument expr parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn function_define_statement_with_block_test() {
    const SRC: &str = "fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span: _ }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(!public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else { panic!("Wrong argument expr parsed") }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else { panic!("Wrong argument expr parsed") }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {} else { panic!("Wrong value in statement block parsed") }
            } else { panic!("Wrong statement parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn function_define_statement_public_test() {
    const SRC: &str = "pub fn foo(a: i32, b: u64) { return 1; }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::FunctionDefineStatement { name, datatype, arguments, block, public, span: _ }) => {
            assert_eq!(name, "foo");
            assert_eq!(datatype, &Type::Void);
            assert!(!block.is_empty());
            assert!(!arguments.is_empty());
            assert!(public);

            if let Some((argname, argtype)) = arguments.first() {
                assert_eq!(argname, "a");
                assert_eq!(argtype, &Type::I32);
            } else { panic!("Wrong argument expr parsed") }

            if let Some((argname, argtype)) = arguments.get(1) {
                assert_eq!(argname, "b");
                assert_eq!(argtype, &Type::U64);
            } else { panic!("Wrong argument expr parsed") }

            if let Some(Statements::ReturnStatement { value, span: _ }) = block.first() {
                if let Expressions::Value(Value::Integer(1), _) = value {} else { panic!("Wrong value in statement block parsed") }
            } else { panic!("Wrong statement parsed") }
        }
        _ => panic!("Wrong statement parsed")
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
        Some(Statements::FunctionCallStatement { name, arguments, span: _ }) => {
            assert_eq!(name, "foo");
            assert!(arguments.is_empty());
        }
        _ => panic!("Wrong statement parsed")
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
        Some(Statements::FunctionCallStatement { name, arguments, span: _ }) => {
            assert_eq!(name, "foo");
            assert!(!arguments.is_empty());

            if let Some(Expressions::Value(Value::Integer(1), _)) = arguments.first() {} else { panic!("Wrong #1 argument parsed") }
            if let Some(Expressions::Value(Value::Integer(2), _)) = arguments.get(1) {} else { panic!("Wrong #2 argument parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn struct_define_statement() {
    const SRC: &str = "struct Person { name: string, age: u8 }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement { name, fields, functions, public, span: _ }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(functions.is_empty());

            if let Some(Type::String) = fields.get("name") {} else { panic!("Wrong argument parsed") }
            if let Some(Type::U8) = fields.get("age") {} else { panic!("Wrong argument parsed") }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn struct_define_with_fn_statement() {
    const SRC: &str = "struct Person { name: string, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement { name, fields, functions, public, span: _ }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!public);
            assert!(!functions.is_empty());

            if let Some(Type::String) = fields.get("name") {} else { panic!("Wrong argument parsed") }
            if let Some(Type::U8) = fields.get("age") {} else { panic!("Wrong argument parsed") }

            if let Some(Statements::FunctionDefineStatement { name, datatype, arguments: _, block: _, public: _, span: _ }) = functions.get("foo") {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
            }
        }
        _ => panic!("Wrong statement parsed")
    }
}

#[test]
fn struct_define_public_statement() {
    const SRC: &str = "pub struct Person { name: string, age: u8, fn foo() {} }";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    match ast.first() {
        Some(Statements::StructDefineStatement { name, fields, functions, public, span: _ }) => {
            assert_eq!(name, "Person");
            assert!(!fields.is_empty());
            assert!(!functions.is_empty());
            assert!(public);

            if let Some(Type::String) = fields.get("name") {} else { panic!("Wrong argument parsed") }
            if let Some(Type::U8) = fields.get("age") {} else { panic!("Wrong argument parsed") }

            if let Some(Statements::FunctionDefineStatement { name, datatype, arguments: _, block: _, public: _, span: _ }) = functions.get("foo") {
                assert_eq!(name, "foo");
                assert_eq!(datatype, &Type::Void);
            }
        }
        _ => panic!("Wrong statement parsed")
    }
}
