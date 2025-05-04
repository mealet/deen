use deen_lexer::Lexer;
use deen_parser::{Parser, expressions::Expressions, statements::Statements, value::Value};

#[test]
fn basic_values() {
    const SRC: &str = "123; 5.0; 'a'; \"some\"; true";
    const FILENAME: &str = "test.dn";

    let mut lexer = Lexer::new(SRC, "test.dn");
    let (tokens, _) = lexer.tokenize().unwrap();

    let mut parser = Parser::new(tokens, SRC, FILENAME);
    let (ast, _) = parser.parse().unwrap();

    let mut ast_iter = ast.into_iter();

    if let Some(Statements::Expression(Expressions::Value(Value::Integer(_), _))) = ast_iter.next()
    {
    } else {
        panic!("Test failure for: {:?}", ast_iter.next().unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Float(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Char(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::String(_), _))) = ast_iter.next() {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }

    if let Some(Statements::Expression(Expressions::Value(Value::Boolean(_), _))) = ast_iter.next()
    {
    } else {
        panic!("Test failure for: {:?}", ast_iter.nth_back(0).unwrap())
    }
}
