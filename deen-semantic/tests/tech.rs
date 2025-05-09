use deen_parser::types::Type;
use deen_semantic::Analyzer;

#[test]
fn is_integer() {
    assert!(Analyzer::is_integer(&Type::I8));
    assert!(Analyzer::is_integer(&Type::I16));
    assert!(Analyzer::is_integer(&Type::I32));
    assert!(Analyzer::is_integer(&Type::I64));
    assert!(Analyzer::is_integer(&Type::U8));
    assert!(Analyzer::is_integer(&Type::U16));
    assert!(Analyzer::is_integer(&Type::U32));
    assert!(Analyzer::is_integer(&Type::U64));
    assert!(Analyzer::is_integer(&Type::USIZE));
}

#[test]
fn is_unsigned_integer() {
    assert!(Analyzer::is_unsigned_integer(&Type::U8));
    assert!(Analyzer::is_unsigned_integer(&Type::U16));
    assert!(Analyzer::is_unsigned_integer(&Type::U32));
    assert!(Analyzer::is_unsigned_integer(&Type::U64));
    assert!(Analyzer::is_unsigned_integer(&Type::USIZE));
}

#[test]
fn unsigned_to_signed_integer() {
    assert_eq!(Type::I8, Analyzer::unsigned_to_signed_integer(&Type::U8));
    assert_eq!(Type::I16, Analyzer::unsigned_to_signed_integer(&Type::U16));
    assert_eq!(Type::I32, Analyzer::unsigned_to_signed_integer(&Type::U32));
    assert_eq!(Type::I64, Analyzer::unsigned_to_signed_integer(&Type::U64));
    assert_eq!(
        Type::I64,
        Analyzer::unsigned_to_signed_integer(&Type::USIZE)
    );
}

#[test]
fn integer_order() {
    assert!(Analyzer::integer_order(&Type::I8) < Analyzer::integer_order(&Type::I16));
    assert!(Analyzer::integer_order(&Type::I16) < Analyzer::integer_order(&Type::I32));
    assert!(Analyzer::integer_order(&Type::I32) < Analyzer::integer_order(&Type::I64));
    assert!(Analyzer::integer_order(&Type::I64) < Analyzer::integer_order(&Type::USIZE));

    assert!(Analyzer::integer_order(&Type::I8) == Analyzer::integer_order(&Type::U8));
    assert!(Analyzer::integer_order(&Type::I16) == Analyzer::integer_order(&Type::U16));
    assert!(Analyzer::integer_order(&Type::I32) == Analyzer::integer_order(&Type::U32));
    assert!(Analyzer::integer_order(&Type::I64) == Analyzer::integer_order(&Type::U64));
}

#[test]
fn is_float() {
    assert!(Analyzer::is_float(&Type::F32));
    assert!(Analyzer::is_float(&Type::F64));
}

#[test]
fn float_order() {
    assert!(Analyzer::float_order(&Type::F32) < Analyzer::float_order(&Type::F64));
}
