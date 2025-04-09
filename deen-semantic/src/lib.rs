#![allow(unused)]

use crate::{
    scope::Scope,
    error::{SemanticError, SemanticWarning}
};
use deen_parser::{statements::Statements, expressions::Expressions, value::Value, types::Type};
use miette::NamedSource;

mod error;
mod scope;

type SemanticOk = Vec<SemanticWarning>;
type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

pub struct Analyzer {
    scope: Scope,
    source: NamedSource<String>,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,
}

impl Analyzer {
    pub fn new(src: &str, filename: &str) -> Self {
        Analyzer {
            scope: Scope::new(),
            source: NamedSource::new(src, filename.to_owned()),

            errors: Vec::new(),
            warnings: Vec::new()
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<SemanticOk, SemanticErr> {
        for statement in ast {
            self.visit_statement(statement);
        }

        Ok(self.warnings.clone())
    }

    fn error(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

        self.errors.push(
            error::SemanticError {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        );
    }

    #[allow(unused)]
    fn warning(&mut self, message: String, span: (usize, usize)) {
        let span = (span.0, span.1.wrapping_sub(span.0));

        self.warnings.push(
            error::SemanticWarning {
                message,
                span: span.into(),
                src: self.source.clone()
            }
        )
    }
}

impl Analyzer {
    fn visit_statement(&mut self, statement: &Statements) {
        match statement {
            Statements::AssignStatement { identifier, value, span } => {},
            Statements::BinaryAssignStatement { identifier, operand, value, span } => {},
            Statements::DerefAssignStatement { identifier, value, span } => {},
            Statements::SliceAssignStatement { identifier, index, value, span } => {},

            Statements::AnnotationStatement { identifier, datatype, value, span } => {}
            Statements::FunctionDefineStatement { name, datatype, arguments, block, span } => {},
            Statements::FunctionCallStatement { name, arguments, span } => {},
            
            Statements::IfStatement { condition, then_block, else_block, span } => {},
            Statements::WhileStatement { condition, block, span } => {},
            Statements::ForStatement { binding, iterator, block, span } => {},
            Statements::ImportStatement { path, span } => {},
            Statements::BreakStatements { span } => {},
            Statements::ReturnStatement { value, span } => {},
            
            Statements::Expression(expr) => {},
            Statements::None => unreachable!()
        }
    }

    fn visit_expression(&mut self, expr: &Expressions) -> Type {
        match expr {
            Expressions::Binary { operand, lhs, rhs, span } => {
                let left = self.visit_expression(lhs);
                let right = self.visit_expression(rhs);


            },
            Expressions::Unary { operand, object, span } => todo!(),
            Expressions::Boolean { operand, lhs, rhs, span } => todo!(),
            Expressions::Bitwise { operand, lhs, rhs, span } => todo!(),

            Expressions::Argument { name, r#type, span } => todo!(),
            Expressions::SubElement { parent, child, span } => todo!(),

            Expressions::FnCall { name, arguments, span } => todo!(),
            Expressions::Reference { object, span } => todo!(),
            Expressions::Dereference { object, span } => todo!(),

            Expressions::Array { values, len, span } => todo!(),
            Expressions::Slice { object, index, span } => todo!(),

            Expressions::Value(value, span) => todo!(),
            Expressions::None => unreachable!()
        }
    }

    fn visit_value(&mut self, value: Value, signed: bool) -> Result<Type, String> {
        match value {
            Value::Integer(int) => {
                if int > i32::MAX as i64 {
                    if !signed {
                        if int < 0 { return Err(String::from("Expected unsigned value but found signed")) }
                        return Ok(Type::U64);
                    }
                    return Ok(Type::I64);
                }
                Ok(Type::I32)
            },
            Value::Float(float) => {
                if float > f32::MAX as f64 {
                    return Ok(Type::F64);
                }
                Ok(Type::F32)
            },
            Value::Identifier(id) => {
                match self.scope.get_var(&id) {
                    Some(var) => {
                        if !var.initialized { return Err(format!("Variable `{}` isn't initalized", id)) }
                        Ok(var.datatype)
                    },
                    None => Err(format!("Variable `{}` is not defined here", id))
                }
            },
            Value::String(_) => Ok(Type::String),
            Value::Char(_) => Ok(Type::Char),
            Value::Boolean(_) => Ok(Type::Bool),
            Value::Keyword(_) => Ok(Type::Void),
        }
    }
}

impl Analyzer {
    fn is_integer(&self, typ: &Type) -> bool {
        [
            Type::I8, Type::I16,
            Type::I32, Type::I64,
            
            Type::U8, Type::U16,
            Type::U32, Type::U64
        ].contains(typ)
    }

    fn is_unsigned_integer(&self, typ: &Type) -> bool {
        [Type::U8, Type::U16, Type::U32, Type::U64].contains(typ)
    }

    fn is_float(&self, typ: &Type) -> bool {
        [Type::F32, Type::F64].contains(typ)
    }
}
