#![allow(unused)]

use crate::{
    scope::Scope,
    error::{SemanticError, SemanticWarning}
};
use deen_parser::{statements::Statements, expressions::Expressions, value::Value, types::Type};

mod error;
mod scope;
mod typechecker;

type SemanticOk = Vec<SemanticWarning>;
type SemanticErr = (Vec<SemanticError>, Vec<SemanticWarning>);

pub struct Analyzer {
    scope: Scope,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            scope: Scope::new(),

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
            Expressions::Binary { operand, lhs, rhs, span } => todo!(),
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
}
