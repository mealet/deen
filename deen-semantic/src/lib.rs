#![allow(unused)]

use crate::{
    scope::Scope,
    error::{SemanticError, SemanticWarning}
};
use deen_parser::{statements::Statements, expressions::Expressions, value::Value, types::Type};

mod error;
mod scope;
mod typechecker;

pub struct Analyzer {
    scope: Scope,

    errors: Vec<SemanticError>,
    warnings: Vec<SemanticWarning>,
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            scope: Scope::new(),

            errors: Vec::new(),
            warnings: Vec::new()
        }
    }

    pub fn analyze(&mut self, ast: &[Statements]) -> Result<Vec<SemanticWarning>, (Vec<SemanticError>, Vec<SemanticWarning>)> {
        for statement in ast {
            self.visit_statement(statement);
        }

        Ok(self.warnings.clone())
    }
}

impl Analyzer {
    fn visit_statement(&mut self, statement: &Statements) {
        match statement {
            Statements::AssignStatement { identifier, value, span } => {}
            _ => {}
        }
    }
}
