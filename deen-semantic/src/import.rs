use deen_parser::{statements::Statements, types::Type};
use std::collections::HashMap;

#[derive(Debug, Clone)]
#[derive(Default)]
pub struct Import {
    pub functions: HashMap<String, Type>,
    pub ast: Vec<Statements>
}


impl Import {
    pub fn new(ast: Vec<Statements>) -> Self {
        Self {
            functions: HashMap::new(),
            ast
        }
    }

    pub fn add_fn(&mut self, name: String, typ: Type) {
        self.functions.insert(name, typ);
    }
}
