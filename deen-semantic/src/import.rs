use deen_parser::{statements::Statements, types::Type};

#[derive(Debug, Clone)]
#[derive(Default)]
pub struct Import {
    pub functions: Vec<(String, Type)>,
    pub ast: Vec<Statements>
}


impl Import {
    pub fn new(ast: Vec<Statements>) -> Self {
        Self {
            functions: Vec::new(),
            ast
        }
    }

    pub fn add_fn(&mut self, function: (String, Type)) {
        self.functions.push(function);
    }
}
