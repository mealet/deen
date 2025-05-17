use deen_parser::{statements::Statements, types::Type};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub imports: HashMap<String, Import>
}

#[derive(Debug, Clone, Default)]
pub struct Import {
    pub functions: HashMap<String, Type>,
    pub structs: HashMap<String, Type>,
    pub enums: HashMap<String, Type>,

    pub embedded_symtable: SymbolTable,
    pub source: String,
    pub ast: Vec<Statements>,
}

impl Import {
    pub fn new(ast: Vec<Statements>, source: &str) -> Self {
        Self {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),

            embedded_symtable: SymbolTable::default(),
            source: source.to_owned(),
            ast,
        }
    }

    pub fn add_fn(&mut self, name: String, typ: Type) {
        self.functions.insert(name, typ);
    }

    pub fn add_struct(&mut self, name: String, typ: Type) {
        self.structs.insert(name, typ);
    }

    pub fn add_enum(&mut self, name: String, typ: Type) {
        self.enums.insert(name, typ);
    }
}
