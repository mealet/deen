use deen_parser::{statements::Statements, types::Type};
use std::collections::HashMap;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub imports: HashMap<String, Import>,
}

#[derive(Debug, Clone)]
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

    pub fn get_struct(&self, name: impl std::convert::AsRef<str>) -> Option<Type> {
        self.structs.get(name.as_ref()).cloned()
    }

    pub fn get_enum(&self, name: impl std::convert::AsRef<str>) -> Option<Type> {
        self.enums.get(name.as_ref()).cloned()
    }

    pub fn get_fn(&self, name: impl std::convert::AsRef<str>) -> Option<Type> {
        self.functions.get(name.as_ref()).cloned()
    }
}

impl Default for Import {
    fn default() -> Self {
        Self::new(Vec::new(), "")
    }
}
