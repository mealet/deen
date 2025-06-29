//! # Symbol Table
//! A data structure which contains intermediate units for compiler. <br/>
//! Wikipedia Explanation: <https://en.wikipedia.org/wiki/Symbol_table>

use deen_parser::{statements::Statements, types::Type};
use std::collections::HashMap;

/// Symbol Table Structure
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub imports: HashMap<String, Import>,
    pub included: HashMap<String, Include>,
}

/// User Import Instance
/// ### Usage
/// ```rust
/// use deen_parser::{types::Type, statements::Statements};
/// use deen_semantic::symtable::Import;
///
/// let mut import = Import::new(
///     vec![Statements::None],
///     "source code"
/// );
/// // or with default
/// let mut import = Import::default();
///
/// import.add_fn(String::from("func"), Type::Undefined);
/// assert!(import.get_fn("func").is_some());
/// ```
#[derive(Debug, Clone)]
pub struct Import {
    pub functions: HashMap<String, Type>,
    pub structs: HashMap<String, Type>,
    pub enums: HashMap<String, Type>,

    pub embedded_symtable: SymbolTable,
    pub source: String,
    pub ast: Vec<Statements>,
}

#[derive(Debug, Clone)]
pub struct Include {
    pub ast: Vec<Statements>
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
