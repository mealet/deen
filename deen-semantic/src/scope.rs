use std::collections::HashMap;
use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Scope {
    pub expected: Type,
    pub returned: Type,
    pub parent: Option<Box<Scope>>,

    variables: HashMap<String, Variable>,
    functions: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub datatype: Type,
    pub initialized: bool
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            expected: Type::Void,
            returned: Type::Void,

            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: None
        }
    }

    #[inline]
    pub fn add_var(&mut self, name: String, datatype: Type, initialized: bool) {
        self.variables.insert(name, Variable { datatype, initialized });
    }

    #[inline]
    pub fn get_var(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).cloned().or_else(|| {
            self.parent.as_ref().and_then(|parent| parent.get_var(name))
        })
    }

    #[inline]
    pub fn add_fn(&mut self, name: String, return_type: Type) -> Result<(), String> {
        if self.functions.contains_key(&name) {
            return Err(format!("Function `{}` already declared", name));
        }
        self.functions.insert(name, return_type);
        Ok(())
    }

    #[inline]
    pub fn get_fn(&self, name: &str) -> Option<Type> {
        self.functions.get(name).cloned().or_else(|| {
            self.parent.as_ref().and_then(|parent| parent.get_fn(name))
        })
    }
}
