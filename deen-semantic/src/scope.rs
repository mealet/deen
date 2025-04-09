use std::collections::HashMap;
use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<String, Type>,
    functions: HashMap<String, Type>,
    parent: Option<Box<Scope>>
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent: None
        }
    }

    #[inline]
    pub fn add_var(&mut self, name: String, datatype: Type) {
        self.variables.insert(name, datatype);       
    }

    #[inline]
    pub fn get_var(&self, name: String) -> Option<Type> {
        self.variables.get(&name).cloned().or_else(|| {
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
    pub fn get_fn(&self, name: String) -> Option<Type> {
        self.functions.get(&name).cloned().or_else(|| {
            self.parent.as_ref().and_then(|parent| parent.get_fn(name))
        })
    }
}
