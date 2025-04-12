use std::collections::HashMap;
use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Scope {
    pub expected: Type,
    pub returned: Type,
    pub parent: Option<Box<Scope>>,

    pub is_loop: bool,
    pub is_main: bool,

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
            parent: None,

            is_loop: false,
            is_main: false,
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
    pub fn set_init_var(&mut self, name: &str, value: bool) -> Result<(), String> {
        match self.get_var(name) {
            Some(mut var) => {
                var.initialized = true;
                self.add_var(name.to_owned(), var.datatype, var.initialized);

                Ok(())
            },
            None => Err(format!("Variable \"{}\" is not defined her", name))
        }
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
