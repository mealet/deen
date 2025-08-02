use crate::{element::ScopeElement, error::{self, SemanticError}};
use miette::NamedSource;
use deen_parser::types::Type;
use std::collections::HashMap;

type UnusedVariable = (String, (usize, usize));

#[derive(Debug, Clone)]
pub struct Scope {
    pub expected: Type,
    pub returned: Type,
    pub parent: Option<Box<Scope>>,

    pub is_loop: bool,
    pub is_main: bool,

    pub variables: HashMap<String, Variable>,
    pub functions: HashMap<String, ScopeElement>,
    pub structures: HashMap<String, ScopeElement>,
    pub enums: HashMap<String, ScopeElement>,
    pub typedefs: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub datatype: Type,
    pub initialized: bool,
    pub span: (usize, usize),
    pub used: bool,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            expected: Type::Void,
            returned: Type::Void,

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enums: HashMap::new(),
            typedefs: HashMap::new(),

            parent: None,

            is_loop: false,
            is_main: false,
        }
    }

    #[inline]
    pub fn add_var(
        &mut self,
        name: String,
        datatype: Type,
        initialized: bool,
        span: (usize, usize),
    ) {
        if name == "_" {
            return;
        };
        self.variables.insert(
            name,
            Variable {
                datatype,
                initialized,
                used: false,
                span,
            },
        );
    }

    #[inline]
    pub fn get_var(&mut self, name: &str) -> Option<Variable> {
        if name == "_" {
            return Some(Variable {
                datatype: Type::Void,
                initialized: true,
                span: (0, 0),
                used: true,
            });
        }

        self.get_mut_var(name).map(|var| {
            var.used = true;
            var.clone()
        })
    }

    pub fn get_mut_var(&mut self, name: &str) -> Option<&mut Variable> {
        if name == "_" {
            return None;
        }

        let mut var_instance = self.variables.get_mut(name).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_var(name))
        });

        if let Some(var) = var_instance.as_mut() {
            var.used = true;
        }

        var_instance
    }

    #[inline]
    pub fn check_unused_variables(&self) -> Option<Vec<UnusedVariable>> {
        let mut unused = Vec::new();

        self.variables.iter().for_each(|(name, signature)| {
            if !signature.used {
                unused.push((name.clone(), signature.span));
            }
        });

        if unused.is_empty() {
            return None;
        }

        Some(unused)
    }

    #[inline]
    pub fn set_init_var(&mut self, name: &str, value: bool, source: NamedSource<String>, span: (usize, usize)) -> Result<(), SemanticError> {
        // format!("Variable \"{name}\" is not defined her")
        match self.get_mut_var(name) {
            Some(var) => {
                var.initialized = value;
                Ok(())
            }
            None => Err(
                SemanticError::UnresolvedName {
                    exception: format!("variable \"{name}\" is not defined here"),
                    help: format!("Verify provided identifier"),
                    src: source,
                    span: error::position_to_span(span)
                }
            ),
        }
    }

    #[inline]
    pub fn add_fn(&mut self, name: String, return_type: Type, public: bool) -> Result<(), String> {
        if self.functions.contains_key(&name) {
            return Err(format!("function `{name}` already declared"));
        }
        self.functions.insert(
            name.clone(),
            ScopeElement {
                datatype: return_type,
                public,
            },
        );
        Ok(())
    }

    #[inline]
    pub fn get_fn(&self, name: &str) -> Option<Type> {
        self.functions
            .get(name)
            .map(|elem| elem.datatype.clone())
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_fn(name)))
    }

    #[inline]
    #[allow(unused)]
    pub fn add_struct(
        &mut self,
        name: String,
        struct_type: Type,
        public: bool,
    ) -> Result<(), String> {
        if self.structures.contains_key(&name) {
            return Err(format!("structure `{name}` already declared"));
        }
        self.structures.insert(
            name.clone(),
            ScopeElement {
                datatype: struct_type,
                public,
            },
        );
        Ok(())
    }

    #[inline]
    pub fn get_struct(&self, name: &str) -> Option<Type> {
        self.structures
            .get(name)
            .map(|elem| elem.datatype.clone())
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.get_struct(name))
            })
    }

    #[inline]
    pub fn get_mut_struct(&mut self, name: &str) -> Option<&mut ScopeElement> {
        self.structures.get_mut(name).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_struct(name))
        })
    }

    #[inline]
    pub fn add_enum(&mut self, name: String, enum_type: Type, public: bool) -> Result<(), String> {
        if self.enums.contains_key(&name) {
            return Err(format!("enum `{name}` already declared"));
        }
        self.enums.insert(
            name.clone(),
            ScopeElement {
                datatype: enum_type,
                public,
            },
        );
        Ok(())
    }

    #[inline]
    pub fn get_enum(&self, name: &str) -> Option<Type> {
        self.enums
            .get(name)
            .map(|elem| elem.datatype.clone())
            .or_else(|| {
                self.parent
                    .as_ref()
                    .and_then(|parent| parent.get_enum(name))
            })
    }

    #[inline]
    pub fn add_typedef(&mut self, name: String, typ: Type) -> Result<(), String> {
        if self.typedefs.contains_key(&name) {
            return Err(format!("type `{name}` already declared"));
        }
        self.typedefs.insert(name, typ);
        Ok(())
    }

    #[inline]
    pub fn get_typedef(&self, name: &str) -> Option<Type> {
        self.typedefs.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_typedef(name))
        })
    }

    #[inline]
    pub fn is_loop(&self) -> bool {
        let mut cursor = self;
        while !cursor.is_loop && cursor.parent.is_some() {
            cursor = cursor.parent.as_ref().unwrap();
        }

        cursor.is_loop
    }
}
