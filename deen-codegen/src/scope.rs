use crate::{enumeration::Enumeration, function::Function, structure::Structure, variable::Variable, CodeGen};
use deen_parser::types::Type;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Scope<'ctx> {
    parent: Option<Box<Scope<'ctx>>>,

    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, Function<'ctx>>,
    structures: HashMap<String, Structure<'ctx>>,
    enumerations: HashMap<String, Enumeration<'ctx>>,
    typedefs: HashMap<String, Type>,
}

impl<'ctx> Scope<'ctx> {
    pub fn new() -> Self {
        Self {
            parent: None,

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enumerations: HashMap::new(),
            typedefs: HashMap::new()
        }
    }

    // variables
    pub fn set_variable(&mut self, id: impl std::convert::AsRef<str>, object: Variable<'ctx>) {
        self.variables.insert(id.as_ref().into(), object);
    }

    pub fn get_variable(&self, id: impl std::convert::AsRef<str>) -> Option<Variable<'ctx>> {
        self.variables
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_variable(id)))
    }

    pub fn remove_variable(&mut self, id: impl std::convert::AsRef<str>) -> Option<Variable<'ctx>> {
        self.variables.remove(id.as_ref())
    }

    // functions 
    pub fn set_function(&mut self, id: impl std::convert::AsRef<str>, object: Function<'ctx>) {
        self.functions.insert(id.as_ref().into(), object);
    }

    pub fn get_function(&self, id: impl std::convert::AsRef<str>) -> Option<Function<'ctx>> {
        self.functions
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_function(id)))
    }

    // structures
    pub fn set_struct(&mut self, id: impl std::convert::AsRef<str>, object: Structure<'ctx>) {
        self.structures.insert(id.as_ref().into(), object);
    }

    pub fn get_struct(&self, id: impl std::convert::AsRef<str>) -> Option<Structure<'ctx>> {
        self.structures
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_struct(id)))
    }

    // enums
    pub fn set_enum(&mut self, id: impl std::convert::AsRef<str>, object: Enumeration<'ctx>) {
        self.enumerations.insert(id.as_ref().into(), object);
    }

    pub fn get_enum(&self, id: impl std::convert::AsRef<str>) -> Option<Enumeration<'ctx>> {
        self.enumerations
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_enum(id)))
    }

    // typedefs
    pub fn set_typedef(&mut self, id: impl std::convert::AsRef<str>, object: Type) {
        self.typedefs.insert(id.as_ref().into(), object);
    }

    pub fn get_typedef(&self, id: impl std::convert::AsRef<str>) -> Option<Type> {
        self.typedefs
            .get(id.as_ref())
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|parent| parent.get_typedef(id)))
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn enter_scope(&mut self, mut scope: Scope<'ctx>) {
        let parent = self.scope.to_owned();
        scope.parent = Some(parent);

        self.scope = Box::new(scope);
    }

    pub fn enter_new_scope(&mut self) {
        let new_scope = Scope::new();
        self.enter_scope(new_scope);
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scope.parent.to_owned() {
            self.scope = parent;
        }
    }
}
