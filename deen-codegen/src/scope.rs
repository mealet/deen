use crate::{
    CodeGen, enumeration::Enumeration, function::Function, structure::Structure, variable::Variable,
};
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

impl<'ctx> Default for Scope<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> Scope<'ctx> {
    pub fn new() -> Self {
        Self {
            parent: None,

            variables: HashMap::new(),
            functions: HashMap::new(),
            structures: HashMap::new(),
            enumerations: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    // variables
    pub fn set_variable(&mut self, id: impl std::convert::AsRef<str>, object: Variable<'ctx>) {
        self.variables.insert(id.as_ref().into(), object);
    }

    pub fn get_variable(&self, id: impl std::convert::AsRef<str>) -> Option<Variable<'ctx>> {
        self.variables.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(id))
        })
    }

    pub fn get_mut_variable(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Variable<'ctx>> {
        self.variables.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_variable(id))
        })
    }

    pub fn remove_variable(&mut self, id: impl std::convert::AsRef<str>) -> Option<Variable<'ctx>> {
        self.variables.remove(id.as_ref())
    }

    // functions
    pub fn set_function(&mut self, id: impl std::convert::AsRef<str>, object: Function<'ctx>) {
        self.functions.insert(id.as_ref().into(), object);
    }

    pub fn get_function(&self, id: impl std::convert::AsRef<str>) -> Option<Function<'ctx>> {
        self.functions.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_function(id))
        })
    }

    pub fn get_mut_function(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Function<'ctx>> {
        self.functions.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_function(id))
        })
    }

    // structures
    pub fn set_struct(&mut self, id: impl std::convert::AsRef<str>, object: Structure<'ctx>) {
        self.structures.insert(id.as_ref().into(), object);
    }

    pub fn get_struct(&self, id: impl std::convert::AsRef<str>) -> Option<Structure<'ctx>> {
        self.structures.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_struct(id))
        })
    }

    pub fn get_mut_struct(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Structure<'ctx>> {
        self.structures.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_struct(id))
        })
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

    pub fn get_mut_enum(
        &mut self,
        id: impl std::convert::AsRef<str>,
    ) -> Option<&mut Enumeration<'ctx>> {
        self.enumerations.get_mut(id.as_ref()).or_else(|| {
            self.parent
                .as_mut()
                .and_then(|parent| parent.get_mut_enum(id))
        })
    }

    // typedefs
    pub fn set_typedef(&mut self, id: impl std::convert::AsRef<str>, object: Type) {
        self.typedefs.insert(id.as_ref().into(), object);
    }

    pub fn get_typedef(&self, id: impl std::convert::AsRef<str>) -> Option<Type> {
        self.typedefs.get(id.as_ref()).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_typedef(id))
        })
    }

    // tech
    pub fn stricted_variables(&self) -> HashMap<String, Variable<'ctx>> {
        self.variables.to_owned()
    }

    pub fn stricted_functions(&self) -> HashMap<String, Function<'ctx>> {
        self.functions.to_owned()
    }

    pub fn stricted_structs(&self) -> HashMap<String, Structure<'ctx>> {
        self.structures.to_owned()
    }

    pub fn stricted_enums(&self) -> HashMap<String, Enumeration<'ctx>> {
        self.enumerations.to_owned()
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

    pub fn cleanup_variables(&mut self) {
        let scope_variables = self.scope.stricted_variables();

        scope_variables.into_iter().for_each(|(name, var)| {
            match var.datatype.clone() {
                Type::Alias(alias) => {
                    if matches!(self.get_alias_type(var.datatype.clone(), None), Some("struct")) && name != "self" && !var.no_drop {
                        let structure = self.scope.get_struct(&alias).unwrap();

                        if let Some(destructor) = structure.functions.get("drop") {
                            let called = self.scope.get_function(format!("struct_{alias}__drop")).unwrap().called;
                            if destructor.arguments == vec![Type::Pointer(Box::new(var.datatype))] && !called {
                                let _ = self.builder.build_call(
                                    destructor.value,
                                    &[
                                        var.ptr.into()
                                    ],
                                    ""
                                );
                            }
                        }
                    }
                }
                Type::Struct(_, _) => {
                    panic!("Something went wrong, developer got brainrot, please report that on Github Issue")
                },
                _ => {}
            }
        })
    }

    pub fn exit_scope_raw(&mut self) {
        if let Some(parent) = self.scope.parent.to_owned() {
            self.scope = parent;
        }
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scope.parent.to_owned() {
            let insert_block = self.builder.get_insert_block().unwrap();

            if let Some(instr) = insert_block.get_terminator() {
                self.builder.position_before(&instr);
                self.cleanup_variables();

                self.builder.position_at_end(insert_block);
            } else {
                self.cleanup_variables();
            }

            self.scope = parent;
        }
    }
}
