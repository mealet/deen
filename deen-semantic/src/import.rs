use deen_parser::types::Type;

#[derive(Debug, Clone)]
pub struct Import {
    pub functions: Vec<(String, Type)>
}

impl Default for Import {
    fn default() -> Self {
        Self {
            functions: Vec::new()
        }
    }
}

impl Import {
    pub fn add_fn(&mut self, function: (String, Type)) {
        self.functions.push(function);
    }
}
