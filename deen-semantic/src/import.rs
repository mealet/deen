use deen_parser::types::Type;

#[derive(Debug, Clone)]
#[derive(Default)]
pub struct Import {
    pub functions: Vec<(String, Type)>
}


impl Import {
    pub fn add_fn(&mut self, function: (String, Type)) {
        self.functions.push(function);
    }
}
