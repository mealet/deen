use crate::Analyzer;
use deen_parser::{expressions::Expressions, types::Type};

pub use print::PrintMacro;
pub use println::PrintlnMacro;
pub use format::FormatMacro;
pub use panic::PanicMacro;
pub use sizeof::SizeofMacro;
pub use cast::CastMacro;

mod print;
mod println;
mod format;
mod panic;
mod sizeof;
mod cast;

pub trait MacroObject: std::fmt::Debug {
    fn verify_call(&self, analyzer: &mut Analyzer, arguments: &Vec<Expressions>, span: &(usize, usize)) -> Type;
}

/// Enumeration of all existing macros
#[derive(Debug, Clone)]
pub enum CompilerMacros {
    PrintMacro(PrintMacro),
    PrintlnMacro(PrintlnMacro),
    FormatMacro(FormatMacro),
    PanicMacro(PanicMacro),
    SizeofMacro(SizeofMacro),
    CastMacro(CastMacro),
    None,
}

impl MacroObject for CompilerMacros {
    fn verify_call(&self, analyzer: &mut Analyzer, arguments: &Vec<Expressions>, span: &(usize, usize)) -> Type {
        match self {
            CompilerMacros::PrintMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::PrintlnMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::FormatMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::PanicMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::SizeofMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::CastMacro(instance) => instance.verify_call(analyzer, arguments, span),
            CompilerMacros::None => Type::Void
        }
    }
}
