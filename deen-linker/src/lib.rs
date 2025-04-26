use std::{io::Error, process::Output};

use inkwell::{
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, FileType},
    OptimizationLevel
};

pub struct ObjectCompiler;
pub struct ObjectLinker;

impl ObjectCompiler {
    pub fn compile_module(module: &Module, name: &str) {
        const OPTIMIZATION_LEVEL: OptimizationLevel = OptimizationLevel::Aggressive;
        const RELOC_MODE: RelocMode = RelocMode::PIC;
        const CODE_MODEL: CodeModel = CodeModel::Large;

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target.create_target_machine(
            &target_triple,
            "generic",
            "",
            OPTIMIZATION_LEVEL,
            RELOC_MODE,
            CODE_MODEL
        ).unwrap();

        let output_name = format!("{}.o", name);
        let output_path = std::path::Path::new(&output_name);
        target_machine.write_to_file(module, FileType::Object, output_path).unwrap();
    }
}

impl ObjectLinker {
    pub fn link(module_name: &str, output: &str) -> Result<Output, Error> {
        let mut output_path = output.to_string();
        if cfg!(windows) && !output.contains(".exe") {
            output_path = format!("{}.exe", output_path);
        }

        let input = format!("{}.o", module_name);
        let linker_output = std::process::Command::new("cc")
            .arg(input.clone())
            .arg("-o")
            .arg(output_path)
            .output();

        std::fs::remove_file(input).expect("Unable to delete object file");
        linker_output
    }
}
