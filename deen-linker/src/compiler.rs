pub struct ObjectCompiler;

impl ObjectCompiler {
    pub fn compile_module(module: &Module, name: &str) {
        const OPTIMIZATION_LEVEL: OptimizationLevel = OptimizationLevel::Default;
        const RELOC_MODE: RelocMode = RelocMode::PIC;
        const CODE_MODEL: CodeModel = CodeModel::Default;

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

use inkwell::{
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, FileType},
    OptimizationLevel
};
