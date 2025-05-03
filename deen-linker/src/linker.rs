use std::{io::Error, process::Output};

pub struct ObjectLinker;

impl ObjectLinker {
    pub fn link(module_name: &str, output: &str) -> Result<Output, Error> {
        let mut output_path = output.to_string();
        if cfg!(windows) && !output.contains(".exe") {
            output_path = format!("{output_path}.exe");
        }

        let input = format!("{module_name}.o");
        let linker_output = std::process::Command::new("cc")
            .arg(input.clone())
            .arg("-o")
            .arg(output_path)
            .output();

        std::fs::remove_file(input).expect("Unable to delete object file");
        linker_output
    }
}
