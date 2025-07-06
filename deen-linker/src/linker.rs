use std::path::PathBuf;

pub struct ObjectLinker;

impl ObjectLinker {
    fn detect_compiler() -> Option<String> {
        const COMPILERS: [&str; 2] = ["clang", "cc"];

        for compiler in COMPILERS {
            if std::process::Command::new(compiler).arg("--version").output().is_ok() {
                return Some(compiler.to_string())
            }
        }

        None
    }

    pub fn link(module_name: &str, output: &str, includes: Vec<PathBuf>) -> Result<(), String> {
        let mut output_path = output.to_string();
        if cfg!(windows) && !output.contains(".exe") {
            output_path = format!("{output_path}.exe");
        }

        let includes_formatted = includes
            .iter()
            .map(|inc| inc.as_os_str())
            .collect::<Vec<_>>();
        let input = format!("{module_name}.o");

        if let Some(compiler) = Self::detect_compiler() {
            let linker_output = std::process::Command::new(compiler)
                .arg(input.clone())
                .args(includes_formatted)
                .arg("-fPIC")
                .arg("-lm")
                .arg("-o")
                .arg(output_path)
                .output();

            std::fs::remove_file(input).expect("Unable to delete object file");

            let output = linker_output.unwrap();
            if output.status.success() {
                return Ok(());
            }

            return Err(String::from_utf8_lossy(&output.stderr).to_string());
        }
        return Err(String::from("No supported C compilers found in system. Please install `clang` from official site!"));
    }
}
