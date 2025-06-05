use std::path::PathBuf;

pub struct ObjectLinker;

impl ObjectLinker {
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

        let linker_output = std::process::Command::new("cc")
            .arg(input.clone())
            .args(includes_formatted)
            .arg("-o")
            .arg(output_path)
            .output();

        std::fs::remove_file(input).expect("Unable to delete object file");

        let output = linker_output.unwrap();
        if output.status.success() {
            return Ok(());
        }
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}
