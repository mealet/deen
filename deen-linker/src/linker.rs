use std::path::PathBuf;

pub struct ObjectLinker;

impl ObjectLinker {
    pub fn detect_compiler() -> Option<String> {
        let candidates = match std::env::consts::OS {
            "windows" => ["link", "gcc", "clang"],
            "macos" => ["clang", "gcc", "cc"],
            _ => ["gcc", "clang", "cc"],
        };

        for compiler in candidates {
            if std::process::Command::new(compiler)
                .arg("--version")
                .output()
                .is_ok()
            {
                return Some(compiler.to_string());
            }
        }

        None
    }

    pub fn link(module_name: &str, output: &str, includes: Vec<PathBuf>) -> Result<String, String> {
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
            let linker_output = match compiler.as_str() {
                "link" => {
                    let msvc_path = r"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Tools\MSVC\14.29.30133\lib\x64";
                    let sdk_um_path =
                        r"C:\Program Files (x86)\Windows Kits\10\lib\10.0.22000.0\um\x64";
                    let sdk_ucrt_path =
                        r"C:\Program Files (x86)\Windows Kits\10\lib\10.0.22000.0\ucrt\x64";

                    std::process::Command::new(&compiler)
                        .args([
                            &input,
                            &format!("/OUT:{output_path}"),
                            &format!("/LIBPATH:{msvc_path}"),
                            &format!("/LIBPATH:{sdk_um_path}"),
                            &format!("/LIBPATH:{sdk_ucrt_path}"),
                            "/SUBSYSTEM:CONSOLE",
                            "/MACHINE:X64",
                            "/ENTRY:mainCRTStartup",
                            "/NODEFAULTLIB:msvcrt.lib",
                            "libcmt.lib",
                            "kernel32.lib",
                            "user32.lib",
                        ])
                        .output()
                }
                _ => std::process::Command::new(&compiler)
                    .arg(input.clone())
                    .args(includes_formatted)
                    .arg("-fPIC")
                    .arg("-lm")
                    .arg("-o")
                    .arg(output_path)
                    .output(),
            };

            std::fs::remove_file(input).expect("Unable to delete object file");

            let output = linker_output.unwrap();
            if output.status.success() {
                return Ok(compiler);
            }

            let error_message = if output.stderr.is_empty() {
                String::from_utf8_lossy(&output.stdout).to_string()
            } else {
                String::from_utf8_lossy(&output.stderr).to_string()
            };
            return Err(error_message);
        }

        Err(String::from(
            "No supported C compilers found in system. Recommended: gcc/clang",
        ))
    }
}
