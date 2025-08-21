use assert_cmd::Command;
use std::fs;

struct FailedTest {
    pub test_case: String,
    pub error: String
}

fn discover_test_cases() -> Vec<(String, String)> {
    const TEST_CASES_DIRECTORY: &str = "tests/test_cases";
    let mut test_cases = Vec::new();

    discover_test_cases_recursive(TEST_CASES_DIRECTORY, &mut test_cases, "");

    test_cases
}

fn discover_test_cases_recursive(dir: &str, test_cases: &mut Vec<(String, String)>, relative_path: &str) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            
            if path.is_dir() {
                let dir_name = path.file_name().unwrap().to_str().unwrap();

                let new_relative_path = if relative_path.is_empty() {
                    dir_name.to_string()
                } else {
                    format!("{}/{}", relative_path, dir_name)
                };

                discover_test_cases_recursive(path.to_str().unwrap(), test_cases, &new_relative_path);
            } else if path.extension().and_then(|os_str| os_str.to_str()) == Some("dn") {
                if let Some(stem) = path.file_stem().and_then(|os_str| os_str.to_str()) {
                    let expected_path = path.with_extension("expected");
                    
                    if expected_path.exists() {
                        let test_name = if relative_path.is_empty() {
                            stem.to_string()
                        } else {
                            format!("{}/{}", relative_path, stem)
                        };

                        let test_path = if relative_path.is_empty() {
                            stem.to_string()
                        } else {
                            format!("{}/{}", relative_path, stem)
                        };

                        test_cases.push((test_name, test_path));
                    }
                }
            }
        }
    }
}

#[test]
fn golden_system_tests() -> anyhow::Result<()> {
    let test_cases = discover_test_cases();
    let mut failed_tests: Vec<FailedTest> = Vec::new();

    for (test_name, test_path) in test_cases {
        println!("Running test: `{}`", test_name);

        let input_file = format!("tests/test_cases/{}.dn", test_path);
        let expected_file = format!("tests/test_cases/{}.expected", test_path);

        let binary_name = test_path.replace("/", "_");
        let mut compile_cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;

        compile_cmd.arg(&input_file)
                   .arg(&binary_name);

        compile_cmd.assert().success();
        
        let binary_path = format!("{}{}", binary_name, if cfg!(windows) { ".exe" } else { Default::default() });
        let expected_output = fs::read_to_string(&expected_file)?;

        let mut run_cmd = Command::new(format!("./{}", &binary_path));
        let _ = run_cmd.assert().success().try_stdout(expected_output).map_err(|err| {
            failed_tests.push(FailedTest { test_case: test_name, error: err.to_string() });
        });

        let _ = fs::remove_file(binary_path);
    }

    if !failed_tests.is_empty() {
        println!("");
        println!("Failed {} tests, showing up:", failed_tests.len());
        println!("");
    };

    failed_tests.into_iter().for_each(|failed_test| {
        let title = format!("|-- FAILED: `{}` --|", failed_test.test_case);

        println!("{}\n", title);
        println!("{}", failed_test.error);
        println!("|{}|", "-".repeat(title.len() - 2));
    });

    Ok(())
}
