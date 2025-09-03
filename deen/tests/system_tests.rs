use assert_cmd::Command;
use std::fs;

struct FailedTest {
    pub test_case: String,
    pub error: String,
}

fn count_digits(mut number: usize) -> usize {
    if number == 0 {
        return 1;
    };
    if number / 10 == 0 {
        return 1;
    };

    let mut count = 0;

    while number != 0 {
        number /= 10;
        count += 1;
    }

    count
}

fn discover_test_cases() -> Vec<(String, String)> {
    const TEST_CASES_DIRECTORY: &str = "tests/test_cases";
    let mut test_cases = Vec::new();

    discover_test_cases_recursive(TEST_CASES_DIRECTORY, &mut test_cases, "");

    test_cases
}

fn discover_test_cases_recursive(
    dir: &str,
    test_cases: &mut Vec<(String, String)>,
    relative_path: &str,
) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                let dir_name = path.file_name().unwrap().to_str().unwrap();

                let new_relative_path = if relative_path.is_empty() {
                    dir_name.to_string()
                } else {
                    format!("{relative_path}/{dir_name}")
                };

                discover_test_cases_recursive(
                    path.to_str().unwrap(),
                    test_cases,
                    &new_relative_path,
                );
            } else if path.extension().and_then(|os_str| os_str.to_str()) == Some("dn") {
                if let Some(stem) = path.file_stem().and_then(|os_str| os_str.to_str()) {
                    let expected_path = path.with_extension("expected");

                    if expected_path.exists() {
                        let test_name = if relative_path.is_empty() {
                            stem.to_string()
                        } else {
                            format!("{relative_path}/{stem}")
                        };

                        let test_path = if relative_path.is_empty() {
                            stem.to_string()
                        } else {
                            format!("{relative_path}/{stem}")
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
    let tests_count = test_cases.len();
    let tests_count_digits = count_digits(tests_count);

    let mut failed_tests: Vec<FailedTest> = Vec::new();

    for (index, (test_name, test_path)) in test_cases.into_iter().enumerate() {
        let current_number_digits = count_digits(index + 1).wrapping_sub(1);
        let numeration = format!(
            "{}{}|",
            index + 1,
            " ".repeat(tests_count_digits - current_number_digits)
        );

        println!("{numeration} Running test: `{test_name}`");

        let input_file = format!("tests/test_cases/{test_path}.dn");
        let expected_file = format!("tests/test_cases/{test_path}.expected");

        let binary_name = test_path.replace("/", "_");
        let mut compile_cmd = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;

        compile_cmd.arg(&input_file).arg(&binary_name);

        let compilation_result = compile_cmd.assert().try_success();
        if let Err(compilation_error) = compilation_result {
            let err = compilation_error.to_string();
            failed_tests.push(FailedTest {
                test_case: test_name.clone(),
                error: err.clone(),
            });
            continue;
        }

        let binary_path = format!(
            "{}{}",
            binary_name,
            if cfg!(windows) {
                ".exe"
            } else {
                Default::default()
            }
        );
        let (expected_output, expected_exit_code) = {
            let mut content = fs::read_to_string(&expected_file)?;
            let mut exit_code = 0;

            if content.starts_with("@! ") {
                let chars = content.chars();
                let mut cursor = chars
                    .into_iter()
                    .skip_while(|&chr| chr == '@' || chr == '!' || chr.is_whitespace());
                let mut current = cursor.next().unwrap_or('\0');

                let mut number = 0;
                let mut number_digits = 0;

                while current != '\0' && current.is_ascii_digit() {
                    number = number * 10 + current.to_digit(10).unwrap_or(0);
                    number_digits += 1;
                    current = cursor.next().unwrap_or('\0');
                }

                // `+1` is used to remove '\n' symbol
                (0..(3 + number_digits + 1)).for_each(|_| {
                    content.remove(0);
                });

                exit_code = number as i32;
            }

            (content, exit_code)
        };

        let mut run_cmd = Command::new(format!("./{}", &binary_path));
        let code_assertion = run_cmd.assert().try_code(expected_exit_code);

        if let Ok(code_assertion) = code_assertion {
            let _ = code_assertion.try_stdout(expected_output).map_err(|err| {
                failed_tests.push(FailedTest {
                    test_case: test_name,
                    error: err.to_string(),
                });
            });
        } else {
            let _ = code_assertion.map_err(|err| {
                failed_tests.push(FailedTest {
                    test_case: test_name,
                    error: err.to_string(),
                });
            });
        }

        let _ = fs::remove_file(binary_path);
    }

    let is_failed = !failed_tests.is_empty();
    if is_failed {
        println!();
        println!("Failed {} tests, showing up:", failed_tests.len());
        println!();
    };

    failed_tests.into_iter().for_each(|failed_test| {
        let title = format!("|-- FAILED: `{}` --|", failed_test.test_case);

        println!("{title}\n");
        println!("{}", failed_test.error);
        println!("|{}|", "-".repeat(title.len() - 2));
    });

    if is_failed {
        println!("");
        return Err(anyhow::anyhow!("Tests failed"))
    }

    Ok(())
}
