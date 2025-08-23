# System Golden Tests Guide
**System Golden Tests** is a kind of integrational unit tests, that provides toolchain for running project and comparing its result. <br/>
In this realization happens:
1. Function scans all tests (package of `file.dn` and `file.expected` duo).
2. Runs full compiler process and running it's binary.
3. Asserts binary output with text from `file.expected`.

### How to create new test?
1. Choose right subdirectory in `test_cases`, or create new one.
2. Create 2 files: `test_name.dn`, `test_name.expected`.
3. Write your code in `.dn` file (main function should return 0 for no error).
4. Write expected output in `.expected` file.
5. **If you want to specify expected exit code** use this format at start of `.expected` file: **`@! EXIT_CODE`**
