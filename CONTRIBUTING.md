Hello! Thank you for your interest in project's life 🍀 <br/>
Here's contribution guide for you:

### 🍏 Repository Issues
Found a bug? Wanna suggest an improvement? You can always open issue in this repository:
1. Open repo's [issues](https://github.com/mealet/deen/issues)
2. Please ensure that your problem/improvement wasn't already reported
3. Push the button `Create issue`
4. Choose any template, or create blank issue
5. Follow the instructions in template

### 🚀 Pull Requests
> [!NOTE]
> ### Ensure that
> - your code matches the project's code style
> - your code doesn't break compiler's work
> - you've written readable and understandable code
> - you've added/changed necessary tests

> [!NOTE]
> ### Please do...
> - Fix all analyzer warnings. Use: `make clippy`
> - Format code by rust formatter. Use: `make fmt`
> - Provide related unit tests for the changes
> - Provide short description of your work in pull request

### ⌛ Tests
All tests running with **cargo** package manager (`cargo test -- --show-output`). <br/>
It is used to ensure, that your changes doesn't break other program pieces <br/>
Integrational tests are the most important part of contribution, so always be sure, that you've added necessary tests (including system golden tests, read guide for them below). <br/>
- Rust Tests Guide: [Cargo Tests Guide](https://doc.rust-lang.org/cargo/guide/tests.html)
- Integrational Tests Guide: [Deen System Tests Guide](deen/tests/TESTS_GUIDE.md)
