[Latest Release]: https://github.com/mealet/deen/releases/latest

<div align="center">
  <picture>
    <img src="https://github.com/mealet/deen/blob/master/assets/Deen%20Logo.png" width="35%" />
  </picture>

  <div>
    <h1>Deen Programming Language</h1>
    <i>A simple language for efficient software</i>
  </div>
  <br/>
  <div>
    <a href="https://github.com/mealet/deen/releases/latest"><img src="https://img.shields.io/badge/dynamic/toml?url=https%3A%2F%2Fraw.githubusercontent.com%2Fmealet%2Fdeen%2Frefs%2Fheads%2Fmaster%2FCargo.toml%3Fraw%3Dtrue&query=workspace.package.version&logo=hackthebox&logoColor=fff&label=version&color=%2319a63e" /></a>
    <a href="https://github.com/mealet/deen/blob/master/LICENSE"><img src="https://img.shields.io/github/license/mealet/deen?style=flat&color=%2319a63e&logo=opensourcehardware&logoColor=fff" /></a>
  </div>
</div>

## 👀 Description
**Deen** - is a static-typed compiling programming language, that is inspired by languages like: C, C++, Zig and Rust. <br>
It provides tools for the system programming, like: structures and C-like enums with supported functions, type definitions, backward compatability with C, pointers, recursion and so on.

## 🎯 Features
* ⚡ **Simplicity**. Language syntax is easy to read and write.
* 🚀 **Fast**. The compiler uses LLVM as backend for maximum performance.
* ✨ **Clean**. Nothing superfluous, just basic tools for everything.
* 🌐 **Modern**. Syntax and mechanics are inspired by Rust and Zig.
* 📑 **Strict**. Analyzers and checkers will prevent most of compile time errors.
* 🌎 **Open Source**. You can always take a part of project development.

## ⚙️ Technical Content
#### 🔧 Main
- **Language:** Rust
- **Build Systems:** Cargo, Make
- **Backend:** inkwell (LLVM 1.18.6^)
- **Errors:** thiserror
- **Errors Reporting:** miette, colored
- **Command Line Interface:** clap

#### 🌀 Structure
Project is separated to submodules by virtual workspace environment.
- `deen` - main executable module. Connects all submodules into main process.
- `deen-lexer` - lexical analyzer. Turns source code into abstract data types - Tokens.
- `deen-parser` - syntax analyzer. Analyzes and converts tokens to Abstract Syntax Tree.
- `deen-semantic` - semantic analyzer. Recursively checks AST for types and principles matching.
- `deen-codegen` - code generator. Recursively compiles AST to LLVM IR module.
- `deen-linker` - module linker. Compiles LLVM IR module to the object file and links it.

## 💫 Installation
1. Install any of the C compilers. Recommended: [gcc](https://gcc.gnu.org/), [clang](https://clang.llvm.org/)
2. Download the latest release from Github: [Latest Release]
3. Unpack it anywhere and add to your `PATH` variable. Instructions for: [Windows](https://stackoverflow.com/questions/44272416/how-to-add-a-folder-to-path-environment-variable-in-windows-10-with-screensho), [Linux](https://phoenixnap.com/kb/linux-add-to-path) and [Mac OS](https://stackoverflow.com/questions/22465332/setting-path-environment-variable-in-macos-permanently)
4. Restart your system or environment.

## 🛠️ Building
1. Install [Rust Programming Language](https://www.rust-lang.org/) from the official site.
2. Install [LLVM](https://www.llvm.org/docs/GettingStarted.html) by official tutorial.
3. Clone this repository by command: `git clone https://github.com/mealet/deen`
4. Go to its directory and run the build command: `cargo build --release`
5. Executable file will be at the `target/release` folder.

## 📎 Official Documentation
_soon..._

## 🧊 Example
```rust
fn greet(name: *char) *char {
  return format!("Hello, {}!", name);
}

fn main() {
  println!("{}", greet("mealet"));
}
```
**Output:**
```
Hello, mealet!
```
----
```rust
struct Person {
  age: u32,

  fn birthday(self: Person) {
    println!("Happy Birthday!");
    self.age = self.age + 1;
  }
}

fn main() {
  let man = Person { .age = 20 };
  man.birthday();

  println!("The age is {}", man.age);
}
```
**Output:**
```
Happy Birthday!
The age is 21
```

## 👮 License
Project is licensed under the BSD-3 Clause License. <br>
For more information see [License File](https://github.com/mealet/deen/blob/master/LICENSE)
