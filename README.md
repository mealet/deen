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
    <a href="https://github.com/mealet/deen" /><img src="https://img.shields.io/github/actions/workflow/status/mealet/deen/test.yml?logo=speedtest&logoColor=fff&label=tests&color=19a63e" /></a>
    <a href="https://github.com/mealet/deen/blob/master/LICENSE"><img src="https://img.shields.io/github/license/mealet/deen?style=flat&color=%2319a63e&logo=opensourcehardware&logoColor=fff" /></a>
  </div>
</div>

## 👀 Description
**Deen** - a statically-typed compiling programming language inspired by languages like C, C++, Zig, and Rust. <br><br>
It provides tools for system programming, including: structures, C-like enums with supported functions, type definitions, backward compatibility with C, pointers, recursion, and more.

**⚠️ The project is currently under active development and may be unstable.**

## 🎯 Features
* ⚡ **Simplicity**. The language syntax is easy to read and write.
* 🚀 **Fast**. The compiler uses LLVM as a backend for the best performance.
* ✨ **Clean**. Nothing superfluous - just basic tools for everything.
* 🌐 **Modern**. Syntax and mechanics are inspired by Rust and Zig.
* 📑 **Strict**. Analyzers and checkers will prevent most compile-time errors.
* 🌎 **Open Source**. You can always participate in the project's development.

## ⚙️ Technical Details
#### 🔧 Main
- **Language:** Rust
- **Build Systems:** Cargo, Make
- **Backend:** inkwell (LLVM 1.18.6^)
- **Errors:** thiserror
- **Error Reporting:** miette, colored
- **Command Line Interface:** clap

#### 🌀 Structure
The project is divided into submodules using a virtual workspace environment:
- `deen` - main executable module. Combines all submodules into the main process.
- `deen-lexer` - lexical analyzer. Converts source code into abstract data types (Tokens).
- `deen-parser` - syntax analyzer. Analyzes and converts tokens into an Abstract Syntax Tree.
- `deen-semantic` - semantic analyzer. Recursively checks the AST for type and principle matching.
- `deen-codegen` - code generator. Recursively compiles the AST to an LLVM IR module.
- `deen-linker` - module linker. Compiles the LLVM IR module to an object file and links it.

## 💫 Installation
1. Install any C compiler. Recommended: [gcc](https://gcc.gnu.org/), [clang](https://clang.llvm.org/)
2. Download the latest release from GitHub: [Latest Release]
3. Unpack it anywhere and add to your `PATH` variable. Instructions for: [Windows](https://stackoverflow.com/questions/44272416/how-to-add-a-folder-to-path-environment-variable-in-windows-10-with-screensho), [Linux](https://phoenixnap.com/kb/linux-add-to-path), and [macOS](https://stackoverflow.com/questions/22465332/setting-path-environment-variable-in-macos-permanently)
4. Restart your system or environment.

## 🛠️ Building
1. Install the [Rust Programming Language](https://www.rust-lang.org/) from the official site.
2. Install [LLVM](https://www.llvm.org/docs/GettingStarted.html) following the official tutorial.
3. Clone this repository: `git clone https://github.com/mealet/deen`
4. Go to its directory and run: `cargo build --release`
5. The executable file will be in the `target/release` folder.

## 📎 Official Documentation
_Coming soon..._

## 🧊 Examples

Basic Functions and Standart Output
```rust
fn greet(name: *char) *char {
  return format!("Hello, {}!", name);
}

fn main() {
  println!("{}", greet("mealet"));
}
```
**OUTPUT:**
```
Hello, mealet!
```
----

Structures and Methods
```rust
struct Person {
  age: u32,

  fn birthday(&self) {
    println!("Happy Birthday!");
    self.age += 1;
  }
}

fn main() {
  let man = Person { .age = 20 };
  man.birthday();

  println!("The age is {}", man.age);
}
```
**OUTPUT:**
```
Happy Birthday!
The age is 21
```

----

Advanced Structures Usage
```rust
struct Man {
  name: *char,
  age: u32,

  fn new(name: *char, age: u32) Man {
    return Man { .name = name, .age = age };
  }

  fn display(&self) *char {
    return format!("{} <{}>", self.name, self.age);
  }
}

fn main() {
  let man = Man.new("Jason", 35);
  println!("{}", man);
}
```
**OUTPUT:**
```
Jason <35>
```

## 👮 License
The project is licensed under the BSD-3 Clause License. <br>
For more information see [License File](https://github.com/mealet/deen/blob/master/LICENSE)
