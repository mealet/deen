use clap::Parser;
use colored::Colorize;
use std::path::PathBuf;

/// Command Line Interface with [`clap`]
#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Compiler for Deen Programming Language",
    long_about = None,
    help_template = "{options}",
)]
pub struct Args {
    /// Path to source code
    pub path: PathBuf,
    /// Path to output file
    pub output: PathBuf,

    /// `-n --no-warns` flag to disable compiler's warnings
    #[arg(short, long, action, help = "Disable compiler's warnings")]
    pub no_warns: bool,

    /// `--llvm` flag to enable compilation into LLVM IR
    #[arg(long, action, help = "Enable compilation into LLVM IR")]
    pub llvm: bool,

    /// `--object` flag to compile code without linkage and main function checker
    #[arg(long, action, help = "Compile code to object file without linkage")]
    pub object: bool,

    /// `-l --link` flag to link C library to linker
    #[arg(short, long, action, help = "Link C library to linker")]
    pub link: Vec<PathBuf>,

}

/// Prints formatted red error message to _stderr_
pub fn error(message: &str) {
    eprintln!("{} {}", "Error:".red().bold(), message);
}

/// Prints formatted green info message to _stdout_
pub fn info(start: &str, message: &str) {
    println!("{} {}", start.green().bold(), message);
}

/// Prints formatted yellow warning message to _stdout_
pub fn warn(message: &str) {
    println!("{} {}", "Warning:".yellow().bold(), message);
}
