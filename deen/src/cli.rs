use clap::Parser;
use colored::Colorize;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(
    author,
    version,
    about = "Compiler for Deen Programming Language",
    long_about = None,
    help_template = "\
{options}
"
)]
pub struct Args {
    pub path: PathBuf,
    pub output: PathBuf,

    #[arg(short, long, action, help = "Disable compiler's warnings")]
    pub no_warns: bool,

    #[arg(short, long, action, help = "Enable compilation into LLVM IR")]
    pub llvm: bool,

    #[arg(short, long, action, help = "Include C library to linker")]
    pub include: Vec<PathBuf>,
}

pub fn error(message: &str) {
    eprintln!("{} {}", "Error:".red().bold(), message);
}

pub fn info(start: &str, message: &str) {
    println!("{} {}", start.green().bold(), message);
}

pub fn warn(message: &str) {
    println!("{} {}", "Warning:".yellow().bold(), message);
}
