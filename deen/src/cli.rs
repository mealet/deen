use clap::Parser;
use colored::Colorize;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub path: PathBuf,
    pub output: PathBuf,

    #[arg(short, long, action)]
    pub no_warns: bool,

    #[arg(short, long, action)]
    pub llvm: bool,

    #[arg(short, long, action)]
    pub include: Vec<PathBuf>
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
