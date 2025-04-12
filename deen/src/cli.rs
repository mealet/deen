use clap::Parser;
use colored::Colorize;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub path: String,
    pub output: String,

    #[arg(short, long, action)]
    pub no_warns: bool,

    #[arg(short, long, action)]
    pub llvm: bool
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
