use clap::Parser;

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
