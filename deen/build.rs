use std::process::Command;

fn main() {
    let git_output = Command::new("git")
        .arg("rev-parse")
        .arg("HEAD")
        .output()
        .unwrap();

    let git_hash = String::from_utf8(git_output.stdout).unwrap();

    println!("cargo:rerun-if-changed=.git/HEAD");
    println!("cargo:rustc-env=GIT_HASH={git_hash}");
}
