use miette::Result;

fn main() -> Result<()> {
    let mut lexer = deen_lexer::Lexer::new(
        String::from("1 + 1 * 2"),
        String::from("test.dn")
    );

    let (tokens, warns) = match lexer.tokenize() {
        Ok(res) => res,
        Err((errors, warns)) => {
            errors.iter().for_each(|e| {
                    let a = e.clone();
                    eprintln!("{:?}", a)
                }
            );
            warns.iter().for_each(|w| eprintln!("{:?}", *w));

            std::process::exit(1);
        }
    };

    warns.iter().for_each(|w| eprintln!("{:?}", w));

    println!("\n");
    tokens.iter().for_each(|tok| println!("{:?}", tok));

    Ok(())
}
