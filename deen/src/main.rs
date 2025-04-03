use miette::Result;

fn main() -> Result<()> {
    let mut lexer = deen_lexer::Lexer::new(
        String::from("1 + 1 * 2"),
        String::from("test.dn")
    );

    let handler = miette::GraphicalReportHandler::new();

    let (tokens, warns) = match lexer.tokenize() {
        Ok(res) => res,
        Err((errors, warns)) => {
            errors.into_iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, &e).unwrap();

                eprintln!("{}", buf);
            });

            warns.into_iter().for_each(|w| {
                let mut buf = String::new();
                handler.render_report(&mut buf, &w).unwrap();

                eprintln!("{}", buf);
            });

            std::process::exit(1);
        }
    };

    warns.into_iter().for_each(|w| {
        let mut buf = String::new();
        handler.render_report(&mut buf, &w).unwrap();

        eprintln!("{}", buf);
    });

    println!("\n");
    tokens.iter().for_each(|tok| println!("{:?}", tok));

    Ok(())
}
