use clap::Parser;

mod cli;

fn main() {
    let args = cli::Args::parse();
    let fname = std::path::Path::new(&args.path)
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let src = std::fs::read_to_string(&args.path).unwrap_or_else(|_| {
        eprintln!("Unable to open path: {}", args.path);
        std::process::exit(1);
    });

    let mut lexer = deen_lexer::Lexer::new(
        &src, fname
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

    let mut parser = deen_parser::Parser::new(tokens, &src, fname);
    let (ast, warns) = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            let (errors, warns) = e;

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

    let mut analyzer = deen_semantic::Analyzer::new(&src, fname);
    let warns = match analyzer.analyze(&ast) {
        Ok(warns) => warns,
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
}
