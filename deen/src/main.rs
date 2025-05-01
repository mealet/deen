use clap::Parser;

mod cli;

fn main() {
    let args = cli::Args::parse();
    let no_warns = args.no_warns;

    let fname = std::path::Path::new(&args.path)
        .file_name()
        .unwrap_or_else(|| {
            cli::error("Unable to find source file");
            std::process::exit(1);
        })
        .to_str()
        .unwrap_or_else(|| {
            cli::error("Unable to get source module name");
            std::process::exit(1);
        });

    cli::info("Reading", &format!(
        "`{}` ({})",
            &fname,
            std::fs::canonicalize(&args.path).unwrap_or_else(|_| {
                cli::error(&format!("File `{}` does not exist", &fname));
                std::process::exit(1);
            }).display()
        )
    );

    let src = std::fs::read_to_string(&args.path).unwrap_or_else(|_| {
        eprintln!("Unable to open path: {}", std::path::Path::new(&args.path).display());
        std::process::exit(1);
    });

    cli::info("Processing", &format!("tokens ({} lines of code)", src.lines().count()));

    let mut lexer = deen_lexer::Lexer::new(
        &src, fname
    );

    let handler = miette::GraphicalReportHandler::new();
    let mut total_warns = 0;

    let (tokens, warns) = match lexer.tokenize() {
        Ok(res) => res,
        Err((errors, warns)) => {
            println!();
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{}", buf);
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{}", buf);
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if !warns.is_empty() && !no_warns { cli::warn(&format!("`{}` generated {} warnings", &fname, warns.len())) }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{}", buf);
        });
    }

    total_warns += warns.len();

    cli::info("Parsing", &format!("syntax tree ({} tokens)", tokens.len()));

    let mut parser = deen_parser::Parser::new(tokens, &src, fname);
    let (ast, warns) = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            let (errors, warns) = e;

            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{}", buf);
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{}", buf);
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if (!warns.is_empty() || total_warns > 0) && !no_warns {
                cli::warn(&format!("`{}` generated {} warnings", &fname, warns.len() + total_warns));
            }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{}", buf);
        });
    }

    total_warns += warns.len();

    cli::info("Analyzing", &format!("processed code ({} global statements)", ast.len()));

    let mut analyzer = deen_semantic::Analyzer::new(&src, fname, true);
    let (imports, warns) = match analyzer.analyze(&ast) {
        Ok(res) => res,
        Err((errors, warns)) => {
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{}", buf);
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{}", buf);
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if (!warns.is_empty() || total_warns > 0) && !no_warns {
                cli::warn(&format!("`{}` generated {} warnings", &fname, warns.len() + total_warns));
            }


            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{}", buf);
        });
    }

    total_warns += warns.len();

    if total_warns > 0 && !no_warns {
        cli::warn(&format!("`{}` generated {} warnings", &fname, total_warns));
    }

    cli::info("Compiling", &format!(
            "`{}` to binary",
            &fname,
        )
    );

    let module_name = fname
        .split(".").next()
        .map(|n| n.to_string())
        .unwrap_or(fname.replace(".dn", ""));

    let ctx = deen_codegen::CodeGen::create_context();
    let mut codegen = deen_codegen::CodeGen::new(&ctx, &module_name, imports, true);
    
    let module_ref = codegen.compile(ast, None);

    if args.llvm {
        module_ref.print_to_file(
            format!("{}.ll", args.output)
        ).unwrap_or_else(|_| {
            cli::error("Unable to write LLVM IR file!");
            std::process::exit(1);
        });

        cli::info("Successfully", &format!("compiled to LLVM IR: `{}.ll`", args.output))
    } else {
        deen_linker::compiler::ObjectCompiler::compile_module(module_ref, &module_name);
        deen_linker::linker::ObjectLinker::link(&module_name, &args.output).unwrap_or_else(|err| {
            cli::error("Linker catched an error!");
            println!("\n{}", err);
            std::process::exit(1);
        });

        cli::info("Successfully", &format!("compiled to binary: `{}`", args.output))
    };
}
