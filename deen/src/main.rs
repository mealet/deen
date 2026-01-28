//! # Deen Programming Language
//! **Deen** - a statically-typed compiling programming language inspired by languages like C, C++, Zig, and Rust. <br/><br/>
//! It provides tools for system programming, including: structures, C-like enums with supported functions, type definitions, backward compatibility with C, pointers, recursion, and more.
//!
//! # Technical Details
//! - **Minimum recommended `rustc` version:** 1.88
//! - **Language Backend:** [`inkwell`] (LLVM 1.18.6^)
//! - **Errors Handling:** [`thiserror`]
//! - **Error Reporting:** [`miette`], [`colored`]
//! - **Command Line Interface:** [`clap`]
//!
//! # Project Structure
//! Project separated to multiple submodels by virtual Cargo manifest (cargo workspace):
//! - [`deen`](crate) - main executable module. Combines all submodules into the main process
//! - [`deen-preprocessor`](deen_preprocessor) - C-like preprocessor. Resolves compile time definitions and coniditons (based on [minipre](https://github.com/Diggsey/minipre) by Diggsey)
//! - [`deen-lexer`](deen_lexer) - lexical analyzer. Converts source code into abstract data types (tokens)
//! - [`deen-parser`](deen_parser) - syntax analyzer. Analyzes and converts provided input into [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
//! - [`deen-semantic`](deen_semantic) - semantical analyzer. Recursively checks the AST for type and principle matching
//! - [`deen-codegen`](deen_codegen) - code generator. Recursively compiles the AST into LLVM IR
//! - [`deen-linker`](deen_linker) - module linker. Compiles the LLVM IR module to an object file and links it to a binary file.
//!
//! # License
//! Project is licensed under the MIT License. <br/>
//! See LICENSE file on Github: <https://github.com/mealet/deen/blob/master/LICENSE>

use clap::{CommandFactory, Parser};
use colored::Colorize;

/// Command Line Interface module
mod cli;

fn main() {
    // Getting args with advanced helper
    let args = cli::Args::try_parse().unwrap_or_else(|e| {
        let mut command = cli::Args::command();

        // Git commit hash where build was made. Check the `build.rs`
        let git_hash: String = env!("GIT_HASH").chars().take(8).collect();
        let version_fmt = format!("v{} {}", env!("CARGO_PKG_VERSION"), git_hash);

        let authors_env = env!("CARGO_PKG_AUTHORS");
        let authors_fmt = if authors_env.contains(":") {
            format!("\n| {}", authors_env.replace(":", "\n| "))
        } else {
            authors_env.to_owned()
        };

        match e.kind() {
            clap::error::ErrorKind::DisplayVersion => {
                // --version flag
                // Just return necessary information and exit with 0
                eprintln!("{}", "🚀 Deen Programming Language".bold().cyan());
                eprintln!("| - version: {version_fmt}");
                eprintln!("| - authors: {authors_fmt}");

                std::process::exit(0);
            }
            _ => {
                // Wrong arguments or --help flag
                eprintln!("{}", "🚀 Deen Programming Language".bold().cyan());
                eprintln!("| - version: {version_fmt}");
                eprintln!("| - authors: {authors_fmt}");
                eprintln!();
                eprintln!("{}", "🍀 Options:".bold().cyan());

                command.print_help().unwrap();

                eprintln!();
                eprintln!("{}", "🎓 Examples of usage:".bold().cyan());
                eprintln!("  deen example.dn output");
                eprintln!("  deen example.dn output --no-warns");
                eprintln!("  deen example.dn output --include foo.c");

                // Checking for the error kind (if just --help flag returning without error)
                if e.kind() == clap::error::ErrorKind::DisplayHelp {
                    std::process::exit(0);
                }
                std::process::exit(1);
            }
        }
    });

    if args.llvm && args.object {
        cli::error("Flags `--llvm` and `--object` are incompatible");
        std::process::exit(1);
    }

    let no_warns = args.no_warns;

    // Getting filename from provided path
    let fname = args
        .path
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

    cli::info(
        "Reading",
        &format!(
            "`{}` ({})",
            &fname,
            std::fs::canonicalize(&args.path)
                .unwrap_or_else(|_| {
                    cli::error(&format!("File `{}` does not exist", &fname));
                    std::process::exit(1);
                })
                .display()
        ),
    );

    // Reading it (on old devices it might take a little bit longer)
    let mut src = std::fs::read_to_string(&args.path).unwrap_or_else(|err| {
        eprintln!(
            "Unable to open path: {}. System error: {}",
            std::path::Path::new(&args.path).display(),
            err
        );
        std::process::exit(1);
    });

    cli::info(
        "Processing",
        &format!("tokens ({} lines of code)", src.lines().count()),
    );

    // Extracting module name from filename
    let module_name = fname
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(fname.replace(".dn", ""));

    let handler = miette::GraphicalReportHandler::new();

    // Preprocessor unit
    let mut preprocessor = deen_preprocessor::PreProccessor::new();

    src = preprocessor.process(src, &module_name).unwrap_or_else(|err| {
        let mut buf = String::new();
        handler.render_report(&mut buf, &err).unwrap();

        eprintln!("{buf}");

        std::process::exit(1);
    });

    // Lexical Analyzer Initialization
    let mut lexer = deen_lexer::Lexer::new(&src, fname);

    // `miette` graphical reporter (for this amazing error reports).
    // "total_warns" variable made just for reporting how much warnings we've got at the end
    let mut total_warns = 0;

    let (tokens, warns) = match lexer.tokenize() {
        Ok(res) => res,
        Err((errors, warns)) => {
            println!();
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{buf}");
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{buf}");
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if !warns.is_empty() && !no_warns {
                cli::warn(&format!("`{}` generated {} warnings", &fname, warns.len()))
            }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{buf}");
        });
    }

    total_warns += warns.len();

    cli::info("Parsing", &format!("syntax tree ({} tokens)", tokens.len()));

    // Syntax Analyzer initialization.
    // It takes full ownership for tokens vector (because we don't need them anymore)
    let mut parser = deen_parser::Parser::new(tokens, &src, fname);
    let (ast, warns) = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            let (errors, warns) = e;

            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{buf}");
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{buf}");
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if (!warns.is_empty() || total_warns > 0) && !no_warns {
                cli::warn(&format!(
                    "`{}` generated {} warnings",
                    &fname,
                    warns.len() + total_warns
                ));
            }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{buf}");
        });
    }

    total_warns += warns.len();

    cli::info(
        "Analyzing",
        &format!("processed code ({} global statements)", ast.len()),
    );

    // Semantical Analyzer initialization.
    //
    // Last argument `is_main` is used for imports functionality.
    // Imports aren't working currently | 20/06/2025 v0.0.4
    //
    // Analyzer takes only reference to AST (because we only provide checking)
    let mut analyzer =
        deen_semantic::Analyzer::new(&src, fname, args.path.clone(), &mut preprocessor, !(args.object || args.llvm));
    let (symtable, warns) = match analyzer.analyze(&ast) {
        Ok(res) => res,
        Err((errors, warns)) => {
            errors.iter().for_each(|e| {
                let mut buf = String::new();
                handler.render_report(&mut buf, e).unwrap();

                eprintln!("{buf}");
            });

            if !no_warns {
                warns.iter().for_each(|w| {
                    let mut buf = String::new();
                    handler.render_report(&mut buf, w).unwrap();

                    eprintln!("{buf}");
                });
            }

            cli::error(&format!("`{}` returned {} errors", &fname, errors.len()));
            if (!warns.is_empty() || total_warns > 0) && !no_warns {
                cli::warn(&format!(
                    "`{}` generated {} warnings",
                    &fname,
                    warns.len() + total_warns
                ));
            }

            std::process::exit(1);
        }
    };

    if !no_warns {
        warns.iter().for_each(|w| {
            let mut buf = String::new();
            handler.render_report(&mut buf, w).unwrap();

            eprintln!("{buf}");
        });
    }

    total_warns += warns.len();

    if total_warns > 0 && !no_warns {
        cli::warn(&format!("`{}` generated {} warnings", &fname, total_warns));
    }

    cli::info("Compiling", &format!("`{}` to binary", &fname));

    // Combining linkages
    let linked_list: Vec<std::path::PathBuf> = symtable.linked.iter().cloned().collect();
    let external_linkages = [args.link, linked_list].concat();

    // Code Generator Initialization.
    // Creating custom context and a very big wrapper for builder.
    let ctx = deen_codegen::CodeGen::create_context();
    let mut codegen = deen_codegen::CodeGen::new(&ctx, &module_name, &src, symtable);

    // Compiling: AST -> LLVM IR Module Reference
    let (module_ref, _) = codegen.compile(ast, None);

    // --llvm argument allows user to export LLVM IR module into file
    if args.llvm {
        module_ref
            .print_to_file(format!("{}.ll", args.output.display()))
            .unwrap_or_else(|_| {
                cli::error("Unable to write LLVM IR file!");
                std::process::exit(1);
            });

        cli::info(
            "Successfully",
            &format!("compiled to LLVM IR: `{}.ll`", args.output.display()),
        )
    } else {
        deen_linker::compiler::ObjectCompiler::compile_module(module_ref, &module_name);

        if args.object {
            cli::info(
                "Successfully",
                &format!("compiled to object file: {}.o", args.output.display()),
            );
            return;
        }

        let compiler = deen_linker::linker::ObjectLinker::link(
            &module_name,
            args.output.to_str().unwrap(),
            external_linkages,
        )
        .unwrap_or_else(|err| {
            let object_linker = deen_linker::linker::ObjectLinker::detect_compiler()
                .unwrap_or(String::from("none"));
            cli::error(&format!(
                "Linker catched an error! (object linker: `{object_linker}`)"
            ));
            println!("\n{err}\n");

            cli::error(
                "Please make sure you linked all the necessary libraries (check the '-i' argument)",
            );
            std::process::exit(1);
        });

        let formatted_output =
            if cfg!(windows) && !args.output.display().to_string().contains(".exe") {
                format!("{}.exe", args.output.display())
            } else {
                args.output.display().to_string()
            };

        cli::info(
            "Successfully",
            &format!("compiled to binary (with `{compiler}`): `{formatted_output}`"),
        )
    };
}
