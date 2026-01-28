use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

pub struct PreProccessor {
    pub context: minipre::Context
}

#[derive(Debug, Error, Diagnostic, Clone, PartialEq, Eq)]
pub enum PreProccessorError {
    #[error("Preprocessor module returned IO error")]
    #[diagnostic(
        severity(Error),
        code(deen::pre::io_error),
    )]
    Io,

    #[error("Preprocessor module returned syntax error")]
    #[diagnostic(
        severity(Error),
        code(deen::pre::syntax_error),
    )]
    Syntax {
        message: String,

        #[source_code]
        src: NamedSource<String>,
        #[label("{message}")]
        span: SourceSpan,
    },
}

impl PreProccessor {
    pub fn new() -> Self {
        let mut context = minipre::Context::new();

        context.define("__LANG__", "deen");
        context.define("__LANG_VERSION__", env!("CARGO_PKG_VERSION"));

        let mut _self = Self {
            context
        };

        _self.define_os();
        _self.define_arch();
        _self.define_env();

        _self
    }

    pub fn process(&mut self, code: impl AsRef<str>, module_name: impl AsRef<str>) -> Result<String, PreProccessorError> {
        let code = code.as_ref().to_owned();

        minipre::process_str(&code, &mut self.context).map_err(|err| {
            match err {
                minipre::Error::Io(_) => PreProccessorError::Io,
                minipre::Error::Syntax { line, msg } => {
                    let start = code
                        .lines()
                        .take(line as usize - 1)
                        .map(|l| l.len() +  1)
                        .sum::<usize>();

                    let len = code.lines().nth(line as usize - 1).unwrap_or("").len();
                    let src = NamedSource::new(module_name, code);

                    PreProccessorError::Syntax {
                        message: msg.to_owned(),
                        src: src,
                        span: (start, len).into()
                    }
                }
            }
        })
    }
}

impl PreProccessor {
    fn define_os(&mut self) {
        self.context.define("__OS_LINUX__", format!("{}", cfg!(target_os = "linux") as u8));
        self.context.define("__OS_WINDOWS__", format!("{}", cfg!(target_os = "windows") as u8));
        self.context.define("__OS_MACOS__", format!("{}", cfg!(target_os = "macos") as u8));
        self.context.define("__OS_FREEBSD__", format!("{}", cfg!(target_os = "freebsd") as u8));
        self.context.define("__OS_NETBSD__", format!("{}", cfg!(target_os = "netbsd") as u8));
        self.context.define("__OS_OPENBSD__", format!("{}", cfg!(target_os = "openbsd") as u8));
        self.context.define("__OS_ANDROID__", format!("{}", cfg!(target_os = "android") as u8));
        self.context.define("__OS_IOS__", format!("{}", cfg!(target_os = "ios") as u8));

        self.context.define("__OS__", std::env::consts::OS);
    }

    fn define_arch(&mut self) {
        self.context.define("__ARCH_X86__", format!("{}", cfg!(target_arch = "x86") as u8));
        self.context.define("__ARCH_X86_64__", format!("{}", cfg!(target_arch = "x86_64") as u8));
        self.context.define("__ARCH_ARM__", format!("{}", cfg!(target_arch = "arm") as u8));
        self.context.define("__ARCH_AARCH64__", format!("{}", cfg!(target_arch = "aarch64") as u8));
        self.context.define("__ARCH_RISCV64__", format!("{}", cfg!(target_arch = "riscv64") as u8));
        self.context.define("__ARCH_WASM32__", format!("{}", cfg!(target_arch = "wasm32") as u8));

        self.context.define("__ARCH__", std::env::consts::ARCH);
    }

    fn define_env(&mut self) {
        self.context.define("__ENV_GNU__", format!("{}", cfg!(target_env = "gnu") as u8));
        self.context.define("__ENV_MUSL__", format!("{}", cfg!(target_env = "musl") as u8));
        self.context.define("__ENV_MSVC__", format!("{}", cfg!(target_env = "msvc") as u8));
    }
}
