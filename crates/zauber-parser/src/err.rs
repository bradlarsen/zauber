use std::path::PathBuf;

#[derive(Debug)]
pub struct Location {
    pub fname: Option<PathBuf>,
    pub line_num: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match &self.fname {
            Some(fname) => write!(f, "{}:{}", fname.display(), self.line_num),
            None => write!(f, "<unknown>:{}", self.line_num),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{location}: {message}\n\n    {line}\n")]
    ParseError {
        location: Location,
        message: String,
        line: String,
    },

    #[error("IO error: {0}")]
    IOError(#[from] std::io::Error),

    #[error("{location}: trailing input")]
    TrailingInput {
        location: Location,
    },
}
