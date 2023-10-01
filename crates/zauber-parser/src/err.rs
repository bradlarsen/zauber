use std::path::PathBuf;

#[derive(Debug)]
pub struct Location {
    pub fname: PathBuf,
    pub line_num: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}", self.fname.display(), self.line_num)
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
