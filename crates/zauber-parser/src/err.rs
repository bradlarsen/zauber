use std::path::PathBuf;

#[derive(Debug)]
pub struct Location {
    pub fname: PathBuf,
    pub line_num: usize,
    pub column_num: usize,
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("{}:{}:{}: {message}\n\n    {line}\n", location.fname.display(), location.line_num, location.column_num)]
    ParseError {
        location: Location,
        message: String,
        line: String,
    },

    #[error("IO error: {0}")]
    IOError(#[from] std::io::Error),

    #[error("{}:{}:{}: trailing input", location.fname.display(), location.line_num, location.column_num)]
    TrailingInput {
        location: Location,
    },
}
