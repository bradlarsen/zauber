pub mod err;
pub mod parser;

pub use err::Error;
pub use parser::{parse_magic_file, parse_magic_file_contents};

#[cfg(test)]
mod test;
