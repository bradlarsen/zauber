pub mod ast;
pub mod err;
pub mod parser;

pub use ast::MagicPattern;
pub use err::Error;
pub use parser::parse_magic_file;
