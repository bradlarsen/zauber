use zauber_parser::MagicPattern;

#[derive(thiserror::Error, Debug)]
pub enum Error {
}

type Result<T> = std::result::Result<T, Error>;

pub fn compile(_patterns: &[MagicPattern]) -> Result<()> {
    Ok(())
}
