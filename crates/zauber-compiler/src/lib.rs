use zauber_ast::MagicPattern;

#[derive(thiserror::Error, Debug)]
pub enum Error {
}

type Result<T> = std::result::Result<T, Error>;

pub fn compile(_patterns: &[MagicPattern]) -> Result<()> {
    Ok(())
}
