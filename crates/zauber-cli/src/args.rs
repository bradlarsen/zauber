use clap::Parser;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(author, version, about)]
pub struct Options {
    #[arg()]
    pub files: Vec<PathBuf>,
}
