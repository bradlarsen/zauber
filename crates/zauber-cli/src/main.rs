use anyhow::Result;
use clap::Parser;

use zauber_parser;

mod args;

fn main() -> Result<()> {
    let opts = args::Options::parse();
    println!("{opts:?}");

    for fname in opts.files {
        let patterns = zauber_parser::parse_magic_file(fname)?;
        for pattern in patterns {
            println!("{pattern:?}");
        }
    }

    Ok(())
}
