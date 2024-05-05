use anyhow::Result;
use clap::Parser;

use zauber_parser;
use zauber_compiler;

mod args;

// SKETCH:
// - zauber-ast has AST definitions
// - zauber-parser has libmagic parsing code
// - zauber-ir has IR definitions
// - zauber-util has fundamental utility code
// - zauber-compiler converts AST into IR and optimizes it
// - zauber-runtime has the actual evaluator for IR
// - zauber-cli is the command-line driver program
fn main() -> Result<()> {
    let opts = args::Options::parse();
    println!("{opts:?}");

    for fname in &opts.files {
        println!("### CONTENTS ##################################################################");
        let contents = std::fs::read_to_string(fname)?;
        println!("{}", contents);

        println!("### PARSED ####################################################################");
        let patterns = zauber_parser::parse_magic_file(fname)?;
        for pattern in patterns.iter() {
            println!("{pattern:#?}");
        }

        println!("### COMPILED ###################################################################");
        let compiled = zauber_compiler::compile(&patterns)?;
        println!("{compiled:#?}");
    }

    Ok(())
}
