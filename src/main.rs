use std::fs;

use lexer::{Lexer, LexerError, Token};

mod lexer;
mod parser;

fn main() -> Result<(), ()> {
    let input = fs::read_to_string("./test.sl")
        .map_err(|e| eprintln!("ERROR: couldn't open file `./test.sl`: {e}"))?;

    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<Token>, LexerError>>()
        .map_err(|e| eprintln!("ERROR: couldn't tokenize file: {e}"))?;

    println!("Tokens: {tokens:?}");

    for token in tokens {
        let kind = token.kind;
        let span = token.span;
        println!("Token: {kind:?}: {}", input[span.start..span.end].to_string().trim());
    }

    Ok(())
}
