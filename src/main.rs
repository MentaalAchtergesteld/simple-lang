use std::{cell::RefCell, fs, rc::Rc};

use interpreter::{Environment, Function, Interpreter, Value};
use lexer::{Lexer, LexerError, Token};
use parser::Parser;

mod lexer;
mod parser;
mod interpreter;
mod stackcompiler;

fn main() -> Result<(), ()> {
    let input = fs::read_to_string("./test.sl")
        .map_err(|e| eprintln!("ERROR: couldn't open file `./test.sl`: {e}"))?;

    let lexer = Lexer::new(&input);
    let tokens = lexer.collect::<Result<Vec<Token>, LexerError>>()
        .map_err(|e| eprintln!("{}", e.with_source(&input)))?;

    let mut parser = Parser::new(&tokens);
    let tree = parser.parse_program()
        .map_err(|e| eprintln!("{}", e.with_source(&input)))?;

    let mut env = Environment::new();

    env.define("print".into(), Value::Function(Function::Native {
        arity: 1,
        func: Rc::new(|args| {
            println!("{}", args[0]);
            Ok(Value::Unit)
        })
    }));

    // let mut interpreter = Interpreter { env: Rc::new(RefCell::new(env)) };
    //
    // interpreter.interpret_program(tree)
    //     .map_err(|e| eprintln!("ERROR: couldn't interpret program: {e}"))?;
    
    let mut stack_compiler = stackcompiler::Compiler::new();

    stack_compiler.compile_program(&tree);
    println!("{:?}", stack_compiler.code);

    Ok(())
}
