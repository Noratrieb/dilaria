#![deny(clippy::disallowed_type)]

mod ast;
mod bytecode;
mod compile;
mod errors;
mod lex;
mod parse;
mod value;

use crate::ast::Program;
use bumpalo::Bump;

pub use lex::*;
pub use parse::*;

pub fn run_program(program: &str) {
    let ast_alloc = Bump::new();

    let lexer = lex::Lexer::new(program);
    let ast = parse::parse(lexer, &ast_alloc);

    match ast {
        Ok(ast) => process_ast(program, ast),
        Err(err) => errors::display_error(program, err),
    }
}

fn process_ast(program: &str, ast: Program) {
    println!("AST:\n{:?}\n", ast);

    let bytecode_alloc = Bump::new();

    let bytecode = compile::compile(&ast, &bytecode_alloc);

    match bytecode {
        Ok(code) => println!("Bytecode:\n{:#?}\n", code),
        Err(err) => errors::display_error(program, err),
    }
}
