#![deny(clippy::disallowed_type)]

mod ast;
mod bytecode;
mod compile;
mod errors;
mod gc;
mod lex;
mod parse;
mod vm;

use crate::ast::Program;
use crate::gc::RtAlloc;

pub use bumpalo::Bump;
pub use lex::*;
pub use parse::*;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_type)]
type HashMap<K, V> = std::collections::HashMap<K, V>;

#[cfg(feature = "fxhash")]
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_type)]
type HashSet<T> = std::collections::HashSet<T>;

#[cfg(feature = "fxhash")]
type HashSet<T> = rustc_hash::FxHashSet<T>;

pub fn run_program(program: &str) {
    let ast_alloc = Bump::new();

    // SAFETY: I will try to 🥺
    let mut runtime = unsafe { RtAlloc::new() };

    let lexer = lex::Lexer::new(program, &mut runtime);
    let ast = parse::parse(lexer, &ast_alloc);

    match ast {
        Ok(ast) => process_ast(program, ast, runtime),
        Err(err) => errors::display_error(program, err),
    }
}

fn process_ast(program: &str, ast: Program, mut runtime: RtAlloc) {
    println!("AST:\n{:?}\n", ast);

    let bytecode_alloc = Bump::new();

    let bytecode = compile::compile(&ast, &bytecode_alloc, &mut runtime);

    match bytecode {
        Ok(code) => {
            println!("Bytecode:\n{:#?}\n", code);

            let result = vm::execute(&code, runtime);
            if let Err(result) = result {
                eprintln!("error: {}", result);
            }
        }
        Err(err) => errors::display_error(program, err),
    }
}
