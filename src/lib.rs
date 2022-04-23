mod ast;
mod errors;

use crate::ast::Program;

pub fn process_ast(program: &str, ast: &Program) {
    dbg(ast);
}

pub fn dbg(x: impl dbg_pls::DebugPls) {
    eprintln!("{}", dbg_pls::pretty(&x))
}
