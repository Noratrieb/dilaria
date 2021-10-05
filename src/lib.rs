mod alloc;
mod lex;
mod parse;
mod string;

pub fn run_program(program: &str) {
    let lexer = lex::Lexer::lex(program);
    let tokens: Result<Vec<_>, _> = lexer.collect();
    println!("{:#?}", tokens);
}
