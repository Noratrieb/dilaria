mod alloc;
mod lex;
mod parse;

pub fn run_program(program: &str) {
    let lexer = lex::Lexer::lex(program);
    let tokens: Result<Vec<_>, _> = lexer.collect();
    println!(
        "{:#?}",
        tokens.map(|tokens| tokens
            .into_iter()
            .map(|token| token.kind)
            .collect::<Vec<_>>())
    );
}
