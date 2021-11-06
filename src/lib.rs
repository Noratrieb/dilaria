mod ast;
mod bird;
mod errors;
mod lex;
mod parse;

pub use lex::*;

pub fn run_program(program: &str) {
    let lexer = lex::Lexer::lex(program);
    let (success, errors) = lexer.partition::<Vec<_>, _>(|result| result.is_ok());

    if errors.is_empty() {
        let tokens = success.into_iter().collect::<Result<Vec<_>, _>>().unwrap();

        println!(
            "{:?}",
            tokens.iter().map(|token| &token.kind).collect::<Vec<_>>()
        );

        let ast = parse::parse(tokens);

        match ast {
            Ok(ast) => println!("{:?}", ast),
            Err(err) => errors::display_error(program, err),
        }
    } else {
        errors
            .into_iter()
            .map(Result::unwrap_err)
            .for_each(|err| errors::display_error(program, err));
    }
}
