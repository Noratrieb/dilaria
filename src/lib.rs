mod alloc;
mod ast;
mod errors;
mod lex;
mod parse;

pub fn run_program(program: &str) {
    let lexer = lex::Lexer::lex(program);
    let (success, errors) = lexer.partition::<Vec<_>, _>(|result| result.is_ok());

    // terrible, but works
    let tokens = success.into_iter().collect::<Result<_, _>>();
    let _ast = parse::parse(tokens.unwrap());

    // if errors.is_empty() {
    //     println!(
    //         "{:#?}",
    //         success
    //             .into_iter()
    //             .map(Result::unwrap)
    //             .map(|token| token.kind)
    //             .collect::<Vec<_>>()
    //     );
    // } else {
    //     errors
    //         .into_iter()
    //         .map(Result::unwrap_err)
    //         .for_each(|err| crate::errors::display_error(program, err));
    // }
}
