#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let lexer = script_lang::Lexer::lex(&data);
    let tokens = lexer.collect::<Result<Vec<_>, _>>();

    if let Ok(tokens) = tokens {
        let _ast = script_lang::parse(tokens);
    }
});
