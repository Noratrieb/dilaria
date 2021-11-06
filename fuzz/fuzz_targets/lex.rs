#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let lexer = script_lang::Lexer::lex(&data);
    let _tokens = lexer.collect::<Vec<_>>();
});
