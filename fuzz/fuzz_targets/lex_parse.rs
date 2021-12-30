#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let ast_alloc = dilaria::Bump::new();

    let lexer = dilaria::Lexer::new(&data);

    let _ast = dilaria::parse(lexer, &ast_alloc);
});
