#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: String| {
    let lexer = dilaria::Lexer::new(&data);
    for _ in lexer {}
});
