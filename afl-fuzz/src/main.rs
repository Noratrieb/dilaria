#[macro_use]
extern crate afl;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            let tokens = script_lang::Lexer::lex(s).collect::<Result<Vec<_>, _>>();
            if let Ok(tokens) = tokens {
                let _ = script_lang::parse(tokens);
            }
        }
    });
}
