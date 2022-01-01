#[macro_export]
macro_rules! run_test {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            let code = $code;
            let output = _run_test(code);
            insta::assert_debug_snapshot!(output);
        }
    };
}

#[doc(hidden)]
pub fn _run_test(code: &str) -> String {
    let mut stdout = Vec::<u8>::new();
    let mut cfg = dilaria::Config {
        debug: false,
        stdout: &mut stdout,
    };

    dilaria::run_program(code, &mut cfg);

    String::from_utf8(stdout).unwrap()
}
