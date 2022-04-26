#[macro_export]
macro_rules! run_test {
    ($(#[$attr:tt])* $name:ident, $code:expr) => {
        $(#[$attr])*
        #[test]
        fn $name() {
            let code = $code;
            let output = $crate::common::_run_test(code);
            insta::assert_debug_snapshot!(output);
        }
    };
}

pub fn _run_test(code: &str) -> String {
    let mut stdout = Vec::<u8>::new();
    let mut cfg = dilaria::Config {
        debug: false,
        step: false,
        stdout: &mut stdout,
    };

    dilaria::run_program(code, &mut cfg);

    String::from_utf8(stdout).unwrap()
}
