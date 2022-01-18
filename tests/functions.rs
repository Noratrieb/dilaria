mod common;

run_test!(
    single_call,
    r#"
fn test() {
    print "correct";
}

test();
"#
);

run_test!(
    single_call_expect_return,
    r#"
fn test() {
    print "correct1";
}

test();

print "correct2";
"#
);
