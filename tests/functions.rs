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

run_test!(
    multiple_calls,
    r#"
fn test1() {
    print "correct1";
}

fn test2() {
    print "correct2";
}

test1();
test2();

print "correct3";
"#
);
