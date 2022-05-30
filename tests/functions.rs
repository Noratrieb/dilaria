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

run_test!(
    parameters,
    r#"
fn fancy_print(str) {
    print str;
}

fancy_print("correct");
"#
);

run_test!(
    return_value,
    r#"
fn get_value() {
    return 1;
}

let x = get_value();
print x;
"#
);

run_test!(
    parameters_and_return,
    r#"
fn add(a, b) {
    return a + b;
}

let added = add(1, 5);

if added == 6 {
    print "correct";
} else {
    print "FAILED";
}
"#
);

run_test!(
    #[ignore]
    nested_calls,
    r#"
fn cooler_add(a, b) {
    return a + b;
}

fn add(a, b) {
    return cooler_add(a, b);
}

let added = add(1, 5);

if added == 6 {
    print "correct";
} else {
    print "FAILED";
}
"#
);

run_test!(
    #[ignore]
    fib5,
    r#"
fn fib(n) {
    if n < 2 {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

let fib5 = fib(5);
print fib5;
"#
);
