mod common;
use crate::common::_run_test;

run_test!(
    single_if,
    r#"
if true {
    print "true!";
}

if false {
    print "WRONG";
}
"#
);

run_test!(
    if_else,
    r#"
if true {
    print "true!";
} else {
    print "WRONG";
}

if false {
    print "WRONG";
} else {
    print "true!";
}
"#
);

run_test!(
    if_else_if,
    r#"
if false {
    print "WRONG";
} else if true {
    print "true!";
} else {
    print "WRONG";
}

if false {
    print "WRONG";
} else if false {
    print "WRONG";
} else {
    print "true!";
}
"#
);
