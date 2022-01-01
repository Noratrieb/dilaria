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

run_test!(
    if_else_long_comparison_chain,
    r#"
let string = "hi ._./";

if string == "no" {
    print "WRONG";
} else if string == "no as well" {
    print "WRONG";
} else if string == "wrong" {
    print "WRONG";
} else if string == "not the correct one" {
    print "WRONG";
} else if string == "hi ._. (wrong)" {
    print "WRONG";
} else if string == "" {
    print "WRONG";
} else if string == "how wrong should it be?" {
    print "WRONG";
} else if string == "hi ._./" {
    print "true!";
} else {
    print "WRONG";
}
"#
);
