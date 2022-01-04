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

run_test!(
    while_single_loop,
    r#"
let x = true;
while x {
    x = false;
    print "iter";
}
print "done";
"#
);

run_test!(
    while_count_to_100,
    r#"
let i = 0;

while i < 100 {
    print i;
    i = i + 1;
}

print "done";
    "#
);

run_test!(
    while_run_never,
    r#"
let not_run = true;

while false {
    print "WRONG";
    not_run = false;
}

if not_run {
    print "good.";
}
"#
);
