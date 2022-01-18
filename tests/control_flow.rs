mod common;

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

run_test!(
    fizzbuzz,
    r#"
let i = 1;

while i < 100 {
    if i % 15 == 0 {
        print "FizzBuzz";
    } else if i % 5 == 0 {
        print "Buzz";
    } else if i % 3 == 0 {
        print "Fizz";
    } else {
        print i;
    }
    i = i + 1;
}
"#
);

run_test!(
    break_out_loop,
    r#"
print "Start";

loop {
    break;
    print "WRONG";
}  

print "Good end";
"#
);

run_test!(
    break_out_while,
    r#"
print "Start";

while true {
    break;
    print "WRONG";
}  

print "Good end";
"#
);

run_test!(
    fizzbuzz_with_loop,
    r#"
let i = 1;

loop {
    if i % 15 == 0 {
        print "FizzBuzz";
    } else if i % 5 == 0 {
        print "Buzz";
    } else if i % 3 == 0 {
        print "Fizz";
    } else {
        print i;
    }
    i = i + 1;
    
    if i >= 100 {
        break;
    }
}
"#
);

run_test!(
    nested_loop_break,
    r#"
print "Start";
loop {
    print "Start inner";
    loop {
        print "inside inner";
        break;
        print "WRONG";
    }
    print "Outside inner";
    break;
    print "WRONG";
}
print "End";
"#
);
