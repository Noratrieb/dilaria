language_name is a small embeddable scripting language

language_name is inspired by Javascript, Lox, Lua, Python, Rust and more

# Reference

## Overview

Declaring variables using `let`

```
let hello = 4;
```

Semicolons are needed :)

```
let test = 5;
let another = 4;
```

The language has strings, numbers, arrays, objects and null and booleans

```
let string = "hallo";
let number = 4;
let array = [];
let object = {};
let _null = null;
let bool = true;
```

You access properties on objects using `.`

```
let obj = {};
obj.hi = "hi!";
```

Functions are first class

```
let obj = {};
obj.hello = helloFn;
obj.hello();
```

Functions are declared using `fn`

```
fn greet(name) {
    return "hello, " + name;
}
```

Comments using `#`
```
# hi!
```

Multiline comments using `##` until `##`
```
##
hi
comment
##
```

There are many native functions, that can easily be customized and added/removed by the host

```
# rocket game
turnRocketLeft(29);
turnRocketRight(32);

# chat bot
message.respond("hi");

# dangerous http requests
fn callback(html) {
    print(html);
}
fetch("https://github.com/Nilstrieb", callback);
```

Basic arithmetic and boolean logic is available

```
let a = 5;
let b = 5;
print(a + b / b * b - a % b);
print(true and false or false or true and false);
```

Loops and conditionals

```
let x = true;
if x {
    print("true!");
} else {
    print("false :(");
}

loop {
    while 1 > 5 {
        print("yeet");
        break;
    }
    # no for loops for now, but will be added (probably like python)
}
```

_ is dynamically and *strongly* typed

## Detail

### Reserved Keywords

#### Statements
`fn`
`let`
`if`
`else`
`loop`
`while`
`for`
`break`

#### Values
`true`
`false`
`null`

#### Operators
`not`
`and`
`or`

### Operators
`==`
`>=`
`>`
`<=`
`<`
`!=`
`+`
`-`
`*`
`/`
`%`
