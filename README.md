There is currently a tree-walking interpreter called `bird` in progress, but the plan is to have a bytecode vm in the end

language_name is a small embeddable scripting language

language_name is inspired by Javascript, Lox, Lua, Python, Rust and more

# Reference

## Overview

Declaring variables using `let`

```rust
let hello = 4;
```

Semicolons are needed :)

```rust
let test = 5;
let another = 4;
```

The language has strings, numbers, arrays, objects and null and booleans

```rust
let string = "hallo";
let number = 4; 
let array = [];
let object = {};
let _null = null;
let bool = true;
```

You access properties on objects using `.`

```rust
let obj = {};
obj.hi = "hi!";
```

There is the `print` statement to print a value, but this will be removed
```rust
let name = "nils";
print name;
```

Functions are first class

```rust
let obj = {};
obj.hello = helloFn;
obj.hello();
```

Functions are declared using `fn`

```rust
fn greet(name) {
    return "hello, " + name;
}
```

Functions are closures

Comments using `#`
```py
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

```rust
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

```rust
let a = 5;
let b = 5;
print(a + b / b * b - a % b);
print(true and false or false or true and false);
```

Loops and conditionals

```rust
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
