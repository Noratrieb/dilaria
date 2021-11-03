use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Hash)]
enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
}
