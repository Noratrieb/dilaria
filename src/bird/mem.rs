use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(Rc<String>),
    Object(Rc<HashMap<String, Value>>),
    Array(Rc<Vec<Value>>),
    Fn(Rc<()>),
}
