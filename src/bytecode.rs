use crate::value::{HashMap, Symbol};
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct FnBlock {
    pub code: Vec<Instr>,
    pub stack_sizes: Vec<usize>,
    pub arity: u8,
}

// todo: this should be copy in the end tbh
#[derive(Debug)]
pub enum Instr {
    /// Store the current value on the stack to the stack location with the local offset `usize`
    Store(usize),
    /// Load the variable value from the local offset `usize` onto the stack
    Load(usize),
    /// Push a value onto the stack
    PushVal(Box<Value>),
    /// Negate the top value on the stack. Only works with numbers and booleans
    Neg,
    BinAdd,
    BinSub,
    BinMul,
    BinDiv,
    BinMod,
    BinAnd,
    BinOr,
    CmpGreater,
    CmpGreaterEq,
    CmpLess,
    CmpLessEq,
    CmpEq,
    CmpNotEq,
}

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    String(Rc<str>),
    Array(Vec<Value>),
    Object(HashMap<Symbol, Value>),
}
