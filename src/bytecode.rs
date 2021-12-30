//! The bytecode that is executed in the vm

use crate::errors::Span;
use crate::value::{HashMap, NewSym};
use bumpalo::boxed::Box;
use bumpalo::collections::Vec;
use std::rc::Rc;

#[derive(Debug)]
pub struct FnBlock<'bc> {
    pub code: Vec<'bc, Instr<'bc>>,
    pub stack_sizes: Vec<'bc, usize>,
    pub spans: Vec<'bc, Span>,
    pub arity: u8,
}

// todo: this should be copy in the end tbh
#[derive(Debug)]
pub enum Instr<'bc> {
    /// Store the current value on the stack to the stack location with the local offset `usize`
    Store(usize),
    /// Load the variable value from the local offset `usize` onto the stack
    Load(usize),
    /// Push a value onto the stack
    PushVal(Box<'bc, Value>),
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

    /// Println the value on top of the stack
    Print,
}

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    String(Rc<str>),
    Array,
    Object(HashMap<NewSym, Value>),
}
