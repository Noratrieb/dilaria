//! The bytecode that is executed in the vm

use crate::errors::Span;
use crate::vm::Value;
use bumpalo::collections::Vec;

#[derive(Debug)]
pub struct FnBlock<'bc> {
    pub code: Vec<'bc, Instr>,
    pub stack_sizes: Vec<'bc, usize>,
    pub spans: Vec<'bc, Span>,
    pub arity: u8,
}

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    /// Store the current value on the stack to the stack location with the local offset `usize`
    Store(usize),
    /// Load the variable value from the local offset `usize` onto the stack
    Load(usize),
    /// Push a value onto the stack
    PushVal(Value),
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

    /// If the current stack value is true, skip `usize` instructions.
    JumpFalse(usize),
    /// Same as `JumpCond`, but unconditional
    Jmp(usize),
}
