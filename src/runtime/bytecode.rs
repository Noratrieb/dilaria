//! The bytecode that is executed in the vm
//!
//! # Details
//!
//! ## Function Blocks
//! Every function is compiled into a bytecode block. These blocks are self-contained, and contain
//! all debug- and other information associated with it. The final bytecode is a collection of
//! these blocks.
//!
//! Note: Because of closures, function blocks have more required inputs than just the parameters,
//! but the compiler should handle that correctly.
//!
//! ## Local offsets
//! Variables offsets are calculated as `local offsets`. Local offsets are calculated relative to
//! the start of the space of the stack required by that function. The interpreter must keep track
//! of the stack start of each function, to be able to calculate these offsets.
//!
//! # Function calls
//! After the function returns, the interpreter resets its stack manually back
//! to the length before the call. This means the interpreter has to do some bookkeeping, but it has
//! to do that anyways.
//!
//! It is the compilers job to generate the correct loading of the arguments and assure that the arity
//! is correct before the `Call` instruction.
//!
//! See [`stack_frame`](`super::stack_frame`) for mode details

use std::fmt::{Debug, Formatter};

use bumpalo::collections::Vec;

use crate::{errors::Span, runtime::vm::Value};

/// This struct contains all data for a function.
pub struct FnBlock<'bc> {
    /// The bytecode of the function
    pub code: Vec<'bc, Instr>,
    /// The sizes of the stack required by the function after the instruction at the same index.
    /// This is only used during compilation to calculate local variable offsets.
    pub stack_sizes: Vec<'bc, usize>,
    /// The corresponding source code location of each instruction. This is debuginfo and only
    /// used if there are errors.
    pub spans: Vec<'bc, Span>,
    /// How many parameters the function accepts.
    /// Yes, it supports 4294967295 parameters. I dare you to overflow that.
    pub arity: u32,
}

impl Debug for FnBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.code.fmt(f)
    }
}

/// Index into the block list
pub type Function = usize;

/// A bytecode instruction. For more details on the structure of the bytecode,
/// read the module level docs [`bytecode`](`self`)
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "_debug", derive(dbg_pls::DebugPls))]
pub enum Instr {
    /// An operation that does nothing.
    Nop,

    /// Store the current value on the stack to the stack location with the local offset `usize`
    Store(usize),
    /// Load the variable value from the local offset `usize` onto the stack
    Load(usize),
    /// Push a value onto the stack
    PushVal(Value),
    /// Negate the top value on the stack. Only works with numbers and booleans
    Neg,

    // The binary operations. The `rhs` is on top of the stack, and `lhs` is below it
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

    /// If the current stack value is false, skip `usize` instructions.
    JmpFalse(isize),
    /// Same as `JmpFalse`, but unconditional
    Jmp(isize),

    /// Calls the function at the top of the stack, after the parameters
    Call,
    /// Returns from the function, removing that stack frame
    Return,

    /// Stop the program
    Exit,

    /// Shrinks the stack by `usize` elements, should always be emitted before backwards jumps
    ShrinkStack(usize),
}

#[cfg(feature = "_debug")]
impl dbg_pls::DebugPls for FnBlock<'_> {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_struct("FnBlock")
            .field("arity", &self.arity)
            .field("code", &self.code.as_slice())
            .field("stack_sizes", &self.stack_sizes.as_slice())
            .finish_non_exhaustive()
    }
}
