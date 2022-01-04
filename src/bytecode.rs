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

use crate::errors::Span;
use crate::vm::Value;
use bumpalo::collections::Vec;
use debug2::Formatter;

/// This struct contains all data for a function.
#[derive(Debug)]
pub struct FnBlock<'bc> {
    /// The bytecode of the function
    pub code: Vec<'bc, Instr>,
    /// The sizes of the stack required by the function at each instruction. This is only used
    /// during compilation to calculate local variable offsets.
    pub stack_sizes: Vec<'bc, usize>,
    /// The corresponding source code location of each instruction. This is debuginfo and only
    /// used if there are errors.
    pub spans: Vec<'bc, Span>,
    /// How many parameters the function accepts.
    pub arity: u8,
}

#[cfg(feature = "pretty")]
impl debug2::Debug for FnBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FnBlock")
            .field("code", &self.code.as_slice())
            .field("stack_sizes", &self.stack_sizes.as_slice())
            .field("spans", &self.spans.as_slice())
            .field("arity", &self.arity)
            .finish()
    }
}

/// A bytecode instruction. For more details on the structure of the bytecode, read the module level docs [`bytecode`](`self`)
#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "pretty", derive(debug2::Debug))]
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
}
