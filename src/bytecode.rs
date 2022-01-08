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
//! # ABI
//! Function arguments are passed on the stack and can be loaded just like local variables. They belong
//! to the stack frame of the new function and are cleaned up after returning, leaving the return value where
//! the stack frame was.
//!
//! When a call happens, the current stack offset is pushed onto the stack as a `Value::Native` and
//! the element before it is stored as the new offset.
//! Then all parameters are pushed onto the stack, from last to first.
//! Afterwards, execution of the code is started. A function always has to return, and compiler
//! inserts `return null` at the end of every function implicitly.
//!
//! If a return happens, the VM loads the current value on the stack. It then goes to the start
//! of the stack frame and saves the `Value::Native` that stores the old stack offset and loads that
//! into its stack offset. It then removes the whole stack frame from the stack, and pushes the
//! returned value.
//!
//! ```text
//!  old stack offset ╮     ╭ Parameters ╮       ╭ local
//!                   v     v            v       v
//! ───────┬────────────┬───────────┬──────────┬─────────╮
//! Num(6) │ Native(20) │   Num(5)  │  Num(6)  │  Num(5) │
//! ───────┴────────────┴───────────┴──────────┴─────────╯
//!        ╰────────────────────────────────────────────────── current stack frame
//!  ^                  ^
//!  ╰─ old local       ╰╮      
//!                      │
//!                      │
//! Vm                   │
//! Current stack offset ╯
//!
//! ```

use crate::errors::Span;
use crate::vm::Value;
use bumpalo::collections::Vec;
use debug2::Formatter;

/// This struct contains all data for a function.
#[derive(Debug)]
pub struct FnBlock<'bc> {
    /// The bytecode of the function
    pub code: Vec<'bc, Instr<'bc>>,
    /// The sizes of the stack required by the function after the instruction at the same index. This is only used
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
pub enum Instr<'bc> {
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

    Call(&'bc FnBlock<'bc>),

    Return,

    /// Shrinks the stack by `usize` elements, should always be emitted before backwards jumps
    ShrinkStack(usize),
}
