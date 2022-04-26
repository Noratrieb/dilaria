//! # ABI
//! Function arguments are passed on the stack and can be loaded just like local variables. They belong
//! to the stack frame of the new function and are cleaned up after returning, leaving the return value where
//! the stack frame was
//!
//! When a call happens, the current stack offset is pushed onto the stack as a `Value::Native` and
//! the element before it is stored as the new offset.
//! Then all parameters are pushed onto the stack, from first to last
//! Afterwards, execution of the code is started. A function always has to return, and compiler
//! inserts `return null` at the end of every function implicitly.
//!
//! If a return happens, the VM loads the current value on the stack. It then goes to the start
//! of the stack frame and saves the `Value::Native` that stores the old stack offset and loads that
//! into its stack offset. It then removes the whole stack frame from the stack, and pushes the
//! returned value.
//!
//! ```text
//!             old stack frame offset─╮
//!         ╭─Parameters─╮             │           old Function─╮     local─╮
//!         v            v             v                        v           v  
//! ───────┬─────────┬──────────┬─────────────┬────────────┬──────────┬─────────╮
//! Num(6) │ Num(5)  │  Num(6)  │ NativeU(20) │ NativeU(4) │ Function │  Num(5) │
//! ───────┴─────────┴──────────┴─────────────┴────────────┴──────────┴─────────╯
//!  ^     ╰────────────────────────────────────────────────────────────────── current stack frame
//!  │                                             ^
//!  ╰─ old local                                  ╰─old PC
//!
//!         ^
//! Vm      ╰──────────────────╮
//!                            │
//! Current stack frame offset─╯
//!
//! ```

use crate::runtime::{
    bytecode::Function,
    vm::{Value, Vm},
};

pub struct Frame<'s> {
    frame_slice: &'s [Value],
    params: u32,
}

impl<'s> Frame<'s> {
    /// Create a new stack frame with the VM state. The VM must set its state to the new function itself.
    /// The parameters need to be pushed already and be the topmost values on the stack.
    ///
    /// Returns the new stack frame offset
    pub(super) fn create(vm_state: &'s mut Vm, params: u32) -> usize {
        let new_frame_offset = vm_state.stack.len() - (params as usize);

        let old_stack_offset = vm_state.stack_frame_offset;
        let old_fn_block = vm_state.current_block_index;
        let old_pc = vm_state.pc;

        vm_state.stack.push(Value::NativeU(old_stack_offset));
        vm_state.stack.push(Value::NativeU(old_pc));
        vm_state.stack.push(Value::Function(old_fn_block));

        let frame_slice = &vm_state.stack[new_frame_offset..];

        new_frame_offset
    }

    pub fn new(frame_slice: &'s [Value], params: u32) -> Self {
        Self {
            frame_slice,
            params,
        }
    }

    pub fn old_stack_offset(&self) -> usize {
        self.frame_slice[self.params as usize].unwrap_native_int()
    }

    pub fn old_pc(&self) -> usize {
        self.frame_slice[self.params as usize + 1].unwrap_native_int()
    }

    pub fn old_fn_block(&self) -> Function {
        self.frame_slice[self.params as usize + 2].unwrap_function()
    }
}

impl Value {
    /// Unwrap the Value into a `usize` expecting the `NativeU` variant
    fn unwrap_native_int(&self) -> usize {
        if let Value::NativeU(n) = self {
            *n
        } else {
            unreachable!("expected native int, got {:?}", self);
        }
    }

    /// Unwrap the Value into a `Function` expecting the `Function` variant
    pub fn unwrap_function(&self) -> Function {
        if let Value::Function(fun) = self {
            *fun
        } else {
            unreachable!("expected function, got {:?}", self);
        }
    }
}
