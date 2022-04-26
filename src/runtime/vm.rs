use std::{
    fmt::{Debug, Display, Formatter},
    io::{Read, Write},
};

use crate::{
    runtime::{
        bytecode::{FnBlock, Function, Instr},
        gc::{Object, RtAlloc, Symbol},
    },
    util, Config,
};

type ActualBackingVmError = &'static str;

type VmError = Box<VmErrorInner>;

#[derive(Debug)]
enum VmErrorInner {
    Exit,
    Error(ActualBackingVmError),
}

type VmResult = Result<(), VmError>;

// never get bigger than a machine word.
util::assert_size!(VmResult <= std::mem::size_of::<usize>());

type PublicVmError = ActualBackingVmError;

pub fn execute<'bc>(
    bytecode: &'bc [FnBlock<'bc>],
    alloc: RtAlloc,
    cfg: &mut Config,
) -> Result<(), PublicVmError> {
    let mut vm = Vm {
        blocks: bytecode,
        current: bytecode.first().ok_or("no bytecode found")?,
        current_block_index: 0,
        stack_offset: 0,
        pc: 0,
        stack: Vec::with_capacity(1024 << 5),
        _alloc: alloc,
        stdout: cfg.stdout,
        step: cfg.step,
    };

    match vm.execute_function() {
        Ok(()) => Ok(()),
        Err(boxed) => match *boxed {
            VmErrorInner::Exit => Ok(()),
            VmErrorInner::Error(err) => Err(err),
        },
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "_debug", derive(dbg_pls::DebugPls))]
pub enum Value {
    /// `null`
    Null,
    /// A boolean value
    Bool(bool),
    /// A floating point number
    Num(f64),
    /// An interned string
    String(Symbol),
    /// An array of values
    Array,
    /// A map from string to value
    Object(Object),
    /// A first-class function object
    Function(Function),
    /// A value that is stored by the vm for bookkeeping and should never be accessed for anything else
    NativeU(usize),
}

util::assert_size!(Value <= 24);

const TRUE: Value = Value::Bool(true);
const FALSE: Value = Value::Bool(false);

struct Vm<'bc, 'io> {
    // -- global
    blocks: &'bc [FnBlock<'bc>],
    _alloc: RtAlloc,
    stack: Vec<Value>,
    stdout: &'io mut dyn Write,
    step: bool,

    // -- local to the current function
    /// The current function
    current: &'bc FnBlock<'bc>,
    current_block_index: usize,
    /// The offset of the first parameter of the current function
    stack_offset: usize,
    /// Index of the next instruction being executed. is out of bounds if the current
    /// instruction is the last one
    pc: usize,
}

impl<'bc> Vm<'bc, '_> {
    fn execute_function(&mut self) -> VmResult {
        loop {
            let instr = self.current.code.get(self.pc);
            self.pc += 1;
            match instr {
                Some(&instr) => self.dispatch_instr(instr)?,
                None => return Ok(()),
            }
            if self.pc > 0 {
                // debug_assert_eq!(self.current.stack_sizes[self.pc - 1], self.stack.len());
            }
        }
    }

    fn dispatch_instr(&mut self, instr: Instr) -> VmResult {
        if self.step {
            self.step_debug(instr);
        }

        match instr {
            Instr::Nop => {}
            Instr::Store(index) => {
                let val = self.stack.pop().unwrap();
                self.stack[self.stack_offset + index] = val;
            }
            Instr::Load(index) => self.stack.push(self.stack[self.stack_offset + index]),
            Instr::PushVal(value) => self.stack.push(value),
            Instr::Neg => {
                let val = self.stack.pop().unwrap();
                match val {
                    Value::Bool(bool) => self.stack.push(Value::Bool(!bool)),
                    Value::Num(float) => self.stack.push(Value::Num(-float)),
                    _ => return Err(err("bad type")),
                }
            }
            Instr::BinAdd => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinSub => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinMul => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinDiv => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinMod => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a % b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinAnd => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                _ => Err(err("bad type")),
            })?,
            Instr::BinOr => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpGreater => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a > b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() > b.as_str())),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpGreaterEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a >= b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() >= b.as_str())),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpLess => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a < b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() < b.as_str())),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpLessEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a <= b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() <= b.as_str())),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Null, Value::Null) => Ok(TRUE),
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a == b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
                (Value::Object(_a), Value::Object(_b)) => todo!(),
                (Value::Array, Value::Array) => Ok(TRUE),
                _ => Err(err("bad type")),
            })?,
            Instr::CmpNotEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Null, Value::Null) => Ok(FALSE),
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a != b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a != b)),
                (Value::Object(_a), Value::Object(_b)) => todo!(),
                (Value::Array, Value::Array) => Ok(FALSE),
                _ => Err(err("bad type")),
            })?,
            Instr::Print => {
                let val = self.stack.pop().unwrap();
                writeln!(self.stdout, "{}", val).map_err(|_| err("failed to write to stdout"))?;
            }
            Instr::JmpFalse(pos) => {
                let val = self.stack.pop().unwrap();
                match val {
                    Value::Bool(false) => self.pc = (self.pc as isize + pos) as usize,
                    Value::Bool(true) => {}
                    _ => return Err(err("bad type")),
                }
            }
            Instr::Jmp(pos) => self.pc = (self.pc as isize + pos) as usize,
            Instr::Call => self.call()?,
            Instr::Return => self.ret()?,
            Instr::Exit => return Err(Box::new(VmErrorInner::Exit)),
            Instr::ShrinkStack(size) => {
                assert!(self.stack.len() >= size);
                let new_len = self.stack.len() - size;
                // SAFETY: We only ever shrink the vec, and we don't overflow. Value is copy so no leaks as a bonus
                unsafe { self.stack.set_len(new_len) }
            }
        }

        Ok(())
    }

    fn bin_op<F>(&mut self, f: F) -> VmResult
    where
        F: FnOnce(Value, Value) -> Result<Value, VmError>,
    {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        let result = f(lhs, rhs)?;
        self.stack.push(result);
        Ok(())
    }

    fn call(&mut self) -> VmResult {
        let old_offset = self.stack_offset;
        let old_idx = self.current_block_index;
        let function = self.stack.pop().unwrap();
        let function = function.unwrap_function();
        let fn_block = &self.blocks[function];

        let new_stack_frame_start = self.stack.len();
        self.stack_offset = new_stack_frame_start;

        self.stack.push(Value::NativeU(old_offset));
        self.stack.push(Value::NativeU(self.pc));
        self.stack.push(Value::Function(old_idx));

        self.current_block_index = function;
        self.current = fn_block;

        self.pc = 0;

        // we are now set up correctly, let the next instruction run

        Ok(())
    }

    fn ret(&mut self) -> VmResult {
        let current_arity: usize = self.current.arity.try_into().unwrap();

        // we save the return value first.
        let return_value = self.stack.pop().expect("return value");

        let bookkeeping_offset = self.stack_offset + current_arity;

        let inner_stack_offset = self.stack_offset;

        // now, we get all the bookkeeping info out
        let old_stack_offset = self.stack[bookkeeping_offset].unwrap_native_int();
        let old_pc = self.stack[bookkeeping_offset + 1].unwrap_native_int();
        let old_function = self.stack[bookkeeping_offset + 2].unwrap_function();

        // get the interpreter back to the nice state
        self.stack_offset = old_stack_offset;
        self.pc = old_pc;
        self.current_block_index = old_function;
        self.current = &self.blocks[old_function];

        // and kill the function stack frame
        // note: don't emit a return instruction from the whole global script.
        unsafe { self.stack.set_len(inner_stack_offset) };

        // everything that remains...
        self.stack.push(return_value);

        Ok(())
    }

    fn step_debug(&self, current_instr: Instr) {
        let curr_stack_size = self.stack.len();
        // at this point, we've always incremented the pc already
        let expected_stack_size = &self.current.stack_sizes[self.pc - 1];

        eprintln!(
            "Next Instruction: {current_instr:?}
Current Stack size: {curr_stack_size}
Expected Stack size after instruction: {expected_stack_size}
Stack: {:?}",
            self.stack
        );

        let mut buf = [0; 64];
        let _ = std::io::stdin().read(&mut buf);
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
    fn unwrap_function(&self) -> Function {
        if let Value::Function(fun) = self {
            *fun
        } else {
            unreachable!("expected function, got {:?}", self);
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Bool(bool) => Display::fmt(bool, f),
            Value::Num(num) => Display::fmt(num, f),
            Value::String(str) => f.write_str(str.as_str()),
            Value::Array => todo!(),
            Value::Object(_) => todo!(),
            Value::Function(_) => f.write_str("[function]"),
            Value::NativeU(_) => panic!("Called display on native value!"),
        }
    }
}

fn err(msg: &'static str) -> VmError {
    Box::new(VmErrorInner::Error(msg))
}
