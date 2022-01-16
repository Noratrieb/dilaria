use crate::bytecode::{FnBlock, Function, Instr};
use crate::gc::{Object, RtAlloc, Symbol};
use crate::Config;
use std::fmt::{Debug, Display, Formatter};
use std::io::{Read, Write};
use std::ptr::NonNull;

type VmError = &'static str;
type VmResult = Result<(), VmError>;

pub fn execute<'bc>(
    bytecode: &'bc [FnBlock<'bc>],
    alloc: RtAlloc,
    cfg: &mut Config,
) -> Result<(), VmError> {
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

    vm.execute_function()
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "_debug", derive(debug2::Debug))]
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

#[cfg(target_pointer_width = "64")]
const _: [(); 24] = [(); std::mem::size_of::<Value>()];

#[derive(Debug, Clone, Copy)]
pub struct Ptr(NonNull<()>);

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
    /// Index of the instruction currently being executed
    pc: usize,
}

impl<'bc> Vm<'bc, '_> {
    fn execute_function(&mut self) -> VmResult {
        let code = &self.current.code;

        loop {
            let instr = code.get(self.pc);
            match instr {
                Some(&instr) => self.dispatch_instr(instr)?,
                None => return Ok(()),
            }
            debug_assert_eq!(self.current.stack_sizes[self.pc], self.stack.len());
            self.pc += 1;
        }
    }

    fn dispatch_instr(&mut self, instr: Instr) -> VmResult {
        if self.step {
            self.step_debug();
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
                    _ => return Err(self.type_error()),
                }
            }
            Instr::BinAdd => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                _ => Err("bad type"),
            })?,
            Instr::BinSub => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
                _ => Err("bad type"),
            })?,
            Instr::BinMul => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
                _ => Err("bad type"),
            })?,
            Instr::BinDiv => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
                _ => Err("bad type"),
            })?,
            Instr::BinMod => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a % b)),
                _ => Err("bad type"),
            })?,
            Instr::BinAnd => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                _ => Err("bad type"),
            })?,
            Instr::BinOr => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                _ => Err("bad type"),
            })?,
            Instr::CmpGreater => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a > b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() > b.as_str())),
                _ => Err("bad type"),
            })?,
            Instr::CmpGreaterEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a >= b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() >= b.as_str())),
                _ => Err("bad type"),
            })?,
            Instr::CmpLess => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a < b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() < b.as_str())),
                _ => Err("bad type"),
            })?,
            Instr::CmpLessEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a <= b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a.as_str() <= b.as_str())),
                _ => Err("bad type"),
            })?,
            Instr::CmpEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Null, Value::Null) => Ok(TRUE),
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a == b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
                (Value::Object(_a), Value::Object(_b)) => todo!(),
                (Value::Array, Value::Array) => Ok(TRUE),
                _ => Err("bad type"),
            })?,
            Instr::CmpNotEq => self.bin_op(|lhs, rhs| match (lhs, rhs) {
                (Value::Null, Value::Null) => Ok(FALSE),
                (Value::Num(a), Value::Num(b)) => Ok(Value::Bool(a != b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a != b)),
                (Value::Object(_a), Value::Object(_b)) => todo!(),
                (Value::Array, Value::Array) => Ok(FALSE),
                _ => Err("bad type"),
            })?,
            Instr::Print => {
                let val = self.stack.pop().unwrap();
                writeln!(self.stdout, "{}", val).map_err(|_| "failed to write to stdout")?;
            }
            Instr::JmpFalse(pos) => {
                let val = self.stack.pop().unwrap();
                match val {
                    Value::Bool(false) => self.pc = (self.pc as isize + pos) as usize,
                    Value::Bool(true) => {}
                    _ => return Err("bad type"),
                }
            }
            Instr::Jmp(pos) => self.pc = (self.pc as isize + pos) as usize,
            Instr::Call => self.call()?,
            Instr::Return => todo!(),
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
        if let Value::Function(func) = function {
            let fn_block = &self.blocks[func];

            let new_stack_frame_start = self.stack.len() - fn_block.arity as usize;
            self.stack_offset = new_stack_frame_start;

            self.stack.push(Value::NativeU(old_offset));
            self.stack.push(Value::NativeU(self.pc));
            self.stack.push(Value::Function(old_idx));

            self.current_block_index = func;
            self.current = fn_block;

            self.pc = 0;

            // TODO don't be recursive like this
            self.execute_function()?;
        } else {
            return Err("not a function");
        }

        Ok(())
    }

    fn type_error(&self) -> VmError {
        "bad type"
    }

    fn step_debug(&self) {
        let current_instr = &self.current.code[self.pc];
        let curr_stack_size = self.stack.len();
        let expected_stack_size = &self.current.stack_sizes[self.pc];

        eprintln!(
            "Current Instruction: {:?}
Current Stack size: {}
Expected Stack size after instruction: {}",
            current_instr, curr_stack_size, expected_stack_size
        );

        let mut buf = [0; 64];
        let _ = std::io::stdin().read(&mut buf);
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

#[cfg(feature = "_debug")]
impl debug2::Debug for Ptr {
    fn fmt(&self, f: &mut debug2::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ptr").finish()
    }
}
