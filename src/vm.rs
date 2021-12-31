use crate::bytecode::{FnBlock, Instr};
use crate::gc::{Object, RtAlloc, Symbol};
use std::fmt::{Debug, Display, Formatter};

type VmError = &'static str;
type VmResult = Result<(), VmError>;

pub fn execute<'bc>(bytecode: &'bc [FnBlock<'bc>], alloc: RtAlloc) -> Result<(), VmError> {
    let mut vm = Vm {
        _blocks: bytecode,
        current: bytecode.first().ok_or("no bytecode found")?,
        pc: 0,
        stack: Vec::with_capacity(1024 << 5),
        _alloc: alloc,
    };

    vm.execute_function()
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Null,
    Bool(bool),
    Num(f64),
    String(Symbol),
    Array,
    Object(Object),
}

const _: () = _check_val_size();
const fn _check_val_size() {
    if std::mem::size_of::<Value>() != 24 {
        panic!("value got bigger!");
    }
}

struct Vm<'bc> {
    _blocks: &'bc [FnBlock<'bc>],
    current: &'bc FnBlock<'bc>,
    _alloc: RtAlloc,
    pc: usize,
    stack: Vec<Value>,
}

impl<'bc> Vm<'bc> {
    fn execute_function(&mut self) -> VmResult {
        let code = &self.current.code;

        loop {
            let instr = code.get(self.pc);
            match instr {
                Some(&instr) => self.dispatch_instr(instr)?,
                None => return Ok(()),
            }
            self.pc += 1;
        }
    }

    fn dispatch_instr(&mut self, instr: Instr) -> VmResult {
        match instr {
            Instr::Store(index) => {
                let val = self.stack.pop().unwrap();
                self.stack.insert(index, val);
            }
            Instr::Load(index) => self.stack.push(self.stack[index]),
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
            Instr::CmpGreater => todo!(),
            Instr::CmpGreaterEq => todo!(),
            Instr::CmpLess => todo!(),
            Instr::CmpLessEq => todo!(),
            Instr::CmpEq => todo!(),
            Instr::CmpNotEq => todo!(),
            Instr::Print => {
                let val = self.stack.pop().unwrap();
                println!("{}", val);
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

    fn type_error(&self) -> VmError {
        "bad type"
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
        }
    }
}
