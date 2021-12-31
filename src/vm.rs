use crate::bytecode::FnBlock;
use crate::gc::RtAlloc;

type VmResult = Result<(), ()>;

pub fn execute<'bc>(bytecode: &'bc [FnBlock<'bc>], alloc: RtAlloc) -> Result<(), ()> {
    let _vm = Vm {
        blocks: bytecode,
        current: bytecode.first().ok_or(())?,
        alloc,
    };

    Ok(())
}

struct Vm<'bc> {
    blocks: &'bc [FnBlock<'bc>],
    current: &'bc FnBlock<'bc>,
    alloc: RtAlloc,
}
