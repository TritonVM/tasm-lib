use std::collections::HashMap;

use triton_vm::prelude::BFieldCodec;
use triton_vm::prelude::BFieldElement;

pub fn write_to_memory<T: BFieldCodec>(
    mut pointer: BFieldElement,
    value: T,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    for word in value.encode() {
        memory.insert(pointer, word);
        pointer.increment();
    }
}
