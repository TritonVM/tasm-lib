use std::collections::HashMap;

use crate::twenty_first::shared_math::bfield_codec::BFieldCodec;
use triton_vm::{instruction::LabelledInstruction, triton_asm, triton_instr, BFieldElement};

pub mod dyn_malloc;
pub mod dyn_malloc_const_size;
pub mod memcpy;
pub mod push_ram_to_stack;

/// Non-deterministially initialized memory lives in the range $[0: 2^{32})$
pub const FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS: BFieldElement =
    BFieldElement::new(0);

/// Stores the encoding of the given object into memory at the given address, and returns
/// the address of the first untouched memory cell after.
pub fn encode_to_memory<T: BFieldCodec>(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    object: T,
) -> BFieldElement {
    let encoding = object.encode();
    for (i, e) in encoding.iter().enumerate() {
        memory.insert(address + BFieldElement::new(i as u64), *e);
    }
    address + BFieldElement::new(encoding.len() as u64)
}

/// Return the code to read a `n` words from memory. Top of stack must point
/// to last word of words to read. Leaves mutated pointer on top of stack.
///
/// ```text
/// BEFORE: _ *last_word
/// AFTER:  _ [loaded_words; n] (*first_word - 1)
/// ```
pub fn load_words_from_memory_leave_pointer(n: usize) -> Vec<LabelledInstruction> {
    let num_full_chunk_reads = n / 5;
    let num_remaining_words = n % 5;
    let mut instructions = vec![triton_instr!(read_mem 5); num_full_chunk_reads];
    if num_remaining_words > 0 {
        instructions.extend(triton_asm!(read_mem {
            num_remaining_words
        }));
    }
    instructions
}

/// Return the code to read a `n` words from memory. Top of stack must point
/// to last word of words to read. Pops the memory pointer from the stack.
///
/// ```text
/// BEFORE: _ *last_word
/// AFTER:  _ [loaded_words; n]
/// ```
pub fn load_words_from_memory_pop_pointer(n: usize) -> Vec<LabelledInstruction> {
    let instructions = load_words_from_memory_leave_pointer(n);

    triton_asm!(
        {&instructions}
        pop 1
    )
}

/// Return the code to write `n` words to memory. Leaves a modified pointer
/// on the stack.
///
/// ```text
/// BEFORE: _ [words_to_write; n] *first_word
/// AFTER:  _ (*last_word + 1)
/// ```
pub fn write_words_to_memory_leave_pointer(n: usize) -> Vec<LabelledInstruction> {
    let num_full_chunk_reads = n / 5;
    let num_remaining_words = n % 5;
    let mut instructions = vec![triton_instr!(write_mem 5); num_full_chunk_reads];
    if num_remaining_words > 0 {
        instructions.extend(triton_asm!(write_mem {
            num_remaining_words
        }));
    }

    instructions
}

/// Return the code to write `n` words to memory. Pops the memory pointer.
///
/// ```text
/// BEFORE: _ [words_to_write; n] *first_word
/// AFTER:  _
/// ```
pub fn write_words_to_memory_pop_pointer(n: usize) -> Vec<LabelledInstruction> {
    let instructions = write_words_to_memory_leave_pointer(n);

    triton_asm!(
        {&instructions}
        pop 1
    )
}
