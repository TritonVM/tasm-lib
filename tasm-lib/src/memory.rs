//! Memory convention used in this crate:
//!
//! - A memory page is contiguous memory of size 2^32 words, aligned to 2^32 word boundaries.
//!     There is one exception: due to the [base field's prime][prime] being 2^64 - 2^32 + 1,
//!     the last page, starting at address 2^64 - 2^32, is of size 1.
//! - The dynamic allocator lives at address [`DYN_MALLOC_ADDRESS`][dyn_malloc_addr], _i.e._, -1.
//!     It is a single word, containing the value of the next free page.
//!     It occupies the only page that is not of size 2^32 words.
//! - Page 0 is reserved for non-deterministically initialized memory.
//! - The last full page, number (2^32)-2, starting at address 2^64 - 2·(2^32),
//!     is reserved for [static allocations][static_malloc_addr].
//! - All other pages, i.e., pages 1 through (2^32)-3, are dynamically allocated.
//!
//! [prime]: BFieldElement::P
//! [dyn_malloc_addr]: dyn_malloc::DYN_MALLOC_ADDRESS
//! [static_malloc_addr]: crate::library::STATIC_MEMORY_START_ADDRESS

use std::collections::HashMap;

use triton_vm::prelude::*;

pub mod dyn_malloc;
pub mod memcpy;
pub mod push_ram_to_stack;

/// Non-deterministically initialized memory lives in the range $[0: 2^{32})$
///
/// See the [memory convention][self] for details.
pub const FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS: BFieldElement =
    BFieldElement::new(0);

pub const LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY: BFieldElement =
    BFieldElement::new(u32::MAX as u64);

/// Returns the address of the last populated word belonging to the memory region
/// designated for non-determinism.
pub fn last_populated_nd_memory_address(
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Option<u32> {
    memory
        .keys()
        .map(|b| b.value())
        .filter(|u| *u <= LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY.value())
        .max()
        .map(|address| address.try_into().unwrap())
}

/// Stores the encoding of the given object into memory at the given address, and returns
/// the address of the first untouched memory cell after.
pub fn encode_to_memory<T: BFieldCodec>(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    address: BFieldElement,
    object: &T,
) -> BFieldElement {
    let encoding = object.encode();
    for (i, e) in encoding.iter().enumerate() {
        memory.insert(address + BFieldElement::new(i as u64), *e);
    }
    address + BFieldElement::new(encoding.len() as u64)
}

/// Return the code to read `n` words from memory. Top of stack must point
/// to last word of words to read. Leaves mutated pointer on top of stack.
///
/// ```text
/// BEFORE: _ *last_word
/// AFTER:  _ [loaded_words; n] (*first_word - 1)
/// ```
pub fn load_words_from_memory_leave_pointer(n: usize) -> Vec<LabelledInstruction> {
    let num_full_chunk_reads = n / 5;
    let num_remaining_words = n % 5;
    let mut instructions = triton_asm![read_mem 5; num_full_chunk_reads];
    if num_remaining_words > 0 {
        instructions.extend(triton_asm!(read_mem {
            num_remaining_words
        }));
    }
    instructions
}

/// Return the code to read `n` words from memory. Top of stack must point
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

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use triton_vm::prelude::BFieldElement;

    use super::*;

    #[test]
    fn last_populated_nd_memory_address_looks_sane() {
        let empty_memory = HashMap::default();
        assert!(last_populated_nd_memory_address(&empty_memory).is_none());

        let empty_nd_region: HashMap<_, _> = [(bfe!(1u64 << 32), BFieldElement::new(42))]
            .into_iter()
            .collect();
        assert!(last_populated_nd_memory_address(&empty_nd_region).is_none());

        for address in [0, 1, 100, u32::MAX - 3, u32::MAX - 1, u32::MAX] {
            let one_populated_word: HashMap<_, _> = [(bfe!(address), BFieldElement::new(42))]
                .into_iter()
                .collect();
            assert_eq!(
                Some(address),
                last_populated_nd_memory_address(&one_populated_word)
            );
        }

        for address in [1, 100, u32::MAX - 3, u32::MAX - 1, u32::MAX] {
            let two_populated_words: HashMap<_, _> = [
                (bfe!(address), BFieldElement::new(42)),
                (bfe!(address - 1), BFieldElement::new(42)),
            ]
            .into_iter()
            .collect();
            assert_eq!(
                Some(address),
                last_populated_nd_memory_address(&two_populated_words)
            );
        }

        let much_poulated_zero_is_empty: HashMap<_, _> =
            (1..4000).map(|address| (bfe!(address), bfe!(42))).collect();
        assert_eq!(
            Some(3999),
            last_populated_nd_memory_address(&much_poulated_zero_is_empty)
        );

        let much_poulated_zero_populated: HashMap<_, _> =
            (0..4021).map(|address| (bfe!(address), bfe!(42))).collect();
        assert_eq!(
            Some(4020),
            last_populated_nd_memory_address(&much_poulated_zero_populated)
        );

        let scattered_population: HashMap<_, _> = [(bfe!(30), bfe!(42)), (bfe!(2000), bfe!(42))]
            .into_iter()
            .collect();
        assert_eq!(
            Some(2000),
            last_populated_nd_memory_address(&scattered_population)
        );
    }
}
