//! Memory convention used in this crate:
//!
//! - A memory page is contiguous memory of size 2^32 words, aligned to 2^32 word boundaries.
//!   There is one exception: due to the [base field's prime][prime] being 2^64 - 2^32 + 1,
//!   the last page, starting at address 2^64 - 2^32, is of size 1.
//! - The dynamic allocator lives at address [`DYN_MALLOC_ADDRESS`][dyn_malloc_addr], _i.e._, -1.
//!   It is a single word, containing a counter of allocated pages.
//!   It occupies the only page that is not of size 2^32 words.
//! - Page 0 is reserved for non-deterministically initialized memory.
//! - The last full page, number (2^32)-2, starting at address 2^64 - 2·(2^32),
//!   is reserved for [static allocations][static_malloc_addr].
//! - The two preceding pages, number 2^32-4 and 2^32-3 are used by the
//!   [STARK verifier][stark_verifier] (when standard memory layouts are used).
//! - Pages 1 through 2^31-1 are dynamically allocated by the
//!   [dynamic allocator][dynamic_allocator] snippet.
//! - Pages 2^31 through 2^32-5 are not in use by this crate.
//!
//! [prime]: BFieldElement::P
//! [dyn_malloc_addr]: dyn_malloc::DYN_MALLOC_ADDRESS
//! [static_malloc_addr]: crate::library::STATIC_MEMORY_FIRST_ADDRESS
//! [stark_verifier]: crate::verifier::stark_verify::StarkVerify
//! [dynamic_allocator]: dyn_malloc::DynMalloc

use std::collections::HashMap;

use num::One;
use triton_vm::memory_layout::MemoryRegion;
use triton_vm::prelude::*;

pub mod dyn_malloc;
pub mod memcpy;

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
) -> Option<BFieldElement> {
    memory
        .keys()
        .map(|b| b.value())
        .filter(|u| *u <= LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY.value())
        .max()
        .map(|address| bfe!(address))
}

/// Returns the address of the first unpopulated word belonging to the memory
/// region designated for non-determinism.
pub fn first_free_nd_address(
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Option<BFieldElement> {
    let last_populated = last_populated_nd_memory_address(memory);
    match last_populated {
        None => Some(FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS),
        Some(last_populated) => match last_populated {
            LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY => None,
            addr => Some(addr + BFieldElement::one()),
        },
    }
}

pub fn nd_memory_region() -> MemoryRegion {
    let size = LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY.value()
        - FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS.value();

    MemoryRegion::new(
        FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS,
        size.try_into().unwrap(),
    )
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
        memory.insert(address + bfe!(i), *e);
    }
    address + bfe!(encoding.len())
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

    use num::Zero;
    use proptest::prop_assert;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;
    use triton_vm::prelude::BFieldElement;

    use super::*;

    #[test]
    fn last_populated_nd_memory_address_looks_sane() {
        let empty_memory = HashMap::default();
        assert!(last_populated_nd_memory_address(&empty_memory).is_none());
        assert_eq!(
            Some(BFieldElement::zero()),
            first_free_nd_address(&empty_memory)
        );

        let empty_nd_region: HashMap<_, _> = [(bfe!(1u64 << 32), BFieldElement::new(42))]
            .into_iter()
            .collect();
        assert!(last_populated_nd_memory_address(&empty_nd_region).is_none());
        assert_eq!(
            Some(BFieldElement::zero()),
            first_free_nd_address(&empty_nd_region)
        );

        for address in [0, 1, 100, u32::MAX - 3, u32::MAX - 1, u32::MAX] {
            let one_populated_word: HashMap<_, _> = [(bfe!(address), BFieldElement::new(42))]
                .into_iter()
                .collect();
            assert_eq!(
                Some(bfe!(address)),
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
                Some(bfe!(address)),
                last_populated_nd_memory_address(&two_populated_words)
            );
        }

        let much_poulated_zero_is_empty: HashMap<_, _> =
            (1..4000).map(|address| (bfe!(address), bfe!(42))).collect();
        assert_eq!(
            Some(bfe!(3999)),
            last_populated_nd_memory_address(&much_poulated_zero_is_empty)
        );
        assert_eq!(
            Some(bfe!(4000)),
            first_free_nd_address(&much_poulated_zero_is_empty)
        );

        let much_poulated_zero_populated: HashMap<_, _> =
            (0..4021).map(|address| (bfe!(address), bfe!(42))).collect();
        assert_eq!(
            Some(bfe!(4020)),
            last_populated_nd_memory_address(&much_poulated_zero_populated)
        );
        assert_eq!(
            Some(bfe!(4021)),
            first_free_nd_address(&much_poulated_zero_populated)
        );

        let scattered_population: HashMap<_, _> = [(bfe!(30), bfe!(42)), (bfe!(2000), bfe!(42))]
            .into_iter()
            .collect();
        assert_eq!(
            Some(bfe!(2000)),
            last_populated_nd_memory_address(&scattered_population)
        );
        assert_eq!(
            Some(bfe!(2001)),
            first_free_nd_address(&scattered_population)
        );
    }

    #[test]
    fn first_free_nd_address_looks_sane() {
        for address in [0, 1, 100, u32::MAX - 3, u32::MAX - 3] {
            let one_populated_word: HashMap<_, _> = [(bfe!(address), BFieldElement::new(42))]
                .into_iter()
                .collect();
            assert_eq!(
                Some(bfe!(address + 1)),
                first_free_nd_address(&one_populated_word)
            );

            let two_populated_words: HashMap<_, _> = [
                (bfe!(address), BFieldElement::new(42)),
                (bfe!(address + 1), BFieldElement::new(42)),
            ]
            .into_iter()
            .collect();
            assert_eq!(
                Some(bfe!(address + 2)),
                first_free_nd_address(&two_populated_words)
            );
        }

        let last_word_populated: HashMap<_, _> = [(
            LAST_ADDRESS_AVAILABLE_FOR_NON_DETERMINISTICALLY_ALLOCATED_MEMORY,
            bfe!(42),
        )]
        .into_iter()
        .collect();
        assert!(first_free_nd_address(&last_word_populated).is_none());
    }

    #[proptest]
    fn all_addresses_between_0_and_u32max_belong_to_nd_memory(nd_address: u32) {
        prop_assert!(nd_memory_region().contains_address(bfe!(nd_address)));
    }

    #[proptest]
    fn addresses_outside_u32_range_do_not_belong_to_nd_memory(
        #[strategy(arb())]
        #[filter(#address.value() > u32::MAX as u64)]
        address: BFieldElement,
    ) {
        prop_assert!(!nd_memory_region().contains_address(address));
    }
}
