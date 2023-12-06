use std::collections::HashMap;

use num_traits::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::memory::dyn_malloc;

// Syntactic sugar for setting memory[DYN_MALLOC_ADDRESS]
pub fn rust_dyn_malloc_initialize(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initial_value: usize,
) {
    memory.insert(
        BFieldElement::new(dyn_malloc::DYN_MALLOC_ADDRESS as u64),
        BFieldElement::new(initial_value as u64),
    );
}

pub fn dynamic_allocator(
    size_in_words: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> BFieldElement {
    let allocator_addr = BFieldElement::new(dyn_malloc::DYN_MALLOC_ADDRESS as u64);
    let size = BFieldElement::new(size_in_words as u64);
    let used_memory = memory
        .entry(allocator_addr)
        .and_modify(|e| {
            *e = if e.is_zero() {
                BFieldElement::one()
            } else {
                *e
            }
        })
        .or_insert_with(BFieldElement::one);

    assert!(size.value() < (1u64 << 32));

    let next_addr = *used_memory;

    *used_memory += size;

    next_addr
}
