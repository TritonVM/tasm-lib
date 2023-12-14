use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    empty_stack,
    memory::dyn_malloc::{DynMalloc, DYN_MALLOC_ADDRESS},
    snippet::DeprecatedSnippet,
};

// TODO: DELETE ME, OR FIX ME
pub fn rust_dyn_malloc_initialize(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initial_value: BFieldElement,
) {
    assert!(
        initial_value.value() < (1 << 33) && initial_value.value() > (1 << 32),
        "Dynamic allocator must be initialized to a valid value"
    );
    memory.insert(DYN_MALLOC_ADDRESS, initial_value);
}

pub fn dynamic_allocator(
    size_in_words: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> BFieldElement {
    let mut init_stack = [
        empty_stack(),
        vec![BFieldElement::new(size_in_words as u64)],
    ]
    .concat();
    DynMalloc.rust_shadowing(&mut init_stack, vec![], vec![], memory);
    init_stack.pop().unwrap()
}
