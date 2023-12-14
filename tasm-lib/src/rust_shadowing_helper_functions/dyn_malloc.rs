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
    initial_value: u32,
) {
    memory.insert(DYN_MALLOC_ADDRESS, BFieldElement::new(initial_value as u64));
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
