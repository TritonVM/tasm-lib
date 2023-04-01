use std::collections::HashMap;

use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::dyn_malloc;

pub fn rust_dyn_malloc_initialize(
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    initial_value: usize,
) {
    memory.insert(
        BFieldElement::new(dyn_malloc::DYN_MALLOC_ADDRESS as u64),
        BFieldElement::new(initial_value as u64),
    );
}
