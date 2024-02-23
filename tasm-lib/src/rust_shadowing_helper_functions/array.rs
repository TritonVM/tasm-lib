use std::collections::HashMap;

use triton_vm::twenty_first::shared_math::b_field_element::BFieldElement;

use crate::data_type::DataType;

pub fn insert_random_array(
    element_type: &DataType,
    mut array_pointer: BFieldElement,
    array_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let random_values = element_type.random_elements(array_length);

    for element in random_values {
        for word in element {
            memory.insert(array_pointer, word);
            array_pointer.increment();
        }
    }
}
