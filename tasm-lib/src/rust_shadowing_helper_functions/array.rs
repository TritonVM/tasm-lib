use std::collections::HashMap;

use triton_vm::twenty_first::shared_math::{
    b_field_element::BFieldElement, bfield_codec::BFieldCodec,
};

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

/// Insert a list of elements into memory encoded as if the list was an array
pub fn insert_as_array<T: BFieldCodec>(
    mut array_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    array: Vec<T>,
) {
    for element in array {
        for word in element.encode() {
            memory.insert(array_pointer, word);
            array_pointer.increment();
        }
    }
}