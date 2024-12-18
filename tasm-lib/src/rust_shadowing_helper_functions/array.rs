use std::collections::HashMap;

use num::Zero;
use twenty_first::prelude::*;

use crate::prelude::*;

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

/// Insert a list of elements into memory encoded as if the list was an array.
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

/// Read an element from an array
pub fn array_get(
    pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    let read_word = |i| {
        let word_offset = (element_length * index + i) as u64;
        let word_index = pointer + BFieldElement::new(word_offset);
        memory[&word_index]
    };

    (0..element_length).map(read_word).collect()
}

pub fn array_from_memory<T: BFieldCodec + Clone>(
    mut pointer: BFieldElement,
    array_length: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Vec<T> {
    let mut ret = vec![];
    let element_length = T::static_length().unwrap();
    for _ in 0..array_length {
        let mut element = vec![BFieldElement::zero(); element_length];
        for word in element.iter_mut() {
            *word = *memory.get(&pointer).unwrap_or(&BFieldElement::zero());
            pointer.increment();
        }

        ret.push(*T::decode(&element).unwrap());
    }

    ret
}
