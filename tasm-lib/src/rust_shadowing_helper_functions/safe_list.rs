use num::{One, Zero};
use std::collections::HashMap;
use twenty_first::shared_math::{b_field_element::BFieldElement, bfield_codec::BFieldCodec};

use crate::data_type::DataType;

pub fn safe_list_insert<T: BFieldCodec>(
    list_pointer: BFieldElement,
    capacity: u32,
    vector: Vec<T>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    safe_list_new(list_pointer, capacity, memory);

    for element in vector {
        safe_list_push(
            list_pointer,
            element.encode(),
            memory,
            element.encode().len(),
        );
    }
}

pub fn safe_insert_random_list(
    data_type: &DataType,
    list_pointer: BFieldElement,
    capacity: u32,
    list_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    safe_list_new(list_pointer, capacity, memory);

    let random_values = data_type.random_elements(list_length);

    for element in random_values {
        safe_list_push(list_pointer, element, memory, data_type.stack_size());
    }
}

pub fn safe_list_new(
    list_pointer: BFieldElement,
    capacity: u32,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    // Insert length
    memory.insert(list_pointer, BFieldElement::zero());

    // Insert capacity
    memory.insert(
        list_pointer + BFieldElement::one(),
        BFieldElement::new(capacity as u64),
    );
}

pub fn safe_list_push(
    list_pointer: BFieldElement,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) {
    assert_eq!(
        element_length,
        value.len(),
        "Length must match indicated length"
    );
    let init_list_length_bfe = memory[&list_pointer];
    let init_list_length_u32: u32 = init_list_length_bfe.try_into().unwrap();
    let capacity: u32 = memory[&(list_pointer + BFieldElement::one())]
        .value()
        .try_into()
        .unwrap();
    assert!(
        capacity > init_list_length_u32,
        "Cannot push beyond capacity of list"
    );
    memory.get_mut(&list_pointer).unwrap().increment();

    for (i, elem) in value.iter().enumerate() {
        memory.insert(
            list_pointer
                + BFieldElement::new(2)
                + BFieldElement::new(element_length as u64) * init_list_length_bfe
                + BFieldElement::new(i as u64),
            *elem,
        );
    }
}

pub fn safe_list_pop(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    let init_list_length = memory[&list_pointer];
    assert!(!init_list_length.is_zero(), "Stack underflow");
    memory.get_mut(&list_pointer).unwrap().decrement();

    let mut ret = vec![BFieldElement::zero(); element_length];
    for (i, elem) in ret.iter_mut().enumerate() {
        let key = list_pointer
            + BFieldElement::new(2)
            + BFieldElement::new(element_length as u64) * (init_list_length - BFieldElement::one())
            + BFieldElement::new(i as u64);
        *elem = memory[&key];
    }

    ret
}

/// Read an element from a list.
pub fn safe_list_get(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    // Bounds check
    let length: u32 = memory[&list_pointer].value().try_into().unwrap();
    assert!(
        length > index as u32,
        "Read index must be inside list bounds"
    );

    let mut ret: Vec<BFieldElement> = vec![BFieldElement::zero(); element_length];

    for i in 0..element_length {
        ret[i] =
            memory[&(list_pointer + BFieldElement::new((element_length * index + 2 + i) as u64))];
    }

    ret
}

/// Write an element to a list.
pub fn safe_list_set(
    list_pointer: BFieldElement,
    index: usize,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) {
    assert_eq!(
        element_length,
        value.len(),
        "Element length must match indicated length"
    );
    for (i, v) in value.into_iter().enumerate() {
        memory.insert(
            list_pointer + BFieldElement::new((element_length * index + 2 + i) as u64),
            v,
        );
    }
}

pub fn safe_list_get_length(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> usize {
    let length: u32 = memory[&list_pointer].value().try_into().unwrap();

    length as usize
}

pub fn safe_list_set_length(
    list_pointer: BFieldElement,
    new_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    memory.insert(list_pointer, BFieldElement::new(new_length as u64));
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::{other::random_elements, tip5::Digest};

    use super::*;

    #[test]
    fn new_list_digest() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        let list_length = 99;
        let capacity = 200_000_000;
        let digests: Vec<Digest> = random_elements(list_length);
        safe_list_insert(list_pointer, capacity, digests.clone(), &mut memory);
        assert_eq!(list_length, safe_list_get_length(list_pointer, &memory));
        for (i, elem) in digests.into_iter().enumerate() {
            assert_eq!(elem.encode(), safe_list_get(list_pointer, i, &memory, 5));
        }
    }

    #[test]
    fn new_list_set_length() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        let list_capacity = 99;
        safe_list_new(list_pointer, list_capacity, &mut memory);
        assert!(safe_list_get_length(list_pointer, &memory).is_zero());
        let new_length = 51;
        safe_list_set_length(list_pointer, new_length, &mut memory);
        assert_eq!(new_length, safe_list_get_length(list_pointer, &memory));
    }

    #[test]
    fn new_list_read_length_test_digest() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        let list_length = 99;
        safe_insert_random_list(
            &DataType::Digest,
            list_pointer,
            105,
            list_length,
            &mut memory,
        );
        assert_eq!(list_length, safe_list_get_length(list_pointer, &memory));
    }

    #[test]
    fn new_list_read_length_test_u64() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        let list_length = 99;
        safe_insert_random_list(&DataType::U64, list_pointer, 105, list_length, &mut memory);
        assert_eq!(list_length, safe_list_get_length(list_pointer, &memory));
    }

    #[test]
    fn new_list_read_length_test_bfe() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        let list_length = 99;
        safe_insert_random_list(&DataType::Bfe, list_pointer, 105, list_length, &mut memory);
        assert_eq!(list_length, safe_list_get_length(list_pointer, &memory));
    }
}
