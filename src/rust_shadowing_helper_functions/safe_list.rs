use num::{One, Zero};
use std::collections::HashMap;
use twenty_first::shared_math::{b_field_element::BFieldElement, other::random_elements};

pub fn safe_insert_random_list(
    list_pointer: BFieldElement,
    capacity: u32,
    list_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) {
    safe_list_new(list_pointer, capacity, memory);
    for _ in 0..list_length {
        let random_element: Vec<BFieldElement> = random_elements(element_length);
        safe_list_push(list_pointer, random_element, memory, element_length);
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
