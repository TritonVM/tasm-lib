use num::{One, Zero};
use std::collections::HashMap;
use twenty_first::shared_math::{
    b_field_element::BFieldElement, bfield_codec::BFieldCodec, other::random_elements,
};

pub fn unsafe_list_insert<T: BFieldCodec>(
    list_pointer: BFieldElement,
    vector: Vec<T>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    unsafe_list_new(list_pointer, memory);

    for element in vector {
        unsafe_list_push(
            list_pointer,
            element.encode(),
            memory,
            element.encode().len(),
        );
    }
}

pub fn unsafe_insert_random_list(
    list_pointer: BFieldElement,
    list_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) {
    unsafe_list_new(list_pointer, memory);
    for _ in 0..list_length {
        let random_element: Vec<BFieldElement> = random_elements(element_length);
        unsafe_list_push(list_pointer, random_element, memory, element_length);
    }
}

pub fn unsafe_list_new(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    memory.insert(list_pointer, BFieldElement::zero());
}

pub fn unsafe_list_push(
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
    let init_list_length = memory[&list_pointer];
    memory.get_mut(&list_pointer).unwrap().increment();

    for (i, elem) in value.iter().enumerate() {
        memory.insert(
            list_pointer
                + BFieldElement::one()
                + BFieldElement::new(element_length as u64) * init_list_length
                + BFieldElement::new(i as u64),
            *elem,
        );
    }
}

pub fn unsafe_list_pop(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    let init_list_length = memory[&list_pointer];
    assert!(!init_list_length.is_zero(), "Stack underflow");
    memory.get_mut(&list_pointer).unwrap().decrement();

    // let mut ret = [BFieldElement::zero(); N];
    let mut ret = vec![BFieldElement::zero(); element_length];
    for (i, elem) in ret.iter_mut().enumerate() {
        let key = list_pointer
            + BFieldElement::one()
            + BFieldElement::new(element_length as u64) * (init_list_length - BFieldElement::one())
            + BFieldElement::new(i as u64);
        *elem = memory[&key];
    }

    ret
}

/// Read an element from a list.
pub fn unsafe_list_get(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    let mut ret: Vec<BFieldElement> = vec![BFieldElement::zero(); element_length];

    for i in 0..element_length {
        ret[i] =
            memory[&(list_pointer + BFieldElement::new((element_length * index + 1 + i) as u64))];
    }

    ret
}

/// Write an element to a list.
pub fn unsafe_list_set(
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
            list_pointer + BFieldElement::new((element_length * index + 1 + i) as u64),
            v,
        );
    }
}

pub fn unsafe_list_get_length(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> usize {
    let length: u32 = memory[&list_pointer].value().try_into().unwrap();

    length as usize
}

pub fn unsafe_list_set_length(
    list_pointer: BFieldElement,
    new_length: u32,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    memory.insert(list_pointer, BFieldElement::new(new_length as u64));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_list_set_length() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        unsafe_list_new(list_pointer, &mut memory);
        assert!(unsafe_list_get_length(list_pointer, &memory).is_zero());
        let new_length = 51;
        unsafe_list_set_length(list_pointer, new_length, &mut memory);
        assert_eq!(
            new_length as usize,
            unsafe_list_get_length(list_pointer, &memory)
        );
    }
}
