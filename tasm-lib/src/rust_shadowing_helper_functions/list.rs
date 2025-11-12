use std::collections::HashMap;

use num::Zero;
use num_traits::ConstOne;
use triton_vm::prelude::*;
use twenty_first::math::other::random_elements;

use crate::U32_TO_USIZE_ERR;
use crate::USIZE_TO_U64_ERR;
use crate::list::LIST_METADATA_SIZE;
use crate::memory::dyn_malloc::DYN_MALLOC_PAGE_SIZE;
use crate::prelude::*;

/// Load a list from memory returning each element as a list of `BFieldElement`s.
pub fn load_list_unstructured(
    element_size: usize,
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Vec<Vec<BFieldElement>> {
    let list_length: usize = memory[&list_pointer].value().try_into().unwrap();

    let mut element_pointer = list_pointer + BFieldElement::new(LIST_METADATA_SIZE as u64);

    let mut ret = Vec::with_capacity(list_length);
    for i in 0..list_length {
        ret.push(vec![]);
        for _ in 0..element_size {
            ret[i].push(memory[&element_pointer]);
            element_pointer.increment();
        }
    }

    ret
}

/// Load a list from memory. Elements must be of `Copy` type.
pub fn load_list_with_copy_elements<const ELEMENT_SIZE: usize>(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Vec<[BFieldElement; ELEMENT_SIZE]> {
    let list_length: usize = memory[&list_pointer].value().try_into().unwrap();

    let mut element_pointer = list_pointer + BFieldElement::new(LIST_METADATA_SIZE as u64);

    let mut ret = Vec::with_capacity(list_length);
    for i in 0..list_length {
        ret.push([BFieldElement::zero(); ELEMENT_SIZE]);
        for item in ret[i].iter_mut() {
            *item = memory[&element_pointer];
            element_pointer.increment();
        }
    }

    ret
}

pub fn list_insert<T: BFieldCodec>(
    list_pointer: BFieldElement,
    vector: Vec<T>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    list_new(list_pointer, memory);

    for element in vector {
        list_push(list_pointer, element.encode(), memory);
    }
}

pub fn insert_random_list(
    element_type: &DataType,
    list_pointer: BFieldElement,
    list_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let list = element_type.random_list(&mut rand::rng(), list_length);
    let indexed_list = list
        .into_iter()
        .enumerate()
        .map(|(i, v)| (list_pointer + bfe!(i), v));
    memory.extend(indexed_list);
}

// TODO: Get rid of this stupid "helper" function
pub fn untyped_insert_random_list(
    list_pointer: BFieldElement,
    list_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) {
    list_new(list_pointer, memory);
    for _ in 0..list_length {
        let random_element: Vec<BFieldElement> = random_elements(element_length);
        list_push(list_pointer, random_element, memory);
    }
}

pub fn list_new(list_pointer: BFieldElement, memory: &mut HashMap<BFieldElement, BFieldElement>) {
    memory.insert(list_pointer, BFieldElement::zero());
}

/// Push the given element to the pointed-to list.
///
/// Only supports lists with statically sized elements.
///
/// # Panics
///
/// Panics if the pointed-to list is incorrectly encoded.
pub fn list_push(
    list_pointer: BFieldElement,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let list_length = memory
        .get_mut(&list_pointer)
        .expect("list must be initialized");
    let len = list_length.value();
    list_length.increment();

    let element_size: u64 = value.len().try_into().expect(USIZE_TO_U64_ERR);
    let list_metadata_size: u64 = LIST_METADATA_SIZE.try_into().expect(USIZE_TO_U64_ERR);
    let highest_access_index = list_metadata_size + element_size * (len + 1);
    assert!(highest_access_index < DYN_MALLOC_PAGE_SIZE);

    for (i, word) in (0..).zip(value) {
        let word_offset = bfe!(list_metadata_size + element_size * len + i);
        memory.insert(list_pointer + word_offset, word);
    }
}

/// Pop an element from the pointed-to list.
///
/// Only supports lists with statically sized elements.
///
/// # Panics
///
/// Panics if the pointed-to list is empty, or if the list is incorrectly
/// encoded.
pub fn list_pop(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Vec<BFieldElement> {
    let list_length = memory
        .get_mut(&list_pointer)
        .expect("list must be initialized");
    assert_ne!(0, list_length.value(), "list must not be empty");
    list_length.decrement();
    let last_item_index = list_length.value();

    let element_length: u64 = element_length.try_into().expect(USIZE_TO_U64_ERR);
    let read_word = |i| {
        let word_offset = bfe!(LIST_METADATA_SIZE) + bfe!(element_length * last_item_index + i);
        let word_index = list_pointer + bfe!(word_offset);
        memory[&word_index]
    };

    (0..element_length).map(read_word).collect()
}

/// A pointer to the `i`th element in the list, as well as the size of that
/// element.
///
/// Supports both, lists with statically _and_ lists with dynamically sized
/// elements.
///
/// # Panics
///
/// Panics if the `index` is out of bounds, or if the pointed-to-list is
/// incorrectly encoded.
pub fn list_pointer_to_elem_pointer(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_type: &DataType,
) -> (usize, BFieldElement) {
    let list_len = list_get_length(list_pointer, memory);
    assert!(index < list_len, "out of bounds: {index} >= {list_len}");

    if let Some(element_size) = element_type.static_length() {
        let elem_ptr = list_pointer + bfe!(LIST_METADATA_SIZE + index * element_size);
        return (element_size, elem_ptr);
    }

    let mut elem_pointer = list_pointer + bfe!(LIST_METADATA_SIZE);
    for _ in 0..index {
        elem_pointer += memory[&elem_pointer] + BFieldElement::ONE;
    }
    let elem_size = usize::try_from(memory[&elem_pointer].value()).unwrap();
    (elem_size, elem_pointer + BFieldElement::ONE)
}

/// Read an element from a list.
///
/// Only supports lists with statically sized elements.
///
/// # Panics
///
/// Panics if
/// - the `index` is out of bounds, or
/// - the element that is to be read resides outside the list`s
///   [memory page][crate::memory], or
/// - the pointed-to-list is incorrectly encoded into `memory`.
pub fn list_get(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_size: usize,
) -> Vec<BFieldElement> {
    let list_len = list_get_length(list_pointer, memory);
    assert!(index < list_len, "out of bounds: {index} >= {list_len}");

    let highest_access_index = LIST_METADATA_SIZE + element_size * (index + 1);
    assert!(u64::try_from(highest_access_index).expect(USIZE_TO_U64_ERR) < DYN_MALLOC_PAGE_SIZE);

    let read_word = |i| {
        let word_offset = LIST_METADATA_SIZE + element_size * index + i;
        let word_index = list_pointer + bfe!(word_offset);
        memory[&word_index]
    };

    (0..element_size).map(read_word).collect()
}

/// Write an element to a list.
///
/// Only supports lists with statically sized elements.
///
/// # Panics
///
/// Panics if
/// - the `index` is out of bounds, or
/// - the element that is to be read resides outside the list`s
///   [memory page][crate::memory], or
/// - the pointed-to-list is incorrectly encoded into `memory`.
pub fn list_set(
    list_pointer: BFieldElement,
    index: usize,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let list_len = list_get_length(list_pointer, memory);
    assert!(index < list_len, "out of bounds: {index} >= {list_len}");

    let element_size = value.len();
    let highest_access_index = LIST_METADATA_SIZE + element_size * (index + 1);
    assert!(u64::try_from(highest_access_index).expect(USIZE_TO_U64_ERR) < DYN_MALLOC_PAGE_SIZE);

    for (i, word) in value.into_iter().enumerate() {
        let word_offset = LIST_METADATA_SIZE + element_size * index + i;
        let word_index = list_pointer + bfe!(word_offset);
        memory.insert(word_index, word);
    }
}

pub fn list_get_length(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> usize {
    let length: u32 = memory[&list_pointer].value().try_into().unwrap();

    length.try_into().expect(U32_TO_USIZE_ERR)
}

pub fn list_set_length(
    list_pointer: BFieldElement,
    new_length: usize,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    memory.insert(list_pointer, bfe!(new_length));
}

#[cfg(test)]
mod tests {
    use proptest::prop_assert_eq;
    use proptest_arbitrary_interop::arb;
    use test_strategy::proptest;

    use super::*;

    #[test]
    fn new_list_set_length() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        list_new(list_pointer, &mut memory);
        assert!(list_get_length(list_pointer, &memory).is_zero());
        let new_length = 51;
        list_set_length(list_pointer, new_length, &mut memory);
        assert_eq!(new_length, list_get_length(list_pointer, &memory));
    }

    #[proptest]
    fn element_pointer_from_list_pointer_on_static_list_with_static_length_items(
        #[strategy(arb())] list: Vec<Digest>,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        let indexed_list = list
            .encode()
            .into_iter()
            .enumerate()
            .map(|(i, v)| (list_pointer + bfe!(i), v));

        let mut memory = HashMap::default();
        memory.extend(indexed_list);

        let data_type = DataType::Digest;
        for (i, digest) in list.into_iter().enumerate() {
            dbg!(i);
            let (len, ptr) = list_pointer_to_elem_pointer(list_pointer, i, &memory, &data_type);
            prop_assert_eq!(Digest::LEN, len);
            prop_assert_eq!(digest.values()[0], memory[&ptr]);
        }
    }

    #[proptest]
    fn element_pointer_from_list_pointer_on_static_list_with_dyn_length_items(
        #[strategy(arb())] list: Vec<Vec<BFieldElement>>,
        #[strategy(arb())] list_pointer: BFieldElement,
    ) {
        let indexed_list = list
            .encode()
            .into_iter()
            .enumerate()
            .map(|(i, v)| (list_pointer + bfe!(i), v));

        let mut memory = HashMap::default();
        memory.extend(indexed_list);

        let data_type = DataType::List(Box::new(DataType::Bfe));
        for (i, inner_list) in list.into_iter().enumerate() {
            dbg!(i);
            let (len, ptr) = list_pointer_to_elem_pointer(list_pointer, i, &memory, &data_type);
            prop_assert_eq!(inner_list.encode().len(), len);
            prop_assert_eq!(bfe!(inner_list.len()), memory[&ptr]);
        }
    }
}
