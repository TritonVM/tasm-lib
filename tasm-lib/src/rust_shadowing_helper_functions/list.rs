use std::collections::HashMap;

use num::Zero;
use num_traits::ConstOne;
use num_traits::ConstZero;
use triton_vm::prelude::*;
use twenty_first::math::other::random_elements;

use crate::U32_TO_USIZE_ERR;
use crate::USIZE_TO_U64_ERR;
use crate::list::LIST_METADATA_SIZE;
use crate::memory::dyn_malloc::DYN_MALLOC_PAGE_SIZE;
use crate::prelude::*;
use crate::traits::rust_shadow::RustShadowError;

/// Load a list from memory returning each element as a list of `BFieldElement`s.
pub fn load_list_unstructured(
    element_size: usize,
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Result<Vec<Vec<BFieldElement>>, RustShadowError> {
    let list_length = list_get_length(list_pointer, memory)?;
    let mut element_pointer = list_pointer + bfe!(LIST_METADATA_SIZE);

    let mut list = Vec::with_capacity(list_length);
    for _ in 0..list_length {
        let mut element = Vec::with_capacity(element_size);
        for _ in 0..element_size {
            let Some(element_part) = memory.get(&element_pointer).copied() else {
                return Err(RustShadowError::DecodingError);
            };
            element.push(element_part);
            element_pointer.increment();
        }
        list.push(element);
    }

    Ok(list)
}

/// Load a list from memory. Elements must be of `Copy` type.
pub fn load_list_with_copy_elements<const ELEMENT_SIZE: usize>(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Result<Vec<[BFieldElement; ELEMENT_SIZE]>, RustShadowError> {
    let list_length = list_get_length(list_pointer, memory)?;
    let mut element_pointer = list_pointer + bfe!(LIST_METADATA_SIZE);

    let mut list = Vec::with_capacity(list_length);
    for _ in 0..list_length {
        let mut element = [BFieldElement::ZERO; ELEMENT_SIZE];
        for item in &mut element {
            let Some(element_part) = memory.get(&element_pointer).copied() else {
                return Err(RustShadowError::DecodingError);
            };
            *item = element_part;
            element_pointer.increment();
        }
        list.push(element);
    }

    Ok(list)
}

pub fn list_insert<T: BFieldCodec>(
    list_pointer: BFieldElement,
    vector: Vec<T>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    list_new(list_pointer, memory);

    for element in vector {
        list_push(list_pointer, element.encode(), memory).unwrap();
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
        list_push(list_pointer, random_element, memory).unwrap();
    }
}

pub fn list_new(list_pointer: BFieldElement, memory: &mut HashMap<BFieldElement, BFieldElement>) {
    memory.insert(list_pointer, BFieldElement::ZERO);
}

/// Push the given element to the pointed-to list.
///
/// Only supports lists with statically sized elements.
///
/// # Errors
///
/// Errors if the pointed-to list is incorrectly encoded.
pub fn list_push(
    list_pointer: BFieldElement,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> Result<(), RustShadowError> {
    let Some(list_length) = memory.get_mut(&list_pointer) else {
        return Err(RustShadowError::DecodingError);
    };
    let len = list_length.value();
    list_length.increment();

    let element_size: u64 = value.len().try_into().expect(USIZE_TO_U64_ERR);
    let list_metadata_size: u64 = LIST_METADATA_SIZE.try_into().expect(USIZE_TO_U64_ERR);
    let highest_access_index = list_metadata_size + element_size * (len + 1);
    if highest_access_index >= DYN_MALLOC_PAGE_SIZE {
        return Err(RustShadowError::Other);
    }

    for (i, word) in (0..).zip(value) {
        let word_offset = bfe!(list_metadata_size + element_size * len + i);
        memory.insert(list_pointer + word_offset, word);
    }

    Ok(())
}

/// Pop an element from the pointed-to list.
///
/// Only supports lists with statically sized elements.
///
/// # Errors
///
/// Errors if the pointed-to list is empty, or if the list is incorrectly
/// encoded.
pub fn list_pop(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
    element_length: usize,
) -> Result<Vec<BFieldElement>, RustShadowError> {
    let Some(list_length) = memory.get_mut(&list_pointer) else {
        return Err(RustShadowError::DecodingError);
    };
    if list_length.is_zero() {
        return Err(RustShadowError::Other); // list must not be empty
    }
    list_length.decrement();
    let last_item_index = list_length.value();

    let element_length: u64 = element_length.try_into().expect(USIZE_TO_U64_ERR);
    let read_word = |i| {
        let word_offset = bfe!(LIST_METADATA_SIZE) + bfe!(element_length * last_item_index + i);
        let word_index = list_pointer + bfe!(word_offset);
        memory
            .get(&word_index)
            .copied()
            .ok_or(RustShadowError::DecodingError)
    };

    (0..element_length).map(read_word).collect()
}

/// A pointer to the `i`th element in the list, as well as the size of that
/// element.
///
/// Supports both, lists with statically _and_ lists with dynamically sized
/// elements.
///
/// # Errors
///
/// Errors if the `index` is out of bounds, or if the pointed-to-list is
/// incorrectly encoded.
pub fn list_pointer_to_elem_pointer(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_type: &DataType,
) -> Result<(usize, BFieldElement), RustShadowError> {
    let list_len = list_get_length(list_pointer, memory)?;
    if index >= list_len {
        return Err(RustShadowError::Other); // out of bounds
    }

    if let Some(element_size) = element_type.static_length() {
        let elem_ptr = list_pointer + bfe!(LIST_METADATA_SIZE + index * element_size);
        return Ok((element_size, elem_ptr));
    }

    let mut elem_pointer = list_pointer + bfe!(LIST_METADATA_SIZE);
    for _ in 0..index {
        elem_pointer += memory
            .get(&elem_pointer)
            .copied()
            .ok_or(RustShadowError::DecodingError)?
            + BFieldElement::ONE;
    }
    let elem_size = memory
        .get(&elem_pointer)
        .copied()
        .ok_or(RustShadowError::DecodingError)?;
    let Ok(elem_size) = usize::try_from(elem_size.value()) else {
        return Err(RustShadowError::U64ToUsizeError);
    };

    Ok((elem_size, elem_pointer + BFieldElement::ONE))
}

/// Read an element from a list.
///
/// Only supports lists with statically sized elements.
///
/// # Errors
///
/// Errors if
/// - the `index` is out of bounds, or
/// - the element that is to be read resides outside the list`s
///   [memory page][crate::memory], or
/// - the pointed-to-list is incorrectly encoded into `memory`.
pub fn list_get(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
    element_size: usize,
) -> Result<Vec<BFieldElement>, RustShadowError> {
    let list_len = list_get_length(list_pointer, memory)?;
    if index >= list_len {
        return Err(RustShadowError::Other); // out of bounds
    }

    // for the benefit of 32-bit architectures, convert before doing arithmetic
    let to_u64 = |x| u64::try_from(x).expect(USIZE_TO_U64_ERR);
    let metadata_size = to_u64(LIST_METADATA_SIZE);
    let element_size = to_u64(element_size);
    let index = to_u64(index);

    let highest_access_index = metadata_size + element_size * (index + 1);
    if highest_access_index >= DYN_MALLOC_PAGE_SIZE {
        return Err(RustShadowError::Other);
    }

    let read_word = |i| {
        let word_offset = metadata_size + element_size * index + i;
        let word_index = list_pointer + bfe!(word_offset);
        memory
            .get(&word_index)
            .copied()
            .ok_or(RustShadowError::DecodingError)
    };

    (0..element_size).map(read_word).collect()
}

/// Write an element to a list.
///
/// Only supports lists with statically sized elements.
///
/// # Errors
///
/// Errors if
/// - the `index` is out of bounds, or
/// - the element that is to be read resides outside the list’s
///   [memory page][crate::memory], or
/// - the pointed-to-list is incorrectly encoded into `memory`.
pub fn list_set(
    list_pointer: BFieldElement,
    index: usize,
    value: Vec<BFieldElement>,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> Result<(), RustShadowError> {
    let list_len = list_get_length(list_pointer, memory)?;
    if index >= list_len {
        return Err(RustShadowError::Other); // out of bounds
    }

    let element_size = value.len();
    let highest_access_index = LIST_METADATA_SIZE + element_size * (index + 1);
    if u64::try_from(highest_access_index).expect(USIZE_TO_U64_ERR) >= DYN_MALLOC_PAGE_SIZE {
        return Err(RustShadowError::Other);
    }

    for (i, word) in value.into_iter().enumerate() {
        let word_offset = LIST_METADATA_SIZE + element_size * index + i;
        let word_index = list_pointer + bfe!(word_offset);
        memory.insert(word_index, word);
    }

    Ok(())
}

pub fn list_get_length(
    list_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Result<usize, RustShadowError> {
    let Some(length) = memory.get(&list_pointer).copied() else {
        return Err(RustShadowError::DecodingError);
    };
    let Ok(length) = u32::try_from(length.value()) else {
        return Err(RustShadowError::U64ToU32Error);
    };

    Ok(length.try_into().expect(U32_TO_USIZE_ERR))
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
    use super::*;
    use crate::test_prelude::*;

    #[macro_rules_attr::apply(test)]
    fn new_list_set_length() {
        let mut memory = HashMap::default();
        let list_pointer = BFieldElement::new(20);
        list_new(list_pointer, &mut memory);
        assert!(list_get_length(list_pointer, &memory).unwrap().is_zero());
        let new_length = 51;
        list_set_length(list_pointer, new_length, &mut memory);
        assert_eq!(new_length, list_get_length(list_pointer, &memory).unwrap());
    }

    #[macro_rules_attr::apply(proptest)]
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
            let (len, ptr) = list_pointer_to_elem_pointer(list_pointer, i, &memory, &data_type)?;
            prop_assert_eq!(Digest::LEN, len);
            prop_assert_eq!(digest.values()[0], memory[&ptr]);
        }
    }

    #[macro_rules_attr::apply(proptest)]
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
            let (len, ptr) = list_pointer_to_elem_pointer(list_pointer, i, &memory, &data_type)?;
            prop_assert_eq!(inner_list.encode().len(), len);
            prop_assert_eq!(bfe!(inner_list.len()), memory[&ptr]);
        }
    }
}
