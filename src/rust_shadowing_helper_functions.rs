use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::{Digest, DIGEST_LENGTH};

/// Read an element from a list.
pub fn list_set<const N: usize>(
    list_pointer: BFieldElement,
    index: usize,
    value: [BFieldElement; N],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    for (i, v) in value.into_iter().enumerate() {
        memory.insert(
            list_pointer + BFieldElement::new((N * index + 1 + i) as u64),
            v,
        );
    }
}

/// Read an element from a list.
pub fn list_read<const N: usize>(
    list_pointer: BFieldElement,
    index: usize,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> [BFieldElement; N] {
    let mut ret: [BFieldElement; N] = [BFieldElement::zero(); N];

    for i in 0..N {
        ret[i] = memory[&(list_pointer + BFieldElement::new((N * index + 1 + i) as u64))];
    }

    ret
}

pub fn list_pop<const N: usize>(
    list_pointer: BFieldElement,
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) -> [BFieldElement; N] {
    let init_list_length = memory[&list_pointer];
    assert!(!init_list_length.is_zero(), "Stack underflow");
    memory.get_mut(&list_pointer).unwrap().decrement();

    let mut ret = [BFieldElement::zero(); N];
    for (i, elem) in ret.iter_mut().enumerate() {
        let key = list_pointer
            + BFieldElement::one()
            + BFieldElement::new(N as u64) * (init_list_length - BFieldElement::one())
            + BFieldElement::new(i as u64);
        *elem = memory[&key];
    }

    ret
}

pub fn list_push<const N: usize>(
    list_pointer: BFieldElement,
    value: [BFieldElement; N],
    memory: &mut HashMap<BFieldElement, BFieldElement>,
) {
    let init_list_length = memory[&list_pointer];
    memory.get_mut(&list_pointer).unwrap().increment();

    for (i, elem) in value.iter().enumerate() {
        memory.insert(
            list_pointer
                + BFieldElement::one()
                + BFieldElement::new(N as u64) * init_list_length
                + BFieldElement::new(i as u64),
            *elem,
        );
    }
}

pub fn list_new(list_pointer: BFieldElement, memory: &mut HashMap<BFieldElement, BFieldElement>) {
    memory.insert(list_pointer, BFieldElement::zero());
}

pub fn write_digest_to_std_in(std_in: &mut Vec<BFieldElement>, digest: Digest) {
    let digest_elements = digest.values();
    for i in 0..DIGEST_LENGTH {
        std_in.push(digest_elements[DIGEST_LENGTH - 1 - i]);
    }
}

pub fn write_digest_to_secret_in(secret_in: &mut Vec<BFieldElement>, digest: Digest) {
    let digest_elements = digest.values();
    for i in 0..DIGEST_LENGTH {
        secret_in.push(digest_elements[DIGEST_LENGTH - 1 - i]);
    }
}

pub fn read_digest_from_std_in(std_in: &[BFieldElement], std_in_cursor: &mut usize) -> Digest {
    let mut values = [BFieldElement::zero(); DIGEST_LENGTH];
    let mut i = 0;
    while i < DIGEST_LENGTH {
        values[DIGEST_LENGTH - 1 - i] = std_in[*std_in_cursor];
        *std_in_cursor += 1;
        i += 1;
    }

    Digest::new(values)
}

pub fn read_digest_from_secret_in(
    secret_in: &[BFieldElement],
    secret_in_cursor: &mut usize,
) -> Digest {
    let mut values = [BFieldElement::zero(); DIGEST_LENGTH];
    let mut i = 0;
    while i < DIGEST_LENGTH {
        values[DIGEST_LENGTH - 1 - i] = secret_in[*secret_in_cursor];
        *secret_in_cursor += 1;
        i += 1;
    }

    Digest::new(values)
}

// TODO: Remove this when twenty-first gets a new version with this function in it
pub fn leaf_index_to_mt_index_and_peak_index(leaf_index: u128, leaf_count: u128) -> (u128, u32) {
    // This assert also guarantees that leaf_count is never zero
    assert!(
        leaf_index < leaf_count,
        "Leaf index must be stricly smaller than leaf count. Got leaf_index = {leaf_index}, leaf_count = {leaf_count}"
    );

    let max_tree_height = u128::BITS - leaf_count.leading_zeros() - 1;
    let mut h = max_tree_height;
    let mut ret = leaf_index;
    let mut pow;
    let mut peak_index: u32 = 0;
    loop {
        pow = 1 << h;
        let maybe_pow = pow & leaf_count;
        if h == 0 || (ret < maybe_pow) {
            break;
        }
        ret -= maybe_pow;
        peak_index += (maybe_pow != 0) as u32;
        h -= 1;
    }

    ret += pow;

    (ret, peak_index)
}
