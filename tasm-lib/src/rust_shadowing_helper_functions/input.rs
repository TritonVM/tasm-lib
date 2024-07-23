use num::Zero;
use triton_vm::prelude::*;
use twenty_first::math::other::random_elements;

use crate::Digest;

pub fn write_digest_to_std_in(std_in: &mut Vec<BFieldElement>, digest: Digest) {
    let digest_elements = digest.values();
    for i in 0..Digest::LEN {
        std_in.push(digest_elements[Digest::LEN - 1 - i]);
    }
}

pub fn write_value_to_secret_in(secret_in: &mut Vec<BFieldElement>, value: BFieldElement) {
    secret_in.push(value)
}

pub fn write_digest_to_secret_in(secret_in: &mut Vec<BFieldElement>, digest: Digest) {
    let digest_elements = digest.values();
    for i in 0..Digest::LEN {
        secret_in.push(digest_elements[Digest::LEN - 1 - i]);
    }
}

pub fn write_dummy_ap_path(input: &mut Vec<BFieldElement>, ap_length: usize) {
    input.push(BFieldElement::new(ap_length as u64));
    let ap_elements: Vec<Digest> = random_elements(ap_length);
    for ap_element in ap_elements.iter() {
        write_digest_to_secret_in(input, *ap_element);
    }
}

pub fn read_digest_from_std_in(std_in: &[BFieldElement], std_in_cursor: &mut usize) -> Digest {
    let mut values = [BFieldElement::zero(); Digest::LEN];
    let mut i = 0;
    while i < Digest::LEN {
        values[Digest::LEN - 1 - i] = std_in[*std_in_cursor];
        *std_in_cursor += 1;
        i += 1;
    }

    Digest::new(values)
}

pub fn read_digest_from_secret_in(
    secret_in: &[BFieldElement],
    secret_in_cursor: &mut usize,
) -> Digest {
    let mut values = [BFieldElement::zero(); Digest::LEN];
    let mut i = 0;
    while i < Digest::LEN {
        values[Digest::LEN - 1 - i] = secret_in[*secret_in_cursor];
        *secret_in_cursor += 1;
        i += 1;
    }

    Digest::new(values)
}
