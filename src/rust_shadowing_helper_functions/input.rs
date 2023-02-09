use num::Zero;
use twenty_first::shared_math::{
    b_field_element::BFieldElement,
    rescue_prime_digest::{Digest, DIGEST_LENGTH},
};

pub fn write_digest_to_std_in(std_in: &mut Vec<BFieldElement>, digest: Digest) {
    let digest_elements = digest.values();
    for i in 0..DIGEST_LENGTH {
        std_in.push(digest_elements[DIGEST_LENGTH - 1 - i]);
    }
}

pub fn write_value_to_secret_in(secret_in: &mut Vec<BFieldElement>, value: BFieldElement) {
    secret_in.push(value)
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
