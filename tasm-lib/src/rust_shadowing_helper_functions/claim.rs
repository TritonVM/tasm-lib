use std::collections::HashMap;

use num::Zero;
use triton_vm::proof::Claim;
use triton_vm::twenty_first::shared_math::b_field_element::BFieldElement;

use crate::structure::tasm_object::decode_from_memory_with_size;
use crate::DIGEST_LENGTH;

pub fn load_claim_from_memory(
    claim_pointer: BFieldElement,
    memory: &HashMap<BFieldElement, BFieldElement>,
) -> Claim {
    let output_field_size_pointer = claim_pointer;
    let output_field_size = memory
        .get(&output_field_size_pointer)
        .unwrap_or(&BFieldElement::zero())
        .value();
    let input_field_size_pointer = claim_pointer + BFieldElement::new(output_field_size + 1);
    let input_field_size = memory
        .get(&input_field_size_pointer)
        .unwrap_or(&BFieldElement::zero())
        .value();
    let size = input_field_size + output_field_size + DIGEST_LENGTH as u64 + 2;

    // Load `Claim` struct into memory
    // Notice that it's important to use the `Claim` type from Triton VM here, as it
    // gives a reasonable assurance that the `Claim` type specified in snippets here
    // matches that used in Triton VM.
    *decode_from_memory_with_size(memory, claim_pointer, size as usize).unwrap()
}
