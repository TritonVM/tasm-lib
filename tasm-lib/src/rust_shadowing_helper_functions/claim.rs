use std::collections::HashMap;

use num::Zero;
use triton_vm::prelude::*;

use crate::prelude::*;
use crate::structure::tasm_object::decode_from_memory_with_size;

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
    let version_field_size = 1;
    let size = input_field_size + output_field_size + version_field_size + Digest::LEN as u64 + 2;

    // Load `Claim` struct into memory
    // Notice that it's important to use the `Claim` type from Triton VM here, as it
    // gives a reasonable assurance that the `Claim` type specified in snippets here
    // matches that used in Triton VM.
    *decode_from_memory_with_size(memory, claim_pointer, size as usize).unwrap()
}
