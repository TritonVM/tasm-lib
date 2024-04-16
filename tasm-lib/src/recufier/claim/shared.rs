use crate::data_type::DataType;
use crate::data_type::StructType;

// TODO: This *must* match the type of `Claim` used in Triton VM. It would be preferable if
// it could be derived/generated from that type at the source.
// The tests below *should* verify that this type is consistent with the one used in TVM.
pub(crate) fn claim_type() -> StructType {
    StructType {
        name: "Claim".to_owned(),
        fields: vec![
            ("program_digest".to_owned(), DataType::Digest),
            ("input".to_owned(), DataType::List(Box::new(DataType::Bfe))),
            ("output".to_owned(), DataType::List(Box::new(DataType::Bfe))),
        ],
    }
}

/// Insert a claim into static memory, assuming static memory is empty prior to this insertion.
/// Returns the pointer to the inserted claim, and the size of the encoded claim.
#[cfg(test)]
pub(crate) fn insert_claim_into_static_memory(
    memory: &mut std::collections::HashMap<
        triton_vm::twenty_first::math::b_field_element::BFieldElement,
        triton_vm::twenty_first::math::b_field_element::BFieldElement,
    >,
    claim: triton_vm::proof::Claim,
) -> (
    triton_vm::twenty_first::math::b_field_element::BFieldElement,
    u32,
) {
    // Statically allocated memory starts at -2 and grows downward. So a value of size 1
    // will be assigned to address `-2` if no other static memory allocations have occurred
    // before it.

    use crate::memory::encode_to_memory;
    use triton_vm::prelude::*;
    let size_of_encoded_claim: u32 = claim.encode().len().try_into().unwrap();
    let size_as_i32: i32 = size_of_encoded_claim.try_into().unwrap();
    let claim_pointer = bfe!(-size_as_i32 - 1);

    encode_to_memory(memory, claim_pointer, claim);

    (claim_pointer, size_of_encoded_claim)
}
