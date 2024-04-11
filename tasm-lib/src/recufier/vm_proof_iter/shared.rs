use crate::data_type::DataType;
use crate::data_type::StructType;

pub fn vm_proof_iter_type() -> StructType {
    let name = "VmProofIter".to_string();
    let fields = vec![("current_item_pointer".to_string(), DataType::Bfe)];

    StructType { name, fields }
}

#[cfg(test)]
pub(crate) fn insert_default_proof_iter_into_memory(
    memory: &mut std::collections::HashMap<
        triton_vm::twenty_first::shared_math::b_field_element::BFieldElement,
        triton_vm::twenty_first::shared_math::b_field_element::BFieldElement,
    >,
    address: triton_vm::twenty_first::shared_math::b_field_element::BFieldElement,
) {
    use super::new::FIRST_PROOF_ITEM_ELEMENT_POINTER;

    memory.insert(address, FIRST_PROOF_ITEM_ELEMENT_POINTER);
}
