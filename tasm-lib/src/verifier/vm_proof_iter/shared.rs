use crate::data_type::DataType;
use crate::data_type::StructType;

pub fn vm_proof_iter_type() -> StructType {
    let name = "VmProofIter".to_string();
    let fields = vec![("current_item_pointer".to_string(), DataType::Bfe)];

    StructType { name, fields }
}

/// Assumes the proof starts at address 0 (and that there is only one).
#[cfg(test)]
pub(crate) fn insert_default_proof_iter_into_memory(
    memory: &mut std::collections::HashMap<
        triton_vm::prelude::BFieldElement,
        triton_vm::prelude::BFieldElement,
    >,
    address: triton_vm::prelude::BFieldElement,
) {
    let default_first_proof_item_pointer = triton_vm::prelude::BFieldElement::new(2u64);

    memory.insert(address, default_first_proof_item_pointer);
}
