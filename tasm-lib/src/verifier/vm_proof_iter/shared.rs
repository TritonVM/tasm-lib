use crate::data_type::DataType;
use crate::data_type::StructType;

pub fn vm_proof_iter_type() -> StructType {
    let name = "VmProofIter".to_string();
    let current_item_pointer = ("current_item_pointer".to_string(), DataType::Bfe);
    let current_item_count = ("current_item_count".to_string(), DataType::U32);
    let total_item_count = ("total_item_count".to_string(), DataType::U32);
    let proof_start_pointer = ("proof_start_pointer".to_string(), DataType::Bfe);
    let proof_length = ("proof_length".to_string(), DataType::U32);

    let fields = vec![
        current_item_count,
        total_item_count,
        proof_start_pointer,
        proof_length,
        current_item_pointer,
    ];

    StructType { name, fields }
}

#[cfg(test)]
pub(super) mod vm_proof_iter_struct {
    use triton_vm::prelude::*;
    use triton_vm::proof::Proof;
    use triton_vm::proof_stream::ProofStream;

    use crate::prelude::TasmObject;

    /// Represent Triton VM's knowledge about a proof.
    #[derive(Debug, Clone, Copy, Eq, PartialEq, BFieldCodec, TasmObject)]
    pub(crate) struct VmProofIter {
        pub(crate) current_item_count: u32,
        pub(crate) total_item_count: u32,
        pub(crate) proof_start_pointer: BFieldElement,
        pub(crate) proof_length: u32,
        pub(crate) current_item_pointer: BFieldElement,
    }

    impl VmProofIter {
        pub(crate) fn new(proof_pointer: BFieldElement, proof: &Proof) -> Self {
            let proof_length = proof.0.len() + 1;
            let proof_stream: ProofStream = proof.try_into().unwrap();
            VmProofIter {
                current_item_count: 0,
                total_item_count: proof_stream.items.len().try_into().unwrap(),
                proof_start_pointer: proof_pointer,
                proof_length: proof_length.try_into().unwrap(),

                // uses highly specific knowledge of `BFieldCodec`
                current_item_pointer: proof_pointer + bfe!(4),
            }
        }
    }
}
