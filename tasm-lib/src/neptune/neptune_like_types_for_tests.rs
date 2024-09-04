use arbitrary::Arbitrary;
use triton_vm::prelude::*;

use crate::prelude::TasmObject;

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ProofCollectionLookalike {
    pub removal_records_integrity: Proof,
    pub collect_lock_scripts: Proof,
    pub lock_scripts_halt: Vec<Proof>,
    pub kernel_to_outputs: Proof,
    pub collect_type_scripts: Proof,
    pub type_scripts_halt: Vec<Proof>,
    pub lock_script_hashes: Vec<Digest>,
    pub type_script_hashes: Vec<Digest>,
    pub kernel_mast_hash: Digest,
    pub salted_inputs_hash: Digest,
    pub salted_outputs_hash: Digest,
}
