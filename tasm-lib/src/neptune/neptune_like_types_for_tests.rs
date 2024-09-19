use arbitrary::Arbitrary;
use triton_vm::prelude::*;
use twenty_first::prelude::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;

use crate::prelude::TasmObject;
use crate::verifier::proof_for_nd_memory::ProofForNdMemory;

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ProofCollectionLookalike {
    pub removal_records_integrity: ProofForNdMemory,
    pub collect_lock_scripts: ProofForNdMemory,
    pub lock_scripts_halt: Vec<ProofForNdMemory>,
    pub kernel_to_outputs: ProofForNdMemory,
    pub collect_type_scripts: ProofForNdMemory,
    pub type_scripts_halt: Vec<ProofForNdMemory>,
    pub lock_script_hashes: Vec<Digest>,
    pub type_script_hashes: Vec<Digest>,
    pub kernel_mast_hash: Digest,
    pub salted_inputs_hash: Digest,
    pub salted_outputs_hash: Digest,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct CoinLookalike {
    pub type_script_hash: Digest,
    pub state: Vec<BFieldElement>,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct UtxoLookalike {
    pub lock_script_hash: Digest,
    pub coins: Vec<CoinLookalike>,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct SaltedUtxosLookalike {
    pub utxos: Vec<UtxoLookalike>,
    pub salt: [BFieldElement; 3],
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct CollectLockScriptsWitnessLookalike {
    salted_input_utxos: SaltedUtxosLookalike,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct CollectTypeScriptsWitnessLookalike {
    salted_input_utxos: SaltedUtxosLookalike,
    salted_output_utxos: SaltedUtxosLookalike,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct NeptuneCoinsLookalike(u128);

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ChunkLookalike {
    pub relative_indices: Vec<u32>,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ChunkDictionaryLookalike {
    dictionary: Vec<(u64, (MmrMembershipProof, ChunkLookalike))>,
}

const NUM_TRIALS_LOOKALIKE: usize = 45;
#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct AbsoluteIndexSetLookalike([u128; NUM_TRIALS_LOOKALIKE]);

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct RemovalRecordLookalike {
    pub absolute_indices: AbsoluteIndexSetLookalike,
    pub target_chunks: ChunkDictionaryLookalike,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct AdditionRecordLookalike {
    pub canonical_commitment: Digest,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct PublicAnnouncementLookalike {
    pub message: Vec<BFieldElement>,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct TimestampLookalike(pub(crate) BFieldElement);

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct TransactionKernelLookalike {
    pub inputs: Vec<RemovalRecordLookalike>,

    // `outputs` contains the commitments (addition records) that go into the AOCL
    pub outputs: Vec<AdditionRecordLookalike>,

    pub public_announcements: Vec<PublicAnnouncementLookalike>,
    pub fee: NeptuneCoinsLookalike,
    pub coinbase: Option<NeptuneCoinsLookalike>,

    // number of milliseconds since unix epoch
    pub timestamp: TimestampLookalike,

    pub mutator_set_hash: Digest,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct KernelToOutputsWitnessLookalike {
    pub output_utxos: SaltedUtxosLookalike,
    pub sender_randomnesses: Vec<Digest>,
    pub receiver_digests: Vec<Digest>,
    pub kernel: TransactionKernelLookalike,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct MergeWitnessLookalike {
    left_kernel: TransactionKernelLookalike,
    right_kernel: TransactionKernelLookalike,
    new_kernel: TransactionKernelLookalike,
    left_proof: ProofForNdMemory,
    right_proof: ProofForNdMemory,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct MsMembershipProofLookalike {
    pub sender_randomness: Digest,
    pub receiver_preimage: Digest,
    pub auth_path_aocl: MmrMembershipProof,
    pub aocl_leaf_index: u64,
    pub target_chunks: ChunkDictionaryLookalike,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct RemovalRecordsIntegrityWitnessLookalike {
    input_utxos: SaltedUtxosLookalike,
    membership_proofs: Vec<MsMembershipProofLookalike>,
    aocl_auth_paths: Vec<MmrMembershipProof>,
    removal_records: Vec<RemovalRecordLookalike>,
    aocl: MmrAccumulator,
    swbfi: MmrAccumulator,
    swbfa_hash: Digest,
    mast_path_mutator_set: Vec<Digest>,
    mast_path_inputs: Vec<Digest>,
    mast_root: Digest,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
pub(crate) struct MmrSuccessorProofLookalike {
    pub paths: Vec<Digest>,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
pub(crate) struct UpdateWitnessLookalike {
    old_kernel: TransactionKernelLookalike,
    new_kernel: TransactionKernelLookalike,
    old_kernel_mast_hash: Digest,
    new_kernel_mast_hash: Digest,
    old_proof: ProofForNdMemory,
    new_swbfi_bagged: Digest,
    new_aocl: MmrAccumulator,
    new_swbfa_hash: Digest,
    old_swbfi_bagged: Digest,
    old_aocl: MmrAccumulator,
    old_swbfa_hash: Digest,
    aocl_successor_proof: MmrSuccessorProofLookalike,
    outputs_hash: Digest,
    public_announcements_hash: Digest,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
pub(crate) struct LockScriptAndWitnessLookalike {
    nd_memory: Vec<(BFieldElement, BFieldElement)>,
    nd_tokens: Vec<BFieldElement>,
    nd_digests: Vec<Digest>,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
struct TypeScriptAndWitnessLookalike {
    nd_tokens: Vec<BFieldElement>,
    nd_memory: Vec<(BFieldElement, BFieldElement)>,
    nd_digests: Vec<Digest>,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
struct ActiveWindowLookalike {
    sbf: Vec<u32>,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
pub(crate) struct MutatorSetAccumulatorLookalike {
    aocl: MmrAccumulator,
    swbf_inactive: MmrAccumulator,
    swbf_active: ActiveWindowLookalike,
}

#[derive(Debug, Clone, BFieldCodec, TasmObject, Arbitrary)]
pub(crate) struct PrimitiveWitnessLookalike {
    input_utxos: SaltedUtxosLookalike,
    input_membership_proofs: Vec<MsMembershipProofLookalike>,
    lock_scripts_and_witnesses: Vec<LockScriptAndWitnessLookalike>,
    type_scripts_and_witnesses: Vec<TypeScriptAndWitnessLookalike>,
    output_utxos: SaltedUtxosLookalike,
    output_sender_randomnesses: Vec<Digest>,
    output_receiver_digests: Vec<Digest>,
    mutator_set_accumulator: MutatorSetAccumulatorLookalike,
    kernel: TransactionKernelLookalike,
}
