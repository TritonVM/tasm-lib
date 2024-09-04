use arbitrary::Arbitrary;
use triton_vm::prelude::*;
use twenty_first::prelude::MmrMembershipProof;

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
pub(crate) struct NeptuneCoinsLookalike(u128);

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ChunkLookalike {
    pub relative_indices: Vec<u32>,
}

#[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
pub(crate) struct ChunkDictionaryLookalike {
    dictionary: Vec<(u64, (MmrMembershipProof, ChunkLookalike))>,
}
