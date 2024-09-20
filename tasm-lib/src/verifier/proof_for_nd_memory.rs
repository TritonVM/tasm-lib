use arbitrary::Arbitrary;
use triton_vm::error::ProofStreamError;
use triton_vm::prelude::BFieldCodec;
use triton_vm::prelude::BFieldElement;
use triton_vm::proof::Proof;
use triton_vm::proof_stream::ProofStream;

/// A proof stripped of proof items that are divined in.
///
/// Intended to be loaded into the ND-section of memory prior to Triton VM
/// executions involving verification.
#[derive(Debug, Clone, Eq, PartialEq, BFieldCodec, Arbitrary)]
pub struct ProofForNdMemory(pub Vec<BFieldElement>);

impl TryFrom<Proof> for ProofForNdMemory {
    type Error = ProofStreamError;

    fn try_from(proof: Proof) -> Result<Self, ProofStreamError> {
        (&proof).try_into()
    }
}

/// Strip those parts of the proof stream that are not read from memory by
/// the TASM verifier.
pub(crate) fn strip_non_memory_items(proof_stream: ProofStream) -> ProofStream {
    let mut stripped_proof = ProofStream::new();
    for mut proof_item in proof_stream.items.into_iter() {
        use triton_vm::proof_item::ProofItem::*;
        match &mut proof_item {
            AuthenticationStructure(_) => continue,
            FriResponse(fri_response) => {
                fri_response.auth_structure.clear();
            }
            _ => (),
        }

        stripped_proof.enqueue(proof_item);
    }

    stripped_proof
}

impl TryFrom<&Proof> for ProofForNdMemory {
    type Error = ProofStreamError;

    fn try_from(proof: &Proof) -> Result<Self, ProofStreamError> {
        let proof_stream = *ProofStream::decode(&proof.0)?;
        let stripped_proof = strip_non_memory_items(proof_stream);
        Ok(stripped_proof.into())
    }
}

impl From<ProofForNdMemory> for Proof {
    fn from(value: ProofForNdMemory) -> Self {
        Proof(value.0)
    }
}

impl From<ProofStream> for ProofForNdMemory {
    fn from(proof_stream: ProofStream) -> Self {
        ProofForNdMemory(proof_stream.encode())
    }
}

impl TryFrom<&ProofForNdMemory> for ProofStream {
    type Error = ProofStreamError;

    fn try_from(proof: &ProofForNdMemory) -> Result<Self, ProofStreamError> {
        let proof_stream = *ProofStream::decode(&proof.0)?;
        Ok(proof_stream)
    }
}
