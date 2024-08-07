use itertools::Itertools;
use triton_vm::prelude::*;
use triton_vm::proof_stream::ProofStream;
use triton_vm::table::challenges::Challenges;
use triton_vm::table::extension_table::Quotientable;
use triton_vm::table::master_table::MasterExtTable;
use triton_vm::table::NUM_BASE_COLUMNS;
use triton_vm::table::NUM_EXT_COLUMNS;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::twenty_first::prelude::*;

use crate::Digest;

pub struct StarkProofExtraction {
    pub fri_proof_stream: ProofStream,
    pub base_tree_authentication_paths: Vec<Vec<Digest>>,
    pub ext_tree_authentication_paths: Vec<Vec<Digest>>,
    pub quot_tree_authentication_paths: Vec<Vec<Digest>>,
}

/// Extracts a proof stream that will work for FRI verification from a proof stream that works for
/// the whole STARK verification.
pub fn extract_fri_proof(
    proof_stream: &ProofStream,
    claim: &Claim,
    stark: &Stark,
) -> StarkProofExtraction {
    let mut proof_stream = proof_stream.to_owned();
    let log2_padded_height = proof_stream
        .dequeue()
        .unwrap()
        .try_into_log2_padded_height()
        .unwrap();
    proof_stream.alter_fiat_shamir_state_with(claim);

    // Base-table Merkle root
    let base_table_root = proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Extension challenge weights
    proof_stream.sample_scalars(Challenges::SAMPLE_COUNT);

    // Extension-table Merkle root
    let ext_mt_root = proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Quotient codeword weights
    proof_stream.sample_scalars(MasterExtTable::NUM_CONSTRAINTS);

    // Quotient codeword Merkle root
    let quotient_root = proof_stream
        .dequeue()
        .unwrap()
        .try_into_merkle_root()
        .unwrap();

    // Out-of-domain point current row
    proof_stream.sample_scalars(1);

    // Five out-of-domain values
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_base_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_ext_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_base_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_ext_row()
        .unwrap();
    proof_stream
        .dequeue()
        .unwrap()
        .try_into_out_of_domain_quot_segments()
        .unwrap();

    // `beqd_weights`
    const NUM_DEEP_CODEWORD_COMPONENTS: usize = 3;
    proof_stream.sample_scalars(
        NUM_BASE_COLUMNS + NUM_EXT_COLUMNS + NUM_QUOTIENT_SEGMENTS + NUM_DEEP_CODEWORD_COMPONENTS,
    );

    let padded_height = 1 << log2_padded_height;
    let fri: triton_vm::fri::Fri<Tip5> = stark.derive_fri(padded_height).unwrap();
    let fri_proof_stream = proof_stream.clone();
    let fri_verify_result = fri.verify(&mut proof_stream).unwrap();
    let indices = fri_verify_result.iter().map(|(i, _)| *i).collect_vec();
    let tree_height = fri.domain.length.ilog2() as usize;

    // base
    let base_table_rows = proof_stream
        .dequeue()
        .unwrap()
        .try_into_master_base_table_rows()
        .unwrap();
    let base_authentication_structure = proof_stream
        .dequeue()
        .unwrap()
        .try_into_authentication_structure()
        .unwrap();
    let base_tree_auth_paths = extract_paths(
        base_table_root,
        &indices,
        &base_table_rows,
        &base_authentication_structure,
        tree_height,
    );

    // extension
    let ext_table_rows = proof_stream
        .dequeue()
        .unwrap()
        .try_into_master_ext_table_rows()
        .unwrap();
    let ext_authentication_structure = proof_stream
        .dequeue()
        .unwrap()
        .try_into_authentication_structure()
        .unwrap();
    let ext_tree_auth_paths = extract_paths(
        ext_mt_root,
        &indices,
        &ext_table_rows,
        &ext_authentication_structure,
        tree_height,
    );

    // quotient
    let quot_table_rows = proof_stream
        .dequeue()
        .unwrap()
        .try_into_quot_segments_elements()
        .unwrap();
    let quot_authentication_structure = proof_stream
        .dequeue()
        .unwrap()
        .try_into_authentication_structure()
        .unwrap();
    let quot_tree_auth_paths = extract_paths(
        quotient_root,
        &indices,
        &quot_table_rows,
        &quot_authentication_structure,
        tree_height,
    );

    StarkProofExtraction {
        fri_proof_stream,
        base_tree_authentication_paths: base_tree_auth_paths,
        ext_tree_authentication_paths: ext_tree_auth_paths,
        quot_tree_authentication_paths: quot_tree_auth_paths,
    }
}

fn extract_paths<const N: usize, T: BFieldCodec>(
    root: Digest,
    indices: &[usize],
    rows: &[[T; N]],
    authentication_structure: &[Digest],
    tree_height: usize,
) -> Vec<Vec<Digest>> {
    let leafs = rows.iter().map(Tip5::hash).collect_vec();
    let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
        tree_height,
        indexed_leafs: indices.iter().cloned().zip(leafs).collect_vec(),
        authentication_structure: authentication_structure.to_vec(),
        _hasher: std::marker::PhantomData,
    };
    assert!(inclusion_proof.clone().verify(root));
    inclusion_proof.into_authentication_paths().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_fri_proof_works() {
        let simple_program = triton_program!(halt);
        let public_input = [];
        let non_determinism = NonDeterminism::default();
        let (stark, claim, proof) =
            triton_vm::prove_program(&simple_program, public_input.into(), non_determinism)
                .unwrap();
        let padded_height = proof.padded_height().unwrap();
        let fri = stark.derive_fri(padded_height).unwrap();

        let proof_stream = ProofStream::try_from(&proof).unwrap();
        let mut fri_proof_stream =
            extract_fri_proof(&proof_stream, &claim, &stark).fri_proof_stream;
        assert!(
            fri.verify(&mut fri_proof_stream).is_ok(),
            "Extracted proof must verify"
        );
    }
}
