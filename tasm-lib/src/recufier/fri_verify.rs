use std::collections::HashMap;

use crate::{
    get_init_tvm_stack,
    hashing::merkle_root::MerkleRoot,
    snippet_bencher::BenchmarkCase,
    structure::tasm_object::{load_to_memory, TasmObject},
    Digest, VmHasher, VmHasherState,
};
use anyhow::bail;
use itertools::Itertools;
use num_traits::Zero;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{
    arithmetic_domain::ArithmeticDomain,
    fri::{Fri, FriValidationError},
    instruction::LabelledInstruction,
    proof_stream::ProofStream,
    triton_asm, BFieldElement, NonDeterminism,
};
use twenty_first::{
    shared_math::{
        bfield_codec::BFieldCodec,
        ntt::{intt, ntt},
        other::log_2_ceil,
        polynomial::Polynomial,
        traits::{ModPowU32, PrimitiveRootOfUnity},
        x_field_element::XFieldElement,
    },
    util_types::{algebraic_hasher::Domain, merkle_tree::MerkleTree},
};

use crate::{
    library::Library,
    procedure::Procedure,
    recufier::proof_stream::VmProofStream,
    snippet::{BasicSnippet, DataType},
};

/// `FriVerify` checks that a Reed-Solomon codeword, provided as an oracle, has a low
/// degree interpolant. Specifically, the algorithm takes a `ProofStream` object, runs the
/// verifier of the FRI protocol, and (if successful) returns the lists of indices at
/// which the codeword is probed along with the values of the codeword at those indices.
/// The test succeeds with probability 1 if the codeword is of low degree; and with
/// probability *soundness error* if the codeword is far from low-degree. If the test is
/// not successful, the VM crashes.
#[derive(Debug, Clone, PartialEq, Eq, BFieldCodec, TasmObject)]
pub struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_colinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    domain_generator: BFieldElement,
}

impl FriVerify {
    pub fn new(
        offset: BFieldElement,
        domain_length: u32,
        expansion_factor: u32,
        num_colinearity_checks: u32,
    ) -> Self {
        let domain = ArithmeticDomain::of_length(domain_length as usize).with_offset(offset);
        Self {
            expansion_factor,
            num_colinearity_checks,
            domain_length,
            domain_offset: domain.offset,
            domain_generator: domain.generator,
        }
    }

    pub fn call(
        &self,
        proof_stream: &mut VmProofStream,
        nondeterminism: &NonDeterminism<BFieldElement>,
    ) -> Vec<(u32, XFieldElement)> {
        self.inner_verify(proof_stream, &mut nondeterminism.digests.clone())
            .unwrap()
    }

    /// Computes the number of rounds
    pub fn num_rounds(&self) -> usize {
        let first_round_code_dimension = self.first_round_max_degree() + 1;
        let max_num_rounds = log_2_ceil(first_round_code_dimension as u128);

        // Skip rounds for which Merkle tree verification cost exceeds arithmetic cost,
        // because more than half the codeword's locations are queried.
        let num_rounds_checking_all_locations = self.num_colinearity_checks.ilog2() as u64;
        let num_rounds_checking_most_locations = num_rounds_checking_all_locations + 1;

        max_num_rounds.saturating_sub(num_rounds_checking_most_locations) as usize
    }

    /// Computes the max degree of the codeword interpolant after the last round
    pub fn last_round_max_degree(&self) -> usize {
        self.first_round_max_degree() >> self.num_rounds()
    }

    // Computes the max degree of the very first codeword interpolant
    pub fn first_round_max_degree(&self) -> usize {
        assert!(self.domain_length >= self.expansion_factor);
        (self.domain_length / self.expansion_factor) as usize - 1
    }

    /// Compute a new list containing the `XFieldElement`s of the given list, but lifted
    /// to the type `Digest` via padding with 2 zeros.
    /// TODO: write a snippet for me.
    fn map_convert_xfe_to_digest(xfes: &[XFieldElement]) -> Vec<Digest> {
        xfes.iter().map(|x| (*x).into()).collect()
    }

    /// Get the x-coordinate of an A or B point in a colinearity check, given the point's
    /// index and the round number in which the check takes place. In Triton VM, this
    /// method is called `get_evaluation_argument`.
    /// TODO: write snippet for me
    fn get_colinearity_check_x(&self, idx: u32, round: usize) -> XFieldElement {
        let domain_value = self.domain_offset * self.domain_generator.mod_pow_u32(idx);
        let round_exponent = 2u32.pow(round as u32);
        let evaluation_argument = domain_value.mod_pow_u32(round_exponent);

        evaluation_argument.lift()
    }

    /// Verify the FRI proof embedded in the proof stream. This function expands the list
    /// `nondeterministic_digests` with the digests of the individual authentication paths
    /// obtained from reduplicating the authentication structures that live in the proof
    /// stream.
    fn inner_verify(
        &self,
        proof_stream: &mut VmProofStream,
        nondeterministic_digests: &mut Vec<Digest>,
    ) -> anyhow::Result<Vec<(u32, XFieldElement)>> {
        let mut num_nondeterministic_digests_read = 0;

        // calculate number of rounds
        let num_rounds = self.num_rounds();
        let last_round_max_degree = self.last_round_max_degree();

        // Extract all roots and calculate alpha based on Fiat-Shamir challenge
        let mut roots = Vec::with_capacity(num_rounds);
        let mut alphas = Vec::with_capacity(num_rounds);

        let first_root = proof_stream.dequeue().unwrap().as_merkle_root().unwrap();
        roots.push(first_root);

        for _round in 0..num_rounds {
            // get a challenge from the verifier
            let alpha = proof_stream.sample_scalars(1)[0];
            alphas.push(alpha);

            // get a commitment from the prover
            let root = proof_stream.dequeue().unwrap().as_merkle_root().unwrap();
            roots.push(root);
        }

        // Extract last codeword
        let last_codeword = proof_stream.dequeue().unwrap().as_fri_codeword().unwrap();

        // Check if last codeword matches the given root
        let codeword_digests = Self::map_convert_xfe_to_digest(&last_codeword); // <-- snippet me
        let last_codeword_merkle_root =
            MerkleRoot::call(&codeword_digests, 0, codeword_digests.len()); // snippet ✔
        let last_root = roots.last().unwrap();
        if *last_root != last_codeword_merkle_root {
            bail!(FriValidationError::BadMerkleRootForLastCodeword);
        }

        // Verify that last codeword is of sufficiently low degree

        // Compute interpolant to get the degree of the last codeword.
        // Note that we don't have to scale the polynomial back to the trace subgroup since we
        // only check its degree and don't use it further.
        let log_2_of_n = last_codeword.len().ilog2(); // <-- snippet me
        let mut last_polynomial = last_codeword.clone();

        let last_fri_domain_generator = self
            .domain_generator
            .mod_pow_u32(2u32.pow(num_rounds as u32)); // <-- snippet me
        intt::<XFieldElement>(&mut last_polynomial, last_fri_domain_generator, log_2_of_n); // <-- snippet me
        let last_poly_degree = Polynomial::new(last_polynomial).degree(); // <-- snippet me

        if last_poly_degree > last_round_max_degree as isize {
            println!(
                "last_poly_degree is {last_poly_degree}, \
                degree_of_last_round is {last_round_max_degree}",
            );
            bail!(FriValidationError::LastIterationTooHighDegree)
        }

        // Query phase
        // query step 0: get "A" indices and verify set membership of corresponding values.
        let mut a_indices =
            proof_stream.sample_indices(self.domain_length, self.num_colinearity_checks);

        let tree_height = self.domain_length.ilog2() as usize;
        let fri_response = proof_stream.dequeue().unwrap().as_fri_response().unwrap();
        assert_eq!(a_indices.len(), fri_response.revealed_leaves.len());
        let mut a_values = fri_response.revealed_leaves;

        let leaf_digests = Self::map_convert_xfe_to_digest(&a_values);
        let leafs_and_indices = leaf_digests.iter().copied().zip(a_indices.iter());

        // reduplicate authentication structures if necessary
        if num_nondeterministic_digests_read >= nondeterministic_digests.len() {
            // sanity check: the authentication structure was valid, right?
            assert!(MerkleTree::<VmHasher>::verify_authentication_structure(
                roots[0],
                tree_height,
                &a_indices.iter().map(|i| *i as usize).collect_vec(),
                &leaf_digests,
                &fri_response.auth_structure,
            ));

            let reduplicated_authentication_paths =
                MerkleTree::<VmHasher>::authentication_paths_from_authentication_structure(
                    tree_height,
                    &a_indices.iter().map(|i| *i as usize).collect_vec(),
                    &leaf_digests,
                    &fri_response.auth_structure,
                )?;
            nondeterministic_digests.append(
                &mut reduplicated_authentication_paths
                    .into_iter()
                    .flatten()
                    .collect_vec(),
            );
        }

        // verify authentication paths for A leafs
        for (leaf_digest, leaf_index) in leafs_and_indices {
            let authentication_path = &nondeterministic_digests[num_nondeterministic_digests_read
                ..(num_nondeterministic_digests_read + tree_height)];
            num_nondeterministic_digests_read += tree_height;
            assert!(MerkleTree::<VmHasher>::verify_authentication_structure(
                roots[0],
                tree_height,
                &[*leaf_index as usize],
                &[leaf_digest],
                authentication_path
            ));
        }

        // save indices and revealed leafs of first round's codeword for returning
        let revealed_indices_and_elements_first_half = a_indices
            .iter()
            .copied()
            .zip_eq(a_values.iter().copied())
            .collect_vec();
        // these indices and values will be computed in the first iteration of the main loop below
        let mut revealed_indices_and_elements_second_half = vec![];

        // set up "B" for offsetting inside loop.  Note that "B" and "A" indices can be calcuated
        // from each other.
        let mut b_indices = a_indices.clone();
        let mut current_domain_len = self.domain_length;
        let mut current_tree_height = tree_height as u32;

        // query step 1:  loop over FRI rounds, verify "B"s, compute values for "C"s
        for r in 0..num_rounds {
            // get "B" indices and verify set membership of corresponding values
            b_indices = b_indices
                .iter()
                .map(|x| (x + current_domain_len / 2) % current_domain_len)
                .collect();
            let fri_response = proof_stream.dequeue().unwrap().as_fri_response().unwrap();
            let b_values = fri_response.revealed_leaves;

            let leaf_digests = Self::map_convert_xfe_to_digest(&b_values);
            let leafs_and_indices = leaf_digests.iter().copied().zip(b_indices.iter().copied());

            // reduplicate authentication structures if necessary
            if num_nondeterministic_digests_read >= nondeterministic_digests.len() {
                let reduplicated_authentication_paths =
                    MerkleTree::<VmHasher>::authentication_paths_from_authentication_structure(
                        current_tree_height as usize,
                        &b_indices.iter().map(|i| *i as usize).collect_vec(),
                        &leaf_digests,
                        &fri_response.auth_structure,
                    )?;
                nondeterministic_digests.append(
                    &mut reduplicated_authentication_paths
                        .into_iter()
                        .flatten()
                        .collect_vec(),
                );

                // sanity check: the auth structure was valid, right?
                assert!(MerkleTree::<VmHasher>::verify_authentication_structure(
                    roots[r],
                    current_tree_height as usize,
                    &b_indices.iter().map(|i| *i as usize).collect_vec(),
                    &leaf_digests,
                    &fri_response.auth_structure,
                ));
            }

            // verify authentication paths for B leafs
            for (leaf, index) in leafs_and_indices {
                let authentication_path = &nondeterministic_digests
                    [num_nondeterministic_digests_read
                        ..(num_nondeterministic_digests_read + current_tree_height as usize)];
                num_nondeterministic_digests_read += current_tree_height as usize;
                if !MerkleTree::<VmHasher>::verify_authentication_structure(
                    roots[r],
                    current_tree_height as usize,
                    &[index as usize],
                    &[leaf],
                    authentication_path,
                ) {
                    bail!(FriValidationError::BadMerkleAuthenticationPath);
                }
            }

            debug_assert_eq!(self.num_colinearity_checks, a_indices.len() as u32);
            debug_assert_eq!(self.num_colinearity_checks, b_indices.len() as u32);
            debug_assert_eq!(self.num_colinearity_checks, a_values.len() as u32);
            debug_assert_eq!(self.num_colinearity_checks, b_values.len() as u32);

            if r == 0 {
                // save other half of indices and revealed leafs of first round for returning
                revealed_indices_and_elements_second_half = b_indices
                    .iter()
                    .copied()
                    .zip_eq(b_values.iter().copied())
                    .collect_vec();
            }

            // compute "C" indices and values for next round from "A" and "B" of current round
            current_domain_len /= 2;
            current_tree_height -= 1;
            let c_indices = a_indices.iter().map(|x| x % current_domain_len).collect();
            let c_values = (0..self.num_colinearity_checks as usize)
                .map(|i| {
                    Polynomial::<XFieldElement>::get_colinear_y(
                        (self.get_colinearity_check_x(a_indices[i], r), a_values[i]),
                        (self.get_colinearity_check_x(b_indices[i], r), b_values[i]),
                        alphas[r],
                    )
                })
                .collect();

            // next rounds "A"s correspond to current rounds "C"s
            a_indices = c_indices;
            a_values = c_values;
        }

        // Finally compare "C" values (which are named "A" values in this enclosing scope) with
        // last codeword from the proofstream.
        a_indices = a_indices.iter().map(|x| x % current_domain_len).collect();
        if !(0..self.num_colinearity_checks as usize)
            .all(|i| last_codeword[a_indices[i] as usize] == a_values[i])
        {
            bail!(FriValidationError::MismatchingLastCodeword);
        }

        // compile return object and store to memory
        let revealed_indices_and_elements = revealed_indices_and_elements_first_half
            .into_iter()
            .chain(revealed_indices_and_elements_second_half.into_iter())
            .collect_vec();

        Ok(revealed_indices_and_elements)
    }

    /// Generate a proof, embedded in a proof stream.
    pub fn pseudorandom_fri_proof_stream(&self, seed: [u8; 32]) -> VmProofStream {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let max_degree = self.domain_length as usize / self.expansion_factor as usize - 1;
        let coefficients: Vec<XFieldElement> = (0..=max_degree).map(|_| rng.gen()).collect();
        let mut codeword = [
            coefficients,
            vec![XFieldElement::zero(); self.domain_length as usize - max_degree - 1],
        ]
        .concat();
        let n = codeword.len();
        ntt::<XFieldElement>(
            &mut codeword,
            BFieldElement::primitive_root_of_unity(n as u64).unwrap(),
            n.ilog2(),
        );
        let mut proof_stream = ProofStream::<VmHasher>::new();

        let fri = Fri::new(
            ArithmeticDomain {offset, generator: BFieldElement::primitive_root_of_unity(domain_length), length: domain_length},
            self.expansion_factor as usize,
            self.num_colinearity_checks as usize,
        );
        fri.prove(&codeword, &mut proof_stream);

        VmProofStream::new(&proof_stream.items)
    }
}

impl BasicSnippet for FriVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*proof_stream".to_string()),
            (DataType::VoidPointer, "*fri_verify".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*proof_stream".to_string()),
            (
                DataType::List(Box::new(DataType::Tuple(vec![
                    DataType::U32,
                    DataType::XFE,
                ]))),
                "indices_and_elements".to_string(),
            ),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_fri_verify".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        triton_asm! {
            // BEFORE: _ *proof_stream *fri_verify
            // AFTER: _ *proof_stream *indices_and_leafs
            {entrypoint}:

        }
    }
}

impl Procedure for FriVerify {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        _public_input: &[triton_vm::BFieldElement],
        sponge_state: &mut crate::VmHasherState,
    ) -> Vec<triton_vm::BFieldElement> {
        // read fri object
        let fri_pointer = stack.pop().unwrap();
        let fri_verify = *FriVerify::decode_from_memory(memory, fri_pointer).unwrap();
        assert_eq!(fri_verify, *self);

        // read proof stream
        let proof_stream_pointer = stack.pop().unwrap();
        let mut proof_stream: VmProofStream =
            *VmProofStream::decode_from_memory(memory, proof_stream_pointer).unwrap();

        proof_stream.sponge_state = sponge_state.clone();

        let revealed_indices_and_elements = self.call(&mut proof_stream, nondeterminism);

        let indices_and_leafs_pointer = load_to_memory(memory, revealed_indices_and_elements);

        // put stack in order
        stack.push(proof_stream_pointer);
        stack.push(indices_and_leafs_pointer);
        // set sponge state to correct value
        *sponge_state = proof_stream.sponge_state;

        // no standard output
        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let proof_stream = self.pseudorandom_fri_proof_stream(rng.gen());
        let mut digests = vec![];
        self.inner_verify(&mut proof_stream.clone(), &mut digests)
            .unwrap();

        let mut stack = get_init_tvm_stack();
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let proof_stream_pointer = load_to_memory(&mut memory, proof_stream);
        let fri_verify_pointer = load_to_memory(&mut memory, self.clone());
        let nondeterminism = NonDeterminism::new(vec![])
            .with_ram(memory.clone())
            .with_digests(digests);
        stack.push(proof_stream_pointer);
        stack.push(fri_verify_pointer);
        let stdin = vec![];
        let sponge_state = VmHasherState::new(Domain::VariableLength);

        (stack, memory, nondeterminism, stdin, sponge_state)
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::{thread_rng, Rng, RngCore};
    use triton_vm::{fri::Fri, proof_stream::ProofStream, BFieldElement, arithmetic_domain::ArithmeticDomain};
    use twenty_first::{util_types::{
        algebraic_hasher::Domain,
        merkle_tree::{CpuParallel, MerkleTree},
        merkle_tree_maker::MerkleTreeMaker,
    }, shared_math::traits::PrimitiveRootOfUnity};

    use crate::{Digest, VmHasher, VmHasherState};

    use super::FriVerify;

    #[test]
    fn fri_derived_params_match() {
        let mut rng = thread_rng();
        let expansion_factor = 1 << (rng.next_u32() % 17);
        let colinearity_checks_count = rng.next_u32() % 100;
        let offset: BFieldElement = rng.gen();
        let domain_length = 1u32 << (rng.next_u32() % 21);

        let fri_verify = FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            colinearity_checks_count,
        );

        let fri = Fri::<VmHasher>::new(
            ArithmeticDomain {offset, generator: BFieldElement::primitive_root_of_unity(domain_length), length: domain_length},
            expansion_factor as usize,
            colinearity_checks_count as usize,
        );

        assert_eq!(fri_verify.num_rounds(), fri.num_rounds());
        assert_eq!(
            fri_verify.last_round_max_degree(),
            fri.last_round_max_degree()
        );
        assert_eq!(
            fri_verify.first_round_max_degree(),
            fri.first_round_max_degree()
        );
    }

    #[test]
    fn test_inner_verify() {
        let mut rng = thread_rng();
        let offset = BFieldElement::new(7);
        let domain_length = 1 << 12;
        let expansion_factor = 16;
        let colinearity_checks_count = 20;
        let fri_verify = FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            colinearity_checks_count,
        );
        let mut vm_proof_stream = fri_verify.pseudorandom_fri_proof_stream(rng.gen());

        // try verify the embedded proof using Triton-VM's verify
        let mut vm_proof_stream_copy = vm_proof_stream.clone();
        let mut proof_items = vec![];
        while let Ok(item) = vm_proof_stream_copy.dequeue() {
            proof_items.push(*item);
        }
        let mut verify_proof_stream = ProofStream::<VmHasher> {
            items: proof_items,
            items_index: 0,
            sponge_state: VmHasherState::new(Domain::VariableLength),
        };
        let fri = Fri::new(
            ArithmeticDomain {offset, generator: BFieldElement::primitive_root_of_unity(domain_length), length: domain_length},
            expansion_factor as usize,
            colinearity_checks_count as usize,
        );
        let verify_result = fri.verify(&mut verify_proof_stream, &mut None);
        assert!(
            verify_result.is_ok(),
            "FRI verify error: {:?}",
            verify_result,
        );
        println!("verified FRI proof with Triton-VM.");

        // try verifying the embedded proof using our own inner_verify
        let verify_result = fri_verify.inner_verify(&mut vm_proof_stream, &mut vec![]);
        assert!(
            verify_result.is_ok(),
            "FRI verify error: {:?}",
            verify_result
        );
        println!("verified FRI proof with inner verify.");
    }

    #[test]
    fn test_merkle_authentication_structure() {
        let mut rng = thread_rng();
        let height = 10;
        let n = 1 << height;
        let m = rng.gen_range(0..n);
        let leafs: Vec<Digest> = (0..n).map(|_| rng.gen()).collect_vec();
        let merkle_tree = <CpuParallel as MerkleTreeMaker<VmHasher>>::from_digests(&leafs);
        let root = merkle_tree.get_root();
        let indices = (0..m).map(|_| rng.gen_range(0..n)).collect_vec();
        let leafs = indices.iter().map(|i| leafs[*i]).collect_vec();
        let authentication_structure = merkle_tree.get_authentication_structure(&indices);
        let authentication_paths =
            MerkleTree::<VmHasher>::authentication_paths_from_authentication_structure(
                height,
                &indices,
                &leafs,
                &authentication_structure,
            )
            .unwrap();
        for ((index, leaf), authentication_path) in indices
            .into_iter()
            .zip(leafs.into_iter())
            .zip(authentication_paths.into_iter())
        {
            assert!(MerkleTree::<VmHasher>::verify_authentication_structure(
                root,
                height,
                &[index],
                &[leaf],
                &authentication_path
            ));
        }
    }
}
