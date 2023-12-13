use std::collections::HashMap;

use crate::{
    empty_stack, field,
    hashing::{merkle_root::MerkleRoot, sample_indices::SampleIndices},
    list::{
        higher_order::{
            inner_function::{InnerFunction, RawCode},
            map::Map,
            zip::Zip,
        },
        unsafeimplu32::{
            get::UnsafeGet, length::Length as UnsafeLength, new::UnsafeNew, push::UnsafePush,
        },
        ListType,
    },
    memory::dyn_malloc::DYN_MALLOC_ADDRESS,
    recufier::{
        get_colinear_y::ColinearYXfe,
        get_colinearity_check_x::GetColinearityCheckX,
        proof_stream::{dequeue::Dequeue, sample_scalars::SampleScalars},
        verify_authentication_paths_for_leaf_and_index_list::VerifyAuthenticationPathForLeafAndIndexList,
        xfe_ntt::XfeNtt,
    },
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
    proof_item::FriResponse,
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
    snippet::{BasicSnippet, DataType},
};

use super::proof_stream::vm_proof_stream::VmProofStream;

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
    fn map_convert_xfe_to_digest(xfes: &[XFieldElement]) -> Vec<Digest> {
        xfes.iter().map(|x| (*x).into()).collect()
    }

    /// Get the x-coordinate of an A or B point in a colinearity check, given the point's
    /// index and the round number in which the check takes place. In Triton VM, this
    /// method is called `get_evaluation_argument`.
    pub fn get_colinearity_check_x(&self, idx: u32, round: usize) -> XFieldElement {
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

        println!("Inside inner_verify.");

        // calculate number of rounds
        let num_rounds = self.num_rounds();
        println!("Number of rounds: {num_rounds}");
        let last_round_max_degree = self.last_round_max_degree();
        println!("Max degree in last round: {last_round_max_degree}");

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
        println!("alphas:");
        for alpha in alphas.iter() {
            println!("{}", alpha);
        }

        // Extract last codeword
        let last_codeword = proof_stream.dequeue().unwrap().as_fri_codeword().unwrap();
        assert_eq!(
            last_codeword.len(),
            self.domain_length as usize >> self.num_rounds()
        );

        // Check if last codeword matches the given root
        let codeword_digests = Self::map_convert_xfe_to_digest(&last_codeword);
        let last_codeword_merkle_root =
            MerkleRoot::call(&codeword_digests, 0, codeword_digests.len());

        let last_root = roots.last().unwrap();
        if *last_root != last_codeword_merkle_root {
            bail!(FriValidationError::BadMerkleRootForLastCodeword);
        }

        // Verify that last codeword is of sufficiently low degree

        // Compute interpolant to get the degree of the last codeword.
        // Note that we don't have to scale the polynomial back to the trace
        // subgroup since we only check its degree and don't use it further.
        let log_2_of_n = last_codeword.len().ilog2();
        let mut last_polynomial = last_codeword.clone();

        let last_fri_domain_generator = self
            .domain_generator
            .mod_pow_u32(2u32.pow(num_rounds as u32));
        intt::<XFieldElement>(&mut last_polynomial, last_fri_domain_generator, log_2_of_n);
        let last_poly_degree = Polynomial::new(last_polynomial).degree();

        if last_poly_degree > last_round_max_degree as isize {
            println!(
                "last_poly_degree is {last_poly_degree}, \
                degree_of_last_round is {last_round_max_degree}",
            );
            bail!(FriValidationError::LastRoundPolynomialHasTooHighDegree)
        }

        // QUERY PHASE

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
            .chain(revealed_indices_and_elements_second_half)
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
            ArithmeticDomain {
                offset: BFieldElement::new(7),
                generator: BFieldElement::primitive_root_of_unity(self.domain_length as u64)
                    .unwrap(),
                length: self.domain_length as usize,
            },
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

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let domain_length = field!(FriVerify::domain_length);
        let domain_generator = field!(FriVerify::domain_generator);
        let expansion_factor = field!(FriVerify::expansion_factor);
        let num_colinearity_checks = field!(FriVerify::num_colinearity_checks);
        let new_list_of_digests = library.import(Box::new(UnsafeNew(DataType::Digest)));
        let push_digest_to_list = library.import(Box::new(UnsafePush(DataType::Digest)));
        let read_digest = triton_asm!(
                                        // _ *digest
            push 4 add                  // _ *digest+4
            read_mem swap 1 push -1 add // _ d4 *digest+3
            read_mem swap 1 push -1 add // _ d4 d3 *digest+2
            read_mem swap 1 push -1 add // _ d4 d3 d2 *digest+1
            read_mem swap 1 push -1 add // _ d4 d3 d2 d1 *digest
            read_mem swap 1 pop         // _ d4 d3 d2 d1 d0
        );
        let new_list_xfe = library.import(Box::new(UnsafeNew(DataType::XFE)));
        let get_scalar = library.import(Box::new(UnsafeGet(DataType::XFE)));
        let push_scalar = library.import(Box::new(UnsafePush(DataType::XFE)));
        let proof_stream_dequeue = library.import(Box::new(Dequeue {}));
        let proof_stream_sample_scalars = library.import(Box::new(SampleScalars {}));
        let dequeue_query_phase = format!("{entrypoint}_dequeue_query_phase_remainder");
        let convert_xfe_to_digest = format!("{entrypoint}_convert_xfe_to_digest");
        let map_convert_xfe_to_digest = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(RawCode {
                function: triton_asm!
                (
                    {convert_xfe_to_digest}:
                        // _ xfe2 xfe1 xfe0
                        push 0 push 0       // _ xfe2 xfe1 xfe0 0 0
                        swap 4              // _ 0 xfe1 xfe0 0 xfe2
                        swap 2              // _ 0 xfe1 xfe2 0 xfe0
                        swap 3              // _ 0 xfe0 xfe2 0 xfe1
                        swap 1              // _ 0 xfe0 xfe2 xfe1 0
                        swap 3              // _ 0 0 xfe2 xfe1 xfe0
                        // _ 0 0 xfe2 xfe1 xfe0
                        return
                ),
                input_type: DataType::XFE,
                output_type: DataType::Digest,
            }),
        }));
        let length_of_list_of_digests = library.import(Box::new(UnsafeLength(DataType::Digest)));
        let length_of_list_of_u32s = library.import(Box::new(UnsafeLength(DataType::U32)));
        let length_of_list_of_xfes = library.import(Box::new(UnsafeLength(DataType::XFE)));
        let merkle_root = library.import(Box::new(MerkleRoot));
        let get_digest = library.import(Box::new(UnsafeGet(DataType::Digest)));
        let xfe_ntt = library.import(Box::new(XfeNtt));
        let assert_tail_xfe0 = format!("{entrypoint}_tail_xfe0");
        let length_of_list_of_xfe = library.import(Box::new(UnsafeLength(DataType::XFE)));
        let get_xfe_from_list = library.import(Box::new(UnsafeGet(DataType::XFE)));
        let get_u32_from_list = library.import(Box::new(UnsafeGet(DataType::U32)));
        let sample_indices = library.import(Box::new(SampleIndices {
            list_type: ListType::Unsafe,
        }));
        let revealed_leafs = field!(FriResponse::revealed_leaves);
        let zip_digests_indices = library.import(Box::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::Digest,
            right_type: DataType::U32,
        }));
        let verify_authentication_paths_for_leaf_and_index_list =
            library.import(Box::new(VerifyAuthenticationPathForLeafAndIndexList {
                list_type: ListType::Unsafe,
            }));
        let zip_index_xfe = library.import(Box::new(Zip {
            list_type: ListType::Unsafe,
            left_type: DataType::U32,
            right_type: DataType::XFE,
        }));
        let query_phase_main_loop = format!("{entrypoint}_query_phase_main_loop");
        let add_half_label = format!("{entrypoint}_add_half_domain");
        let map_add_half_domain_length = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(RawCode {
                function: triton_asm! {
                    {add_half_label}:
                                            // _ current_domain_length r half_domain_length [bu ff er] index
                        dup 4 add           // _ current_domain_length r half_domain_length [bu ff er] index+half_domain_length
                        dup 6 swap 1 div_mod// _ current_domain_length r half_domain_length [bu ff er] (index+half_domain_length)/domain_length
                        swap 1 pop          // _ current_domain_length r half_domain_length [bu ff er] (index+half_domain_length)%domain_length
                        return
                },
                input_type: DataType::U32,
                output_type: DataType::U32,
            }),
        }));
        let populate_return_vector_second_half =
            format!("{entrypoint}_populate_return_vector_second_half");
        let populate_loop = format!("{entrypoint}_populate_return_vector_loop");
        let get_u32_and_xfe = library.import(Box::new(UnsafeGet(DataType::Tuple(vec![
            DataType::U32,
            DataType::XFE,
        ]))));
        let push_u32_and_xfe = library.import(Box::new(UnsafePush(DataType::Tuple(vec![
            DataType::U32,
            DataType::XFE,
        ]))));
        let push_xfe_to_list = library.import(Box::new(UnsafePush(DataType::XFE)));
        let reduce_indices_label = format!("{entrypoint}_reduce_indices");
        let map_reduce_indices = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(RawCode {
                function: triton_asm! {
                    {reduce_indices_label}:
                                            // _ half_domain_length [bu ff er] index
                        dup 4 swap 1        // _ half_domain_length [bu ff er] half_domain_length index
                        div_mod             // _ half_domain_length [bu ff er] q r
                        swap 1 pop          // _ half_domain_length [bu ff er] index%half_domain_length
                        return
                },
                input_type: DataType::U32,
                output_type: DataType::U32,
            }),
        }));
        let compute_c_values_loop = format!("{entrypoint}_compute_c_values_loop");
        let get_colinearity_check_x = library.import(Box::new(GetColinearityCheckX));
        let get_colinear_y = library.import(Box::new(ColinearYXfe));
        let identity_label = format!("{entrypoint}_identity");
        let duplicate_list_xfe = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(RawCode {
                input_type: DataType::XFE,
                output_type: DataType::XFE,
                function: triton_asm! {
                    {identity_label}:
                        return
                },
            }),
        }));
        let assert_membership_label = format!("{entrypoint}_assert_codeword_membership");
        let map_assert_membership = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(RawCode {
                input_type: DataType::Tuple(vec![DataType::U32, DataType::XFE]),
                output_type: DataType::Tuple(vec![DataType::U32, DataType::XFE]),
                function: triton_asm! {
                    // BEFORE: _ *codeword [bu ff er] xfe2 xfe1 xfe0 index
                    // AFTER: _ *codeword [bu ff er] xfe2 xfe1 xfe0 index
                    {assert_membership_label}:
                        push 0                  // _ *codeword [bu ff er] xfe2 xfe1 xfe0 index 0
                        dup 8 dup 2             // _ *codeword [bu ff er] xfe2 xfe1 xfe0 index 0 *codeword index
                        call {get_xfe_from_list}// _ *codeword [bu ff er] xfe2 xfe1 xfe0 index 0 xfe2' xfe1' xfe0'
                        dup 4 push 0            // _ *codeword [bu ff er] xfe2 xfe1 xfe0 index 0 xfe2' xfe1' xfe0' index 0
                        assert_vector           // _ *codeword [bu ff er] xfe2 xfe1 xfe0 index 0 xfe2' xfe1' xfe0' index 0
                        pop pop pop pop pop pop // _ *codeword [bu ff er] xfe2 xfe1 xfe0 index
                        return
                },
            }),
        }));

        triton_asm! {
            // INVARIANT:          _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i
            {compute_c_values_loop}:
                // evaluate termination criterion
                // if i == length, then return
                dup 2 call {length_of_list_of_xfes}
                dup 1 eq
                skiz return

                // lift things to top of stack
                dup 15 dup 14 dup 9     // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices
                dup 12 dup 9            // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify

                // get p0x=ax
                dup 3 dup 6             // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify *a_indices i
                call {get_u32_from_list}// _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify a_indices[i]
                dup 1 swap 1 dup 3      // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify *fri_verify a_indices[i] r
                call {get_colinearity_check_x}
                                        // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify ax2 ax1 ax0
                swap 2 pop pop          // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas *a_indices *b_indices r *fri_verify ax0

                // get p1x=bx
                swap 4 pop              // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 *b_indices r *fri_verify

                swap 2  dup 5           // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 *fri_verify r *b_indices i
                call {get_u32_from_list}// _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 *fri_verify r b_indices[i]
                swap 1 call {get_colinearity_check_x}
                                        // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx2 bx1 bx0
                swap 2 pop pop          // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0

                // get p0y = ay
                dup 15 dup 4            // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0 *a_elements i
                call {get_xfe_from_list}// _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0 ay2 ay1 ay0

                // keep r accessible; weneed it
                dup 15                  // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0 ay2 ay1 ay0 r
                dup 13 dup 8            // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0 ay2 ay1 ay0 r *b_elements i
                call {get_xfe_from_list}// _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 bx0 ay2 ay1 ay0 r by2 by1 by0

                // get alphas[r]
                swap 7 swap 1           // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas ax0 by0 ay2 ay1 ay0 r by2 bx0 by1
                swap 8 swap 2           // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i *alphas by0 by0 ay2 ay1 ay0 r ax0 bx0 by2
                swap 9                  // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i by2 by0 by0 ay2 ay1 ay0 r ax0 bx0 *alphas
                dup 3 call {get_xfe_from_list}
                                        // _ *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i by2 by0 by0 ay2 ay1 ay0 r ax0 bx0 alpha2 alpha1 alpha0

                // reorder stack for call get_colinear_y
                // stack needs to be of form: _ [cx] [by] [bx] [ay] [ax]
                // where cx = alphas[r] and where a and b can be switched
                swap 9  // _ by2 by1 alpha0 ay2 ay1 ay0 r ax0 bx0 alpha2 alpha1 by0
                swap 1  // _ by2 by1 alpha0 ay2 ay1 ay0 r ax0 bx0 alpha2 by0 alpha1
                swap 10 // _ by2 alpha1 alpha0 ay2 ay1 ay0 r ax0 bx0 alpha2 by0 by1
                swap 2  // _ by2 alpha1 alpha0 ay2 ay1 ay0 r ax0 bx0 by1 by0 alpha2
                swap 11 // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 r ax0 bx0 by1 by0 by2
                push 0  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 r ax0 bx0 by1 by0 by2 0
                swap 6  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 ax0 bx0 by1 by0 by2 r
                pop     // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 ax0 bx0 by1 by0 by2
                push 0  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 ax0 bx0 by1 by0 by2 0
                swap 5  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 bx0 by1 by0 by2 ax0
                swap 4  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 ax0 by1 by0 by2 bx0
                swap 2  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 ax0 by1 bx0 by2 by0
                swap 1  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 ax0 by1 bx0 by0 by2
                swap 3  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 ax0 by2 bx0 by0 by1
                swap 2  // _ alpha2 alpha1 alpha0 ay2 ay1 ay0 0 0 ax0 by2 by1 by0 bx0
                push 0
                push 0
                swap 2
                call {get_colinear_y}
                                        // _ *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i cy2 cy1 cy0
                dup 4 swap 4 pop        // _ *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values *c_values cy2 cy1 cy0
                call {push_xfe_to_list} // _ *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values
                dup 0 call {length_of_list_of_xfes}
                                        // _ *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_values i+1

                recurse

            // BEFORE: _ *list index
            // AFTER: _ *list length
            {assert_tail_xfe0}:
                dup 1                           // _ *list index *list
                call {length_of_list_of_xfe}    // _ *list index len
                dup 1 eq                        // _ *list index len==index
                skiz return                     // _ *list index

                dup 1 dup 1                     // _ *list index *list index
                call {get_xfe_from_list}        // _ *list index xfe2 xfe1 xfe0
                push 0 eq assert                // _ *list index xfe2 xfe1
                push 0 eq assert                // _ *list index xfe2
                push 0 eq assert                // _ *list index
                push 1 add                      // _ *list index+1
                recurse

            // BEFORE: _ *proof_stream *fri_verify num_rounds last_round_max_degree | num_rounds *roots *alphas
            // AFTER: _ ... | 0 *roots *alphas
            {dequeue_query_phase}:

                // return if done
                dup 2       // _ num_rounds *roots *alphas num_rounds
                push 0 eq   // _ num_rounds *roots *alphas num_rounds==0
                skiz return

                // decrement round number
                swap 2      // _ *alphas *roots num_rounds
                push -1 add // _ *alphas *roots num_rounds-1
                swap 2      // _ num_rounds-1 *roots *alphas

                // sample scalar
                push 1      // _ num_rounds-1 *roots *alphas 1
                call {proof_stream_sample_scalars}
                            // _ num_rounds-1 *roots *alphas *scalars
                dup 1 swap 1// _ num_rounds-1 *roots *alphas *alphas *scalars
                push 0      // _ num_rounds-1 *roots *alphas *alphas *scalars 0
                call {get_scalar}
                            // _ num_rounds-1 *roots *alphas *alphas [scalars[0]]

                call {push_scalar}
                            // _ num_rounds-1 *roots *alphas

                // dequeue Merkle root
                swap 1      // _ num_rounds-1 *alphas *roots
                dup 6       // _ num_rounds-1 *alphas *roots *proof_stream

                call {proof_stream_dequeue} // _ num_rounds-1 *alphas *roots *proof_stream *root_ev
                push 1 add                  // _ num_rounds-1 *alphas *roots *proof_stream *root
                swap 1 pop                  // _ num_rounds-1 *alphas *roots *root
                dup 1 swap 1                // _ num_rounds-1 *alphas *roots *roots *root
                {&read_digest}              // _ num_rounds-1 *alphas *roots *roots [root]
                call {push_digest_to_list}  // _ num_rounds-1 *alphas *roots
                swap 1                      // _ num_rounds-1 *roots *alphas
                recurse

            // BEFORE: _ *proof_stream *fri_verify
            // AFTER: _ *proof_stream *indices_and_leafs
            {entrypoint}:

                // calculate number of rounds
                dup 0 {&domain_length}      // _ *proof_stream *fri_verify *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify domain_length

                dup 1 {&expansion_factor}   // _ *proof_stream *fri_verify domain_length *expansion_factor
                read_mem swap 1 pop         // _ *proof_stream *fri_verify domain_length expansion_factor

                swap 1 div_mod pop          // _ *proof_stream *fri_verify first_round_code_dimension
                log_2_floor                 // _ *proof_stream *fri_verify max_num_rounds

                dup 1 {&num_colinearity_checks}
                read_mem swap 1 pop         // _ *proof_stream *fri_verify max_num_rounds num_colinearity_checks

                log_2_floor push 1 add      // _ *proof_stream *fri_verify max_num_rounds num_rounds_checking_most_locations

                dup 1 dup 1 lt              // _ *proof_stream *fri_verify max_num_rounds num_rounds_checking_most_locations num_rounds_checking_most_locations<max_num_rounds
                swap 2 push -1 mul add      // _ *proof_stream *fri_verify num_rounds_checking_most_locations<max_num_rounds num_rounds_checking_most_locations-max_num_rounds
                mul push -1 mul             // _ *proof_stream *fri_verify if(num_rounds_checking_most_locations<max_num_rounds){max_num_rounds-num_rounds_checking_most_locations}else{0}
                                            // _ *proof_stream *fri_verify num_rounds

                // calculate max degree of last round
                dup 1 {&domain_length}      // _ *proof_stream *fri_verify num_rounds *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds domain_length

                dup 2 {&expansion_factor}   // _ *proof_stream *fri_verify num_rounds domain_length *expansion_factor
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds domain_length expansion_factor

                swap 1 div_mod pop          // _ *proof_stream *fri_verify num_rounds first_round_code_dimension

                push 2 dup 2 swap 1 pow     // _ *proof_stream *fri_verify num_rounds first_round_code_dimension (1<<num_rounds)

                swap 1 div_mod pop          // _ *proof_stream *fri_verify num_rounds first_round_code_dimension>>num_rounds
                push -1 add                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree

                // COMMIT PHASE

                // create lists for roots and alphas
                dup 1 push 1 add
                call {new_list_of_digests}  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *roots
                dup 2
                call {new_list_xfe}         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *roots *alphas

                // dequeue first Merkle root
                swap 1                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots
                dup 5                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *proof_stream
                call {proof_stream_dequeue} // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *proof_stream *root_ev
                swap 1 pop                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *root_ev
                push 1 add                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *root
                dup 1 swap 1                // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *roots *root

                {&read_digest}              // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots *roots [root]

                call {push_digest_to_list}  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots

                // dequeue remaining roots and collect Fiat-Shamir challenges
                dup 3                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *alphas *roots num_rounds
                swap 2                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree num_rounds *roots *alphas
                call {dequeue_query_phase}  // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas

                // dequeue last codeword
                dup 6                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_stream
                call {proof_stream_dequeue} // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_stream *last_codeword_ev
                push 1 add                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_stream *last_codeword

                // clone last codeword for later use
                dup 0                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_stream *last_codeword *last_codeword
                call {duplicate_list_xfe}   // _ *proof_stream *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_stream *last_codeword *last_codeword'
                swap 5 pop                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword

                // compute Merkle root
                dup 0                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *last_codeword
                call {map_convert_xfe_to_digest}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs
                dup 0                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs *leafs
                call {length_of_list_of_digests}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs
                dup 9 dup 9                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs *fri_verify num_rounds
                swap 1                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs num_rounds *fri_verify
                {&domain_length}            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs num_rounds *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs num_rounds domain_length
                swap 1 push 2               // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs domain_length num_rounds 2
                pow                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs domain_length (1<<num_rounds)
                swap 1 div_mod pop          // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs (domain_length>>num_rounds)
                dup 1 eq                    // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs eq
                assert                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs num_leafs
                push 0 swap 1               // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *leafs 0 num_leafs
                call {merkle_root}          // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root]

                // check against last root dequeued
                dup 8                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] *roots
                dup 0                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] *roots *roots
                call {length_of_list_of_digests}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] *roots num_roots
                push -1 add                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] *roots num_roots-1
                call {get_digest}           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] [roots[-1]]
                assert_vector               // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root] [roots[-1]]

                // clean up top of stack
                pop pop pop pop pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword [last_root]
                pop pop pop pop pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword

                // get omega
                dup 7                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *fri_verify
                {&domain_generator}         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *generator
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword domain_generator
                dup 7                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword domain_generator num_rounds
                push 2 pow                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword domain_generator 1<<num_rounds
                swap 1 pow                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword domain_generator^(num_rounds)

                // compute intt (without scaling)
                invert                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword omega_inv
                dup 1 swap 1                // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_codeword *last_codeword omega_inv
                call {xfe_ntt}              // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_polynomial

                // // If the static allocator is not managed correctly, then the above call
                // // to xfe_ntt modifies the first element of *roots. Let's try and see!
                // dup 3 push 0 call {get_digest}
                // push 8888 assert

                // test low degree of polynomial
                dup 5 push 1 add            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_polynomial num_nonzero_coefficients

                call {assert_tail_xfe0}     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *last_polynomial *total_num_coefficients
                pop pop                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream

                // QUERY PHASE

                // get "A" indices and verify membership

                // get index count
                dup 6                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *fri_verify
                {&num_colinearity_checks}   // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *num_indices
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream num_indices

                // get domain length
                dup 7                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream num_indices *fri_verify
                {&domain_length}            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream num_indices *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream num_indices domain_length

                // sample "A" indices
                call {sample_indices}       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *indices

                // get largest tree height
                dup 7                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *indices *fri_verify
                {&domain_length}            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *indices *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *indices domain_length
                log_2_floor                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *proof_stream *indices tree_height

                // dequeue proof item as fri response
                swap 2                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *proof_stream
                call {proof_stream_dequeue} // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *proof_stream *proof_item
                swap 1 pop                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *proof_item
                push 1 add                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *fri_response

                // assert correct length of number of leafs
                {&revealed_leafs}           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements
                dup 1 dup 1                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements
                call {length_of_list_of_xfes}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices num_leafs
                swap 1                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements num_leafs *indices
                call {length_of_list_of_u32s}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements num_leafs num_indices
                eq assert                   // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements
                dup 1 dup 1                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements

                // check batch merkle membership
                call {map_convert_xfe_to_digest}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *revealed_leafs_as_digests
                swap 1                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_leafs_as_digests *indices
                call {zip_digests_indices}  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices
                dup 5 push 0                // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices *roots 0
                call {get_digest}           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]]
                dup 8                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]] tree_height

                call {verify_authentication_paths_for_leaf_and_index_list}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]] tree_height
                pop pop pop pop pop pop pop // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements

                // prepare the return value:
                // the list of opened indices and elements
                dup 1 dup 1                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements
                call {zip_index_xfe}        // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs
                // zip allocates a new unsafe list, which we want to be twice as long
                // (the second half will be populated in the first iteration of the main loop below)
                read_mem                    // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs length
                push 4 mul                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size
                push 0 read_mem             // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size *alloc alloc
                swap 1 swap 2 add           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *alloc size+alloc
                write_mem pop               // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs

                // prepare for query phase main loop
                dup 9                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *fri_verify
                {&domain_length}            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *domain_length
                read_mem swap 1 pop         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs domain_length
                // rename stack elements    // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length
                push 0 // (=r)              // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r

                call {query_phase_main_loop}// _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds

                // verify membership of C elements (here called A elements) in last codeword
                dup 8 dup 5 dup 5           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices *c_elements
                call {zip_index_xfe}        // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices_and_elements
                call {map_assert_membership}// _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices_and_elements
                pop pop                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices_and_elements

                // clean up stack and return
                pop pop                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs
                swap 9                      // _ *proof_stream *revealed_indices_and_leafs num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *fri_verify
                pop pop pop pop pop         // _ *proof_stream *revealed_indices_and_leafs *fri_verify num_rounds last_round_max_degree 0 *roots
                pop pop pop pop             // _ *proof_stream *revealed_indices_and_leafs

                return

            // BEFORE:     _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length 0
            // AFTER:      _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds
            // INVARIANT:  _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r
            {query_phase_main_loop}:
                // test termination condition:
                // if r == num_rounds then return
                dup 10 dup 1
                eq                          // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r num_rounds==r
                skiz return                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r

                // get "B" indices
                push 2 dup 2
                div_mod pop dup 5           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *a_indices
                call {map_add_half_domain_length}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices

                // dequeue fri response and get "B" elements
                dup 14 call {proof_stream_dequeue}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *proof_stream *fri_response_ev
                swap 1 pop push 1 add       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *fri_response
                {&revealed_leafs}           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements

                // if in first round (r==0), populate second half of return vector
                dup 3 push 0 eq
                skiz call {populate_return_vector_second_half}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements

                // check batch merkle membership
                dup 0 call {map_convert_xfe_to_digest}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leafs
                dup 2 call {zip_digests_indices}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices
                dup 11 dup 5                // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices *roots r
                call {get_digest}           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]]
                dup 14                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]] current_tree_height
                call {verify_authentication_paths_for_leaf_and_index_list}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]] current_tree_height
                pop pop pop pop pop pop pop // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements

                // pull *fri_verify to top because needed
                dup 14                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify
                // update tree height
                dup 9 push -1 add           // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1

                // reduce modulo N/2 to get C indices
                dup 4 dup 4                 // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *b_indices
                call {map_reduce_indices}   // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices

                // compute C elements
                dup 0                       // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_indices
                call {length_of_list_of_u32s}
                                            // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices length
                call {new_list_xfe}         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements
                push 0                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements 0
                call {compute_c_values_loop}// _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements length

                // return stack to invariant and keep books for next iteration
                pop                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements
                swap 11                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *a_elements
                pop                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices
                swap 11                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *a_indices
                pop                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length
                swap 7                      // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 current_domain_length
                pop                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1
                swap 10                     // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height
                pop                         // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify
                pop pop pop pop             // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r
                push 1 add                  // _ *proof_stream *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r+1
                recurse

            // BEFORE: _  *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
            // AFTER:  _  *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
            {populate_return_vector_second_half}:
                dup 1 dup 1 call {zip_index_xfe}
                                            // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements
                push 0 call {populate_loop} // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements length
                pop pop                     // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
                return

            // INVARIANT:  _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index
            {populate_loop}:
                // evaluate termination condition:
                // if index == list length, then return
                dup 0                       // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index index
                dup 4 call {length_of_list_of_u32s} eq
                                            // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index index==length
                skiz return

                // prepare push
                dup 7                       // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index *revealed_indices_and_leafs

                // read element
                dup 2 dup 2                 // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index *revealed_indices_and_leafs *indices_and_elements index
                call {get_u32_and_xfe}      // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index *revealed_indices_and_leafs [index-and-element]

                // push
                call {push_u32_and_xfe}     // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index

                // prepare for next iteration
                push 1 add              // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements index+1

                recurse
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

        let mut stack = empty_stack();
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let static_memory_offset = 2; // xfe_ntt spills two words
        memory.insert(
            BFieldElement::new(DYN_MALLOC_ADDRESS as u64),
            BFieldElement::new(static_memory_offset + 1),
        );
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
    use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
    use triton_vm::{
        arithmetic_domain::ArithmeticDomain, fri::Fri, proof_stream::ProofStream, BFieldElement,
    };
    use twenty_first::{
        shared_math::{traits::PrimitiveRootOfUnity, x_field_element::XFieldElement},
        util_types::{
            algebraic_hasher::Domain,
            merkle_tree::{CpuParallel, MerkleTree},
            merkle_tree_maker::MerkleTreeMaker,
        },
    };

    use crate::{
        procedure::{Procedure, ShadowedProcedure},
        structure::tasm_object::TasmObject,
        test_helpers::{
            rust_final_state, tasm_final_state, verify_sponge_equivalence, verify_stack_growth,
        },
        Digest, VmHasher, VmHasherState,
    };

    use super::FriVerify;

    #[test]
    fn fri_derived_params_match() {
        let mut rng = thread_rng();
        for _ in 0..20 {
            let expansion_factor = 2 << rng.gen_range(0..10);
            let colinearity_checks_count = rng.gen_range(1..320);
            let offset: BFieldElement = rng.gen();
            let domain_length = expansion_factor * (1u32 << rng.gen_range(0..20));

            let fri_verify = FriVerify::new(
                offset,
                domain_length,
                expansion_factor,
                colinearity_checks_count,
            );

            let fri = Fri::<VmHasher>::new(
                ArithmeticDomain {
                    offset,
                    generator: BFieldElement::primitive_root_of_unity(domain_length as u64)
                        .unwrap(),
                    length: domain_length as usize,
                },
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
            ArithmeticDomain {
                offset,
                generator: BFieldElement::primitive_root_of_unity(domain_length as u64).unwrap(),
                length: domain_length as usize,
            },
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

    #[test]
    fn test_shadow() {
        let seed: [u8; 32] = thread_rng().gen();
        //  = [
        //     0xf7, 0x41, 0x2a, 0x3e, 0x1e, 0xa7, 0x86, 0xf6, 0xf3, 0x55, 0xdb, 0xcc, 0xe0, 0x32,
        //     0xf3, 0xec, 0x6f, 0x51, 0x26, 0xcb, 0xb2, 0x7c, 0x4a, 0x34, 0xb4, 0xc9, 0xe9, 0xa8,
        //     0x7c, 0x34, 0x11, 0xc5,
        // ];
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let expansion_factor = 1 << rng.gen_range(1..5);
        let domain_length = expansion_factor * (1 << rng.gen_range(8..15));
        let offset = BFieldElement::new(7);
        let num_colinearity_checks = 20; //rng.gen_range(1..min(160, domain_length / 4));
        println!("number of colinearity checks: {num_colinearity_checks}");
        println!("expansion factor: {expansion_factor}");
        let procedure = FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            num_colinearity_checks,
        );

        // ShadowedProcedure::new(procedure).test();
        // Unfortunately, we cannot call the built-in test that comes with anything that
        // impements Procedure because that test checks that the rust and tasm stacks are
        // left in identical states. They both contain a pointer to the same object, but
        // this object lives in a different (and difficult to predict) location in memory.
        // So we check that instead.

        let (stack, memory, nondeterminism, stdin, sponge_state) =
            procedure.pseudorandom_initial_state(seed, None);

        let init_stack = stack.to_vec();
        let words_statically_allocated = 0;
        let shadowed_procedure = ShadowedProcedure::new(procedure);

        let rust = rust_final_state(
            &shadowed_procedure,
            &stack,
            &stdin,
            &nondeterminism,
            &memory,
            &sponge_state,
            words_statically_allocated,
        );

        // run tvm
        let tasm = tasm_final_state(
            &shadowed_procedure,
            &stack,
            &stdin,
            &nondeterminism,
            &memory,
            &sponge_state,
            words_statically_allocated,
        );

        assert_eq!(
            rust.output, tasm.output,
            "Rust shadowing and VM std out must agree"
        );

        verify_stack_growth(&shadowed_procedure, &init_stack, &tasm.final_stack);
        verify_sponge_equivalence(&rust.final_sponge_state, &tasm.final_sponge_state);

        // load the "revealed_indices_and_elements" object from rust and tasm memory and
        // test their equivalence
        let rust_address = rust.final_stack.last().unwrap();
        let rust_object =
            Vec::<(u32, XFieldElement)>::decode_from_memory(&rust.final_ram, *rust_address);
        let tasm_address = tasm.final_stack.last().unwrap();
        let tasm_object =
            Vec::<(u32, XFieldElement)>::decode_from_memory(&tasm.final_ram, *tasm_address);
        rust_object
            .iter()
            .zip(tasm_object.iter())
            .for_each(|(r, t)| {
                assert_eq!(r, t, "returned lists of indices and leafs do not match")
            });
    }
}

#[cfg(test)]
mod bench {
    use triton_vm::BFieldElement;

    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    use super::FriVerify;

    #[test]
    fn bench() {
        let expansion_factor = 2;
        let domain_length = expansion_factor * 2;
        let offset = BFieldElement::new(7);
        let num_colinearity_checks = 2;
        // tiny parameters for FRI yes, but the bench framework is awful atm
        let procedure = FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            num_colinearity_checks,
        );
        ShadowedProcedure::new(procedure).bench();
    }
}
