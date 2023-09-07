use crate::{hashing::merkle_root::MerkleRoot, structure::tasm_object::TasmObject, Digest};
use triton_vm::{
    arithmetic_domain::ArithmeticDomain, instruction::LabelledInstruction, BFieldElement,
};
use twenty_first::shared_math::{
    ntt::intt, other::log_2_ceil, polynomial::Polynomial, traits::ModPowU32,
    x_field_element::XFieldElement,
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
pub struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub colinearity_checks_count: u32,
    pub domain: ArithmeticDomain,
}

impl FriVerify {
    pub fn new(
        offset: BFieldElement,
        domain_length: u32,
        expansion_factor: u32,
        colinearity_checks_count: u32,
    ) -> Self {
        let domain = ArithmeticDomain::of_length(domain_length as usize).with_offset(offset);
        Self {
            domain,
            expansion_factor,
            colinearity_checks_count,
        }
    }

    /// Computes the number of rounds and the max. degree of the last codeword
    fn num_rounds(&self) -> (u8, u32) {
        let max_degree = (self.domain.length / self.expansion_factor as usize) - 1;
        let mut rounds_count = log_2_ceil(max_degree as u128 + 1) as u8;
        let mut max_degree_of_last_round = 0u32;
        if self.expansion_factor < self.colinearity_checks_count {
            let num_missed_rounds = log_2_ceil(
                (self.colinearity_checks_count as f64 / self.expansion_factor as f64).ceil()
                    as u128,
            ) as u8;
            rounds_count -= num_missed_rounds;
            max_degree_of_last_round = 2u32.pow(num_missed_rounds as u32) - 1;
        }

        (rounds_count, max_degree_of_last_round)
    }

    /// Compute a new list containing the `XFieldElement`s of the given list, but lifted
    /// to the type `Digest` via padding with 2 zeros.
    /// TODO: write a snippet for me.
    fn map_convert_xfe_to_digest(xfes: &[XFieldElement]) -> Vec<Digest> {
        xfes.iter().map(|x| (*x).into()).collect()
    }
}

impl BasicSnippet for FriVerify {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*proof_stream".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::U32)),
                "indices".to_string(),
            ),
            (DataType::List(Box::new(DataType::XFE)), "leafs".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_fri_verify".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        todo!()
    }
}

impl Procedure for FriVerify {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        _public_input: &[triton_vm::BFieldElement],
        sponge_state: &mut crate::VmHasherState,
    ) -> Vec<triton_vm::BFieldElement> {
        let (num_rounds, max_degree_of_last_round) = self.num_rounds();
        let num_rounds = num_rounds as usize;

        // read proof stream
        let proof_stream_pointer = stack.pop().unwrap();
        let mut proof_stream: VmProofStream =
            *VmProofStream::decode_from_memory(memory, proof_stream_pointer).unwrap();

        proof_stream.sponge_state = sponge_state.clone();

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
        assert_ne!(*last_root, last_codeword_merkle_root);

        // Verify that last codeword is of sufficiently low degree

        // Compute interpolant to get the degree of the last codeword.
        // Note that we don't have to scale the polynomial back to the trace subgroup since we
        // only check its degree and don't use it further.
        let log_2_of_n = last_codeword.len().ilog2(); // <-- snippet me
        let mut last_polynomial = last_codeword;

        let last_fri_domain_generator = self
            .domain
            .generator
            .mod_pow_u32(2u32.pow(num_rounds as u32)); // <-- snippet me
        intt::<XFieldElement>(&mut last_polynomial, last_fri_domain_generator, log_2_of_n); // <-- snippet me
        let last_poly_degree = Polynomial::new(last_polynomial).degree(); // <-- snippet me

        assert!(
            last_poly_degree <= max_degree_of_last_round as isize,
            "FriValidationError::LastIterationTooHighDegree:\n\
            last_poly_degree is {last_poly_degree}, \
                 degree_of_last_round is {max_degree_of_last_round}",
        ); // FriValidationError::LastIterationTooHighDegree

        // // Query phase
        // // query step 0: get "A" indices and verify set membership of corresponding values.
        // let mut a_indices =
        //     proof_stream.sample_indices(self.domain.length, self.colinearity_checks_count);

        // let tree_height = self.domain.length.ilog2() as usize;
        // let mut a_values =
        //     Self::dequeue_and_authenticate(roots[0], tree_height, &a_indices, proof_stream)?;

        // // save indices and revealed leafs of first round's codeword for returning
        // let revealed_indices_and_elements_first_half = a_indices
        //     .iter()
        //     .copied()
        //     .zip_eq(a_values.iter().copied())
        //     .collect_vec();
        // // these indices and values will be computed in the first iteration of the main loop below
        // let mut revealed_indices_and_elements_second_half = vec![];

        // // set up "B" for offsetting inside loop.  Note that "B" and "A" indices can be calcuated
        // // from each other.
        // let mut b_indices = a_indices.clone();
        // let mut current_domain_len = self.domain.length;
        // let mut current_tree_height = tree_height;

        // // query step 1:  loop over FRI rounds, verify "B"s, compute values for "C"s
        // for r in 0..num_rounds {
        //     // get "B" indices and verify set membership of corresponding values
        //     b_indices = b_indices
        //         .iter()
        //         .map(|x| (x + current_domain_len / 2) % current_domain_len)
        //         .collect();
        //     let b_values = Self::dequeue_and_authenticate(
        //         roots[r],
        //         current_tree_height,
        //         &b_indices,
        //         proof_stream,
        //     )?;
        //     debug_assert_eq!(self.colinearity_checks_count, a_indices.len());
        //     debug_assert_eq!(self.colinearity_checks_count, b_indices.len());
        //     debug_assert_eq!(self.colinearity_checks_count, a_values.len());
        //     debug_assert_eq!(self.colinearity_checks_count, b_values.len());

        //     if r == 0 {
        //         // save other half of indices and revealed leafs of first round for returning
        //         revealed_indices_and_elements_second_half = b_indices
        //             .iter()
        //             .copied()
        //             .zip_eq(b_values.iter().copied())
        //             .collect_vec();
        //     }

        //     // compute "C" indices and values for next round from "A" and "B" of current round
        //     current_domain_len /= 2;
        //     current_tree_height -= 1;
        //     let c_indices = a_indices.iter().map(|x| x % current_domain_len).collect();
        //     let c_values = (0..self.colinearity_checks_count)
        //         .into_par_iter()
        //         .map(|i| {
        //             Polynomial::<XFieldElement>::get_colinear_y(
        //                 (self.get_evaluation_argument(a_indices[i], r), a_values[i]),
        //                 (self.get_evaluation_argument(b_indices[i], r), b_values[i]),
        //                 alphas[r],
        //             )
        //         })
        //         .collect();

        //     // next rounds "A"s correspond to current rounds "C"s
        //     a_indices = c_indices;
        //     a_values = c_values;
        // }

        // Finally compare "C" values (which are named "A" values in this enclosing scope) with
        // last codeword from the proofstream.
        // a_indices = a_indices.iter().map(|x| x % current_domain_len).collect();
        // if (0..self.colinearity_checks_count).any(|i| last_codeword[a_indices[i]] != a_values[i]) {
        //     bail!(FriValidationError::MismatchingLastCodeword);
        // }

        // let revealed_indices_and_elements = revealed_indices_and_elements_first_half
        //     .into_iter()
        //     .chain(revealed_indices_and_elements_second_half.into_iter())
        //     .collect_vec();
        // revealed_indices_and_elements

        // put stack in order
        // return sponge state
        *sponge_state = proof_stream.sponge_state;

        todo!()
    }

    fn pseudorandom_initial_state(
        &self,
        _seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        Vec<triton_vm::BFieldElement>,
        crate::VmHasherState,
    ) {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use rand::{thread_rng, Rng, RngCore};
    use triton_vm::BFieldElement;

    use super::FriVerify;

    #[test]
    fn num_rounds_match() {
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

        let (num_rounds, last_max_degree) = fri_verify.num_rounds();
        assert_eq!(
            ((last_max_degree + 1) << num_rounds) * expansion_factor,
            domain_length
        );

        // fri.num_rounds() is private
        // assert_eq!(fri_verify.num_rounds(), fri.num_rounds());
    }
}
