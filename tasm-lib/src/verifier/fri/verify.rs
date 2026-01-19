use anyhow::bail;
use itertools::Itertools;
use num::Zero;
use triton_vm::arithmetic_domain::ArithmeticDomain;
use triton_vm::error::FriValidationError;
use triton_vm::fri::Fri;
use triton_vm::prelude::*;
use triton_vm::proof_item::FriResponse;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::proof_stream::ProofStream;
use twenty_first::math::polynomial::Polynomial;
use twenty_first::math::polynomial::barycentric_evaluate;
use twenty_first::math::traits::ModPowU32;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::util_types::merkle_tree::MerkleTree;
use twenty_first::util_types::merkle_tree::MerkleTreeInclusionProof;

use crate::U32_TO_USIZE_ERR;
use crate::data_type::StructType;
use crate::field;
use crate::hashing::algebraic_hasher::sample_indices::SampleIndices;
use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::hashing::merkle_root_from_xfes::MerkleRootFromXfes;
use crate::list::get::Get;
use crate::list::higher_order::inner_function::InnerFunction;
use crate::list::higher_order::inner_function::RawCode;
use crate::list::higher_order::map::Map;
use crate::list::higher_order::zip::Zip;
use crate::list::horner_evaluation_dynamic_length::HornerEvaluationDynamicLength;
use crate::list::length::Length;
use crate::list::new::New;
use crate::list::push::Push;
use crate::prelude::*;
use crate::verifier::fri::barycentric_evaluation::BarycentricEvaluation;
use crate::verifier::fri::number_of_rounds::NumberOfRounds;
use crate::verifier::fri::verify_fri_authentication_paths::VerifyFriAuthenticationPaths;
use crate::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::verifier::vm_proof_iter::shared::vm_proof_iter_type;

/// FRI verification of a Reed-Solomon codeword.
///
/// `FriVerify` checks that a Reed-Solomon codeword, provided as an oracle, has a low
/// degree interpolant. Specifically, the algorithm takes a `ProofStream` object, runs the
/// verifier of the FRI protocol, and (if successful) returns the lists of indices at
/// which the codeword is probed along with the values of the codeword at those indices.
/// The test succeeds with probability 1 if the codeword is of low degree; and with
/// probability *soundness error* if the codeword is far from low-degree. If the test is
/// not successful, the VM crashes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, BFieldCodec, TasmObject)]
pub struct FriVerify {
    // expansion factor = 1 / rate
    pub expansion_factor: u32,
    pub num_collinearity_checks: u32,
    pub domain_length: u32,
    pub domain_offset: BFieldElement,
    pub domain_generator: BFieldElement,
}

impl From<Fri> for FriVerify {
    fn from(fri: Fri) -> Self {
        Self {
            domain_generator: fri.domain.generator(),

            // This runtime type-conversion prevents a FRI domain of length 2^32 from being created.
            domain_length: fri.domain.len().try_into().unwrap(),
            domain_offset: fri.domain.offset(),
            expansion_factor: fri.expansion_factor.try_into().unwrap(),
            num_collinearity_checks: fri.num_collinearity_checks.try_into().unwrap(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FriSnippet {
    #[cfg(test)]
    pub(crate) test_instance: FriVerify,
}

pub(super) fn fri_verify_type() -> StructType {
    let name = "FriVerify".to_string();
    let fields = vec![
        ("expansion_factor".to_string(), DataType::U32),
        ("num_collinearity_checks".to_string(), DataType::U32),
        ("domain_length".to_string(), DataType::U32),
        ("domain_offset".to_string(), DataType::Bfe),
        ("domain_generator".to_string(), DataType::Bfe),
    ];

    StructType { name, fields }
}

impl FriSnippet {
    pub(crate) fn indexed_leafs_list_type() -> DataType {
        let indexed_leaf_type = DataType::Tuple(vec![DataType::U32, DataType::Xfe]);
        DataType::List(Box::new(indexed_leaf_type))
    }
}

impl BasicSnippet for FriSnippet {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let proof_iter_ref = DataType::StructRef(vm_proof_iter_type());
        let argument_0 = (proof_iter_ref, "*vm_proof_iter".to_string());

        let fri_verify_ref = DataType::StructRef(fri_verify_type());
        let argument_1 = (fri_verify_ref, "*fri_verify".to_string());

        vec![argument_0, argument_1]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            Self::indexed_leafs_list_type(),
            "indices_and_elements".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_verify".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let fri_num_rounds = library.import(Box::new(NumberOfRounds {}));
        let domain_length = field!(FriVerify::domain_length);
        let domain_generator = field!(FriVerify::domain_generator);
        let domain_offset = field!(FriVerify::domain_offset);
        let expansion_factor = field!(FriVerify::expansion_factor);
        let num_collinearity_checks = field!(FriVerify::num_collinearity_checks);
        let new_list = library.import(Box::new(New));
        let push_digest_to_list = library.import(Box::new(Push::new(DataType::Digest)));
        let read_digest = triton_asm!(
                                // _ *digest
            push 4 add          // _ *digest+4
            read_mem 5 pop 1    // _ [digest; 5]
        );
        let read_xfe = triton_asm!(
            addi {EXTENSION_DEGREE - 1} read_mem {EXTENSION_DEGREE} pop 1
        );
        let push_scalar = library.import(Box::new(Push::new(DataType::Xfe)));

        let vm_proof_iter_dequeue_next_as_merkle_root =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::MerkleRoot)));
        let vm_proof_iter_dequeue_next_as_fri_codeword =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::FriCodeword)));
        let vm_proof_iter_dequeue_next_as_fri_polynomial = library.import(Box::new(
            DequeueNextAs::new(ProofItemVariant::FriPolynomial),
        ));
        let polynomial_evaluation = library.import(Box::new(HornerEvaluationDynamicLength));
        let barycentric_evaluation = library.import(Box::new(BarycentricEvaluation));
        let vm_proof_iter_dequeue_next_as_fri_response =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::FriResponse)));

        let vm_proof_iter_sample_one_scalar =
            library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
                num_elements: 1,
            }));
        let dequeue_commit_phase = format!("{entrypoint}_dequeue_commit_phase_remainder");
        let length_of_list = library.import(Box::new(Length));
        let merkle_root_from_xfes = library.import(Box::new(MerkleRootFromXfes));
        let get_xfe_from_list = library.import(Box::new(Get::new(DataType::Xfe)));
        let get_digest_from_list = library.import(Box::new(Get::new(DataType::Digest)));
        let sample_indices = library.import(Box::new(SampleIndices));
        let verify_authentication_paths_for_leaf_and_index_list =
            library.import(Box::new(VerifyFriAuthenticationPaths));
        let zip_index_xfe = library.import(Box::new(Zip::new(DataType::U32, DataType::Xfe)));
        let query_phase_main_loop = format!("{entrypoint}_query_phase_main_loop");
        let reduce_indices_label = format!("{entrypoint}_reduce_indices");
        let map_buffer_len = Map::NUM_INTERNAL_REGISTERS;
        let map_reduce_indices =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                function: triton_asm! {
                    {reduce_indices_label}:
                                        // _ half_domain_length [bu ff er] index
                    dup {map_buffer_len + 1}
                    place 1             // _ half_domain_length [bu ff er] half_domain_length index
                    div_mod             // _ half_domain_length [bu ff er] q r
                    pick 1 pop 1        // _ half_domain_length [bu ff er] index%half_domain_length
                    return
                },
                input_type: DataType::U32,
                output_type: DataType::U32,
            }))));
        let compute_c_values_loop = format!("{entrypoint}_compute_c_values_loop");
        let assert_membership_label = format!("{entrypoint}_assert_codeword_membership");
        let map_assert_membership =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                input_type: DataType::Tuple(vec![DataType::U32, DataType::Xfe]),
                output_type: DataType::Tuple(vec![DataType::U32, DataType::Xfe]),
                function: triton_asm! {
                    // BEFORE: _ *codeword [bu ff er] index xfe2 xfe1 xfe0
                    // AFTER:  _ *codeword [bu ff er] index xfe2 xfe1 xfe0
                    {assert_membership_label}:
                        hint element_to_check: Xfe = stack[0..3]
                        hint codeword_index = stack[3]
                        hint codeword: Pointer = stack[8]
                        push 0                  // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0
                        dup 4 dup {map_buffer_len + 6}
                        dup 1                   // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index *codeword index
                        call {get_xfe_from_list}// _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index xfe2' xfe1' xfe0'
                        push 0                  // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index xfe2' xfe1' xfe0' 0
                        assert_vector           // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0
                        pop 1                   // _ *codeword [bu ff er] index xfe2 xfe1 xfe0
                        return
                },
            }))));

        let verify_a_values_authentication_paths_against_input_codeword = triton_asm!(
            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs

            dup 3
            push -1
            add
            // _ ... (dom_len - 1)

            dup 4
            // _ ... (dom_len - 1) dom_len

            dup 3
            // _ ... (dom_len - 1) dom_len *a_elements

            dup 3
            push {EXTENSION_DEGREE}
            mul
            add
            // _ ... (dom_len - 1) dom_len *a_elements_last_word

            dup 5
            // _ ... (dom_len - 1) dom_len *a_elements_last_word *idx

            dup 0
            dup 5
            add
            // _ ... (dom_len - 1) dom_len *a_elements_last_word *idx *idx_last

            dup 10
            push 0
            call {get_digest_from_list}
            // _ ... (dom_len - 1) dom_len *a_elements_last_word *idx *idx_last [root]

            call {verify_authentication_paths_for_leaf_and_index_list}
            // _ ...

            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs
        );

        let verify_b_values_authentication_paths_in_main_loop = triton_asm!(
            /*Verify the authentication paths for the b-elements read from the proof stream */

            // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx_last *a_last *b_last

            dup 11
            push -1
            add
            // _ ... (dom_len - 1)

            dup 12
            dup 12
            add
            // _ ... (dom_len - 1) (dom_len + half_dom_len)

            dup 2
            // _ ... (dom_len - 1) (dom_len + half_dom_len) *b_last

            dup 6
            dup 6
            // _ ... (dom_len - 1) (dom_len + half_dom_len) *b_last *idx *idx_last

            dup 14
            dup 13
            // _ ... (dom_len - 1) (dom_len + half_dom_len) *b_last *idx *idx_last *roots r

            call {get_digest_from_list}
            // _ ... (dom_len - 1) (dom_len + half_dom_len) *b_last *idx *idx_last [root]

            call {verify_authentication_paths_for_leaf_and_index_list}
            // _ ...

            // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx_last *a_last *b_last
        );

        triton_asm! {
            // BEFORE: _ *proof_iter *fri_verify
            // AFTER:  _ *indices_and_leafs
            {entrypoint}:
                hint fri_verify_pointer = stack[0]
                hint proof_iter_pointer = stack[1]

                // calculate number of rounds
                dup 0                       // _ *vm_proof_iter *fri_verify *fri_verify
                call {fri_num_rounds}       // _ *vm_proof_iter *fri_verify num_rounds
                hint num_rounds = stack[0]

                // calculate max degree of last round
                dup 1 {&domain_length}      // _ *vm_proof_iter *fri_verify num_rounds *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds domain_length
                hint domain_length = stack[0]

                dup 2 {&expansion_factor}   // _ *vm_proof_iter *fri_verify num_rounds domain_length *expansion_factor
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds domain_length expansion_factor
                hint expansion_factor = stack[0]

                pick 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension

                push 2 dup 2 pick 1 pow     // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension (1<<num_rounds)

                pick 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension>>num_rounds
                push -1 add                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree
                hint last_round_max_degree = stack[0]

                // COMMIT PHASE

                // create lists for roots and alphas
                call {new_list}             // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots
                    hint roots: ListPointer = stack[0]
                call {new_list}             // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas
                    hint folding_challenges: ListPointer = stack[0]

                // dequeue first Merkle root
                dup 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *proof_iter
                call {vm_proof_iter_dequeue_next_as_merkle_root}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *root
                dup 2 place 1               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *roots *root

                {&read_digest}              // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *roots [root]
                call {push_digest_to_list}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas

                // dequeue remaining roots and collect Fiat-Shamir challenges
                dup 3                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas num_rounds
                place 2                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree num_rounds *roots *alphas
                call {dequeue_commit_phase} // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas
                pick 2 pop 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas

                // dequeue last codeword and check length
                dup 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_codeword}
                    hint last_fri_codeword: ListPointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *roots *alphas *last_codeword
                place 2                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas

                // compute Merkle root
                dup 2
                dup 0
                call {length_of_list}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len

                dup 6 dup 8                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len num_rounds *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len num_rounds *domain_length
                read_mem 1
                    hint domain_length = stack[1]
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len num_rounds domain_length
                pick 1 push 2               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len domain_length num_rounds 2
                pow                         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len domain_length (1<<num_rounds)
                pick 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword codeword_len (domain_length>>num_rounds)
                eq                          // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword (codeword_len == (domain_length>>num_rounds))
                assert                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_codeword
                call {merkle_root_from_xfes}// _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root]
                hint merkle_root: Digest = stack[0..5]

                // check against last root dequeued
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root] *roots
                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root] *roots *roots
                call {length_of_list}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root] *roots num_roots
                addi -1                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root] *roots num_roots-1
                call {get_digest_from_list} // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root] [roots[-1]]
                assert_vector               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas [last_root]

                // clean up top of stack
                pop 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas

                // dequeue polynomial
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *vm_proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_polynomial}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial
                hint last_fri_polynomial: ListPointer = stack[0]


                // QUERY PHASE

                // get "A" indices and verify membership

                // get index count
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial *fri_verify
                {&num_collinearity_checks}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial *num_indices
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial num_indices

                // get domain length
                dup 7                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial num_indices *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial num_indices *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial num_indices domain_length

                // sample "A" indices
                call {sample_indices}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *last_polynomial *indices

                // Verify low degree of last polynomial
                pick 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_polynomial
                addi 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs

                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *last_poly_coeffs
                call {length_of_list}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs num_coefficients_received

                dup 6 addi 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs num_coefficients_received num_coefficients_allowed
                lt                          // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs (num_coefficients_received>num_coefficients_allowed)
                push 0 eq                   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs (num_coefficients_received<=num_coefficients_allowed)
                assert                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs

                // check that last polynomial agrees with codeword
                call {vm_proof_iter_sample_one_scalar}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates
                push 0 push 0 dup 3 dup 3   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 *last_poly_coeffs *indeterminates
                {&read_xfe}                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 *last_poly_coeffs [x]
                call {polynomial_evaluation}// _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 [poly(x)]
                push 0 push 0 dup 12 dup 8  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 [poly(x)] 0 0 *last_codeword *indeterminates
                {&read_xfe}                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 [poly(x)] 0 0 *last_codeword [x]
                call {barycentric_evaluation}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *last_poly_coeffs *indeterminates 0 0 [poly(x)] 0 0 [codeword(x)]
                assert_vector
                pop 5 pop 2                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices

                // get domain length
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices *domain_length
                    hint domain_length_pointer: Pointer = stack[0]
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas *indices dom_len
                    hint domain_length = stack[0]
                place 1                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices

                // dequeue proof item as fri response
                dup 8                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_response}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *fri_response

                // assert correct length of number of leafs
                {&field!(FriResponse::revealed_leaves)}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements
                dup 1 dup 1                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *indices *a_elements
                call {length_of_list}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *indices num_leafs
                pick 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs *indices
                call {length_of_list}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs num_indices
                dup 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs num_indices num_leafs
                eq assert                   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs

                /* Verify round-0 authentication paths for a-values */
                {&verify_a_values_authentication_paths_against_input_codeword}
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements num_leafs

                pop 1
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements


                // prepare the return value:
                // the list of opened indices and elements
                dup 1 dup 1                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *indices *a_elements
                call {zip_index_xfe}        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs
                hint indices_and_leafs = stack[0]

                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs

                /* Prepare stack for main-loop */
                dup 4                       // .. *alphas
                dup 10                      // .. *alphas *fri_verify
                {&num_collinearity_checks}  // .. *alphas *num_checks
                read_mem 1 pop 1            // .. *alphas num_checks
                dup 12                      // .. *alphas num_checks *vm_proof_iter
                dup 12                      // .. *alphas num_checks *vm_proof_iter *fri_verify
                {&domain_length}            // .. *alphas num_checks *vm_proof_iter *domain_len
                read_mem 1 pop 1            // .. *alphas num_checks *vm_proof_iter dom_len
                push 2 dup 1 div_mod pop 1  // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len
                dup 10                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots
                dup 15                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots *fri_verify
                dup 15                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots *fri_verify num_rounds
                place 1                     // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds *fri_verify
                push 0 place 1              // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r *fri_verify
                dup 0                       // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r *fri_verify *fri_verify
                {&domain_generator}         // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r *fri_verify *g_0
                read_mem 1 pop 1            // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r *fri_verify g_0
                pick 1                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 *fri_verify
                {&domain_offset}            // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 *domain_offset
                read_mem 1 pop 1            // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0
                dup 8                       // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 num_checks
                call {new_list}             // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 num_checks *c
                write_mem 1                 // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 (*c + 1)
                push -1 add                 // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 *c
                dup 13                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 *c *idx
                dup 0                       // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 *c *idx *idx
                dup 14                      // .. *alphas num_checks *vm_proof_iter dom_len half_dom_len *roots num_rounds r g_0 offset_0 *c *idx *idx *a

                // _ ... *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a
                call {query_phase_main_loop}
                // _ ... *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a

                // _ ... *a_elements *revealed_indices_and_leafs *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a
                place 15 pick 14

                // _ ... *a *revealed_indices_and_leafs *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a_elements
                pop 5
                pop 5
                pop 4
                // _ ... *a *revealed_indices_and_leafs

                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs

                // verify membership of C elements (here called A elements) in last codeword
                dup 6 dup 3                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *indices

                dup 11
                {&domain_length}
                read_mem 1 pop 1
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *indices dom_len_round_0

                dup 11
                push 2
                pow
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *indices dom_len_round_0 (1 << num_rounds)

                pick 1
                div_mod
                pop 1
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *indices (dom_len_round_0 / (1 << num_rounds))
                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *indices dom_len_round_last

                pick 1
                call {map_reduce_indices}
                place 1
                pop 1

                // TODO: Get rid of the next two higher-order function calls
                dup 3                        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *c_indices *c_elements
                call {zip_index_xfe}         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *c_indices_and_elements
                call {map_assert_membership} // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs *last_codeword *c_indices_and_elements

                // clean up stack
                pop 2                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword *roots *alphas dom_len *indices *a_elements *revealed_indices_and_leafs
                place 10 pop 5 pop 5        // _ *revealed_indices_and_leafs

                return

            // START    : _ g_r offset_r *c_last_elem_first_word *idx_end_condition *idx_last_word *a_last_word    *b_last_word    [alphas[r]]
            // INVARIANT: _ g_r offset_r *c[n]_first_word        *idx_end_condition *idx[n]        *a[n]_last_word *b[n]_last_word [alphas[r]]
            {compute_c_values_loop}:
                // Strategy for one iteration:
                // 1. Read `a_y`
                // 2. Calculate `a_x`
                // 3. Read `b_y`
                // 4. Calculate `[a_y- b_y]`, preserve [a_y]
                // 5. Calculate `-b_x = a_x`
                // 6. Calculate `1 / (a_x - b_x)` while preserving `a_x`
                // 7. Calculate `(a_y - b_y) / (a_x - b_x)`
                // 8: Read `[c_x]`
                // 9. Calculate `c_x - a_x`
                // 10. Calculate final `c_y`
                // 11. Write c_y to *c_elem
                hint alpha_r: Xfe          = stack[0..3]
                hint b_values: Pointer     = stack[3]
                hint a_values: Pointer     = stack[4]
                hint idx: Pointer          = stack[5]
                hint idx_end_cond: Pointer = stack[6]
                hint c_values: Pointer     = stack[7]
                hint offset_r: Bfe         = stack[8]
                hint g_r: Bfe              = stack[9]

                // _ g_r offset_r *c *idx_end_condition *idx *a *b [alphas[r]]

                // 1: Read `a_y`
                pick 4
                read_mem {EXTENSION_DEGREE}
                place 7
                // _ g_r offset_r *c *idx_end_condition *idx *a' *b [alphas[r]] [a_y]

                // 2: Calculate `a_x`: $g_r^{a_index} * offset_r$
                pick 8
                read_mem 1
                place 9
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b [alphas[r]] [a_y] a_index

                dup 13
                pow
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b [alphas[r]] [a_y] (g_r**a_index)

                dup 12
                mul
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b [alphas[r]] [a_y] (g_r**a_index * offset_r)
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b [alphas[r]] [a_y] a_x

                // 3: Read `b_y`
                pick 7
                read_mem {EXTENSION_DEGREE}
                place 10
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [b_y]

                // 4: Calculate [a_y - b_y], preserve [a_y]
                push -1
                xb_mul
                dup 6
                dup 6
                dup 6
                xx_add
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [a_y - b_y]

                // 5: Calculate `-b_x = a_x`
                dup 3
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [a_y - b_y] (-b_x)

                // 6: Calculate `1 / (a_x - b_x)`, preserve `a_x`
                dup 4
                add
                invert
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [a_y - b_y] (1/(a_x-b_x))

                // 7: Calculate `(a_y - b_y) / (a_x - b_x)`
                xb_mul
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [(a_y - b_y)/(a_x - b_x)]

                // 8: Read `[c_x] = alphas[r]`
                dup 9
                dup 9
                dup 9
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [(a_y - b_y)/(a_x - b_x)] [alphas[r]]
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] a_x [(a_y - b_y)/(a_x - b_x)] [c_x]

                // 9: Calculate `c_x - a_x`
                pick 6
                push -1
                mul
                add
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] [(a_y - b_y)/(a_x - b_x)] [c_x - a_x]

                // 10. Calculate final `c_y`
                xx_mul
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y] [(a_y - b_y)/(a_x - b_x) * (c_x - a_x)]

                xx_add
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [a_y + (a_y - b_y)/(a_x - b_x) * (c_x - a_x)]
                // _ g_r offset_r *c *idx_end_condition *idx' *a' *b' [alphas[r]] [c_y]

                // 11. Write `c_y` to `*c` list
                pick 10
                write_mem {EXTENSION_DEGREE}
                addi  {-2 * EXTENSION_DEGREE as i32}
                place 7
                // _ g_r offset_r *c' *idx_end_condition *idx' *a' *b' [alphas[r]]

                recurse_or_return

            // BEFORE: _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree | num_rounds *roots *alphas
            // AFTER:  _ ... | 0 *roots *alphas
            {dequeue_commit_phase}:

                // return if done
                dup 2       // _ num_rounds *roots *alphas num_rounds
                push 0 eq   // _ num_rounds *roots *alphas num_rounds==0
                skiz return

                // decrement round number
                pick 2      // _ *alphas *roots num_rounds
                addi -1     // _ *alphas *roots num_rounds-1
                place 2     // _ num_rounds-1 *roots *alphas

                // sample scalar
                call {vm_proof_iter_sample_one_scalar}
                            // _ num_rounds-1 *roots *alphas *scalars
                dup 1       // _ num_rounds-1 *roots *alphas *scalars *alphas
                pick 1      // _ num_rounds-1 *roots *alphas *alphas *scalars
                {&read_xfe}
                            // _ num_rounds-1 *roots *alphas *alphas [scalars[0]]

                call {push_scalar}
                            // _ num_rounds-1 *roots *alphas

                // dequeue Merkle root
                pick 1      // _ num_rounds-1 *alphas *roots
                dup 6       // _ num_rounds-1 *alphas *roots *vm_proof_iter

                            // _ num_rounds-1 *alphas *roots *proof_iter
                call {vm_proof_iter_dequeue_next_as_merkle_root}
                                            // _ num_rounds-1 *alphas *roots *root
                dup 1 pick 1                // _ num_rounds-1 *alphas *roots *roots *root
                {&read_digest}              // _ num_rounds-1 *alphas *roots *roots [root]
                call {push_digest_to_list}  // _ num_rounds-1 *alphas *roots
                place 1                     // _ num_rounds-1 *roots *alphas
                recurse

            // INVARIANT:  _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a
            {query_phase_main_loop}:
                hint a_pointer          = stack[0]
                hint idx_pointer        = stack[1]
                hint idx_pointer        = stack[2]
                hint c_pointer          = stack[3]
                hint offset_r           = stack[4]
                hint g_r                = stack[5]
                hint r                  = stack[6]
                hint num_rounds         = stack[7]
                hint roots_pointer      = stack[8]
                hint half_dom_len       = stack[9]
                hint dom_len            = stack[10]
                hint proof_iter_pointer = stack[11]
                hint num_checks         = stack[12]
                hint alphas_pointer     = stack[13]

                /* Check end condition, r == num_rounds */
                dup 7
                dup 7
                eq
                skiz
                    return
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a

                /* Dequeue FRI response and get "B" elements */
                dup 11
                call {vm_proof_iter_dequeue_next_as_fri_response}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a *fri_response

                {&field!(FriResponse::revealed_leaves)}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a *b_elements
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a *b

                /* Verify that *b_elements has expected length */
                read_mem 1
                addi 1
                place 1
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a *b b_len

                dup 14
                eq
                assert // b_len == num_checks
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a *b

                // TODO: Verify *b_elements against relevant Merkle-root

                // There are three vectors to read from in the C-values calculating loop:
                // `*indices`, `*a_elements`, `*b_elements`.
                // There is one list to write to:
                // `*c_elements`
                // This indicates it is beneficial to traverse the lists from highest to lowest
                // address, giving three of the four pointer updates for free. This requires
                // updating the corresponding pointers to point to their respecive list's last
                // element now.

                /* Update `*idx` to point to last element */
                pick 2
                dup 13
                add
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *a *b *idx_last

                pick 2
                dup 13
                push {EXTENSION_DEGREE}
                mul
                add
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *b *idx_last *a_last

                /* Update `*b_elements` to point to last element */
                pick 2
                dup 13
                push {EXTENSION_DEGREE}
                mul
                add
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx_last *a_last *b_last

                {&verify_b_values_authentication_paths_in_main_loop}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx_last *a_last *b_last

                /* Update `*c` to point to last element */
                pick 4
                dup 13
                push {EXTENSION_DEGREE}
                mul
                add
                addi {-((EXTENSION_DEGREE - 1) as isize)}
                place 4
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c_last *idx *idx_last *a_last *b_last

                /* Put round-challenge on top of stack for the c-values loop */
                dup 14
                dup 8
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c_last *idx *idx_last *a_last *b_last *alphas r

                call {get_xfe_from_list}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c_last *idx *idx_last *a_last *b_last [alphas[r]]

                call {compute_c_values_loop}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r (*c - 2) *idx *idx *a *b [alphas[r]]

                pop {EXTENSION_DEGREE + 1}
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r (*c - 2) *idx *idx *a

                pick 3
                addi {EXTENSION_DEGREE - 1}
                place 3
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r g_r offset_r *c *idx *idx *a

                /* Update round parameters */
                /* Update round index r' = r + 1 */
                pick 6
                addi 1
                place 6
                // _ *alphas num_checks *proof_iter dom_len half_dom_len *roots num_rounds r' g_r offset_r *c *idx *idx *a

                /* dom_len' = half_dom_len */
                pick 10 pop 1
                    hint dom_len = stack[9]
                // _ *alphas num_checks *proof_iter dom_len' *roots num_rounds r' g_r offset_r *c *idx *idx *a

                /* half_dom_len' = half_dom_len / 2 */
                dup 9
                log_2_floor
                addi -1
                push 2 pow
                    hint half_dom_len = stack[0]
                place 9
                // _ *alphas num_checks *proof_iter dom_len' half_dom_len' *roots num_rounds r' g_r offset_r *c *idx *idx *a

                /* g_r' = g_r ** 2 */
                pick 5
                dup 0
                mul
                place 5
                // _ *alphas num_checks *proof_iter dom_len' half_dom_len' *roots num_rounds r' g_r' offset_r *c *idx *idx *a

                /* offset_r' = offset_r ** 2 */
                pick 4
                dup 0
                mul
                place 4
                // _ *alphas num_checks *proof_iter dom_len' half_dom_len' *roots num_rounds r' g_r' offset_r' *c *idx *idx *a

                /* a' = c, overwriting already happened in the c-values loop, but for r zero, this is necessary */
                pop 1
                dup 2
                // _ *alphas num_checks *proof_iter dom_len' half_dom_len' *roots num_rounds r' g_r' offset_r' *c *idx *idx *a'

                recurse
        }
    }
}

impl FriVerify {
    /// Return a dummy FRI verify structure that can be used when an instance is not needed but the
    /// compiler thinks it is. Is probably only needed when the FRI snippet is used in an external
    /// test.
    pub fn dummy() -> Self {
        Self {
            expansion_factor: 0,
            num_collinearity_checks: 0,
            domain_length: 0,
            domain_offset: BFieldElement::zero(),
            domain_generator: BFieldElement::zero(),
        }
    }

    pub fn extract_digests_required_for_proving(&self, proof_stream: &ProofStream) -> Vec<Digest> {
        let mut digests = vec![];
        self.inner_verify(&mut proof_stream.clone(), &mut digests)
            .unwrap();
        digests
    }

    pub fn to_fri(self) -> Fri {
        let fri_domain = ArithmeticDomain::of_length(self.domain_length as usize)
            .unwrap()
            .with_offset(self.domain_offset);
        let maybe_fri = Fri::new(
            fri_domain,
            self.expansion_factor as usize,
            self.num_collinearity_checks as usize,
        );

        maybe_fri.unwrap()
    }

    /// Verify the FRI proof embedded in the proof stream. This function expands the list
    /// `nondeterministic_digests` with the digests of the individual authentication paths
    /// obtained from reduplicating the authentication structures that live in the proof
    /// stream.
    fn inner_verify(
        &self,
        proof_stream: &mut ProofStream,
        nondeterministic_digests: &mut Vec<Digest>,
    ) -> anyhow::Result<Vec<(u32, XFieldElement)>> {
        let mut num_nondeterministic_digests_read = 0;

        // calculate number of rounds
        let num_rounds = self.num_rounds();

        // Extract all roots and calculate alpha based on Fiat-Shamir challenge
        let mut roots = Vec::with_capacity(num_rounds);
        let mut alphas = Vec::with_capacity(num_rounds);

        let first_root = proof_stream
            .dequeue()
            .unwrap()
            .try_into_merkle_root()
            .unwrap();
        roots.push(first_root);

        for _round in 0..num_rounds {
            // get a challenge from the verifier
            let alpha = proof_stream.sample_scalars(1)[0];
            alphas.push(alpha);

            // get a commitment from the prover
            let root = proof_stream
                .dequeue()
                .unwrap()
                .try_into_merkle_root()
                .unwrap();
            roots.push(root);
        }

        // Extract last codeword and last polynomial
        let last_codeword = proof_stream
            .dequeue()
            .unwrap()
            .try_into_fri_codeword()
            .unwrap();
        assert_eq!(
            last_codeword.len(),
            self.domain_length as usize >> self.num_rounds()
        );

        let last_polynomial = proof_stream
            .dequeue()
            .unwrap()
            .try_into_fri_polynomial()
            .unwrap();

        // QUERY PHASE

        // query step 0: get "A" indices and verify set membership of corresponding values.
        let domain_length = self.domain_length as usize;
        let num_collinearity_check = self.num_collinearity_checks as usize;
        let mut a_indices = proof_stream.sample_indices(domain_length, num_collinearity_check);

        let tree_height = self.domain_length.ilog2();
        let fri_response = proof_stream
            .dequeue()
            .unwrap()
            .try_into_fri_response()
            .unwrap();
        assert_eq!(a_indices.len(), fri_response.revealed_leaves.len());
        let mut a_values = fri_response.revealed_leaves;

        let leaf_digests = Self::map_convert_xfe_to_digest(&a_values);
        let indexed_a_leaves = a_indices.iter().copied().zip_eq(leaf_digests).collect_vec();

        // Check if last codeword matches the given root
        let codeword_digests = Self::map_convert_xfe_to_digest(&last_codeword);
        let mt: MerkleTree = MerkleTree::par_new(&codeword_digests).unwrap();
        let last_codeword_merkle_root = mt.root();

        let last_root = roots.last().unwrap();
        if *last_root != last_codeword_merkle_root {
            bail!(FriValidationError::BadMerkleRootForLastCodeword);
        }

        // Verify that last codeword is of sufficiently low degree
        let barycentric_indeterminate = proof_stream.sample_scalars(1)[0];
        let barycentric_evaluation =
            barycentric_evaluate(&last_codeword, barycentric_indeterminate);
        let horner_evaluation = last_polynomial.evaluate(barycentric_indeterminate);

        if barycentric_evaluation != horner_evaluation {
            bail!(FriValidationError::LastRoundPolynomialEvaluationMismatch)
        }

        if last_polynomial.degree() > self.last_round_max_degree() as isize {
            bail!(FriValidationError::LastRoundPolynomialHasTooHighDegree)
        }

        // reduplicate authentication structures if necessary
        if num_nondeterministic_digests_read >= nondeterministic_digests.len() {
            let inclusion_proof = MerkleTreeInclusionProof {
                tree_height,
                indexed_leafs: indexed_a_leaves.clone(),
                authentication_structure: fri_response.auth_structure,
            };

            // sanity check: the authentication structure was valid, right?
            assert!(inclusion_proof.clone().verify(roots[0]));
            let reduplicated_authentication_paths = inclusion_proof.into_authentication_paths()?;
            nondeterministic_digests.extend(
                reduplicated_authentication_paths
                    .into_iter()
                    .rev()
                    .flatten(),
            );
        }

        // verify authentication paths for A leafs
        for indexed_leaf in indexed_a_leaves.iter().rev() {
            let auth_path_end = num_nondeterministic_digests_read
                + usize::try_from(tree_height).expect(U32_TO_USIZE_ERR);
            let authentication_path =
                &nondeterministic_digests[num_nondeterministic_digests_read..auth_path_end];
            num_nondeterministic_digests_read = auth_path_end;
            let inclusion_proof = MerkleTreeInclusionProof {
                tree_height,
                indexed_leafs: vec![*indexed_leaf],
                authentication_structure: authentication_path.to_vec(),
            };
            assert!(inclusion_proof.verify(roots[0]));
        }

        // save indices and revealed leafs of first round's codeword for returning
        let revealed_indices_and_elements_round_0 = a_indices
            .iter()
            .map(|&idx| idx as u32)
            .zip_eq(a_values.iter().copied())
            .collect_vec();

        // set up "B" for offsetting inside loop.  Note that "B" and "A" indices can be calculated
        // from each other.
        let mut b_indices = a_indices.clone();
        let mut current_domain_len = self.domain_length as usize;
        let mut current_tree_height = tree_height;

        // query step 1:  loop over FRI rounds, verify "B"s, compute values for "C"s
        for r in 0..num_rounds {
            // get "B" indices and verify set membership of corresponding values
            b_indices = b_indices
                .iter()
                .map(|x| (x + current_domain_len / 2) % current_domain_len)
                .collect();
            let fri_response = proof_stream
                .dequeue()
                .unwrap()
                .try_into_fri_response()
                .unwrap();
            let b_values = fri_response.revealed_leaves;

            let leaf_digests = Self::map_convert_xfe_to_digest(&b_values);
            let indexed_b_leaves = b_indices.iter().copied().zip_eq(leaf_digests).collect_vec();

            // reduplicate authentication structures if necessary
            if num_nondeterministic_digests_read >= nondeterministic_digests.len() {
                let inclusion_proof = MerkleTreeInclusionProof {
                    tree_height: current_tree_height,
                    indexed_leafs: indexed_b_leaves.clone(),
                    authentication_structure: fri_response.auth_structure,
                };

                // sanity check: the auth structure was valid, right?
                assert!(inclusion_proof.clone().verify(roots[r]));
                let reduplicated_authentication_paths =
                    inclusion_proof.into_authentication_paths()?;
                nondeterministic_digests.extend(
                    reduplicated_authentication_paths
                        .into_iter()
                        .rev()
                        .flatten(),
                );
            }

            // verify authentication paths for B leafs
            for indexed_leaf in indexed_b_leaves.iter().rev() {
                let auth_path_end = num_nondeterministic_digests_read
                    + usize::try_from(current_tree_height).expect(U32_TO_USIZE_ERR);
                let authentication_path =
                    &nondeterministic_digests[num_nondeterministic_digests_read..auth_path_end];
                num_nondeterministic_digests_read = auth_path_end;
                let inclusion_proof = MerkleTreeInclusionProof {
                    tree_height: current_tree_height,
                    indexed_leafs: vec![*indexed_leaf],
                    authentication_structure: authentication_path.to_vec(),
                };
                if !inclusion_proof.verify(roots[r]) {
                    bail!(FriValidationError::BadMerkleAuthenticationPath);
                }
            }

            debug_assert_eq!(self.num_collinearity_checks, a_indices.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, b_indices.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, a_values.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, b_values.len() as u32);

            // compute "C" indices and values for next round from "A" and "B" of current round
            current_domain_len /= 2;
            current_tree_height -= 1;
            let c_indices = a_indices.iter().map(|x| x % current_domain_len).collect();
            let c_values = (0..self.num_collinearity_checks as usize)
                .map(|i| {
                    let a_x = self.get_collinearity_check_x(a_indices[i] as u32, r);
                    let b_x = self.get_collinearity_check_x(b_indices[i] as u32, r);
                    Polynomial::<XFieldElement>::get_colinear_y(
                        (a_x, a_values[i]),
                        (b_x, b_values[i]),
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
        if !(0..self.num_collinearity_checks as usize)
            .all(|i| last_codeword[a_indices[i]] == a_values[i])
        {
            bail!(FriValidationError::LastCodewordMismatch);
        }

        Ok(revealed_indices_and_elements_round_0)
    }

    /// Computes the number of rounds
    pub fn num_rounds(&self) -> usize {
        let first_round_code_dimension = self.first_round_max_degree() + 1;
        let max_num_rounds = first_round_code_dimension.next_power_of_two().ilog2();

        // Skip rounds for which Merkle tree verification cost exceeds arithmetic cost,
        // because more than half the codeword's locations are queried.
        let num_rounds_checking_all_locations = self.num_collinearity_checks.ilog2() as u64;
        let num_rounds_checking_most_locations = num_rounds_checking_all_locations + 1;

        (max_num_rounds as u64).saturating_sub(num_rounds_checking_most_locations) as usize
    }

    /// Computes the max degree of the codeword interpolant after the last round
    pub fn last_round_max_degree(&self) -> usize {
        self.first_round_max_degree() >> self.num_rounds()
    }

    /// Computes the max degree of the very first codeword interpolant
    pub fn first_round_max_degree(&self) -> usize {
        assert!(self.domain_length >= self.expansion_factor);
        (self.domain_length / self.expansion_factor) as usize - 1
    }

    /// Compute a new list containing the `XFieldElement`s of the given list, but lifted
    /// to the type `Digest` via padding with 2 zeros.
    fn map_convert_xfe_to_digest(xfes: &[XFieldElement]) -> Vec<Digest> {
        xfes.iter().map(|x| (*x).into()).collect()
    }

    /// Get the x-coordinate of an A or B point in a collinearity check, given the point's
    /// index and the round number in which the check takes place. In Triton VM, this
    /// method is called `get_evaluation_argument`.
    pub fn get_collinearity_check_x(&self, idx: u32, round: usize) -> XFieldElement {
        let domain_value = self.domain_offset * self.domain_generator.mod_pow_u32(idx);
        let round_exponent = 2u32.pow(round as u32);
        let evaluation_argument = domain_value.mod_pow_u32(round_exponent);

        evaluation_argument.lift()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::panic::catch_unwind;

    use num_traits::Zero;
    use proptest::collection::vec;
    use rayon::prelude::*;
    use test_strategy::proptest;
    use triton_vm::proof_item::ProofItem;
    use twenty_first::math::ntt::ntt;
    use twenty_first::util_types::sponge::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::structure::tasm_object::decode_from_memory_with_size;
    use crate::test_helpers::rust_final_state;
    use crate::test_helpers::tasm_final_state;
    use crate::test_helpers::verify_sponge_equivalence;
    use crate::test_helpers::verify_stack_growth;
    use crate::test_prelude::*;

    impl FriVerify {
        pub fn new(
            offset: BFieldElement,
            domain_length: u32,
            expansion_factor: u32,
            num_collinearity_checks: u32,
        ) -> Self {
            let domain = ArithmeticDomain::of_length(domain_length as usize)
                .unwrap()
                .with_offset(offset);
            Self {
                expansion_factor,
                num_collinearity_checks,
                domain_length,
                domain_offset: domain.offset(),
                domain_generator: domain.generator(),
            }
        }

        pub fn call(
            &self,
            proof_stream: &mut ProofStream,
            nondeterminism: &NonDeterminism,
        ) -> Vec<(u32, XFieldElement)> {
            self.inner_verify(proof_stream, &mut nondeterminism.digests.clone())
                .unwrap()
        }

        /// Generate a proof, embedded in a proof stream.
        pub fn pseudorandom_fri_proof_stream(&self, seed: [u8; 32]) -> ProofStream {
            let max_degree = self.first_round_max_degree();
            let mut rng = StdRng::from_seed(seed);
            let polynomial_coefficients = (0..=max_degree).map(|_| rng.random()).collect_vec();

            let mut codeword = polynomial_coefficients;
            codeword.resize(self.domain_length as usize, XFieldElement::zero());
            ntt(&mut codeword);

            let mut proof_stream = ProofStream::new();
            let fri = self.to_fri();
            fri.prove(&codeword, &mut proof_stream).unwrap();

            ProofStream {
                items: proof_stream.items,
                items_index: 0,
                sponge: Tip5::init(),
            }
        }
    }

    impl FriSnippet {
        /// Test helper – panics if verification fails.
        fn verify_from_proof_with_digests(&self, proof: Vec<BFieldElement>, digests: Vec<Digest>) {
            let items = *Vec::<ProofItem>::decode(&proof).unwrap();
            let proof_stream = ProofStream {
                items,
                items_index: 0,
                sponge: Tip5::init(),
            };
            let (stack, nondeterminism) =
                self.set_up_stack_and_non_determinism_using_digests(proof_stream, digests);

            let shadowed_snippet = ShadowedProcedure::new(self.to_owned());
            let _tasm = tasm_final_state(
                &shadowed_snippet,
                &stack,
                &[],
                nondeterminism,
                &Some(Tip5::init()),
            );
        }

        fn set_up_stack_and_non_determinism(
            &self,
            proof_stream: ProofStream,
        ) -> (Vec<BFieldElement>, NonDeterminism) {
            let digests = self
                .test_instance
                .extract_digests_required_for_proving(&proof_stream);
            self.set_up_stack_and_non_determinism_using_digests(proof_stream, digests)
        }

        fn set_up_stack_and_non_determinism_using_digests(
            &self,
            proof_stream: ProofStream,
            digests: Vec<Digest>,
        ) -> (Vec<BFieldElement>, NonDeterminism) {
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            let vm_proof_iter_pointer = BFieldElement::zero();
            // uses highly specific knowledge about `BFieldCodec`
            let proof_iter_current_item_pointer = vm_proof_iter_pointer + BFieldElement::new(2);

            let fri_verify_pointer =
                encode_to_memory(&mut memory, vm_proof_iter_pointer, &proof_stream);
            let proof_iter_pointer =
                encode_to_memory(&mut memory, fri_verify_pointer, &self.test_instance);
            encode_to_memory(
                &mut memory,
                proof_iter_pointer,
                &proof_iter_current_item_pointer,
            );
            let nondeterminism = NonDeterminism::default()
                .with_ram(memory)
                .with_digests(digests);

            let mut stack = empty_stack();
            stack.push(proof_iter_pointer);
            stack.push(fri_verify_pointer);

            (stack, nondeterminism)
        }
    }

    impl Procedure for FriSnippet {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let fri_pointer = stack.pop().unwrap();
            let fri_verify = *FriVerify::decode_from_memory(memory, fri_pointer).unwrap();
            assert_eq!(fri_verify, self.test_instance);

            let proof_iter_pointer = stack.pop().unwrap();

            // uses highly specific knowledge about `BFieldCodec` and the test setup
            let proof_stream_pointer =
                *memory.get(&proof_iter_pointer).unwrap() - BFieldElement::new(2);
            // todo: hack using local knowledge: `fri_verify` lives directly after `vm_proof_iter`.
            //  Replace this once we have a better way to decode.
            let proof_stream_size = (fri_pointer - proof_stream_pointer).value() as usize;
            let mut proof_stream = decode_from_memory_with_size::<ProofStream>(
                memory,
                proof_stream_pointer,
                proof_stream_size,
            )
            .unwrap();

            let revealed_indices_and_elements = fri_verify.call(&mut proof_stream, nondeterminism);

            let indices_and_leafs_pointer = DYN_MALLOC_FIRST_ADDRESS;
            encode_to_memory(
                memory,
                indices_and_leafs_pointer,
                &revealed_indices_and_elements,
            );

            stack.push(indices_and_leafs_pointer);

            *sponge = Some(proof_stream.sponge);

            // no standard output
            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let vm_proof_iter = self.test_instance.pseudorandom_fri_proof_stream(seed);
            let (stack, nondeterminism) = self.set_up_stack_and_non_determinism(vm_proof_iter);

            ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: vec![],
                sponge: Some(Tip5::init()),
            }
        }
    }

    #[derive(Debug, Clone, test_strategy::Arbitrary)]
    struct ArbitraryFriVerify {
        #[strategy(arb())]
        #[filter(!#offset.is_zero())]
        offset: BFieldElement,

        #[strategy(0_u32..12)]
        domain_length_exponent: u32,

        #[strategy(0_u32..6)]
        expansion_factor_exponent: u32,

        #[strategy(1_u32..320)]
        num_collinearity_checks: u32,
    }

    impl ArbitraryFriVerify {
        fn fri_verify(&self) -> FriVerify {
            let expansion_factor = 2 << self.expansion_factor_exponent;
            let domain_length = expansion_factor * (1 << self.domain_length_exponent);
            FriVerify::new(
                self.offset,
                domain_length,
                expansion_factor,
                self.num_collinearity_checks,
            )
        }
    }

    #[derive(Debug, Clone, test_strategy::Arbitrary)]
    pub(super) struct TestCase {
        #[strategy(any::<ArbitraryFriVerify>().prop_map(|x| x.fri_verify()))]
        pub(super) fri_verify: FriVerify,

        #[strategy(vec(arb(), #fri_verify.first_round_max_degree()))]
        polynomial_coefficients: Vec<XFieldElement>,
    }

    impl TestCase {
        fn thirty_two_coefficients() -> [u64; 32] {
            [
                92, 42, 91, 59, 86, 64, 5, 64, 74, 53, 54, 68, 54, 23, 24, 58, 15, 44, 33, 31, 38,
                97, 25, 69, 11, 67, 66, 33, 37, 58, 43, 14,
            ]
        }

        /// A test case with no FRI rounds.
        fn tiny_case() -> Self {
            let fri_verify = FriVerify::new(bfe!(1), 2, 2, 1);
            assert_eq!(0, fri_verify.num_rounds());
            assert_eq!(0, fri_verify.last_round_max_degree());

            Self {
                fri_verify,
                polynomial_coefficients: vec![xfe!([42; 3])],
            }
        }

        /// A test case with 2 FRI rounds.
        fn small_case() -> Self {
            let fri_verify = FriVerify::new(bfe!(1), 64, 2, 7);
            assert_eq!(2, fri_verify.num_rounds(), "test case must be meaningful");

            let coefficients = Self::thirty_two_coefficients();
            assert_eq!(fri_verify.first_round_max_degree() + 1, coefficients.len());

            Self {
                fri_verify,
                polynomial_coefficients: coefficients.map(|n| xfe!([n; 3])).to_vec(),
            }
        }

        /// A test case resembling that which will be run by the STARK verifier
        pub(super) fn recursive_case(
            inner_padded_height: usize,
            log2_expansion_factor: usize,
        ) -> Self {
            const SECURITY_LEVEL: usize = 160;
            let stark = Stark::new(SECURITY_LEVEL, log2_expansion_factor);

            let fri_verify: FriVerify = stark.fri(inner_padded_height).unwrap().into();

            let coefficients = Self::thirty_two_coefficients();

            Self {
                fri_verify,
                polynomial_coefficients: coefficients.map(|n| xfe!([n; 3])).to_vec(),
            }
        }

        fn fri(&self) -> Fri {
            self.fri_verify.to_fri()
        }

        fn codeword(&self) -> Vec<XFieldElement> {
            let domain_length = self.fri_verify.domain_length;
            let mut codeword = self.polynomial_coefficients.clone();
            codeword.resize(domain_length as usize, XFieldElement::zero());

            ntt(&mut codeword);
            codeword
        }

        fn proof_items(&self) -> Vec<ProofItem> {
            let fri = self.fri();
            let codeword = self.codeword();
            let mut proof_stream = ProofStream::new();
            fri.prove(&codeword, &mut proof_stream).unwrap();

            proof_stream.items
        }

        fn proof_stream(&self) -> ProofStream {
            ProofStream {
                items: self.proof_items(),
                items_index: 0,
                sponge: Tip5::init(),
            }
        }

        pub(super) fn initial_state(&self) -> ProcedureInitialState {
            let fri_verify = self.fri_verify;
            let proof_stream = self.proof_stream();
            let snippet = FriSnippet {
                test_instance: fri_verify,
            };
            let (stack, nondeterminism) = snippet.set_up_stack_and_non_determinism(proof_stream);

            ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: vec![],
                sponge: Some(Tip5::init()),
            }
        }
    }

    #[test]
    fn assert_behavioral_equivalence_of_tiny_fri_instance() {
        assert_behavioral_equivalence_of_fris(TestCase::tiny_case());
    }

    /// Unfortunately, we cannot call the built-in test, _i.e._,
    /// `ShadowedProcedure::new(procedure).test()`: that test checks that the rust and tasm stacks
    /// are left in identical states. While they both contain a pointer to the same object, this
    /// object lives in a different (and difficult to predict) location in memory. This means the
    /// pointers are different. In the end, we don't care about the pointers. Therefore, instead of
    /// complete stack equivalence, we check equivalence of the pointed-to objects.
    fn assert_behavioral_equivalence_of_fris(test_case: TestCase) {
        let ProcedureInitialState {
            stack: initial_stack,
            nondeterminism,
            public_input: stdin,
            sponge,
        } = test_case.initial_state();

        let snippet = FriSnippet {
            test_instance: test_case.fri_verify,
        };
        let shadowed_procedure = ShadowedProcedure::new(snippet);
        let rust = rust_final_state(
            &shadowed_procedure,
            &initial_stack,
            &stdin,
            &nondeterminism,
            &sponge,
        );

        let tasm = tasm_final_state(
            &shadowed_procedure,
            &initial_stack,
            &stdin,
            nondeterminism,
            &sponge,
        );

        assert_eq!(rust.public_output, tasm.public_output);
        verify_stack_growth(&shadowed_procedure, &initial_stack, &tasm.op_stack.stack);
        verify_sponge_equivalence(&rust.sponge, &tasm.sponge);

        type IndexedLeaves = Vec<(u32, XFieldElement)>;
        let &rust_address = rust.stack.last().unwrap();
        let &tasm_address = tasm.op_stack.stack.last().unwrap();
        let rust_object = IndexedLeaves::decode_from_memory(&rust.ram, rust_address).unwrap();
        let tasm_object = IndexedLeaves::decode_from_memory(&tasm.ram, tasm_address).unwrap();
        assert_eq!(rust_object, tasm_object);
    }

    #[proptest]
    fn fri_derived_params_match(test_case: ArbitraryFriVerify) {
        let fri_verify = test_case.fri_verify();
        let fri = fri_verify.to_fri();

        prop_assert_eq!(fri_verify.num_rounds(), fri.num_rounds());
        prop_assert_eq!(
            fri_verify.last_round_max_degree(),
            fri.last_round_max_degree()
        );
        prop_assert_eq!(
            fri_verify.first_round_max_degree(),
            fri.first_round_max_degree()
        );
    }

    #[proptest(cases = 10)]
    fn test_inner_verify(test_case: TestCase) {
        let fri = test_case.fri();
        let mut vm_proof_iter = test_case.proof_stream();
        let verify_result = fri.verify(&mut vm_proof_iter);
        prop_assert!(verify_result.is_ok(), "FRI verify error: {verify_result:?}");

        let fri_verify = test_case.fri_verify;
        let mut vm_vm_proof_iter = test_case.proof_stream();
        let verify_result = fri_verify.inner_verify(&mut vm_vm_proof_iter, &mut vec![]);
        prop_assert!(verify_result.is_ok(), "FRI verify error: {verify_result:?}");
    }

    #[proptest(cases = 3)]
    fn test_shadow_prop(test_case: TestCase) {
        assert_behavioral_equivalence_of_fris(test_case);
    }

    #[test]
    fn test_shadow_small() {
        assert_behavioral_equivalence_of_fris(TestCase::small_case());
    }

    #[test]
    fn modifying_any_element_in_vm_proof_iter_of_small_test_case_causes_verification_failure() {
        let test_case = TestCase::small_case();
        let fri_verify = test_case.fri_verify;
        let vm_proof_iter = test_case.proof_stream();
        let digests = fri_verify.extract_digests_required_for_proving(&vm_proof_iter);

        // The digests required for verification are extracted and given to the verifier via
        // non-determinism. The corresponding digests in the proof are subsequently ignored by the
        // verifier. As a result, modifying one such digest will not cause a verification failure.
        // Therefore, any word occuring in any digest is ignored when looking for verification
        // failures. While this is a heuristic, the probability of a false negative is negligible.
        let words_occurring_in_some_digest: HashSet<_> =
            digests.iter().flat_map(|digest| digest.values()).collect();

        // sanity check
        let proof = test_case.proof_items().encode();
        let snippet = FriSnippet {
            test_instance: test_case.fri_verify,
        };
        snippet.verify_from_proof_with_digests(proof.clone(), digests.clone());

        let proof_len = proof.len();
        (0..proof_len).into_par_iter().for_each(|i| {
            if words_occurring_in_some_digest.contains(&proof[i]) {
                return;
            }
            let mut proof = proof.clone();
            proof[i].increment();
            catch_unwind(|| snippet.verify_from_proof_with_digests(proof, digests.clone()))
                .expect_err(&format!(
                    "Verification must fail, but succeeded at element {i}/{proof_len}"
                ));
        });
    }

    #[proptest(cases = 2)]
    fn assert_nondeterministic_digests_are_all_used(test_case: TestCase) {
        let ProcedureInitialState {
            stack: initial_stack,
            nondeterminism,
            public_input: stdin,
            sponge,
        } = test_case.initial_state();

        let snippet = FriSnippet {
            test_instance: test_case.fri_verify,
        };
        let shadowed_procedure = ShadowedProcedure::new(snippet);

        let tasm = tasm_final_state(
            &shadowed_procedure,
            &initial_stack,
            &stdin,
            nondeterminism,
            &sponge,
        );

        assert!(tasm.secret_digests.is_empty());
    }
}

#[cfg(test)]
mod bench {
    use std::fs::File;
    use std::fs::create_dir_all;
    use std::io::Write;
    use std::path::Path;
    use std::path::PathBuf;

    use super::*;
    use crate::generate_full_profile;
    use crate::test_helpers::prepend_program_with_sponge_init;
    use crate::test_helpers::prepend_program_with_stack_setup;
    use crate::test_prelude::*;
    use crate::verifier::fri::verify::tests::TestCase;

    #[test]
    fn bench() {
        let expansion_factor = 4;
        let domain_length = 1 << 10;
        let offset = BFieldElement::new(7);
        let num_collinearity_checks = 80;
        let fri_verify = FriVerify::new(
            offset,
            domain_length,
            expansion_factor,
            num_collinearity_checks,
        );
        let snippet = FriSnippet {
            test_instance: fri_verify,
        };
        ShadowedProcedure::new(snippet).bench();
    }

    #[ignore = "Takes many minutes to run"]
    #[test]
    fn profile_fri() {
        const INNER_PADDED_HEIGHTS: [usize; 4] = [1 << 8, 1 << 9, 1 << 10, 1 << 11];
        const SECURITY_LEVEL: usize = 160;

        for inner_padded_height in INNER_PADDED_HEIGHTS {
            for log2_fri_expansion_factor in 1..=5 {
                let stark = Stark::new(SECURITY_LEVEL, log2_fri_expansion_factor);
                profile_and_bench_fri_run(stark, inner_padded_height);
            }
        }
    }

    fn profile_and_bench_fri_run(stark: Stark, inner_padded_height: usize) {
        let test_case = TestCase::recursive_case(
            inner_padded_height,
            stark.fri_expansion_factor.ilog2() as usize,
        );
        let fri_params = stark.fri(inner_padded_height).unwrap();
        let name = format!(
            "fri_verify_expansion_factor_inner_padded_height_{}_expansion_factor_{}",
            inner_padded_height, fri_params.expansion_factor
        );

        let ProcedureInitialState {
            stack: initial_stack,
            nondeterminism,
            public_input: stdin,
            sponge: _,
        } = test_case.initial_state();
        let snippet = FriSnippet {
            test_instance: test_case.fri_verify,
        };
        let code = snippet.link_for_isolated_run();
        let program = Program::new(&code);
        let program = prepend_program_with_stack_setup(&initial_stack, &program);
        let program = prepend_program_with_sponge_init(&program);

        let profile = generate_full_profile(&name, program, &stdin.into(), &nondeterminism);

        // write profile to profile file
        let mut path = PathBuf::new();
        path.push("profiles");
        create_dir_all(&path).expect("profiles directory must be created successfully");

        path.push(Path::new(&name).with_extension("profile"));
        let mut file = File::create(path).expect("open file for writing");
        write!(file, "{profile}").unwrap();

        println!("{profile}");

        // Don't do this since entrypoint label of FRI snippet is not parameterized over its
        // variables, meaning all benchmarks are overwritten.
        // // Also create benchmarks for quick reference
        // ShadowedProcedure::new(snippet).bench();
    }
}
