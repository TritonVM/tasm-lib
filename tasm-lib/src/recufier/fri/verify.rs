use anyhow::bail;
use itertools::Itertools;
use triton_vm::arithmetic_domain::ArithmeticDomain;
use triton_vm::error::FriValidationError;
use triton_vm::fri::Fri;
use triton_vm::prelude::*;
use triton_vm::proof_item::FriResponse;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::proof_stream::ProofStream;
use triton_vm::twenty_first::shared_math::ntt::intt;
use triton_vm::twenty_first::shared_math::other::log_2_ceil;
use triton_vm::twenty_first::shared_math::polynomial::Polynomial;
use triton_vm::twenty_first::shared_math::traits::ModPowU32;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;
use triton_vm::twenty_first::util_types::merkle_tree::MerkleTreeInclusionProof;

use crate::data_type::DataType;
use crate::data_type::StructType;
use crate::field;
use crate::hashing::algebraic_hasher::sample_indices::SampleIndices;
use crate::hashing::algebraic_hasher::sample_scalars_static_length_dyn_malloc::SampleScalarsStaticLengthDynMalloc;
use crate::hashing::merkle_root::MerkleRoot;
use crate::library::Library;
use crate::list::get::Get;
use crate::list::higher_order::inner_function::InnerFunction;
use crate::list::higher_order::inner_function::RawCode;
use crate::list::higher_order::map::Map;
use crate::list::higher_order::zip::Zip;
use crate::list::length::Length;
use crate::list::new::New;
use crate::list::push::Push;
use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::recufier::verify_authentication_paths_for_leaf_and_index_list::VerifyAuthenticationPathForLeafAndIndexList;
use crate::recufier::vm_proof_iter::dequeue_next_as::DequeueNextAs;
use crate::recufier::vm_proof_iter::new::vm_proof_iter_type;
use crate::recufier::xfe_ntt::XfeNtt;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;

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
    domain_generator: BFieldElement,
}

impl From<Fri<Tip5>> for FriVerify {
    fn from(value: Fri<Tip5>) -> Self {
        Self {
            domain_generator: value.domain.generator,

            // This runtime type-conversion prevents a FRI domain of length 2^32 from being created.
            domain_length: value.domain.length.try_into().unwrap(),
            domain_offset: value.domain.offset,
            expansion_factor: value.expansion_factor.try_into().unwrap(),
            num_collinearity_checks: value.num_collinearity_checks.try_into().unwrap(),
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
    fn indexed_leaves_list_type() -> DataType {
        let indexed_leaf_type = DataType::Tuple(vec![DataType::U32, DataType::Xfe]);
        DataType::List(Box::new(indexed_leaf_type))
    }
}

impl BasicSnippet for FriSnippet {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let proof_iter_ref = DataType::StructRef(vm_proof_iter_type());
        let argument_0 = (proof_iter_ref, "*vm_proof_iter".to_string());

        let fri_verify_ref = DataType::StructRef(fri_verify_type());
        let argument_1 = (fri_verify_ref, "*fri_verify".to_string());

        vec![argument_0, argument_1]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            Self::indexed_leaves_list_type(),
            "indices_and_elements".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_fri_verify".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let domain_length = field!(FriVerify::domain_length);
        let domain_generator = field!(FriVerify::domain_generator);
        let domain_offset = field!(FriVerify::domain_offset);
        let expansion_factor = field!(FriVerify::expansion_factor);
        let num_collinearity_checks = field!(FriVerify::num_collinearity_checks);
        let new_list_of_digests = library.import(Box::new(New::new(DataType::Digest)));
        let push_digest_to_list = library.import(Box::new(Push::new(DataType::Digest)));
        let read_digest = triton_asm!(
                                // _ *digest
            push 4 add          // _ *digest+4
            read_mem 5 pop 1    // _ [digest; 5]
        );
        let read_xfe = triton_asm!(
            push {EXTENSION_DEGREE - 1} add read_mem {EXTENSION_DEGREE} pop 1
        );
        let new_list_xfe = library.import(Box::new(New::new(DataType::Xfe)));
        let push_scalar = library.import(Box::new(Push::new(DataType::Xfe)));

        let vm_proof_iter_dequeue_next_as_merkle_root =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::MerkleRoot)));
        let vm_proof_iter_dequeue_next_as_fri_codeword =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::FriCodeword)));
        let vm_proof_iter_dequeue_next_as_fri_response =
            library.import(Box::new(DequeueNextAs::new(ProofItemVariant::FriResponse)));

        let vm_proof_iter_sample_one_scalar =
            library.import(Box::new(SampleScalarsStaticLengthDynMalloc {
                num_elements: 1,
            }));
        let dequeue_commit_phase = format!("{entrypoint}_dequeue_commit_phase_remainder");
        let convert_xfe_to_digest = format!("{entrypoint}_convert_xfe_to_digest");
        let map_convert_xfe_to_digest =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                function: triton_asm!(
                    {convert_xfe_to_digest}:
                        // _ xfe2 xfe1 xfe0
                        push 0 push 0       // _ xfe2 xfe1 xfe0 0 0
                        swap 3              // _ xfe2 0 xfe0 0 xfe1
                        swap 1              // _ xfe2 0 xfe0 xfe1 0
                        swap 4              // _ 0 0 xfe0 xfe1 xfe2
                        swap 2              // _ 0 0 xfe2 xfe1 xfe0
                        return
                ),
                input_type: DataType::Xfe,
                output_type: DataType::Digest,
            }))));
        let length_of_list_of_digests = library.import(Box::new(Length::new(DataType::Digest)));
        let length_of_list_of_u32s = library.import(Box::new(Length::new(DataType::U32)));
        let length_of_list_of_xfes = library.import(Box::new(Length::new(DataType::Xfe)));
        let merkle_root = library.import(Box::new(MerkleRoot));
        let get_digest = library.import(Box::new(Get::new(DataType::Digest)));
        let xfe_ntt = library.import(Box::new(XfeNtt));
        let assert_tail_xfe0 = format!("{entrypoint}_tail_xfe0");
        let length_of_list_of_xfe = library.import(Box::new(Length::new(DataType::Xfe)));
        let get_xfe_from_list = library.import(Box::new(Get::new(DataType::Xfe)));
        let sample_indices = library.import(Box::new(SampleIndices));
        let revealed_leafs = field!(FriResponse::revealed_leaves);
        let zip_digests_indices =
            library.import(Box::new(Zip::new(DataType::U32, DataType::Digest)));
        let verify_authentication_paths_for_leaf_and_index_list =
            library.import(Box::new(VerifyAuthenticationPathForLeafAndIndexList));
        let zip_index_xfe = library.import(Box::new(Zip::new(DataType::U32, DataType::Xfe)));
        let query_phase_main_loop = format!("{entrypoint}_query_phase_main_loop");
        let add_half_label = format!("{entrypoint}_add_half_domain");
        let map_add_half_domain_length =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                function: triton_asm! {
                    {add_half_label}:
                                        // _ current_domain_length r half_domain_length [bu ff er] index
                    dup 4 add           // _ current_domain_length r half_domain_length [bu ff er] index+half_domain_length
                    dup 6 swap 1 div_mod// _ current_domain_length r half_domain_length [bu ff er] (index+half_domain_length)/domain_length
                    swap 1 pop 1        // _ current_domain_length r half_domain_length [bu ff er] (index+half_domain_length)%domain_length
                    return
                },
                input_type: DataType::U32,
                output_type: DataType::U32,
            }))));
        let populate_return_vector_second_half =
            format!("{entrypoint}_populate_return_vector_second_half");
        let populate_loop = format!("{entrypoint}_populate_return_vector_loop");
        let get_u32_and_xfe = library.import(Box::new(Get::new(DataType::Tuple(vec![
            DataType::U32,
            DataType::Xfe,
        ]))));
        let push_u32_and_xfe = library.import(Box::new(Push::new(DataType::Tuple(vec![
            DataType::U32,
            DataType::Xfe,
        ]))));
        let reduce_indices_label = format!("{entrypoint}_reduce_indices");
        let map_reduce_indices =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                function: triton_asm! {
                    {reduce_indices_label}:
                                        // _ half_domain_length [bu ff er] index
                    dup 4 swap 1        // _ half_domain_length [bu ff er] half_domain_length index
                    div_mod             // _ half_domain_length [bu ff er] q r
                    swap 1 pop 1        // _ half_domain_length [bu ff er] index%half_domain_length
                    return
                },
                input_type: DataType::U32,
                output_type: DataType::U32,
            }))));
        let compute_c_values_loop = format!("{entrypoint}_compute_c_values_loop");
        let identity_label = format!("{entrypoint}_identity");
        let duplicate_list_xfe =
            library.import(Box::new(Map::new(InnerFunction::RawCode(RawCode {
                input_type: DataType::Xfe,
                output_type: DataType::Xfe,
                function: triton_asm! { {identity_label}: return },
            }))));
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
                        hint codeword: Pointer = stack[7]
                        push 0                  // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0
                        dup 4 dup 9 dup 1       // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index *codeword index
                        call {get_xfe_from_list}// _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index xfe2' xfe1' xfe0'
                        push 0                  // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0 index xfe2' xfe1' xfe0' 0
                        assert_vector           // _ *codeword [bu ff er] index xfe2 xfe1 xfe0 0
                        pop 1                   // _ *codeword [bu ff er] index xfe2 xfe1 xfe0
                        return
                },
            }))));

        triton_asm! {
            // BEFORE: _ *proof_iter *fri_verify
            // AFTER:  _ *proof_iter *indices_and_leafs
            {entrypoint}:
                hint fri_verify_pointer = stack[0]
                hint proof_iter_pointer = stack[1]

                // calculate number of rounds
                dup 0 {&domain_length}      // _ *vm_proof_iter *fri_verify *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify domain_length
                hint domain_length = stack[0]

                dup 1 {&expansion_factor}   // _ *vm_proof_iter *fri_verify domain_length *expansion_factor
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify domain_length expansion_factor
                hint expansion_factor = stack[0]

                swap 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify first_round_code_dimension
                log_2_floor                 // _ *vm_proof_iter *fri_verify max_num_rounds
                hint max_num_rounds = stack[0]

                dup 1 {&num_collinearity_checks}
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify max_num_rounds num_collinearity_checks
                hint num_collinearity_checks = stack[0]

                log_2_floor push 1 add      // _ *vm_proof_iter *fri_verify max_num_rounds num_rounds_checking_most_locations

                dup 1 dup 1 lt              // _ *vm_proof_iter *fri_verify max_num_rounds num_rounds_checking_most_locations (num_rounds_checking_most_locations<max_num_rounds)
                swap 2 push -1 mul add      // _ *vm_proof_iter *fri_verify (num_rounds_checking_most_locations<max_num_rounds) num_rounds_checking_most_locations-max_num_rounds
                mul push -1 mul             // _ *vm_proof_iter *fri_verify if(num_rounds_checking_most_locations<max_num_rounds){max_num_rounds-num_rounds_checking_most_locations}else{0}
                                            // _ *vm_proof_iter *fri_verify num_rounds
                hint num_rounds = stack[0]

                // calculate max degree of last round
                dup 1 {&domain_length}      // _ *vm_proof_iter *fri_verify num_rounds *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds domain_length
                hint domain_length = stack[0]

                dup 2 {&expansion_factor}   // _ *vm_proof_iter *fri_verify num_rounds domain_length *expansion_factor
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds domain_length expansion_factor
                hint expansion_factor = stack[0]

                swap 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension

                push 2 dup 2 swap 1 pow     // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension (1<<num_rounds)

                swap 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds first_round_code_dimension>>num_rounds
                push -1 add                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree
                hint last_round_max_degree = stack[0]

                // COMMIT PHASE

                // create lists for roots and alphas
                call {new_list_xfe}         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas
                    hint folding_challenges: ListPointer = stack[0]
                call {new_list_of_digests}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots
                    hint roots: ListPointer = stack[0]

                // dequeue first Merkle root
                dup 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots *proof_iter
                call {vm_proof_iter_dequeue_next_as_merkle_root}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots *root
                dup 1 swap 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots *roots *root

                {&read_digest}              // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots *roots [root]
                call {push_digest_to_list}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots

                // dequeue remaining roots and collect Fiat-Shamir challenges
                dup 3                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *alphas *roots num_rounds
                swap 2                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree num_rounds *roots *alphas
                call {dequeue_commit_phase}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas

                // dequeue last codeword and check length
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_codeword}
                    hint last_fri_codeword: ListPointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *last_codeword
                dup 7 swap 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *vm_proof_iter *last_codeword

                // clone last codeword for later use
                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *vm_proof_iter *last_codeword *last_codeword
                call {duplicate_list_xfe}   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree 0 *roots *alphas *vm_proof_iter *last_codeword *last_codeword'
                    hint last_fri_codeword_copy: ListPointer = stack[0]
                swap 5 pop 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword

                // compute Merkle root
                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *last_codeword
                call {map_convert_xfe_to_digest}
                    hint list_of_leaves: ListPointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs
                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs *leafs
                call {length_of_list_of_digests}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs
                dup 9 dup 9                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs *fri_verify num_rounds
                swap 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs num_rounds *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs num_rounds *domain_length
                    hint domain_length_pointer: Pointer = stack[0]
                read_mem 1
                    hint domain_length = stack[1]
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs num_rounds domain_length
                swap 1 push 2               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs domain_length num_rounds 2
                pow                         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs domain_length (1<<num_rounds)
                swap 1 div_mod pop 1        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs (domain_length>>num_rounds)
                dup 1 eq                    // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs eq
                assert                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs num_leafs
                push 0 swap 1               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *leafs 0 num_leafs
                call {merkle_root}          // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root]
                hint merkle_root: Digest = stack[0..5]

                // check against last root dequeued
                dup 8                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root] *roots
                dup 0                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root] *roots *roots
                call {length_of_list_of_digests}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root] *roots num_roots
                push -1 add                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root] *roots num_roots-1
                call {get_digest}           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root] [roots[-1]]
                assert_vector               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword [last_root]

                // clean up top of stack
                pop 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword

                // get omega
                dup 7                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *fri_verify
                {&domain_generator}         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *generator
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword domain_generator
                dup 7                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword domain_generator num_rounds
                push 2 pow                  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword domain_generator 1<<num_rounds
                swap 1 pow                  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword domain_generator^(num_rounds)

                // compute intt (without scaling)
                invert                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword omega_inv
                dup 1 swap 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_codeword *last_codeword omega_inv
                call {xfe_ntt}              // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_polynomial

                // test low degree of polynomial
                dup 5 push 1 add            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_polynomial num_nonzero_coefficients

                call {assert_tail_xfe0}     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *last_polynomial *total_num_coefficients
                pop 2                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter

                // QUERY PHASE

                // get "A" indices and verify membership

                // get index count
                dup 6                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *fri_verify
                {&num_collinearity_checks}   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *num_indices
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter num_indices

                // get domain length
                dup 7                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter num_indices *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter num_indices *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter num_indices domain_length

                // sample "A" indices
                call {sample_indices}       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *indices

                // get largest tree height
                dup 7                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *indices *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *indices *domain_length
                    hint domain_length_pointer: Pointer = stack[0]
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *indices domain_length
                    hint domain_length = stack[0]
                log_2_floor                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas *vm_proof_iter *indices tree_height
                // dequeue proof item as fri response
                swap 2                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_response}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *fri_response

                // assert correct length of number of leafs
                {&revealed_leafs}           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements
                dup 1 dup 1                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements
                call {length_of_list_of_xfes}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices num_leafs
                swap 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements num_leafs *indices
                call {length_of_list_of_u32s}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements num_leafs num_indices
                eq assert                   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements
                dup 1 dup 1                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements

                // check batch merkle membership
                call {map_convert_xfe_to_digest}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *revealed_leafs_as_digests
                call {zip_digests_indices}  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices
                dup 5 push 0                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices *roots 0
                call {get_digest}           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]]
                dup 8                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]] tree_height

                call {verify_authentication_paths_for_leaf_and_index_list}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *leafs_indices [root[0]] tree_height
                pop 5 pop 2                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements

                // prepare the return value:
                // the list of opened indices and elements
                dup 1 dup 1                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *indices *a_elements
                call {zip_index_xfe}        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs
                hint indices_and_leafs = stack[0]
                // zip allocates a new list, which we want to be twice as long
                // (the second half will be populated in the first iteration of the main loop below)
                read_mem 1                  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements length (*revealed_indices_and_leafs - 1)
                push 1 add swap 1           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs length
                push 4 mul                  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size
                push {DYN_MALLOC_ADDRESS} read_mem 1
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size alloc (*alloc - 1)
                push 1 add swap 1           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size *alloc alloc
                swap 1 swap 2 add           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *alloc size+alloc
                swap 1                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs size+alloc *alloc
                write_mem 1 pop 1           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs

                // prepare for query phase main loop
                dup 9                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *fri_verify
                {&domain_length}            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs *domain_length
                read_mem 1 pop 1            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas tree_height *indices *a_elements *revealed_indices_and_leafs domain_length
                // rename stack elements    // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length
                push 0 // (=r)              // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r

                call {query_phase_main_loop}// _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds

                // verify membership of C elements (here called A elements) in last codeword
                dup 8 dup 5 dup 5           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices *c_elements
                call {zip_index_xfe}        // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices_and_elements
                call {map_assert_membership}// _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds *last_codeword' *c_indices_and_elements

                // clean up stack and return
                pop 4                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs
                swap 9                      // _ *vm_proof_iter *revealed_indices_and_leafs num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *fri_verify
                pop 5                       // _ *vm_proof_iter *revealed_indices_and_leafs *fri_verify num_rounds last_round_max_degree 0 *roots
                pop 4                       // _ *vm_proof_iter *revealed_indices_and_leafs
                swap 1 pop 1                // _ *revealed_indices_and_leafs

                return

            // Loop's end condition is determined by pointer values, so we don't need a loop counter value
            // All pointers are traversed from highest address to lowest
            // INVARIANT:          _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elements *a_indices *b_elements *b_indices
            {compute_c_values_loop}:

                // evaluate termination criterion
                // c_elem
                dup 9
                dup 6
                eq
                skiz return

                // Strategy:
                // 1. Read `a_y`
                // 2. Calculate `a_x`
                // 3. Calculate `[a_y- b_y]`, preserve a[y]
                // 4. Calculate `b_x`
                // 5. Calculate `-b_x`
                // 6. Calculate `1 / (a_x - b_x)` while preserving `a_x`
                // 7. Calculate `(a_y - b_y) / (a_x - b_x)`
                // 8: Read `[c_x]`
                // 9. Calculate `c_x - a_x`
                // 10. Calculate final `c_y`
                // 11. Write c_y to *c_elem

                // _ *c_end_condition g offset r *c_elem *alphas[r] *a_elem *a_index *b_elem *b_index

                // 1: Read `a_y`
                dup 3
                read_mem {EXTENSION_DEGREE}
                swap 7
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index *b_elem *b_index [a_y]

                // 2: Calculate `a_x`
                dup 11
                dup 6
                read_mem 1
                swap 8
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] (1<<round) a_index

                dup 12
                pow
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] (1<<round) (g^a_index)

                dup 11
                mul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] (1<<round) (g^a_index * offset)

                pow
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] (g^a_index * offset)^(1<<round)
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] a_x

                // 3: Calculate `[a_y- b_y]`, preserve a[y]
                dup 5
                read_mem {EXTENSION_DEGREE}
                swap 9
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] a_x [b_y]

                push -1
                xbmul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] a_x [-b_y]

                dup 6
                dup 6
                dup 6
                xxadd
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index [a_y] a_x [a_y-b_y]

                // 4: Calculate `b_x`
                dup 15
                dup 15
                dup 9
                read_mem 1
                swap 11
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (1<<round) g b_index

                swap 1
                pow
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (1<<round) (g^b_index)

                dup 15
                mul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (1<<round) (g^b_index * offset)

                pow
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (g^b_index * offset)^((1<<round))
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] b_x

                // 5: Calculate `-b_x`
                push -1
                mul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (-b_x)

                // 6: Calculate `1 / (a_x - b_x)` while preserving `a_x`
                dup 4
                add
                invert
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [a_y-b_y] (1/(a_x-b_x))

                // 7:  Calculate `(a_y - b_y) / (a_x - b_x)`

                xbmul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [(a_y-b_y)/(a_x-b_x)]

                // 8: Read `[c_x]`
                dup 11
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [(a_y-b_y)/(a_x-b_x)] [c_x]

                // 9:  Calculate `c_x - a_x`
                dup 6
                push -1
                mul
                add
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [(a_y-b_y)/(a_x-b_x)] [c_x - a_x]

                // 10: Calculate final `c_y`
                xxmul
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] a_x [(a_y-b_y)/(a_x-b_x) * (c_x -a_x)]

                swap 1
                swap 2
                swap 3
                pop 1
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [a_y] [(a_y-b_y)/(a_x-b_x) * (c_x -a_x)]

                xxadd
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [(a_y-b_y)/(a_x-b_x) * (c_x -a_x) + a_y]
                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elem_prev *a_index_prev *b_elem *b_index_prev [c_y]

                // 11. Write c_y to *c_elem
                dup 8
                write_mem {EXTENSION_DEGREE}
                push {- 2 * EXTENSION_DEGREE as i32}
                add
                swap 6
                pop 1

                recurse

            // BEFORE: _ *list index
            // AFTER:  _ *list length
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

            // BEFORE: _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree | num_rounds *roots *alphas
            // AFTER:  _ ... | 0 *roots *alphas
            {dequeue_commit_phase}:

                // return if done
                dup 2       // _ num_rounds *roots *alphas num_rounds
                push 0 eq   // _ num_rounds *roots *alphas num_rounds==0
                skiz return

                // decrement round number
                swap 2      // _ *alphas *roots num_rounds
                push -1 add // _ *alphas *roots num_rounds-1
                swap 2      // _ num_rounds-1 *roots *alphas

                // sample scalar
                call {vm_proof_iter_sample_one_scalar}
                            // _ num_rounds-1 *roots *alphas *scalars
                dup 1 swap 1// _ num_rounds-1 *roots *alphas *alphas *scalars
                {&read_xfe}
                            // _ num_rounds-1 *roots *alphas *alphas [scalars[0]]

                call {push_scalar}
                            // _ num_rounds-1 *roots *alphas

                // dequeue Merkle root
                swap 1      // _ num_rounds-1 *alphas *roots
                dup 6       // _ num_rounds-1 *alphas *roots *vm_proof_iter

                            // _ num_rounds-1 *alphas *roots *proof_iter
                call {vm_proof_iter_dequeue_next_as_merkle_root}
                                            // _ num_rounds-1 *alphas *roots *root
                dup 1 swap 1                // _ num_rounds-1 *alphas *roots *roots *root
                {&read_digest}              // _ num_rounds-1 *alphas *roots *roots [root]
                call {push_digest_to_list}  // _ num_rounds-1 *alphas *roots
                swap 1                      // _ num_rounds-1 *roots *alphas
                recurse

            // BEFORE:     _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length 0
            // AFTER:      _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length num_rounds
            // INVARIANT:  _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r
            {query_phase_main_loop}:
                hint current_round: u32 = stack[0]
                hint current_domain_len = stack[1]
                hint revealed_indices_and_leafs: Pointer = stack[2]
                hint a_elements: Pointer = stack[3]
                hint a_indices: Pointer = stack[4]
                hint current_tree_height = stack[5]
                hint alphas: Pointer = stack[6]
                hint roots: Pointer = stack[7]
                hint last_codeword: Pointer = stack[8]
                hint last_round_max_degree = stack[9]
                hint num_rounds = stack[10]
                // test termination condition:
                // if r == num_rounds then return
                dup 10 dup 1
                eq                          // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r num_rounds==r
                skiz return                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r

                // get "B" indices
                push 2 dup 2
                div_mod pop 1               // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length
                hint half_domain_len = stack[0]
                dup 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *a_indices
                call {map_add_half_domain_length}
                hint b_indices: Pointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices

                // dequeue fri response and get "B" elements
                dup 14                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *proof_iter
                call {vm_proof_iter_dequeue_next_as_fri_response}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *fri_response
                {&revealed_leafs}           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
                hint b_elements: Pointer = stack[0]

                // if in first round (r==0), populate second half of return vector
                dup 3 push 0 eq
                skiz call {populate_return_vector_second_half}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements

                // check batch merkle membership
                dup 0 call {map_convert_xfe_to_digest}
                hint b_leaves: Pointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leafs
                dup 2 swap 1                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_indices *b_leafs
                call {zip_digests_indices}
                hint b_indices_and_leaves: Pointer = stack[0]
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices
                dup 11 dup 5                // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices *roots r
                call {get_digest}           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]]
                dup 14                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]] current_tree_height
                call {verify_authentication_paths_for_leaf_and_index_list}
                                            // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *b_leaf_and_indices [roots[r]] current_tree_height
                pop 5 pop 2                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements

                // pull *fri_verify to top because needed
                dup 14                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify
                // update tree height
                dup 9 push -1 add           // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1

                // reduce modulo N/2 to get C indices
                dup 4 dup 4                 // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *b_indices
                call {map_reduce_indices}   // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices
                hint c_indices = stack[0]

                // compute C elements
                call {new_list_xfe}         // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements
                hint c_elements = stack[0]


                // Prepare stack for c-values loop
                dup 14                      // _ ... *alphas
                dup 9                       // _ ... *alphas r
                push 1
                add
                push {EXTENSION_DEGREE}
                mul
                add                         // _ ... *alphas[r]_last_word
                                            // _ ... *alphas[r]            <-- rename

                // Move `*a_elements` to top to stack and point to last element
                dup 12
                dup 3                      // _ ... *alphas[r] *a_elements *c_indices
                read_mem 1 pop 1           // _ ... *alphas[r] *a_elements *c_indices num_indices
                push {EXTENSION_DEGREE}
                mul
                add                         // _ ,, *alphas[r] *a_elements_last_word
                                            // _ ,, *alphas[r] *a_elements   <-- rename

                // Move `*a_indices` to top of stack and point to last element
                dup 14                      // _ ... *alphas[r] *a_elements *a_indices
                dup 0
                read_mem 1 pop 1
                add                         // _ ... *alphas[r] *a_elements *a_indices_last_word
                                            // _ ... *alphas[r] *a_elements *a_indices <-- rename

                // Move `c_elements` to top of stack and calculate loop-terminal condition
                dup 3
                push {-(EXTENSION_DEGREE as i32 - 1)}
                add                        // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition

                dup 12
                push 2
                pow                        // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round)

                dup 9
                {&domain_generator}        // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g
                read_mem 1
                pop 1

                dup 10
                {&domain_offset}           // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset
                read_mem 1
                pop 1

                dup 7                      // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_elements
                dup 9                      // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset*c_elements *c_indices
                read_mem 1 pop 1           // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset*c_elements c_indices_len

                // Write length to *c_elements
                dup 0                       // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_elements c_indices_len c_indices_len
                swap 2                      // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset c_indices_len c_indices_len *c_elements
                write_mem 1
                swap 1                      // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset (*c_elements + 1) c_indices_len

                push -1
                add
                push {EXTENSION_DEGREE}
                mul
                add
                                            // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word

                dup 14
                                            // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *b_indices
                dup 0
                read_mem 1
                pop 1
                add                         // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *b_index_last

                dup 14
                dup 10
                read_mem 1
                pop 1
                                            // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *b_index_last *b_elements c_len

                push {EXTENSION_DEGREE}
                mul
                add
                                            // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *b_index_last *b_elem_last

                dup 9                       // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *b_index_last *b_elem_last *alphas[r]

                swap 2
                swap 1                     // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *alphas[r] *b_index_last *b_elem_last

                dup 9
                dup 9                      // _ ... *alphas[r] *a_elements *a_indices *c_elements_end_condition (1<<round) g offset *c_last_elem_first_word *alphas[r] *b_indices *b_elements *a_elements *a_indices

                swap 2
                swap 1
                swap 3

                // _ *c_end_condition (1<<round) g offset *c_elem *alphas[r] *a_elements *a_indices *b_elements *b_indices
                call {compute_c_values_loop}

                pop 5
                pop 5
                pop 2

                // return stack to invariant and keep books for next iteration
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *a_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *c_elements
                swap 11                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices *a_elements
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *a_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *c_indices
                swap 11                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length *a_indices
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 half_domain_length
                swap 7                      // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1 current_domain_length
                pop 1                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height-1
                swap 10                     // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r half_domain_length *b_indices *b_elements *fri_verify current_tree_height
                pop 5                       // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r
                push 1 add                  // _ *vm_proof_iter *fri_verify num_rounds last_round_max_degree *last_codeword' *roots *alphas current_tree_height-1 *c_indices *c_elements *revealed_indices_and_leafs half_domain_length r+1
                recurse

            // BEFORE: _  *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
            // AFTER:  _  *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
            {populate_return_vector_second_half}:
                dup 1 dup 1 call {zip_index_xfe}
                                            // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements
                push 0 call {populate_loop} // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements *indices_and_elements length
                pop 2                       // _ *revealed_indices_and_leafs current_domain_length r half_domain_length *b_indices *b_elements
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

impl FriVerify {
    pub fn extract_digests_required_for_proving(
        &self,
        proof_stream: &ProofStream<Tip5>,
    ) -> Vec<Digest> {
        let mut digests = vec![];
        self.inner_verify(&mut proof_stream.clone(), &mut digests)
            .unwrap();
        digests
    }

    pub fn to_fri(self) -> Fri<Tip5> {
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
        proof_stream: &mut ProofStream<Tip5>,
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
        println!("alphas:");
        for alpha in alphas.iter() {
            println!("{}", alpha);
        }

        // Extract last codeword
        let last_codeword = proof_stream
            .dequeue()
            .unwrap()
            .try_into_fri_codeword()
            .unwrap();
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
        let domain_length = self.domain_length as usize;
        let num_collinearity_check = self.num_collinearity_checks as usize;
        let mut a_indices = proof_stream.sample_indices(domain_length, num_collinearity_check);

        let tree_height = self.domain_length.ilog2() as usize;
        let fri_response = proof_stream
            .dequeue()
            .unwrap()
            .try_into_fri_response()
            .unwrap();
        assert_eq!(a_indices.len(), fri_response.revealed_leaves.len());
        let mut a_values = fri_response.revealed_leaves;

        let leaf_digests = Self::map_convert_xfe_to_digest(&a_values);
        let indexed_a_leaves = a_indices.iter().copied().zip_eq(leaf_digests).collect_vec();

        // reduplicate authentication structures if necessary
        if num_nondeterministic_digests_read >= nondeterministic_digests.len() {
            let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                tree_height,
                indexed_leaves: indexed_a_leaves.clone(),
                authentication_structure: fri_response.auth_structure,
                ..Default::default()
            };

            // sanity check: the authentication structure was valid, right?
            assert!(inclusion_proof.clone().verify(roots[0]));
            let reduplicated_authentication_paths = inclusion_proof.into_authentication_paths()?;
            nondeterministic_digests
                .extend(reduplicated_authentication_paths.into_iter().flatten());
        }

        // verify authentication paths for A leafs
        for indexed_leaf in indexed_a_leaves {
            let authentication_path = &nondeterministic_digests[num_nondeterministic_digests_read
                ..(num_nondeterministic_digests_read + tree_height)];
            num_nondeterministic_digests_read += tree_height;
            let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                tree_height,
                indexed_leaves: vec![indexed_leaf],
                authentication_structure: authentication_path.to_vec(),
                ..Default::default()
            };
            assert!(inclusion_proof.verify(roots[0]));
        }

        // save indices and revealed leafs of first round's codeword for returning
        let revealed_indices_and_elements_first_half = a_indices
            .iter()
            .map(|&idx| idx as u32)
            .zip_eq(a_values.iter().copied())
            .collect_vec();
        // these indices and values will be computed in the first iteration of the main loop below
        let mut revealed_indices_and_elements_second_half = vec![];

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
                let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                    tree_height: current_tree_height,
                    indexed_leaves: indexed_b_leaves.clone(),
                    authentication_structure: fri_response.auth_structure,
                    ..Default::default()
                };

                // sanity check: the auth structure was valid, right?
                assert!(inclusion_proof.clone().verify(roots[r]));
                let reduplicated_authentication_paths =
                    inclusion_proof.into_authentication_paths()?;
                nondeterministic_digests
                    .extend(reduplicated_authentication_paths.into_iter().flatten());
            }

            // verify authentication paths for B leafs
            for indexed_leaf in indexed_b_leaves {
                let authentication_path = &nondeterministic_digests
                    [num_nondeterministic_digests_read
                        ..(num_nondeterministic_digests_read + current_tree_height)];
                num_nondeterministic_digests_read += current_tree_height;
                let inclusion_proof = MerkleTreeInclusionProof::<Tip5> {
                    tree_height: current_tree_height,
                    indexed_leaves: vec![indexed_leaf],
                    authentication_structure: authentication_path.to_vec(),
                    ..Default::default()
                };
                if !inclusion_proof.verify(roots[r]) {
                    bail!(FriValidationError::BadMerkleAuthenticationPath);
                }
            }

            debug_assert_eq!(self.num_collinearity_checks, a_indices.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, b_indices.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, a_values.len() as u32);
            debug_assert_eq!(self.num_collinearity_checks, b_values.len() as u32);

            if r == 0 {
                // save other half of indices and revealed leafs of first round for returning
                revealed_indices_and_elements_second_half = b_indices
                    .iter()
                    .map(|&idx| idx as u32)
                    .zip_eq(b_values.iter().copied())
                    .collect_vec();
            }

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

        // compile return object and store to memory
        let revealed_indices_and_elements = revealed_indices_and_elements_first_half
            .into_iter()
            .chain(revealed_indices_and_elements_second_half)
            .collect_vec();

        Ok(revealed_indices_and_elements)
    }

    /// Computes the number of rounds
    pub fn num_rounds(&self) -> usize {
        let first_round_code_dimension = self.first_round_max_degree() + 1;
        let max_num_rounds = log_2_ceil(first_round_code_dimension as u128);

        // Skip rounds for which Merkle tree verification cost exceeds arithmetic cost,
        // because more than half the codeword's locations are queried.
        let num_rounds_checking_all_locations = self.num_collinearity_checks.ilog2() as u64;
        let num_rounds_checking_most_locations = num_rounds_checking_all_locations + 1;

        max_num_rounds.saturating_sub(num_rounds_checking_most_locations) as usize
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
mod test {
    use std::collections::HashMap;
    use std::collections::HashSet;
    use std::panic::catch_unwind;

    use itertools::Itertools;
    use num_traits::One;
    use num_traits::Zero;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest_arbitrary_interop::arb;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use rayon::prelude::*;
    use test_strategy::proptest;
    use triton_vm::arithmetic_domain::ArithmeticDomain;
    use triton_vm::fri::Fri;
    use triton_vm::proof_item::ProofItem;
    use triton_vm::proof_stream::ProofStream;
    use triton_vm::twenty_first::shared_math::traits::PrimitiveRootOfUnity;
    use triton_vm::twenty_first::util_types::algebraic_hasher::Sponge;

    use crate::empty_stack;
    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::structure::tasm_object::decode_from_memory_with_size;
    use crate::test_helpers::*;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::twenty_first::shared_math::ntt::ntt;

    use super::*;

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
                domain_offset: domain.offset,
                domain_generator: domain.generator,
            }
        }

        pub fn call(
            &self,
            proof_stream: &mut ProofStream<Tip5>,
            nondeterminism: &NonDeterminism<BFieldElement>,
        ) -> Vec<(u32, XFieldElement)> {
            self.inner_verify(proof_stream, &mut nondeterminism.digests.clone())
                .unwrap()
        }

        /// Generate a proof, embedded in a proof stream.
        pub fn pseudorandom_fri_proof_stream(&self, seed: [u8; 32]) -> ProofStream<Tip5> {
            let max_degree = self.first_round_max_degree();
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let polynomial_coefficients = (0..=max_degree).map(|_| rng.gen()).collect_vec();

            let mut codeword = polynomial_coefficients;
            codeword.resize(self.domain_length as usize, XFieldElement::zero());
            let primitive_root =
                BFieldElement::primitive_root_of_unity(self.domain_length as u64).unwrap();
            let log_2_of_n = self.domain_length.ilog2();
            ntt::<XFieldElement>(&mut codeword, primitive_root, log_2_of_n);

            let mut proof_stream = ProofStream::<Tip5>::new();
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
            proof_stream: ProofStream<Tip5>,
        ) -> (Vec<BFieldElement>, NonDeterminism<BFieldElement>) {
            let digests = self
                .test_instance
                .extract_digests_required_for_proving(&proof_stream);
            self.set_up_stack_and_non_determinism_using_digests(proof_stream, digests)
        }

        fn set_up_stack_and_non_determinism_using_digests(
            &self,
            proof_stream: ProofStream<Tip5>,
            digests: Vec<Digest>,
        ) -> (Vec<BFieldElement>, NonDeterminism<BFieldElement>) {
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
            let vm_proof_iter_pointer = BFieldElement::zero();
            // uses highly specific knowledge about `BFieldCodec`
            let proof_iter_current_item_pointer = vm_proof_iter_pointer + BFieldElement::new(2);

            let fri_verify_pointer =
                encode_to_memory(&mut memory, vm_proof_iter_pointer, proof_stream);
            let proof_iter_pointer =
                encode_to_memory(&mut memory, fri_verify_pointer, self.test_instance);
            encode_to_memory(
                &mut memory,
                proof_iter_pointer,
                proof_iter_current_item_pointer,
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
            nondeterminism: &NonDeterminism<BFieldElement>,
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
            let mut proof_stream = decode_from_memory_with_size::<ProofStream<Tip5>>(
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
                revealed_indices_and_elements,
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
    struct TestCase {
        #[strategy(any::<ArbitraryFriVerify>().prop_map(|x| x.fri_verify()))]
        fri_verify: FriVerify,

        #[strategy(vec(arb(), #fri_verify.first_round_max_degree()))]
        polynomial_coefficients: Vec<XFieldElement>,
    }

    impl TestCase {
        /// A test case with no FRI rounds.
        fn tiny_case() -> Self {
            let fri_verify = FriVerify::new(BFieldElement::one(), 2, 2, 1);
            assert_eq!(0, fri_verify.num_rounds());

            Self {
                fri_verify,
                polynomial_coefficients: vec![XFieldElement::new_u64([42; 3])],
            }
        }

        /// A test case with 2 FRI rounds.
        fn small_case() -> Self {
            let fri_verify = FriVerify::new(BFieldElement::one(), 64, 2, 7);
            assert_eq!(2, fri_verify.num_rounds(), "test case must be meaningful");

            let coefficients = [
                92, 42, 91, 59, 86, 64, 5, 64, 74, 53, 54, 68, 54, 23, 24, 58, 15, 44, 33, 31, 38,
                97, 25, 69, 11, 67, 66, 33, 37, 58, 43, 14,
            ];
            assert_eq!(fri_verify.first_round_max_degree() + 1, coefficients.len());

            Self {
                fri_verify,
                polynomial_coefficients: coefficients
                    .map(|n| XFieldElement::new_u64([n; 3]))
                    .to_vec(),
            }
        }

        fn fri(&self) -> Fri<Tip5> {
            self.fri_verify.to_fri()
        }

        fn codeword(&self) -> Vec<XFieldElement> {
            let domain_length = self.fri_verify.domain_length;
            let mut codeword = self.polynomial_coefficients.clone();
            codeword.resize(domain_length as usize, XFieldElement::zero());

            let root_of_unity =
                BFieldElement::primitive_root_of_unity(domain_length as u64).unwrap();
            let log_2_of_n = domain_length.ilog2();
            ntt::<XFieldElement>(&mut codeword, root_of_unity, log_2_of_n);
            codeword
        }

        fn proof_items(&self) -> Vec<ProofItem> {
            let fri = self.fri();
            let codeword = self.codeword();
            let mut proof_stream = ProofStream::new();
            fri.prove(&codeword, &mut proof_stream).unwrap();

            proof_stream.items
        }

        fn proof_stream(&self) -> ProofStream<Tip5> {
            ProofStream {
                items: self.proof_items(),
                items_index: 0,
                sponge: Tip5::init(),
            }
        }

        fn initial_state(&self) -> ProcedureInitialState {
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
        let verify_result = fri.verify(&mut vm_proof_iter, &mut None);
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
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench() {
        let expansion_factor = 4;
        let domain_length = 1 << 10;
        let offset = BFieldElement::new(7);
        let num_collinearity_checks = 80;
        // tiny parameters for FRI yes, but the bench framework is awful atm
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
}
