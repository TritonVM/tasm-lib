use std::fmt::Debug;
use std::marker::PhantomData;

use triton_vm::prelude::*;

use crate::prelude::*;

/// Verify size-indicator integrity of preloaded data, return size.
///
/// Crashes the VM if the structure in question is not entirely contained within
/// the non-deterministic section of memory as defined in the memory layout.
#[derive(Clone, Debug)]
pub struct VerifyNdSiIntegrity<PreloadedData: TasmObject + Clone + Debug> {
    _phantom_data: PhantomData<PreloadedData>,
}

impl<T: TasmObject + Clone + Debug> Default for VerifyNdSiIntegrity<T> {
    fn default() -> Self {
        Self {
            _phantom_data: PhantomData,
        }
    }
}

impl<T: TasmObject + Clone + Debug> BasicSnippet for VerifyNdSiIntegrity<T> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*struct".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "struct size".to_owned())]
    }

    fn entrypoint(&self) -> String {
        let name = T::label_friendly_name();
        format!("tasmlib_structure_verify_nd_si_integrity___{name}")
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let si_integrity_check_code = T::compute_size_and_assert_valid_size_indicator(library);

        triton_asm!(
            {entrypoint}:
                // _ *struct

                dup 0
                {&si_integrity_check_code}
                // _ *struct calculated_size

                /* Verify that both pointer and end of struct is in ND-region */
                dup 1
                // _ *struct calculated_size *struct

                pop_count // verifies that `*struct` is a valid u32
                pop 1
                // _ *struct calculated_size

                swap 1
                dup 1
                add
                // _ calculated_size *end_of_struct

                pop_count // verifies that end of struct is located in ND-region
                pop 1
                // _ calculated_size

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use num_traits::ConstZero;
    use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::neptune::neptune_like_types_for_tests::*;
    use crate::test_prelude::*;

    impl<T> VerifyNdSiIntegrity<T>
    where
        T: TasmObject + BFieldCodec + for<'a> Arbitrary<'a> + Debug + Clone,
    {
        fn initial_state(&self, address: BFieldElement, t: T) -> AccessorInitialState {
            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, address, &t);

            AccessorInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![address]].concat(),
                memory,
            }
        }

        fn prepare_random_object(&self, randomness: &[u8]) -> T {
            let unstructured = Unstructured::new(randomness);
            T::arbitrary_take_rest(unstructured).unwrap()
        }
    }

    impl<T> Accessor for VerifyNdSiIntegrity<T>
    where
        T: TasmObject + for<'a> Arbitrary<'a> + Debug + Clone,
    {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
        ) {
            // If the type can be decoded then it must have valid size indicators
            let pointer = stack.pop().unwrap();
            let obj = T::decode_from_memory(memory, pointer).unwrap();
            let encoding_len = obj.encode().len();
            let encoding_len: u32 = encoding_len.try_into().unwrap();

            // Verify contained in ND-region
            let start_address: u32 = pointer.value().try_into().unwrap();
            let _end_address = start_address.checked_add(encoding_len).unwrap();

            stack.push(bfe!(obj.encode().len() as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            let mut rng = StdRng::from_seed(seed);

            let t: T = {
                let mut randomness = [0u8; 100000];
                rng.fill(&mut randomness);
                self.prepare_random_object(&randomness)
            };

            let address: u32 = rng.random_range(0..(1 << 30));
            let address = bfe!(address);
            self.initial_state(address, t)
        }

        fn corner_case_initial_states(&self) -> Vec<AccessorInitialState> {
            // This *should* always return `None` if `T: Option<S>`, and empty
            // vec if type is Vec<T>. So some notion of "empty" or default.
            let empty_struct: T = {
                let unstructured = Unstructured::new(&[]);
                T::arbitrary_take_rest(unstructured).unwrap()
            };

            println!("empty_struct:\n{empty_struct:?}");
            let empty_struct_at_zero = self.initial_state(BFieldElement::ZERO, empty_struct);

            vec![empty_struct_at_zero]
        }
    }

    macro_rules! test_case {
        (fn $test_name:ident for $t:ty) => {
            #[test]
            fn $test_name() {
                ShadowedAccessor::new(VerifyNdSiIntegrity::<$t>::default()).test();
            }
        };
        (fn $test_name:ident for new type $t:ty: $($type_declaration:tt)*) => {
            #[test]
            fn $test_name() {
                #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
                $($type_declaration)*
                ShadowedAccessor::new(VerifyNdSiIntegrity::<$t>::default()).test();
            }
        };
    }

    mod simple_struct {
        use super::*;
        use crate::test_helpers::negative_test;
        use crate::test_helpers::test_assertion_failure;

        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct TestStruct {
            a: Vec<u128>,
            b: Digest,
            c: Vec<Digest>,
        }

        test_case! { fn test_pbt_simple_struct for TestStruct }

        #[test]
        fn struct_not_contained_in_nd_region() {
            let snippet = VerifyNdSiIntegrity::<TestStruct>::default();

            let t = snippet.prepare_random_object(&[]);
            let begin_address = bfe!((1u64 << 32) - 4);
            let init_state = snippet.initial_state(begin_address, t.clone());

            let actual_size = t.encode().len();
            let end_address = begin_address + bfe!(actual_size as u64);
            let expected_err =
                InstructionError::OpStackError(OpStackError::FailedU32Conversion(end_address));
            negative_test(
                &ShadowedAccessor::new(snippet),
                init_state.into(),
                &[expected_err],
            )
        }

        #[test]
        fn struct_does_not_start_in_nd_region() {
            let snippet = VerifyNdSiIntegrity::<TestStruct>::default();

            let begin_address = bfe!(-4);
            let init_state =
                snippet.initial_state(begin_address, snippet.prepare_random_object(&[]));
            let expected_err =
                InstructionError::OpStackError(OpStackError::FailedU32Conversion(begin_address));
            negative_test(
                &ShadowedAccessor::new(snippet),
                init_state.into(),
                &[expected_err],
            )
        }

        #[test]
        fn lie_about_digest_vec_size() {
            let snippet = VerifyNdSiIntegrity::<TestStruct>::default();

            let begin_address = bfe!(4);
            let mut init_state =
                snippet.initial_state(begin_address, snippet.prepare_random_object(&[]));
            let true_value = init_state.memory[&begin_address];
            init_state
                .memory
                .insert(begin_address, true_value + bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), init_state.into(), &[181])
        }

        #[test]
        fn lie_about_digest_vec_len() {
            let snippet = VerifyNdSiIntegrity::<TestStruct>::default();

            let begin_address = bfe!(4);
            let mut init_state =
                snippet.initial_state(begin_address, snippet.prepare_random_object(&[42u8; 20000]));
            let vec_digest_len_indicator = begin_address + bfe!(1);
            let true_value = init_state.memory[&vec_digest_len_indicator];
            init_state
                .memory
                .insert(vec_digest_len_indicator, true_value + bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), init_state.into(), &[181])
        }
    }

    mod option_types {
        use super::*;
        use crate::test_helpers::test_assertion_failure;

        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct StatSizedPayload {
            a: Option<Digest>,
        }

        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary, Default)]
        struct DynSizedPayload {
            a: Option<Vec<u128>>,
            b: Digest,
            c: Vec<Vec<BFieldElement>>,
            d: Option<Vec<Option<BFieldElement>>>,
        }

        test_case! {fn test_option_stat_sized_elem for StatSizedPayload }
        test_case! {fn test_option_dyn_sized_elem for DynSizedPayload }

        #[test]
        fn lie_about_option_payload_field_size() {
            let snippet = VerifyNdSiIntegrity::<DynSizedPayload>::default();

            let begin_address = bfe!(4);
            let randomness = rand::random::<[u8; 100_000]>();
            let obj = snippet.prepare_random_object(&randomness);
            let true_init_state = snippet.initial_state(begin_address, obj.clone());

            /*  Lie about size of field 'd'*/
            let mut manipulated_si_outer = true_init_state.clone();
            let outer_option_payload_si_ptr = begin_address; // field size-indicator of `d` field
            let true_value = true_init_state.memory[&outer_option_payload_si_ptr];
            manipulated_si_outer
                .memory
                .insert(outer_option_payload_si_ptr, true_value + bfe!(1));

            test_assertion_failure(
                &ShadowedAccessor::new(snippet),
                manipulated_si_outer.into(),
                &[181],
            );
        }

        #[test]
        fn illegal_discriminant_value_for_option() {
            let snippet = VerifyNdSiIntegrity::<DynSizedPayload>::default();

            let obj = DynSizedPayload::default();
            let begin_address = bfe!(4);
            let mut manipulated_init_state = snippet.initial_state(begin_address, obj.clone());
            let option_discriminant_ptr = begin_address + bfe!(1);
            manipulated_init_state
                .memory
                .insert(option_discriminant_ptr, bfe!(2));

            test_assertion_failure(
                &ShadowedAccessor::new(snippet.clone()),
                manipulated_init_state.into(),
                &[200],
            );
        }

        #[test]
        fn lie_about_option_payload_size() {
            let snippet = VerifyNdSiIntegrity::<DynSizedPayload>::default();

            let obj = DynSizedPayload {
                d: Some(vec![Some(bfe!(14)), None, Some(bfe!(15))]),
                ..Default::default()
            };
            let begin_address = bfe!(4);
            let true_init_state = snippet.initial_state(begin_address, obj.clone());

            /*  Lie about size of payload of outer Some(...)*/
            let mut add_one = true_init_state.clone();
            let len_of_dyn_sized_list_elem_0 = begin_address + bfe!(3);
            let true_value = true_init_state.memory[&len_of_dyn_sized_list_elem_0];
            add_one
                .memory
                .insert(len_of_dyn_sized_list_elem_0, true_value + bfe!(1));

            test_assertion_failure(
                &ShadowedAccessor::new(snippet.clone()),
                add_one.into(),
                &[211],
            );

            let mut sub_one = true_init_state.clone();
            sub_one
                .memory
                .insert(len_of_dyn_sized_list_elem_0, true_value - bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), sub_one.into(), &[211]);
        }
    }

    test_case! { fn test_stat_sized_tuple for new type TestStruct:
        struct TestStruct { field: (Digest, XFieldElement) }
    }

    test_case! { fn test_dyn_state_sized_tuple_right_dyn for new type RightIsDyn:
        struct RightIsDyn { field: (Digest, Vec<XFieldElement>) }
    }

    test_case! { fn test_dyn_state_sized_tuple_left_dyn for new type LeftIsDyn:
        struct LeftIsDyn { field: (Vec<XFieldElement>, Digest) }
    }

    test_case! { fn test_dyn_state_sized_tuple_both_dyn for new type BothDyn:
        struct BothDyn { field: (Vec<XFieldElement>, Vec<XFieldElement>) }
    }

    test_case! { fn proof for Proof }
    test_case! { fn coin for CoinLookalike }
    test_case! { fn utxo for UtxoLookalike }
    test_case! { fn salted_utxos for SaltedUtxosLookalike }
    test_case! { fn collect_lock_scripts_witness for CollectLockScriptsWitnessLookalike }
    test_case! { fn collect_type_scripts_witness for CollectTypeScriptsWitnessLookalike }
    test_case! { fn claim for Claim }
    test_case! { fn neptune_coins for NeptuneCoinsLookalike }
    test_case! { fn option_neptune_coins for Option<NeptuneCoinsLookalike> }
    test_case! { fn chunk for ChunkLookalike }
    test_case! { fn chunk_dictionary for ChunkDictionaryLookalike }
    test_case! { fn proof_collection for ProofCollectionLookalike }
    test_case! { fn absolute_index_set for AbsoluteIndexSetLookalike }
    test_case! { fn removal_record for RemovalRecordLookalike }
    test_case! { fn addition_record for AdditionRecordLookalike }
    test_case! { fn public_announcement for PublicAnnouncementLookalike }
    test_case! { fn timestamp for TimestampLookalike }
    test_case! { fn transaction_kernel for TransactionKernelLookalike }
    test_case! { fn kernel_to_outputs_witness for KernelToOutputsWitnessLookalike }
    test_case! { fn merge_witness for MergeWitnessLookalike }
    test_case! { fn ms_membership_proof for MsMembershipProofLookalike }
    test_case! { fn removal_records_witness for RemovalRecordsIntegrityWitnessLookalike }
    test_case! { fn update_witness for UpdateWitnessLookalike }
    test_case! { fn lock_script_and_witness for LockScriptAndWitnessLookalike }
    test_case! { fn mutator_set_accumulator for MutatorSetAccumulatorLookalike }
    test_case! { fn primitive_witness for PrimitiveWitnessLookalike }
    test_case! { fn mmr_successor_proof for MmrSuccessorProof }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::neptune::neptune_like_types_for_tests::ProofCollectionLookalike;
    use crate::neptune::neptune_like_types_for_tests::TransactionKernelLookalike;
    use crate::test_prelude::*;

    #[test]
    fn bench_proof_collection_lookalike() {
        ShadowedAccessor::new(VerifyNdSiIntegrity::<ProofCollectionLookalike>::default()).bench();
    }

    #[test]
    fn bench_transaction_kernel_lookalike() {
        ShadowedAccessor::new(VerifyNdSiIntegrity::<TransactionKernelLookalike>::default()).bench();
    }
}
