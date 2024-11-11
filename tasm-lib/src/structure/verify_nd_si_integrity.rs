use std::fmt::Debug;
use std::marker::PhantomData;

use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;
use crate::prelude::TasmObject;

/// Verify size-indicator integrity of preloaded data, return size.
///
/// Crashes the VM if the structure in question is not entirely contained within
/// the non-deterministic section of memory as defined in the memory layout.
#[derive(Clone, Debug)]
pub struct VerifyNdSiIntegrity<PreloadedData: TasmObject + BFieldCodec + Clone + Debug> {
    _phantom_data: PhantomData<PreloadedData>,
}

impl<T: TasmObject + BFieldCodec + Clone + Debug> Default for VerifyNdSiIntegrity<T> {
    fn default() -> Self {
        Self {
            _phantom_data: PhantomData,
        }
    }
}

impl<T: TasmObject + BFieldCodec + Clone + Debug> BasicSnippet for VerifyNdSiIntegrity<T> {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*struct".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
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
    use std::collections::HashMap;
    use std::fmt::Debug;

    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use num_traits::ConstZero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use twenty_first::util_types::mmr::mmr_successor_proof::MmrSuccessorProof;

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::neptune::neptune_like_types_for_tests::*;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::accessor::Accessor;
    use crate::traits::accessor::AccessorInitialState;
    use crate::traits::accessor::ShadowedAccessor;
    use crate::traits::rust_shadow::RustShadow;

    impl<T: TasmObject + BFieldCodec + for<'a> Arbitrary<'a> + Debug + Clone> VerifyNdSiIntegrity<T> {
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

    impl<T: TasmObject + BFieldCodec + for<'a> Arbitrary<'a> + Debug + Clone> Accessor
        for VerifyNdSiIntegrity<T>
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
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let t: T = {
                let mut randomness = [0u8; 100000];
                rng.fill(&mut randomness);
                self.prepare_random_object(&randomness)
            };

            let address: u32 = rng.gen_range(0..(1 << 30));
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

        #[test]
        fn test_pbt_simple_struct() {
            let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
            ShadowedAccessor::new(snippet).test();
        }

        #[test]
        fn struct_not_contained_in_nd_region() {
            let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

            let t: TestStruct = snippet.prepare_random_object(&[]);
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
            let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

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
            let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

            let begin_address = bfe!(4);
            let mut init_state =
                snippet.initial_state(begin_address, snippet.prepare_random_object(&[]));
            let true_value = init_state.memory[&begin_address];
            init_state
                .memory
                .insert(begin_address, true_value + bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), init_state.into(), &[])
        }

        #[test]
        fn lie_about_digest_vec_len() {
            let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

            let begin_address = bfe!(4);
            let mut init_state =
                snippet.initial_state(begin_address, snippet.prepare_random_object(&[42u8; 20000]));
            let vec_digest_len_indicator = begin_address + bfe!(1);
            let true_value = init_state.memory[&vec_digest_len_indicator];
            init_state
                .memory
                .insert(vec_digest_len_indicator, true_value + bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), init_state.into(), &[])
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

        #[test]
        fn test_option_stat_sized_elem() {
            let snippet: VerifyNdSiIntegrity<StatSizedPayload> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
            ShadowedAccessor::new(snippet).test();
        }

        #[test]
        fn test_option_dyn_sized_elem() {
            let snippet: VerifyNdSiIntegrity<DynSizedPayload> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
            ShadowedAccessor::new(snippet).test();
        }

        #[test]
        fn lie_about_option_payload_field_size() {
            let snippet: VerifyNdSiIntegrity<DynSizedPayload> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

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
                &[],
            );
        }

        #[test]
        fn illegal_discriminant_value_for_option() {
            let snippet: VerifyNdSiIntegrity<DynSizedPayload> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

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
                &[],
            );
        }

        #[test]
        fn lie_about_option_payload_size() {
            let snippet: VerifyNdSiIntegrity<DynSizedPayload> = VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };

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

            test_assertion_failure(&ShadowedAccessor::new(snippet.clone()), add_one.into(), &[]);

            let mut sub_one = true_init_state.clone();
            sub_one
                .memory
                .insert(len_of_dyn_sized_list_elem_0, true_value - bfe!(1));

            test_assertion_failure(&ShadowedAccessor::new(snippet), sub_one.into(), &[]);
        }
    }

    #[test]
    fn test_stat_sized_tuple() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct TestStruct {
            a: (Digest, XFieldElement),
        }
        let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_dyn_state_sized_tuple_right_dyn() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct RightIsDyn {
            a: (Digest, Vec<XFieldElement>),
        }
        let snippet: VerifyNdSiIntegrity<RightIsDyn> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_dyn_state_sized_tuple_left_dyn() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct LeftIsDyn {
            a: (Vec<XFieldElement>, Digest),
        }
        let snippet: VerifyNdSiIntegrity<LeftIsDyn> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_dyn_state_sized_tuple_both_dyn() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct BothDyn {
            a: (Vec<XFieldElement>, Vec<XFieldElement>),
        }
        let snippet: VerifyNdSiIntegrity<BothDyn> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_pbt_proof() {
        let snippet: VerifyNdSiIntegrity<Proof> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_pbt_coin() {
        let snippet: VerifyNdSiIntegrity<CoinLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn pbt_utxo() {
        let snippet: VerifyNdSiIntegrity<UtxoLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn pbt_salted_utxos() {
        let snippet: VerifyNdSiIntegrity<SaltedUtxosLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn pbt_collect_lock_scripts_witness() {
        let snippet: VerifyNdSiIntegrity<CollectLockScriptsWitnessLookalike> =
            VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn pbt_collect_type_scripts_witness() {
        let snippet: VerifyNdSiIntegrity<CollectTypeScriptsWitnessLookalike> =
            VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_pbt_claim() {
        let snippet: VerifyNdSiIntegrity<Claim> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_neptune_coins() {
        let snippet: VerifyNdSiIntegrity<NeptuneCoinsLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_option_neptune_coins() {
        let snippet: VerifyNdSiIntegrity<Option<NeptuneCoinsLookalike>> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_chunk() {
        let snippet: VerifyNdSiIntegrity<ChunkLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_chunk_dictionary() {
        let snippet: VerifyNdSiIntegrity<ChunkDictionaryLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_pbt_proof_collection_lookalike() {
        let snippet: VerifyNdSiIntegrity<ProofCollectionLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_absolute_index_set_lookalike() {
        let snippet: VerifyNdSiIntegrity<AbsoluteIndexSetLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_removal_record_lookalike() {
        let snippet: VerifyNdSiIntegrity<RemovalRecordLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_addition_record_lookalike() {
        let snippet: VerifyNdSiIntegrity<AdditionRecordLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_public_announcement_lookalike() {
        let snippet: VerifyNdSiIntegrity<PublicAnnouncementLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_timestamp_lookalike() {
        let snippet: VerifyNdSiIntegrity<TimestampLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_transaction_kernel_lookalike() {
        let snippet: VerifyNdSiIntegrity<TransactionKernelLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_kernel_to_outputs_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<KernelToOutputsWitnessLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_merge_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<MergeWitnessLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_ms_membership_proof_lookalike() {
        let snippet: VerifyNdSiIntegrity<MsMembershipProofLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_removal_records_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<RemovalRecordsIntegrityWitnessLookalike> =
            VerifyNdSiIntegrity {
                _phantom_data: PhantomData,
            };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_update_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<UpdateWitnessLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_lock_script_and_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<LockScriptAndWitnessLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_mutator_set_accumulator_lookalike() {
        let snippet: VerifyNdSiIntegrity<MutatorSetAccumulatorLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_primitive_witness_lookalike() {
        let snippet: VerifyNdSiIntegrity<PrimitiveWitnessLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_mmr_successor_proof_auto_derived_code() {
        // TODO:
        // Test to verify that the auto-derived code for `MmrSuccessorProof`
        // handles size-indicator verification correctly. `MmrSuccessorProof`
        // is wrapped in another type since `Arbitrary` was not implemented for
        // upstream `MmrSuccessorProof` at the time of writing. Once it is,
        // feel free to simplify this test.
        #[derive(Debug, Clone, BFieldCodec, TasmObject)]
        struct MmrSuccessorProofWrapper {
            pub msp: MmrSuccessorProof,
        }

        impl<'a> Arbitrary<'a> for MmrSuccessorProofWrapper {
            fn arbitrary(u: &mut Unstructured) -> arbitrary::Result<Self> {
                let digests = <Vec<Digest> as Arbitrary>::arbitrary(u)?;
                Ok(Self {
                    msp: MmrSuccessorProof { paths: digests },
                })
            }
        }
        let snippet: VerifyNdSiIntegrity<MmrSuccessorProofWrapper> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::neptune::neptune_like_types_for_tests::ProofCollectionLookalike;
    use crate::neptune::neptune_like_types_for_tests::TransactionKernelLookalike;
    use crate::traits::accessor::ShadowedAccessor;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench_proof_collection_lookalike() {
        let snippet: VerifyNdSiIntegrity<ProofCollectionLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).bench();
    }

    #[test]
    fn bench_transaction_kernel_lookalike() {
        let snippet: VerifyNdSiIntegrity<TransactionKernelLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).bench();
    }
}
