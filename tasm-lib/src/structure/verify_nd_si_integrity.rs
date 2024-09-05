use std::marker::PhantomData;

use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;
use crate::prelude::TasmObject;

pub struct VerifyNdSiIntegrity<T: TasmObject + BFieldCodec> {
    _phantom_data: PhantomData<T>,
}

impl<T: TasmObject + BFieldCodec> BasicSnippet for VerifyNdSiIntegrity<T> {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*struct".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "struct size".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_structure_verify_nd_si_integrity___{}",
            T::label_friendly_name()
        )
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

    use crate::memory::encode_to_memory;
    use crate::neptune::neptune_like_types_for_tests::*;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::accessor::Accessor;
    use crate::traits::accessor::AccessorInitialState;
    use crate::traits::accessor::ShadowedAccessor;
    use crate::traits::rust_shadow::RustShadow;
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use num_traits::ConstZero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;

    use super::*;

    #[test]
    fn test_pbt_simple_struct() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct TestStruct {
            a: Vec<u128>,
            b: Digest,
            c: Vec<Digest>,
        }
        let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_option_stat_sized_elem() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct TestStruct {
            a: Option<Digest>,
        }
        let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
    }

    #[test]
    fn test_option_dyn_sized_elem() {
        #[derive(Debug, Clone, TasmObject, BFieldCodec, Arbitrary)]
        struct TestStruct {
            a: Option<Vec<u128>>,
            b: Digest,
            c: Vec<Vec<BFieldElement>>,
            d: Option<Vec<Option<BFieldElement>>>,
        }
        let snippet: VerifyNdSiIntegrity<TestStruct> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).test();
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

    impl<T: TasmObject + BFieldCodec> VerifyNdSiIntegrity<T> {
        fn initial_state(&self, address: BFieldElement, t: T) -> AccessorInitialState {
            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, address, &t);

            AccessorInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![address]].concat(),
                memory,
            }
        }
    }

    impl<T: TasmObject + BFieldCodec + for<'a> Arbitrary<'a> + Debug> Accessor
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

            stack.push(bfe!(obj.encode().len() as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> AccessorInitialState {
            fn prepare_random_object<T: for<'a> Arbitrary<'a>>(randomness: &[u8; 100000]) -> T {
                let unstructured = Unstructured::new(randomness);
                T::arbitrary_take_rest(unstructured).unwrap()
            }

            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let t: T = {
                let mut randomness = [0u8; 100000];
                rng.fill(&mut randomness);
                prepare_random_object(&randomness)
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
}

#[cfg(test)]
mod benches {
    use crate::neptune::neptune_like_types_for_tests::ProofCollectionLookalike;
    use crate::traits::accessor::ShadowedAccessor;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_pbt_proof_collection_lookalike() {
        let snippet: VerifyNdSiIntegrity<ProofCollectionLookalike> = VerifyNdSiIntegrity {
            _phantom_data: PhantomData,
        };
        ShadowedAccessor::new(snippet).bench();
    }
}
