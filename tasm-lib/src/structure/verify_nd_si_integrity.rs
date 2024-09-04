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

    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::accessor::Accessor;
    use crate::traits::accessor::AccessorInitialState;
    use crate::traits::accessor::ShadowedAccessor;
    use crate::traits::rust_shadow::RustShadow;
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::RngCore;
    use rand::SeedableRng;

    use super::*;

    #[test]
    fn unit_test() {
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

    impl<'a, T: TasmObject + BFieldCodec + Arbitrary<'a>> Accessor for VerifyNdSiIntegrity<T> {
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
            fn prepare_random_object<'a, T: Arbitrary<'a>>(randomness: &'static [u8; 100000]) -> T {
                let unstructured = Unstructured::new(randomness);
                T::arbitrary_take_rest(unstructured).unwrap()
            }

            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let t: T = unsafe {
                use std::ptr::addr_of;
                use std::ptr::addr_of_mut;
                static mut RANDOMNESS: [u8; 100_000] = [0u8; 100_000];
                rng.fill_bytes(&mut *addr_of_mut!(RANDOMNESS));
                prepare_random_object(&*addr_of!(RANDOMNESS))
            };

            let mut memory = HashMap::default();
            let address: u32 = rng.gen_range(0..(1 << 30));
            let address = bfe!(address);
            encode_to_memory(&mut memory, address, &t);

            AccessorInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![address]].concat(),
                memory,
            }
        }
    }
}
