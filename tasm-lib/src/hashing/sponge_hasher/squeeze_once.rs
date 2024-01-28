use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::RATE;
use triton_vm::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::data_type::{ArrayType, DataType};
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;

pub struct SqueezeOnce;

impl BasicSnippet for SqueezeOnce {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Bfe,
                length: RATE,
            })),
            "produce".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_sponge_hasher_squeeze_once".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, RATE, "Code assumes RATE is 10");
        let entrypoint = self.entrypoint();
        let dyn_malloc_label = library.import(Box::new(DynMalloc));

        triton_asm!(
            {entrypoint}:
                // _
                sponge_squeeze
                // _ [word_9..word_0]


                // Allocate memory for the returned array
                push {Tip5::RATE}
                call {dyn_malloc_label}
                // _ [word_9..word_0] *array

                // Write words to array
                write_mem 5
                write_mem 5
                // _ (*array + 10)

                push -10
                add
                // _ *array

                return
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::*;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasherState;
    use crate::{empty_stack, rust_shadowing_helper_functions};
    use arbitrary::*;
    use rand::rngs::StdRng;
    use rand::*;
    use std::collections::HashMap;
    use triton_vm::twenty_first::shared_math::tip5::Tip5State;

    impl Procedure for SqueezeOnce {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge_state: &mut Option<VmHasherState>,
        ) -> Vec<BFieldElement> {
            let mut array_pointer =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(RATE, memory);
            stack.push(array_pointer);
            let produce = Tip5::squeeze_once(sponge_state.as_mut().unwrap());
            for elem in produce.into_iter() {
                memory.insert(array_pointer, elem);
                array_pointer.increment();
            }

            Vec::default()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);
            let mut init_memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            init_memory.insert(DYN_MALLOC_ADDRESS, random());

            ProcedureInitialState {
                stack: empty_stack(),
                nondeterminism: NonDeterminism::default().with_ram(init_memory),
                public_input: Vec::default(),
                sponge_state: Some(Tip5State::arbitrary(&mut unstructured).unwrap()),
            }
        }
    }

    #[test]
    fn squeeze_once_test() {
        ShadowedProcedure::new(SqueezeOnce).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench() {
        ShadowedProcedure::new(SqueezeOnce).bench();
    }
}
