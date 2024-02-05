use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::RATE;
use triton_vm::twenty_first::util_types::algebraic_hasher::SpongeHasher;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Squeeze;

impl BasicSnippet for Squeeze {
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
        "tasm_hashing_sponge_hasher_squeeze".to_string()
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
    use std::collections::HashMap;

    use arbitrary::*;
    use rand::rngs::StdRng;
    use rand::*;
    use triton_vm::twenty_first::shared_math::tip5::Tip5State;

    use crate::empty_stack;
    use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
    use crate::rust_shadowing_helper_functions;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::*;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasherState;

    use super::*;

    impl Procedure for Squeeze {
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
            let produce = Tip5::squeeze(sponge_state.as_mut().unwrap());
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
            let mut init_memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            init_memory.insert(DYN_MALLOC_ADDRESS, random());

            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);

            ProcedureInitialState {
                stack: empty_stack(),
                nondeterminism: NonDeterminism::default().with_ram(init_memory),
                public_input: Vec::default(),
                sponge_state: Some(Tip5State::arbitrary(&mut unstructured).unwrap()),
            }
        }
    }

    #[test]
    fn squeeze_test() {
        ShadowedProcedure::new(Squeeze).test();
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench() {
        ShadowedProcedure::new(Squeeze).bench();
    }
}
