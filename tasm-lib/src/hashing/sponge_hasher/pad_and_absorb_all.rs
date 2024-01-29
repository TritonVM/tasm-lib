use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::data_type::DataType;
use crate::library::Library;
use crate::list::ListType;
use crate::traits::basic_snippet::BasicSnippet;

pub struct PadAndAbsorbAll {
    pub list_type: ListType,
}

impl BasicSnippet for PadAndAbsorbAll {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::List(Box::new(DataType::Bfe)), "input".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_hashing_sponge_hasher_pad_and_absorb_all_{}",
            self.list_type
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let hash_absorb_snippet_subroutine =
            library.import(Box::new(crate::hashing::absorb::Absorb));
        let list_metadata_size = self.list_type.metadata_size();
        triton_asm!(
            {entrypoint}:
                // _ *input_list

                read_mem 1
                // _ length (*input_list - 1)

                push {list_metadata_size + 1}
                add
                // _ length *first_element

                swap 1
                // _ *first_element length

                call {hash_absorb_snippet_subroutine}
                // _

                return
        )
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use arbitrary::*;
    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::*;
    use triton_vm::prelude::*;
    use triton_vm::twenty_first::shared_math::tip5::Tip5State;
    use triton_vm::twenty_first::util_types::algebraic_hasher::SpongeHasher;

    use crate::empty_stack;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::*;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasherState;

    use super::*;

    impl PadAndAbsorbAll {
        fn init_memory_and_stack(
            &self,
            input_length: usize,
        ) -> (HashMap<BFieldElement, BFieldElement>, Vec<BFieldElement>) {
            let list_pointer: BFieldElement = random();
            let init_stack = [empty_stack(), vec![list_pointer]].concat();
            let mut init_memory = HashMap::default();
            self.list_type.rust_shadowing_insert_random_list(
                &DataType::Bfe,
                list_pointer,
                input_length,
                &mut init_memory,
            );

            (init_memory, init_stack)
        }
    }

    impl Procedure for PadAndAbsorbAll {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism<BFieldElement>,
            _public_input: &[BFieldElement],
            sponge_state: &mut Option<VmHasherState>,
        ) -> Vec<BFieldElement> {
            let input_pointer: BFieldElement = stack.pop().unwrap();
            let first_word =
                input_pointer + BFieldElement::new(self.list_type.metadata_size() as u64);
            let input_length = memory[&input_pointer].value();
            let input = (0..input_length)
                .map(|i| memory[&(BFieldElement::new(i) + first_word)])
                .collect_vec();
            Tip5::pad_and_absorb_all(sponge_state.as_mut().unwrap(), &input);

            Vec::default()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);
            let input_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 100,
                Some(BenchmarkCase::WorstCase) => 10000,
                None => rng.gen_range(0..200),
            };
            let (memory, stack) = self.init_memory_and_stack(input_length);

            ProcedureInitialState {
                stack,
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: Vec::default(),
                sponge_state: Some(Tip5State::arbitrary(&mut unstructured).unwrap()),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let empty_input_empty_sponge_state = {
                let (memory, stack) = self.init_memory_and_stack(0);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge_state: Some(Tip5::init()),
                }
            };
            let empty_input_random_sponge_state = {
                let (memory, stack) = self.init_memory_and_stack(0);
                let mut rng: StdRng = SeedableRng::from_seed([12u8; 32]);
                let mut bytes = [0u8; 400];
                rng.fill_bytes(&mut bytes);
                let mut unstructured = Unstructured::new(&bytes);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge_state: Some(Tip5State::arbitrary(&mut unstructured).unwrap()),
                }
            };
            let length_one_input_empty_sponge_state = {
                let (memory, stack) = self.init_memory_and_stack(1);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge_state: Some(Tip5::init()),
                }
            };
            vec![
                empty_input_empty_sponge_state,
                empty_input_random_sponge_state,
                length_one_input_empty_sponge_state,
            ]
        }
    }

    #[test]
    fn pad_and_absorb_all_test() {
        ShadowedProcedure::new(PadAndAbsorbAll {
            list_type: ListType::Safe,
        })
        .test();
        ShadowedProcedure::new(PadAndAbsorbAll {
            list_type: ListType::Unsafe,
        })
        .test();
    }
}
