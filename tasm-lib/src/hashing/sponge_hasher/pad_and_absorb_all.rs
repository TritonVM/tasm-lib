use triton_vm::prelude::*;

use crate::list::LIST_METADATA_SIZE;
use crate::prelude::*;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PadAndAbsorbAll;

impl BasicSnippet for PadAndAbsorbAll {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::List(Box::new(DataType::Bfe)), "input".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_sponge_hasher_pad_and_absorb_all".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let hash_absorb_snippet_subroutine =
            library.import(Box::new(crate::hashing::absorb_multiple::AbsorbMultiple));
        triton_asm!(
            {entrypoint}:
                // _ *input_list

                read_mem 1
                // _ length (*input_list - 1)

                addi {LIST_METADATA_SIZE + 1}
                // _ length *first_element

                pick 1
                // _ *first_element length

                call {hash_absorb_snippet_subroutine}
                // _

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use triton_vm::prelude::*;
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions::list::insert_random_list;
    use crate::test_prelude::*;

    impl PadAndAbsorbAll {
        fn init_memory_and_stack(
            &self,
            input_length: usize,
        ) -> (HashMap<BFieldElement, BFieldElement>, Vec<BFieldElement>) {
            let list_pointer: BFieldElement = rand::random();
            let init_stack = [empty_stack(), vec![list_pointer]].concat();
            let mut init_memory = HashMap::default();
            insert_random_list(&DataType::Bfe, list_pointer, input_length, &mut init_memory);

            (init_memory, init_stack)
        }
    }

    impl Procedure for PadAndAbsorbAll {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            let input_pointer: BFieldElement = stack.pop().unwrap();
            let first_word = input_pointer + BFieldElement::new(LIST_METADATA_SIZE as u64);
            let input_length = memory[&input_pointer].value();
            let input = (0..input_length)
                .map(|i| memory[&(BFieldElement::new(i) + first_word)])
                .collect_vec();
            sponge.pad_and_absorb_all(&input);

            Vec::default()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);
            let input_length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 100,
                Some(BenchmarkCase::WorstCase) => 10000,
                None => rng.random_range(0..200),
            };
            let (memory, stack) = self.init_memory_and_stack(input_length);

            ProcedureInitialState {
                stack,
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: Vec::default(),
                sponge: Some(Tip5::arbitrary(&mut unstructured).unwrap()),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let empty_input_empty_sponge = {
                let (memory, stack) = self.init_memory_and_stack(0);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge: Some(Tip5::init()),
                }
            };
            let empty_input_random_sponge = {
                let (memory, stack) = self.init_memory_and_stack(0);
                let mut rng = StdRng::from_seed([12u8; 32]);
                let mut bytes = [0u8; 400];
                rng.fill_bytes(&mut bytes);
                let mut unstructured = Unstructured::new(&bytes);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge: Some(Tip5::arbitrary(&mut unstructured).unwrap()),
                }
            };
            let length_one_input_empty_sponge = {
                let (memory, stack) = self.init_memory_and_stack(1);
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge: Some(Tip5::init()),
                }
            };
            vec![
                empty_input_empty_sponge,
                empty_input_random_sponge,
                length_one_input_empty_sponge,
            ]
        }
    }

    #[test]
    fn pad_and_absorb_all_test() {
        ShadowedProcedure::new(PadAndAbsorbAll).test();
    }
}
