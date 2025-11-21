use triton_vm::prelude::*;
use twenty_first::tip5::RATE;

use crate::data_type::ArrayType;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Absorb;

impl BasicSnippet for Absorb {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Bfe,
                length: RATE,
            })),
            "input".to_owned(),
        )]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_sponge_hasher_absorb".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, RATE, "Code assumes RATE is 10");

        triton_asm!(
            {self.entrypoint()}:
                // _ *input

                push 0
                push 0
                push 0
                push 0
                pick 4
                // _ 0 0 0 0 *input

                sponge_absorb_mem
                // _ g0 g1 g2 g3 (*input+10)

                pop 5
                // _

                return
        )

        // Can also be implemented without the use of `sponge_absorb_mem`:
        // triton_asm!(
        //     {entrypoint}:
        //         // _ *input

        //         push 9 add
        //         // _ *last_bfe

        //         read_mem 5
        //         read_mem 5
        //         // _ [word_9..word_0] (*first_elem - 1)

        //         pop 1
        //         // _ [word_9..word_0]

        //         sponge_absorb
        //         // _

        //         return
        // )
    }
}

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use twenty_first::math::other::random_elements;
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    fn init_memory_and_stack() -> (HashMap<BFieldElement, BFieldElement>, Vec<BFieldElement>) {
        let array_pointer: BFieldElement = rand::random();
        let init_stack = [empty_stack(), vec![array_pointer]].concat();
        let init_memory: HashMap<BFieldElement, BFieldElement> = (0..RATE)
            .map(|i| array_pointer + BFieldElement::new(i as u64))
            .zip(random_elements(RATE))
            .collect();

        (init_memory, init_stack)
    }

    impl Procedure for Absorb {
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
            let input: [BFieldElement; 10] = (0..10)
                .map(|i| memory[&(BFieldElement::new(i) + input_pointer)])
                .collect_vec()
                .try_into()
                .unwrap();
            sponge.absorb(input);
            Vec::default()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let mut bytes = [0u8; 400];
            rng.fill_bytes(&mut bytes);
            let mut unstructured = Unstructured::new(&bytes);
            let (memory, stack) = init_memory_and_stack();

            ProcedureInitialState {
                stack,
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: Vec::default(),
                sponge: Some(Tip5::arbitrary(&mut unstructured).unwrap()),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            let empty_sponge = {
                let (memory, stack) = init_memory_and_stack();
                ProcedureInitialState {
                    stack,
                    nondeterminism: NonDeterminism::default().with_ram(memory),
                    public_input: Vec::default(),
                    sponge: Some(Tip5::init()),
                }
            };

            vec![empty_sponge]
        }
    }

    #[test]
    fn absorb_test() {
        ShadowedProcedure::new(Absorb).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(Absorb).bench();
    }
}
