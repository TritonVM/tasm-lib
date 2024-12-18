use triton_vm::prelude::*;

use crate::prelude::*;

/// Absorb a sequence of field elements stored in memory, into the Sponge.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AbsorbMultiple;

impl BasicSnippet for AbsorbMultiple {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*sequence".to_string()),
            (DataType::U32, "len".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_absorb_multiple".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let hash_all_full_chunks = format!("{entrypoint}_hash_all_full_chunks");
        let pad_varnum_zeros = format!("{entrypoint}_pad_varnum_zeros");
        let read_remainder = format!("{entrypoint}_read_remainder");

        triton_asm! {
            // BEFORE: _ *bfe_sequence length
            // AFTER:  _
            {entrypoint}:
                dup 0
                push 10
                swap 1
                div_mod     // _ *bfe_sequence length (length/10) (length%10)
                swap 1
                pop 1       // _ *bfe_sequence length (length%10)

                swap 1      // _ *bfe_sequence (length%10) length
                dup 1       // _ *bfe_sequence (length%10) length (length%10)
                push -1 mul // _ *bfe_sequence (length%10) length (-length%10)
                dup 3
                add add     // _ *bfe_sequence (length%10) (*bfe_sequence + length - length%10)
                            // _ *bfe_sequence (length%10) *remainder

                swap 1
                swap 2      // _ (length%10) *remainder *bfe_sequence

                push 0
                push 0
                push 0
                push 0
                swap 4
                // _ (length%10) *remainder 0 0 0 0 *bfe_sequence

                call {hash_all_full_chunks}
                            // _ (length%10) *remainder e f g h *remainder
                pop 5       // _ (length%10) *remainder

                /* Calculate stop condition for reading remainder */
                push -1
                add
                // _ (length%10) (*remainder - 1)

                push 9      // _ (length%10) (*remainder - 1) 9
                dup 2       // _ (length%10) (*remainder - 1) 9 (length%10)
                push -1     // _ (length%10) (*remainder - 1) 9 (length%10) -1
                mul add     // _ (length%10) (*remainder - 1) (9-length%10)
                call {pad_varnum_zeros}
                            // _ [0; 9-length%10] (length%10) (*remainder - 1) 0
                pop 1
                push 1      // _ [0; 9-length%10] (length%10) (*remainder - 1) 1
                swap 2      // _ [0; 9-length%10] 1 (*remainder - 1) (length%10)
                dup 1 add   // _ [0; 9-length%10] 1 (*remainder - 1) *last_word
                call {read_remainder}
                            // _ [last_chunk_padded; 10] (*remainder - 1) (*remainder - 1)
                pop 2
                sponge_absorb
                return

            // BEFORE:    _ *remainder 0 0 0 0 *bfe_sequence
            // INVARIANT: _ *remainder a b c d *bfe_sequence'
            // AFTER:     _ *remainder e f g h *remainder
            {hash_all_full_chunks}:
                dup 5 dup 1 eq
                skiz return

                // _ *remainder a b c d *bfe_sequence
                sponge_absorb_mem

                // _ *remainder e f g h *bfe_sequence'
                recurse

            // BEFORE:    _ (length%10) (*remainder - 1) num_zeros
            // INVARIANT: _ [0; i] (length%10) (*remainder - 1) (num_zeros - i)
            // AFTER:     _ [0; num_zeros] (length%10) (*remainder - 1) 0
            {pad_varnum_zeros}:
                dup 0       // _ [0; i] (length%10) (*remainder - 1) (num_zeros - i)
                push 0 eq   // _ [0; i] (length%10) (*remainder - 1) (num_zeros - i == 0)
                skiz return
                            // _ [0; i] (length%10) (*remainder - 1) (num_zeros - i)

                push 0      // _ [0; i] (length%10) (*remainder - 1) (num_zeros - i) 0
                swap 3
                swap 2
                swap 1      // _ [0; i+1] (length%10) (*remainder - 1) (num_zeros - i)
                push -1
                add
                recurse

            // BEFORE:    _ (*remainder - 1) *last_word
            // INVARIANT: _ [elements; num_elements_read] (*remainder - 1) *some_addr
            // AFTER:     _ [elements; remainder_length] (*remainder - 1) (*remainder - 1)
            {read_remainder}:
                dup 1 dup 1 eq
                skiz return
                            // _ [elements; num_elements_read] (*remainder - 1) *some_addr
                read_mem 1  // _ [elements; num_elements_read] (*remainder - 1) element (*addr-1)
                swap 1      // _ [elements; num_elements_read] (*remainder - 1) (*addr-1) element
                swap 2      // _ [elements; num_elements_read+1] (*addr-1) (*remainder - 1)
                swap 1      // _ [elements; num_elements_read+1] (*remainder - 1) (*addr-1)
                recurse
        }
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    impl Procedure for AbsorbMultiple {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism,
            _: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            // read arguments
            let length = stack.pop().unwrap().value() as usize;
            let address = stack.pop().unwrap();

            // read sequence from memory
            let mut sequence = vec![];
            for i in 0..length {
                sequence.push(
                    memory
                        .get(&(address + BFieldElement::new(i as u64)))
                        .copied()
                        .unwrap(),
                )
            }

            let sponge = sponge.as_mut().expect("sponge must be initialized");
            sponge.pad_and_absorb_all(&sequence);

            // output empty
            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);

            // sample address
            let address = BFieldElement::new(rng.next_u64() % (1 << 20));

            // sample sequence
            let length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 102,
                Some(BenchmarkCase::WorstCase) => 2002,
                None => rng.next_u32() % 30,
            };

            let sequence = (0..length)
                .map(|_| rng.gen::<BFieldElement>())
                .collect_vec();

            // write to memory
            let mut memory = HashMap::new();
            for (i, s) in sequence.into_iter().enumerate() {
                memory.insert(address + BFieldElement::new(i as u64), s);
            }
            let nondeterminism = NonDeterminism::default().with_ram(memory);

            // leave address and length on stack
            let mut stack = empty_stack();
            stack.push(address);
            stack.push(BFieldElement::new(length as u64));

            let vm_hasher_state = Tip5 { state: rng.gen() };

            ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: vec![],
                sponge: Some(vm_hasher_state),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            vec![
                Self::corner_case_initial_state_for_num_words(0),
                Self::corner_case_initial_state_for_num_words(1),
                Self::corner_case_initial_state_for_num_words(2),
                Self::corner_case_initial_state_for_num_words(5),
                Self::corner_case_initial_state_for_num_words(9),
                Self::corner_case_initial_state_for_num_words(10),
                Self::corner_case_initial_state_for_num_words(11),
            ]
        }
    }

    impl AbsorbMultiple {
        fn corner_case_initial_state_for_num_words(num_words: u32) -> ProcedureInitialState {
            let list_address = BFieldElement::new(0);
            let list_length = BFieldElement::from(num_words);
            let sequence = vec![BFieldElement::new(2); num_words as usize];

            let stack = [empty_stack(), vec![list_address, list_length]].concat();
            let ram: HashMap<_, _> = sequence
                .into_iter()
                .enumerate()
                .map(|(i, bfe)| (BFieldElement::from(i as u32), bfe))
                .collect();
            let nondeterminism = NonDeterminism::default().with_ram(ram);

            ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: vec![],
                sponge: Some(Tip5::default()),
            }
        }
    }

    #[test]
    fn test() {
        ShadowedProcedure::new(AbsorbMultiple).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(AbsorbMultiple).bench();
    }
}
