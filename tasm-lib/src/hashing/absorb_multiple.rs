use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Absorb a sequence of field elements stored in memory, into the Sponge.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *sequence [len: u32]
/// AFTER:  _
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// None.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct AbsorbMultiple;

impl BasicSnippet for AbsorbMultiple {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*sequence".to_string()),
            (DataType::U32, "len".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_absorb_multiple".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_all_full_chunks = format!("{entrypoint}_absorb_all_full_chunks");
        let pad_varnum_zeros = format!("{entrypoint}_pad_varnum_zeros");
        let read_remainder = format!("{entrypoint}_read_remainder");

        triton_asm! {
            // BEFORE: _ *bfe_sequence length
            // AFTER:  _
            {entrypoint}:
                push 10
                dup 1
                div_mod     // _ *bfe_sequence length (length/10) (length%10)
                place 2
                pop 1       // _ *bfe_sequence (length%10) length

                dup 1       // _ *bfe_sequence (length%10) length (length%10)
                push -1
                mul         // _ *bfe_sequence (length%10) length (-length%10)
                dup 3
                add
                add         // _ *bfe_sequence (length%10) (*bfe_sequence + length - length%10)
                            // _ *bfe_sequence (length%10) *remainder

                push 0
                push 0
                push 0
                push 0      // _ *bfe_sequence (length%10) *remainder 0 0 0 0
                pick 6      // _ (length%10) *remainder 0 0 0 0 *bfe_sequence

                call {absorb_all_full_chunks}
                            // _ (length%10) *remainder e f g h *remainder
                pop 5       // _ (length%10) *remainder

                /* Calculate stop condition for reading remainder */
                addi -1     // _ (length%10) (*remainder - 1)
                dup 1       // _ (length%10) (*remainder - 1) (length%10)
                push -1     // _ (length%10) (*remainder - 1) (length%10) -1
                mul         // _ (length%10) (*remainder - 1) (-length%10)
                addi 9      // _ (length%10) (*remainder - 1) (9-length%10)
                call {pad_varnum_zeros}
                            // _ [0; 9-length%10] (length%10) (*remainder - 1) 0

                pop 1       // _ [0; 9-length%10] (length%10) (*remainder - 1)
                push 1      // _ [0; 9-length%10] (length%10) (*remainder - 1) 1
                swap 2      // _ [0; 9-length%10] 1 (*remainder - 1) (length%10)
                dup 1
                add         // _ [0; 9-length%10] 1 (*remainder - 1) *last_word
                call {read_remainder}
                            // _ [last_chunk_padded; 10] (*remainder - 1) (*remainder - 1)
                pop 2
                sponge_absorb
                return

            // BEFORE:    _ *remainder 0 0 0 0 *bfe_sequence
            // INVARIANT: _ *remainder a b c d *bfe_sequence'
            // AFTER:     _ *remainder e f g h *remainder
            {absorb_all_full_chunks}:
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
                dup 0
                push 0 eq
                skiz return
                            // _ [0; i] (length%10) (*remainder - 1) (num_zeros - i)
                push 0
                place 3     // _ [0; i+1] (length%10) (*remainder - 1) (num_zeros - i)
                addi -1
                recurse

            // BEFORE:    _ (*remainder - 1) *last_word
            // INVARIANT: _ [elements; num_elements_read] (*remainder - 1) *some_addr
            // AFTER:     _ [elements; remainder_length] (*remainder - 1) (*remainder - 1)
            {read_remainder}:
                dup 1 dup 1 eq
                skiz return
                            // _ [elements; num_elements_read] (*remainder - 1) *some_addr
                read_mem 1  // _ [elements; num_elements_read] (*remainder - 1) element (*addr-1)
                pick 1      // _ [elements; num_elements_read] (*remainder - 1) (*addr-1) element
                place 2     // _ [elements; num_elements_read+1] (*remainder - 1) (*addr-1)
                recurse
        }
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xef7d4688cdc869ab.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions::array::array_from_memory;
    use crate::test_prelude::*;

    impl MemPreserver for AbsorbMultiple {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
            _: VecDeque<BFieldElement>,
            _: VecDeque<Digest>,
            _: VecDeque<BFieldElement>,
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let length = pop_encodable::<u32>(stack).try_into().unwrap();
            let address = stack.pop().unwrap();

            let sponge = sponge.as_mut().expect("sponge must be initialized");
            sponge.pad_and_absorb_all(&array_from_memory::<BFieldElement>(address, length, memory));

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> MemPreserverInitialState {
            let mut rng = StdRng::from_seed(seed);

            let address = rng.random::<BFieldElement>();
            let length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 102,
                Some(BenchmarkCase::WorstCase) => 2002,
                None => rng.random_range(0..=29),
            };

            let memory: HashMap<_, _> = (0..length)
                .map(|i| (address + bfe!(i), rng.random()))
                .collect();

            MemPreserverInitialState {
                stack: [empty_stack(), bfe_vec![address, length]].concat(),
                nondeterminism: NonDeterminism::default().with_ram(memory),
                public_input: VecDeque::new(),
                sponge_state: Some(Tip5 {
                    state: rng.random(),
                }),
            }
        }

        fn corner_case_initial_states(&self) -> Vec<MemPreserverInitialState> {
            let mut states = vec![];

            // (all remainders) x {0, 1, 2 full absorptions}
            for num_words in 0..=29 {
                // populate RAM with 2s because padding consists of 0s and 1s
                let ram: HashMap<_, _> = (0..num_words).map(|i| (bfe!(i), bfe!(2))).collect();

                states.push(MemPreserverInitialState {
                    stack: [empty_stack(), bfe_vec![0, num_words]].concat(),
                    nondeterminism: NonDeterminism::default().with_ram(ram),
                    public_input: VecDeque::new(),
                    sponge_state: Some(Tip5::default()),
                });
            }

            states
        }
    }

    #[test]
    fn test() {
        ShadowedMemPreserver::new(AbsorbMultiple).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedMemPreserver::new(AbsorbMultiple).bench();
    }
}
