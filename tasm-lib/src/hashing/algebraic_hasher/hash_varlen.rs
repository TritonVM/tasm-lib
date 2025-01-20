use triton_vm::prelude::*;
use twenty_first::util_types::sponge::Sponge;

use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::prelude::*;

/// Calculate hash of a raw sequence of a `BFieldElement`.
#[derive(Clone, Debug)]
pub struct HashVarlen;

impl HashVarlen {
    /// Mutate the sponge with the same operations as this snippet
    /// Used to facilitate rust-shadowing in downstream dependencies.
    pub fn sponge_mutation(&self, sponge: &mut Tip5, preimage: &[BFieldElement]) {
        *sponge = Tip5::init();

        sponge.pad_and_absorb_all(preimage);

        sponge.squeeze();
    }
}

impl BasicSnippet for HashVarlen {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*addr".to_owned()),
            (DataType::U32, "length".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_algebraic_hasher_hash_varlen".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_subroutine = library.import(Box::new(AbsorbMultiple));

        triton_asm!(
            // BEFORE:      _ addr len
            // AFTER:       _ digest[4] digest[3] digest[2] digest[1] digest[0]
            {entrypoint}:
                sponge_init
                call {absorb_subroutine}
                sponge_squeeze  // _ d[9] d[8] d[7] d[6] d[5] d[4] d[3] d[2] d[1] d[0]
                swap 5 pop 1    // _ d[9] d[8] d[7] d[6] d[0] d[4] d[3] d[2] d[1]
                swap 5 pop 1    // _ d[9] d[8] d[7] d[1] d[0] d[4] d[3] d[2]
                swap 5 pop 1    // _ d[9] d[8] d[2] d[1] d[0] d[4] d[3]
                swap 5 pop 1    // _ d[9] d[3] d[2] d[1] d[0] d[4]
                swap 5 pop 1    // _ d[4] d[3] d[2] d[1] d[0]
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use super::*;
    use crate::test_helpers::tasm_final_state;
    use crate::test_prelude::*;

    impl HashVarlen {
        fn random_memory_state_read_k(&self, k: u32) -> MemPreserverInitialState {
            let memory_start: BFieldElement = random();
            let memory: HashMap<BFieldElement, BFieldElement> = (0..k)
                .map(|i| (memory_start + BFieldElement::new(i as u64), random()))
                .collect();

            let nondeterminism = NonDeterminism::default().with_ram(memory);
            MemPreserverInitialState {
                stack: [
                    self.init_stack_for_isolated_run(),
                    vec![memory_start, BFieldElement::new(k as u64)],
                ]
                .concat(),
                nondeterminism,
                public_input: VecDeque::new(),
                sponge_state: None,
            }
        }
    }

    impl MemPreserver for HashVarlen {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &HashMap<BFieldElement, BFieldElement>,
            nd_tokens: VecDeque<BFieldElement>,
            nd_digests: VecDeque<Digest>,
            stdin: VecDeque<BFieldElement>,
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            *sponge = Some(Tip5::init());

            let absorb_snippet = AbsorbMultiple;
            absorb_snippet.rust_shadow(stack, memory, nd_tokens, nd_digests, stdin, sponge);

            let mut squeezed = sponge.as_mut().unwrap().squeeze();
            squeezed.reverse();
            stack.extend(squeezed);

            let digest = pop_encodable::<Digest>(stack);
            for _ in 0..Digest::LEN {
                stack.pop().unwrap();
            }
            push_encodable(stack, &digest);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> MemPreserverInitialState {
            let preimage_length: u32 = match bench_case {
                Some(BenchmarkCase::CommonCase) => 25,
                Some(BenchmarkCase::WorstCase) => 1000,
                None => {
                    let mut rng = StdRng::from_seed(seed);
                    rng.gen_range(0..400)
                }
            };

            self.random_memory_state_read_k(preimage_length)
        }
    }

    #[test]
    fn test() {
        ShadowedMemPreserver::new(HashVarlen).test();
    }

    #[test]
    fn sponge_mutation_function_matches_snippet() {
        let snippet = HashVarlen;
        let mut seed = [0u8; 32];
        thread_rng().fill_bytes(&mut seed);
        let init_state = snippet.pseudorandom_initial_state(seed, None);
        let init_sponge = Tip5 { state: random() };

        let preimage_length: u32 = init_state.stack.last().unwrap().value().try_into().unwrap();
        let mut preimage_pointer = init_state.stack[init_state.stack.len() - 2];
        let mut preimage = vec![];
        for _ in 0..preimage_length {
            preimage.push(init_state.nondeterminism.ram[&preimage_pointer]);
            preimage_pointer.increment();
        }

        let final_state = tasm_final_state(
            &ShadowedMemPreserver::new(snippet.clone()),
            &init_state.stack,
            &[],
            init_state.nondeterminism,
            &Some(init_sponge.clone()),
        );

        let mut helper_function_sponge = init_sponge.clone();
        snippet.sponge_mutation(&mut helper_function_sponge, &preimage);

        assert_eq!(final_state.sponge.clone().unwrap(), helper_function_sponge);
        assert_ne!(final_state.sponge.unwrap(), init_sponge);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedMemPreserver::new(HashVarlen).bench();
    }
}
