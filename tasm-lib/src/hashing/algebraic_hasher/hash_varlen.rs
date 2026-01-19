use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Calculate hash of a raw sequence of [`BFieldElement`]s.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ *array [length: u32]
/// AFTER:  _ [digest: Digest]
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
pub struct HashVarlen;

impl BasicSnippet for HashVarlen {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*addr".to_owned()),
            (DataType::U32, "length".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_algebraic_hasher_hash_varlen".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let absorb_subroutine = library.import(Box::new(AbsorbMultiple));

        triton_asm!(
            // BEFORE:      _ addr [len: u32]
            // AFTER:       _ [digest: Digest]
            {self.entrypoint()}:
                sponge_init
                call {absorb_subroutine}
                sponge_squeeze  // _ d_9 d_8 d_7 d_6 d_5 [digest: Digest]
                pick 9
                pick 9
                pick 9
                pick 9
                pick 9
                pop 5           // _ [digest: Digest]
                return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x210c72ced00922c8.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use twenty_first::prelude::*;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

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

            AbsorbMultiple.rust_shadow(stack, memory, nd_tokens, nd_digests, stdin, sponge);
            let [d0, d1, d2, d3, d4, ..] = sponge.as_mut().unwrap().squeeze();
            push_encodable(stack, &Digest::new([d0, d1, d2, d3, d4]));

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> MemPreserverInitialState {
            let mut rng = StdRng::from_seed(seed);
            let length = match bench_case {
                Some(BenchmarkCase::CommonCase) => 25,
                Some(BenchmarkCase::WorstCase) => 1000,
                None => rng.random_range(0..400),
            };

            let address = rng.random::<BFieldElement>();
            let ram: HashMap<_, _> = (0..length)
                .map(|i| (address + bfe!(i), rng.random()))
                .collect();

            MemPreserverInitialState {
                stack: [empty_stack(), bfe_vec![address, length]].concat(),
                nondeterminism: NonDeterminism::default().with_ram(ram),
                ..MemPreserverInitialState::default()
            }
        }
    }

    #[test]
    fn test() {
        ShadowedMemPreserver::new(HashVarlen).test();
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
