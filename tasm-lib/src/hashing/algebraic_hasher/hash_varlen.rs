use std::collections::HashMap;

use rand::random;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::DIGEST_LENGTH;
use triton_vm::twenty_first::util_types::algebraic_hasher::Sponge;

use crate::data_type::DataType;
use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::library::Library;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;
use crate::VmHasher;

/// Calculate hash of a raw sequence of a `BFieldElement`.
#[derive(Clone, Debug)]
pub struct HashVarlen;

impl HashVarlen {
    fn random_memory_state_read_k(&self, k: u32) -> ProcedureInitialState {
        let memory_start: BFieldElement = random();
        let memory: HashMap<BFieldElement, BFieldElement> = (0..k)
            .map(|i| (memory_start + BFieldElement::new(i as u64), random()))
            .collect();

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ProcedureInitialState {
            stack: [
                self.init_stack_for_isolated_run(),
                vec![memory_start, BFieldElement::new(k as u64)],
            ]
            .concat(),
            nondeterminism,
            public_input: vec![],
            sponge: None,
        }
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
        "tasm_hashing_algebraic_hasher_hash_varlen".to_owned()
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

impl Procedure for HashVarlen {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism<BFieldElement>,
        public_input: &[BFieldElement],
        sponge: &mut Option<VmHasher>,
    ) -> Vec<BFieldElement> {
        *sponge = Some(Tip5::init());

        let absorb_snippet = AbsorbMultiple;
        absorb_snippet.rust_shadow(stack, memory, nondeterminism, public_input, sponge);

        // Sponge-squeeze
        let mut squeezed = sponge.as_mut().unwrap().squeeze();
        squeezed.reverse();
        stack.extend(squeezed);

        // Pop returned digest
        let digest = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        // Remove 5 more words:
        for _ in 0..DIGEST_LENGTH {
            stack.pop().unwrap();
        }

        // Put digest back on stack
        stack.extend(digest.reversed().values().to_vec());

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> ProcedureInitialState {
        let preimage_length: u32 = match bench_case {
            Some(BenchmarkCase::CommonCase) => 25,
            Some(BenchmarkCase::WorstCase) => 1000,
            None => {
                let mut rng: StdRng = SeedableRng::from_seed(seed);
                rng.gen_range(0..400)
            }
        };

        self.random_memory_state_read_k(preimage_length)
    }
}

#[cfg(test)]
mod tests {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn test() {
        ShadowedProcedure::new(HashVarlen).test();
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(HashVarlen).bench();
    }
}
