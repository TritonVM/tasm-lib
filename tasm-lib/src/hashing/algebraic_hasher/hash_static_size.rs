use crate::data_type::DataType;
use crate::hashing::absorb_multiple_static_size::AbsorbMultipleStaticSize;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct HashStaticSize {
    pub size: usize,
}

impl BasicSnippet for HashStaticSize {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*addr".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*addr + size".to_owned()),
            (DataType::Digest, "digest".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_hashing_algebraic_hasher_hash_static_size_{}",
            self.size
        )
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_subroutine =
            library.import(Box::new(AbsorbMultipleStaticSize { size: self.size }));

        triton_asm!(
            // BEFORE:      _ addr
            // AFTER:       _ digest[4] digest[3] digest[2] digest[1] digest[0]
            {entrypoint}:
                sponge_init
                call {absorb_subroutine}
                sponge_squeeze  // _ *ret_addr d[9] d[8] d[7] d[6] d[5] d[4] d[3] d[2] d[1] d[0]
                swap 5 pop 1    // _ *ret_addr d[9] d[8] d[7] d[6] d[0] d[4] d[3] d[2] d[1]
                swap 5 pop 1    // _ *ret_addr d[9] d[8] d[7] d[1] d[0] d[4] d[3] d[2]
                swap 5 pop 1    // _ *ret_addr d[9] d[8] d[2] d[1] d[0] d[4] d[3]
                swap 5 pop 1    // _ *ret_addr d[9] d[3] d[2] d[1] d[0] d[4]
                swap 5 pop 1    // _ *ret_addr d[4] d[3] d[2] d[1] d[0]
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use proptest_arbitrary_interop::arb;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::twenty_first::shared_math::tip5::DIGEST_LENGTH;
    use triton_vm::twenty_first::util_types::algebraic_hasher::Sponge;

    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::{Procedure, ProcedureInitialState, ShadowedProcedure};
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    use super::*;

    #[test]
    fn hash_static_size_small_pbt() {
        for size in 0..20 {
            println!("Testing size {size}");
            ShadowedProcedure::new(HashStaticSize { size }).test();
        }
    }

    #[proptest(cases = 50)]
    fn hash_static_size_pbt_pbt(#[strategy(arb())] size: u8) {
        ShadowedProcedure::new(HashStaticSize {
            size: size as usize,
        })
        .test();
    }

    impl Procedure for HashStaticSize {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism<BFieldElement>,
            public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            *sponge = Some(Tip5::init());

            let absorb_snippet = AbsorbMultipleStaticSize { size: self.size };
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
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let memory_start: BFieldElement = rng.gen();
            let memory: HashMap<BFieldElement, BFieldElement> = (0..self.size)
                .map(|i| (memory_start + BFieldElement::new(i as u64), rng.gen()))
                .collect();

            let nondeterminism = NonDeterminism::default().with_ram(memory);
            ProcedureInitialState {
                stack: [self.init_stack_for_isolated_run(), vec![memory_start]].concat(),
                nondeterminism,
                public_input: vec![],
                sponge: None,
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::HashStaticSize;

    // Picked to be the size of a base table row at time of writing
    #[test]
    fn hash_var_lenstatic_size_benchmark_356() {
        ShadowedProcedure::new(HashStaticSize { size: 356 }).bench();
    }

    // Picked to be the size of a extension table row at time of writing
    #[test]
    fn hash_var_lenstatic_size_benchmark_249() {
        ShadowedProcedure::new(HashStaticSize { size: 249 }).bench();
    }

    // Picked to be the size of a quotient table row at time of writing
    #[test]
    fn hash_var_lenstatic_size_benchmark_12() {
        ShadowedProcedure::new(HashStaticSize { size: 12 }).bench();
    }

    // Picked to compare with same size of benchmark for `hash_varlen` at time of writing
    #[test]
    fn hash_var_lenstatic_size_benchmark_1000() {
        ShadowedProcedure::new(HashStaticSize { size: 1000 }).bench();
    }
}
