use triton_vm::prelude::*;

use crate::hashing::absorb_multiple_static_size::AbsorbMultipleStaticSize;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct HashStaticSize {
    pub size: usize,
}

impl BasicSnippet for HashStaticSize {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*addr".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Digest, "digest".to_owned()),
            (DataType::VoidPointer, "*addr + size".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_algebraic_hasher_hash_static_size_{}",
            self.size
        )
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let absorb_subroutine =
            library.import(Box::new(AbsorbMultipleStaticSize { size: self.size }));

        triton_asm!(
            // BEFORE:      _ *addr
            // AFTER:       _ (*addr + size) digest[4] digest[3] digest[2] digest[1] digest[0]
            {entrypoint}:
                sponge_init
                call {absorb_subroutine}
                sponge_squeeze  // _ *ret_addr d[9] d[8] d[7] d[6] d[5] d[4] d[3] d[2] d[1] d[0]
                swap 6 pop 1
                swap 6 pop 1
                swap 6 pop 1
                swap 6 pop 1
                swap 6
                swap 1 pop 1
                // _ d[4] d[3] d[2] d[1] d[0] *ret_addr
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::prelude::*;

    use super::*;
    use crate::test_prelude::*;

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
            nondeterminism: &NonDeterminism,
            public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            *sponge = Some(Tip5::init());

            let absorb_snippet = AbsorbMultipleStaticSize { size: self.size };
            absorb_snippet.rust_shadow(stack, memory, nondeterminism, public_input, sponge);

            // Sponge-squeeze
            let mut squeezed = sponge.as_mut().unwrap().squeeze();
            squeezed.reverse();
            stack.extend(squeezed);

            // Pop returned digest
            let digest = pop_encodable::<Digest>(stack);

            // Remove 5 more words:
            for _ in 0..Digest::LEN {
                stack.pop().unwrap();
            }

            // Remove pointer
            let input_pointer_plus_size = stack.pop().unwrap();

            // Put digest back on stack
            stack.extend(digest.reversed().values().to_vec());

            stack.push(input_pointer_plus_size);

            // _ d4 d3 d2 d1 d0 *ret_addr

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let memory_start: BFieldElement = rng.random();
            let memory: HashMap<BFieldElement, BFieldElement> = (0..self.size)
                .map(|i| (memory_start + BFieldElement::new(i as u64), rng.random()))
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
    use super::*;
    use crate::test_prelude::*;

    // Picked to be the size of a main table row at time of writing
    #[test]
    fn hash_var_lenstatic_size_benchmark_356() {
        ShadowedProcedure::new(HashStaticSize { size: 356 }).bench();
    }

    // Picked to be the size of a auxiliary table row at time of writing
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
