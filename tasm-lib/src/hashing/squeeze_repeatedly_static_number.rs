use triton_vm::prelude::*;
use triton_vm::twenty_first::shared_math::tip5::RATE;

use crate::data_type::DataType;
use crate::traits::basic_snippet::BasicSnippet;

/// Squeeze the sponge `num_squeezes` times, storing all the produced pseudorandom `BFieldElement`s
/// contiguously in memory. It is the caller's responsibility to allocate enough memory.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct SqueezeRepeatedlyStaticNumber {
    num_squeezes: usize,
}

impl BasicSnippet for SqueezeRepeatedlyStaticNumber {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "address".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_hashing_squeeze_repeatedly_static_number_{}",
            self.num_squeezes
        )
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        assert_eq!(10, RATE, "Code assumes RATE is 10");

        let entrypoint = self.entrypoint();
        let one_squeeze = triton_asm!(
            // _ *address

            sponge_squeeze
            // _ *address r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

            dup 10
            // _ *address r9 r8 r7 r6 r5 r4 r3 r2 r1 r0 *address

            write_mem 5
            write_mem 5
            // _ *address (*address + 10)

            swap 1
            pop 1
            // _ (*address + 10)
        );

        let all_squeezes = vec![one_squeeze; self.num_squeezes].concat();

        triton_asm!(
            {entrypoint}:
                // _ *address

                {&all_squeezes}
                 // _ *address'

                pop 1

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    use crate::hashing::squeeze_repeatedly::SqueezeRepeatedly;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    impl Procedure for SqueezeRepeatedlyStaticNumber {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism<BFieldElement>,
            public_input: &[BFieldElement],
            sponge: &mut Option<crate::VmHasher>,
        ) -> Vec<BFieldElement> {
            stack.push(BFieldElement::new(self.num_squeezes as u64));
            let ret =
                SqueezeRepeatedly.rust_shadow(stack, memory, nondeterminism, public_input, sponge);
            stack.pop();
            stack.pop();

            ret
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut init_state = SqueezeRepeatedly.pseudorandom_initial_state(seed, bench_case);
            init_state.stack.pop();
            init_state
        }
    }

    #[test]
    fn squeeze_repeatedly_static_number_pbt() {
        for num_squeezes in 0..25 {
            ShadowedProcedure::new(SqueezeRepeatedlyStaticNumber { num_squeezes }).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_10() {
        ShadowedProcedure::new(SqueezeRepeatedlyStaticNumber { num_squeezes: 10 }).bench();
    }

    #[test]
    fn bench_200() {
        ShadowedProcedure::new(SqueezeRepeatedlyStaticNumber { num_squeezes: 200 }).bench();
    }
}
