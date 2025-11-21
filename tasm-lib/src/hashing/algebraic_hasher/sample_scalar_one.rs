use triton_vm::prelude::*;

use crate::prelude::*;

/// Sample a single scalar from the sponge state
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SampleScalarOne;

impl BasicSnippet for SampleScalarOne {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "scalar".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_algebraic_hasher_sample_scalar_one".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                // _

                sponge_squeeze
                // _ r9 r8 r7 r6 r5 r4 r3 r2 r1 r0

                swap 7
                pop 1
                swap 7
                pop 1
                swap 7
                // _ r2 r1 r0 r6 r5 r4 r3 r9

                pop 5
                // _ r2 r1 r0

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::x_field_element::EXTENSION_DEGREE;
    use twenty_first::util_types::sponge::Sponge;

    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn sample_scalar_one_test() {
        ShadowedProcedure::new(SampleScalarOne).test();
    }

    impl Procedure for SampleScalarOne {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let vals = sponge.as_mut().unwrap().squeeze();

            for word in vals.iter().take(EXTENSION_DEGREE).rev() {
                stack.push(*word)
            }

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            _seed: [u8; 32],
            _bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            ProcedureInitialState {
                stack: self.init_stack_for_isolated_run(),
                nondeterminism: NonDeterminism::default(),
                public_input: vec![],
                sponge: Some(Tip5 {
                    state: rand::random(),
                }),
            }
        }
    }
}
