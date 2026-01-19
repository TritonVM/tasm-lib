use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// ### Behavior
///
/// ```text
/// BEFORE: _ [exp: u32] [base: XFieldElement]
/// AFTER:  _ [base^(2^exp): XFieldElement]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// None.
//
// todo: Explain the precondition. It seems to be a bit arbitrary. The tests
//   only check `exp` in range 0..32, while the assembly should work fine with
//   any number.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ToThePowerOfPowerOf2;

impl BasicSnippet for ToThePowerOfPowerOf2 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "log_2_exponent".to_owned()),
            (DataType::Xfe, "base".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_to_the_power_of_power_of_2".to_owned()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let loop_label = format!("{entrypoint}_loop");

        triton_asm!(
            {entrypoint}:
                call {loop_label}

                pick 3
                pop 1

                return

            // INVARIANT: _ remainder [acc]
            {loop_label}:
                // end condition: remainder == 0
                dup 3
                push 0
                eq
                skiz
                    return

                // _ remainder [acc]
                dup 2
                dup 2
                dup 2
                xx_mul
                // _ remainder [acc^2]

                pick 3
                addi -1
                place 3
                // _ (remainder - 1) [acc^2]

                recurse
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xd05170d4c435dfad.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32::XfeModPowU32;
    use crate::test_prelude::*;

    impl Closure for ToThePowerOfPowerOf2 {
        type Args = (u32, XFieldElement);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (exponent_log_2, base) = pop_encodable::<Self::Args>(stack);
            let result = base.mod_pow_u32(2_u32.pow(exponent_log_2));
            push_encodable(stack, &result);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            let exponent = match bench_case {
                Some(BenchmarkCase::CommonCase) => 20,
                Some(BenchmarkCase::WorstCase) => 31,
                None => rng.random_range(0..32),
            };

            (exponent, rng.random())
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..=5)
                .chain([30, 31])
                .map(|log_2_exp| (log_2_exp, xfe!([14; 3])))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(ToThePowerOfPowerOf2).test()
    }

    #[test]
    fn compare_to_generic_pow_u32() {
        let base = rand::random();
        for log_2_exponent in 0..32 {
            let final_pow_pow_stack = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(ToThePowerOfPowerOf2),
                &ToThePowerOfPowerOf2.set_up_test_stack((log_2_exponent, base)),
                &[],
                &NonDeterminism::default(),
                &None,
                None,
            )
            .op_stack
            .stack;

            let final_generic_stack = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(XfeModPowU32),
                &XfeModPowU32.set_up_test_stack((2_u32.pow(log_2_exponent), base)),
                &[],
                &NonDeterminism::default(),
                &None,
                None,
            )
            .op_stack
            .stack;

            // Assert that height agrees, and the top-3 elements agree
            assert_eq!(19, final_pow_pow_stack.len());
            assert_eq!(19, final_generic_stack.len());
            assert_eq!(final_pow_pow_stack[16..=18], final_generic_stack[16..=18],);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ToThePowerOfPowerOf2).bench();
    }
}
