use triton_vm::prelude::*;

use crate::prelude::*;

pub struct ToThePowerOfPowerOf2;

impl BasicSnippet for ToThePowerOfPowerOf2 {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "log_2_exponent".to_owned()),
            (DataType::Xfe, "base".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_to_the_power_of_power_of_2".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let loop_label = format!("{}_loop", entrypoint);
        let loop_code = triton_asm!(
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

                swap 3
                push -1
                add
                swap 3
                // _ (remainder - 1) [acc^2]

                recurse

        );

        triton_asm!(
            {entrypoint}:
                call {loop_label}

                swap 1
                swap 2
                swap 3
                pop 1

                return

            {&loop_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::arithmetic::xfe::mod_pow_u32_generic::XfeModPowU32Generic;
    use crate::test_prelude::*;

    impl Closure for ToThePowerOfPowerOf2 {
        type Args = (u32, XFieldElement);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (exponent_log_2, base) = pop_encodable::<Self::Args>(stack);
            let result = base.mod_pow_u32(2u32.pow(exponent_log_2));
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
                None => rng.gen_range(0..32),
            };

            (exponent, rng.gen())
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            (0..=5)
                .chain([30, 31])
                .map(|log_2_exp| (log_2_exp, xfe!([14; 3])))
                .collect()
        }
    }

    #[test]
    fn to_the_power_of_power_of_2_pbt() {
        ShadowedClosure::new(ToThePowerOfPowerOf2).test()
    }

    #[test]
    fn compare_to_generic_pow_u32() {
        let base: XFieldElement = random();
        for log_2_exponent in 0..32 {
            let init_stack_pow_pow = [
                ToThePowerOfPowerOf2.init_stack_for_isolated_run(),
                vec![BFieldElement::new(log_2_exponent)],
                base.coefficients.into_iter().rev().collect_vec(),
            ]
            .concat();
            let final_state_pow_pow = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(ToThePowerOfPowerOf2),
                &init_stack_pow_pow,
                &[],
                &NonDeterminism::default(),
                &None,
                None,
            );

            // Run snippet for generic pow
            let init_stack_to_generic = [
                XfeModPowU32Generic.init_stack_for_isolated_run(),
                vec![BFieldElement::new(
                    2u64.pow(log_2_exponent.try_into().unwrap()),
                )],
                base.coefficients.into_iter().rev().collect_vec(),
            ]
            .concat();
            let final_state_from_generic = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(XfeModPowU32Generic),
                &init_stack_to_generic,
                &[],
                &NonDeterminism::default(),
                &None,
                None,
            );

            // Assert that height agrees, and the top-3 elements agree
            assert_eq!(
                final_state_from_generic.op_stack.stack.len(),
                final_state_pow_pow.op_stack.stack.len()
            );
            assert_eq!(
                final_state_from_generic.op_stack.stack[16..=18],
                final_state_pow_pow.op_stack.stack[16..=18],
            );
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
