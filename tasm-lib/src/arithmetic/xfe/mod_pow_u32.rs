use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct XfeModPowU32;

impl BasicSnippet for XfeModPowU32 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "exponent".to_owned()),
            (DataType::Xfe, "base".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_xfe_mod_pow_u32".to_owned()
    }

    // This implementation is far from optimized, not very efficient. To make a snippet
    // with a shorter execution trace, you could e.g. implement this algorithm for
    // statically known exponents.
    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        // Implemented as:
        // ```rust
        // fn mod_pow_u32(exponent: u32, base: XFieldElement) -> Self {
        //     let mut x = base;
        //     let mut acc = Self::one();
        //     let mut i = exponent;

        //     while i > 0 {
        //         if i & 1 == 1 {
        //             acc *= x;
        //         }

        //         x *= x;
        //         i >>= 1;
        //     }

        //     acc
        //

        let entrypoint = self.entrypoint();
        let loop_code_label = format!("{entrypoint}_loop");
        let acc_mul_x_label = format!("{entrypoint}_acc_mul_x");

        triton_asm!(
            {entrypoint}:
                // _ exponent [base]

                push 0
                push 0
                push 1
                // _ exponent [base] [1]

                // Rename
                // _ i [x] [acc]

                call {loop_code_label}
                // _ 0 [x] [result]

                swap 4
                pop 1
                swap 4
                pop 1
                swap 4
                pop 2
                // _ [result]

                return

            // Invariant: i [x] [acc]
            {loop_code_label}:

                // Return iff i == 0
                dup 6
                push 0
                eq
                skiz
                    return
                // _ i [x] [acc]

                dup 6
                push 1
                and
                // _ i [x] [acc] (i & 1)

                skiz
                    call {acc_mul_x_label}

                // _ i [x] [acc']

                dup 5 dup 5 dup 5
                dup 2 dup 2 dup 2
                // _ i [x] [acc'] [x] [x]

                xx_mul
                // _ i [x] [acc'] [x * x]

                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                // _ i [x'] [acc']

                swap 6
                // _ acc'_0 [x'] acc'_2 acc'_1 i

                push 2
                swap 1
                // _ acc'_0 [x'] acc'_2 acc'_1 2 i

                div_mod
                pop 1
                // _ acc'_0 [x'] acc'_2 acc'_1 (i / 2)

                swap 6
                // _ i' [x'] acc'_2 acc'_1 acc'_0
                // _ i' [x'] [acc']

                recurse

            {acc_mul_x_label}:
                 // _ [x] [acc]

                 dup 5
                 dup 5
                 dup 5
                 xx_mul
                 // _ [x] [acc * x]

                 return
        )
    }
}

#[cfg(test)]
pub mod tests {
    use twenty_first::math::traits::ModPowU32;

    use super::*;
    use crate::empty_stack;
    use crate::execute_with_terminal_state;
    use crate::test_prelude::*;

    impl Closure for XfeModPowU32 {
        type Args = (u32, XFieldElement);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (exponent, base) = pop_encodable::<Self::Args>(stack);
            let result = base.mod_pow_u32(exponent);
            push_encodable(stack, &result);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            let exponent = match bench_case {
                Some(BenchmarkCase::CommonCase) => 1 << 25,
                Some(BenchmarkCase::WorstCase) => u32::MAX,
                None => rng.random(),
            };

            (exponent, rng.random())
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            let an_xfe = xfe!([14; 3]);

            (0..=5)
                .chain([u32::MAX - 1, u32::MAX])
                .map(|exp| (exp, an_xfe))
                .collect()
        }
    }

    #[test]
    fn mod_pow_u32_xfe_pbt() {
        ShadowedClosure::new(XfeModPowU32).test()
    }

    #[test]
    fn verify_crash_if_exponent_not_u32() {
        let bfe_14 = BFieldElement::new(14);
        let xfe_14 = XFieldElement::new([bfe_14, bfe_14, bfe_14]);
        let xfe_14: Vec<_> = xfe_14.coefficients.into_iter().rev().collect();
        let code = XfeModPowU32.link_for_isolated_run();

        for exponent in [
            1 << 32,
            1 << 33,
            1 << 32,
            1 << 63,
            BFieldElement::MAX - 1,
            BFieldElement::MAX,
        ] {
            let init_stack = [empty_stack(), bfe_vec![exponent], xfe_14.clone()].concat();
            let tvm_result = execute_with_terminal_state(
                Program::new(&code),
                &[],
                &init_stack,
                &NonDeterminism::default(),
                None,
            );
            assert!(matches!(
                tvm_result.unwrap_err(),
                InstructionError::OpStackError(OpStackError::FailedU32Conversion(_))
            ));
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(XfeModPowU32).bench();
    }
}
