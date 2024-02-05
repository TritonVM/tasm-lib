use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct XfeModPowU32Generic;

impl BasicSnippet for XfeModPowU32Generic {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "exponent".to_owned()),
            (DataType::Xfe, "base".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_xfe_mod_pow_u32".to_owned()
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

                xxmul
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
                 xxmul
                 // _ [x] [acc * x]

                 return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use triton_vm::prelude::*;
    use triton_vm::twenty_first::shared_math::traits::ModPowU32;

    use crate::empty_stack;
    use crate::execute_with_terminal_state;
    use crate::linker::link_for_isolated_run;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    impl Closure for XfeModPowU32Generic {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let base = XFieldElement::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ]);
            let exponent: u32 = stack.pop().unwrap().try_into().unwrap();
            let result = base.mod_pow_u32(exponent);

            for elem in result.coefficients.into_iter().rev() {
                stack.push(elem);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng: StdRng = SeedableRng::from_seed(seed);
            let exponent = match bench_case {
                Some(BenchmarkCase::CommonCase) => 31,
                Some(BenchmarkCase::WorstCase) => u32::MAX,
                None => rng.gen(),
            };

            let base: XFieldElement = rng.gen();
            self.prepare_state(base, exponent)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let bfe_14 = BFieldElement::new(14);
            let an_xfe = XFieldElement::new([bfe_14, bfe_14, bfe_14]);
            (0..=5)
                .chain([u32::MAX - 1, u32::MAX])
                .map(|exp| self.prepare_state(an_xfe, exp))
                .collect_vec()
        }
    }

    impl XfeModPowU32Generic {
        fn prepare_state(&self, base: XFieldElement, exponent: u32) -> Vec<BFieldElement> {
            let base = base.coefficients.into_iter().rev().collect();
            [
                self.init_stack_for_isolated_run(),
                vec![BFieldElement::new(exponent as u64)],
                base,
            ]
            .concat()
        }
    }

    #[test]
    fn mod_pow_u32_xfe_pbt() {
        ShadowedClosure::new(XfeModPowU32Generic).test()
    }

    #[test]
    fn verify_crash_if_exponent_not_u32() {
        let bfe_14 = BFieldElement::new(14);
        let xfe_14 = XFieldElement::new([bfe_14, bfe_14, bfe_14]);
        let xfe_14: Vec<_> = xfe_14.coefficients.into_iter().rev().collect();
        let code = link_for_isolated_run(Rc::new(RefCell::new(XfeModPowU32Generic)), 0);
        let program = Program::new(&code);

        for exponent in [
            1u64 << 32,
            1u64 << 33,
            (1u64 << 32),
            1u64 << 63,
            BFieldElement::MAX - 1,
            BFieldElement::MAX,
        ] {
            let init_stack = [
                empty_stack(),
                vec![BFieldElement::new(exponent)],
                xfe_14.clone(),
            ]
            .concat();
            let tvm_result = execute_with_terminal_state(
                &program,
                &[],
                &init_stack,
                &NonDeterminism::default(),
                None,
            );
            let err = tvm_result.unwrap_err();
            assert!(matches!(err, InstructionError::FailedU32Conversion(_)));
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn xfe_ntt_benchmark() {
        ShadowedClosure::new(XfeModPowU32Generic).bench();
    }
}
