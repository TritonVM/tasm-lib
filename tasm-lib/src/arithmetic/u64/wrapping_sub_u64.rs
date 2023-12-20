use rand::{rngs::StdRng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::closure::Closure;

pub struct WrappingSub;

impl BasicSnippet for WrappingSub {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "lhs".to_owned()),
            (DataType::U64, "rhs".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "wrapped_diff".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_wrapping_sub".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        const TWO_POW_32: u64 = 1 << 32;

        triton_asm!(
            {entrypoint}:
                // _ lhs_hi lhs_lo rhs_hi rhs_lo

                push -1
                mul
                // _ lhs_hi lhs_lo rhs_hi (-rhs_lo)

                swap 1 swap 2
                // _ lhs_hi rhs_hi (-rhs_lo) lhs_lo

                add
                // _ lhs_hi rhs_hi (lhs_lo-rhs_lo)

                push {TWO_POW_32}
                add

                split
                // _ lhs_hi rhs_hi !carry diff_lo

                swap 2 swap 1
                // _ lhs_hi diff_lo rhs_hi !carry

                push 0
                eq
                // _ lhs_hi diff_lo rhs_hi carry

                add
                // _ lhs_hi diff_lo rhs_hi'

                push -1
                mul
                // _ lhs_hi diff_lo (-rhs_hi')

                swap 1 swap 2
                // _ diff_lo (-rhs_hi') lhs_hi

                add
                // _ diff_lo (lhs_hi-rhs_hi')

                push {TWO_POW_32}
                add

                split
                // _ diff_lo !carry diff_hi

                swap 1
                pop 1
                swap 1

                return
        )
    }
}

impl Closure for WrappingSub {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let rhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u64 = rhs_lo as u64 + ((rhs_hi as u64) << 32);
        let lhs: u64 = lhs_lo as u64 + ((lhs_hi as u64) << 32);

        let wrapped_diff = lhs.wrapping_sub(rhs);
        stack.push(BFieldElement::new(wrapped_diff >> 32));
        stack.push(BFieldElement::new(wrapped_diff & (u32::MAX as u64)));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        let (lhs, rhs) = match bench_case {
            Some(crate::snippet_bencher::BenchmarkCase::CommonCase) => {
                (1u64 << 63, (1u64 << 63) - 1)
            }
            Some(crate::snippet_bencher::BenchmarkCase::WorstCase) => (1u64 << 63, 1u64 << 50),
            None => {
                let mut rng = StdRng::from_seed(seed);
                (rng.next_u64(), rng.next_u64())
            }
        };

        [empty_stack(), lhs.encode(), rhs.encode()].concat()
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::NonDeterminism;

    use super::*;
    use crate::traits::rust_shadow::RustShadow;
    use crate::{
        test_helpers::test_rust_equivalence_given_complete_state, traits::closure::ShadowedClosure,
    };

    #[test]
    fn u64_wrapping_sub_pbt() {
        ShadowedClosure::new(WrappingSub).test()
    }

    #[test]
    fn u64_wrapped_sub_unit_test() {
        for (lhs, rhs) in [
            (0u64, 0u64),
            (0, 1),
            (1, 0),
            (1, 1),
            (1 << 32, 1 << 32),
            (1 << 63, 1 << 63),
            (u64::MAX, u64::MAX),
            (u64::MAX, 0),
            (0, u64::MAX),
            (100, 101),
            (101, 100),
            (1 << 40, (1 << 40) + 1),
            ((1 << 40) + 1, 1 << 40),
        ] {
            let init_stack = [
                empty_stack(),
                vec![
                    BFieldElement::new(lhs >> 32),
                    BFieldElement::new(lhs & u32::MAX as u64),
                    BFieldElement::new(rhs >> 32),
                    BFieldElement::new(rhs & u32::MAX as u64),
                ],
            ]
            .concat();
            let expected = lhs.wrapping_sub(rhs);
            let expected_final_stack = [
                empty_stack(),
                vec![(expected >> 32).into(), (expected & u32::MAX as u64).into()],
            ]
            .concat();
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(WrappingSub),
                &init_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                1,
                Some(&expected_final_stack),
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn u64_wrapping_sub_bench() {
        ShadowedClosure::new(WrappingSub).bench()
    }
}
