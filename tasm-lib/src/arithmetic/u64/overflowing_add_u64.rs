use rand::{rngs::StdRng, RngCore, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::shared_math::bfield_codec::BFieldCodec;

use crate::data_type::DataType;
use crate::{closure::Closure, empty_stack, snippet::BasicSnippet};

pub struct OverflowingAdd;

impl BasicSnippet for OverflowingAdd {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (DataType::U64, "lhs".to_string()),
            (DataType::U64, "rhs".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (DataType::U64, "wrapped_sum".to_owned()),
            (DataType::Bool, "overflow".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_overflowing_add".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        triton_asm!(
        // BEFORE: _ lhs_hi lhs_lo rhs_hi rhs_lo
        // AFTER: _ sum_hi sum_lo overflow
        { self.entrypoint() }:

            swap 1 swap 2
            // _ lhs_hi rhs_hi rhs_lo lhs_lo

            add
            split
            // _ lhs_hi rhs_hi carry sum_lo

            swap 3
            // _ sum_lo rhs_hi carry rhs_hi

            add
            add
            // _ sum_lo (lhs_hi+rhs_hi+carry)

            split
            // _ sum_lo overflow sum_hi

            swap 2
            swap 1
            // _ sum_hi sum_lo overflow

            return
        )
    }
}

impl Closure for OverflowingAdd {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let rhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u64 = rhs_lo as u64 + ((rhs_hi as u64) << 32);
        let lhs: u64 = lhs_lo as u64 + ((lhs_hi as u64) << 32);

        let (wrapped_sum, overflow) = lhs.overflowing_add(rhs);
        stack.push(BFieldElement::new(wrapped_sum >> 32));
        stack.push(BFieldElement::new(wrapped_sum & (u32::MAX as u64)));
        stack.push(BFieldElement::new(overflow as u64));
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
    use std::collections::HashMap;

    use triton_vm::NonDeterminism;

    use super::*;
    use crate::closure::ShadowedClosure;
    use crate::snippet::RustShadow;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;

    #[test]
    fn u64_overflowing_add_pbt() {
        ShadowedClosure::new(OverflowingAdd).test()
    }

    #[test]
    fn u64_overflowing_add_unit_test() {
        for (lhs, rhs) in [
            (0u64, 0u64),
            (0, 1),
            (1, 0),
            (1, 1),
            (1 << 32, 1 << 32),
            (1 << 63, 1 << 63),
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
            let expected = lhs.overflowing_add(rhs);
            let expected_final_stack = [
                empty_stack(),
                vec![
                    (expected.0 >> 32).into(),
                    (expected.0 & u32::MAX as u64).into(),
                    (expected.1 as u64).into(),
                ],
            ]
            .concat();
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(OverflowingAdd),
                &init_stack,
                &[],
                &NonDeterminism::new(vec![]),
                &HashMap::default(),
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
    use crate::{closure::ShadowedClosure, snippet::RustShadow};

    #[test]
    fn u64_overflowing_add_bench() {
        ShadowedClosure::new(OverflowingAdd).bench()
    }
}
