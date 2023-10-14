use rand::{rngs::StdRng, RngCore, SeedableRng};
use triton_vm::triton_asm;
use twenty_first::shared_math::{b_field_element::BFieldElement, bfield_codec::BFieldCodec};

use crate::{
    closure::Closure,
    empty_stack,
    snippet::{BasicSnippet, DataType},
};

#[derive(Clone, Debug)]
pub struct Overflowingadd;

impl BasicSnippet for Overflowingadd {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "lhs".to_owned()),
            (DataType::U32, "rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "wrapped_sum".to_owned()),
            (DataType::Bool, "overflow".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_overflowingadd".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                add
                split
                swap 1
                return
        )
    }
}

impl Closure for Overflowingadd {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let (wrapped_sum, overflow) = lhs.overflowing_add(rhs);
        stack.push(BFieldElement::new(wrapped_sum as u64));
        stack.push(BFieldElement::new(overflow as u64));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        let (lhs, rhs) = match bench_case {
            Some(crate::snippet_bencher::BenchmarkCase::CommonCase) => {
                (1u32 << 31, (1u32 << 31) - 1)
            }
            Some(crate::snippet_bencher::BenchmarkCase::WorstCase) => (1u32 << 31, 1u32 << 31),
            None => {
                let mut rng = StdRng::from_seed(seed);
                (rng.next_u32(), rng.next_u32())
            }
        };

        [empty_stack(), lhs.encode(), rhs.encode()].concat()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use triton_vm::NonDeterminism;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use super::*;
    use crate::closure::ShadowedClosure;
    use crate::snippet::RustShadow;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::VmHasherState;

    #[test]
    fn u32_overflowing_add_pbt() {
        ShadowedClosure::new(Overflowingadd).test()
    }

    #[test]
    fn u32_overflowing_add_unit_test() {
        for (lhs, rhs) in [
            (0u32, 0u32),
            (0, 1),
            (1, 0),
            (1, 1),
            (1 << 16, 1 << 16),
            (1 << 31, 1 << 31),
            (u32::MAX, u32::MAX),
        ] {
            let init_stack = [
                empty_stack(),
                vec![
                    BFieldElement::new(lhs as u64),
                    BFieldElement::new(rhs as u64),
                ],
            ]
            .concat();
            let (expected_wrapped_sum, expected_overflow) = lhs.overflowing_add(rhs);
            let expected_final_stack = [
                empty_stack(),
                vec![
                    (expected_wrapped_sum as u64).into(),
                    (expected_overflow as u64).into(),
                ],
            ]
            .concat();
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Overflowingadd),
                &init_stack,
                &[],
                &NonDeterminism::new(vec![]),
                &HashMap::default(),
                &VmHasherState::new(Domain::VariableLength),
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
    fn u32_overflowing_add_bench() {
        ShadowedClosure::new(Overflowingadd).bench()
    }
}
