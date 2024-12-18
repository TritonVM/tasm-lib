use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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
            (DataType::Bool, "is_overflow".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_overflowingadd".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                add
                split
                swap 1
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for Overflowingadd {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (left, right) = pop_encodable::<Self::Args>(stack);
            let (sum, is_overflow) = left.overflowing_add(right);

            push_encodable(stack, &sum);
            push_encodable(stack, &is_overflow);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 31, (1 << 31) - 1),
                Some(BenchmarkCase::WorstCase) => (1 << 31, 1 << 31),
                None => StdRng::from_seed(seed).gen(),
            }
        }
    }

    #[test]
    fn u32_overflowing_add_pbt() {
        ShadowedClosure::new(Overflowingadd).test()
    }

    #[test]
    fn u32_overflowing_add_unit_test() {
        for (lhs, rhs) in [
            (0, 0),
            (0, 1),
            (1, 0),
            (1, 1),
            (1 << 16, 1 << 16),
            (1 << 31, 1 << 31),
            (u32::MAX, u32::MAX),
        ] {
            let initial_stack = Overflowingadd.set_up_test_stack((lhs, rhs));

            let mut expected_final_stack = initial_stack.clone();
            Overflowingadd.rust_shadow(&mut expected_final_stack);

            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Overflowingadd),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_final_stack),
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
        ShadowedClosure::new(Overflowingadd).bench()
    }
}
