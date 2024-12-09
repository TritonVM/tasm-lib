use tasm_lib::prelude::BasicSnippet;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;

#[derive(Clone, Debug)]
pub struct Add;

impl Add {
    pub const OVERFLOW_ERROR_ID: i128 = 310;
}

impl BasicSnippet for Add {
    fn inputs(&self) -> Vec<(DataType, String)> {
        ["rhs", "lhs"]
            .map(|side| (DataType::U64, side.to_string()))
            .to_vec()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "sum".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_add".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        // BEFORE: _ rhs_hi rhs_lo lhs_hi lhs_lo
        // AFTER:  _ sum_hi sum_lo
        triton_asm!({self.entrypoint()}:
            pick 2
            // _ rhs_hi lhs_hi lhs_lo rhs_lo

            add
            split
            // _ rhs_hi lhs_hi carry sum_lo

            place 3
            // _ sum_lo rhs_hi lhs_hi carry

            add
            add
            // _ sum_lo (lhs_hi+rhs_hi+carry)

            split
            // _ sum_lo overflow sum_hi

            place 2
            push 0
            eq
            assert error_id {Self::OVERFLOW_ERROR_ID}
            // _ sum_hi sum_lo

            return
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use proptest::prop_assume;
    use rand::prelude::*;
    use test_strategy::proptest;

    use super::*;
    use crate::pop_encodable;
    use crate::push_encodable;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::test_helpers::test_assertion_failure;
    use crate::test_helpers::test_rust_equivalence_given_execution_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::InitVmState;

    impl Add {
        fn prepare_stack(&self, left: u64, right: u64) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &right);
            push_encodable(&mut stack, &left);

            stack
        }
    }

    impl Closure for Add {
        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let sum = pop_encodable::<u64>(stack) + pop_encodable::<u64>(stack);
            push_encodable(stack, &sum);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> Vec<BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            let left = rng.gen();
            let right = rng.gen_range(0..u64::MAX - left);

            self.prepare_stack(left, right)
        }

        fn corner_case_initial_states(&self) -> Vec<Vec<BFieldElement>> {
            let boundary_points = [0, 1 << 32, u64::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .collect_vec();

            boundary_points
                .iter()
                .cartesian_product(&boundary_points)
                .filter(|(&left, &right)| left.checked_add(right).is_some())
                .map(|(&left, &right)| self.prepare_stack(left, right))
                .collect()
        }
    }

    #[test]
    fn unit_test() {
        ShadowedClosure::new(Add).test();
    }

    #[proptest]
    fn proptest(left: u64, #[strategy(0..u64::MAX - #left)] right: u64) {
        let initial_state = InitVmState::with_stack(Add.prepare_stack(left, right));
        test_rust_equivalence_given_execution_state(&ShadowedClosure::new(Add), initial_state);
    }

    #[proptest]
    fn triton_vm_crashes_on_overflowing_add(left: u64, #[strategy(u64::MAX - #left..)] right: u64) {
        prop_assume!(left.checked_add(right).is_none());

        test_assertion_failure(
            &ShadowedClosure::new(Add),
            InitVmState::with_stack(Add.prepare_stack(left, right)),
            &[Add::OVERFLOW_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench() {
        ShadowedClosure::new(Add).bench()
    }
}
