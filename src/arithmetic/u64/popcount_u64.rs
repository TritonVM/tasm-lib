use num::Zero;
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct PopCountU64;

impl Snippet for PopCountU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_popcount".to_string()
    }

    fn inputs(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["popcount".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // Before: _ x_hi x_lo
            // After: _ popcount
            {entrypoint}:
                pop_count
                swap 1
                pop_count
                add

                return

            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Input are not valid u32s".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for _ in 0..100 {
            ret.push(prepare_state(rng.next_u64()));
        }

        // add cornercases
        let mut init_stack_zero = get_init_tvm_stack();
        init_stack_zero.push(BFieldElement::zero());
        init_stack_zero.push(BFieldElement::zero());
        ret.push(ExecutionState::with_stack(init_stack_zero));

        let mut init_stack_max_value = get_init_tvm_stack();
        init_stack_max_value.push(BFieldElement::new((1u64 << 32) - 1));
        init_stack_max_value.push(BFieldElement::new((1u64 << 32) - 1));
        ret.push(ExecutionState::with_stack(init_stack_max_value));

        let mut init_stack_max_value2 = get_init_tvm_stack();
        init_stack_max_value2.push(BFieldElement::new((1u64 << 32) - 1));
        init_stack_max_value2.push(BFieldElement::new((1u64 << 30) - 1));
        ret.push(ExecutionState::with_stack(init_stack_max_value2));

        let mut init_stack_max_value3 = get_init_tvm_stack();
        init_stack_max_value3.push(BFieldElement::new((1u64 << 30) - 1));
        init_stack_max_value3.push(BFieldElement::new((1u64 << 32) - 1));
        ret.push(ExecutionState::with_stack(init_stack_max_value3));

        ret
    }

    fn common_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 60)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        prepare_state(1 << 60)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let a_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let a_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let a = ((a_hi as u64) << 32) + a_lo as u64;

        let pop_count = a.count_ones();

        stack.push(BFieldElement::new(pop_count as u64));
    }
}

fn prepare_state(a: u64) -> ExecutionState {
    let a = U32s::<2>::try_from(a).unwrap();
    let mut init_stack = get_init_tvm_stack();
    push_encodable(&mut init_stack, &a);
    ExecutionState::with_stack(init_stack)
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::*;

    #[test]
    fn popcount_u64_test() {
        test_rust_equivalence_multiple(&PopCountU64, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn popcount_u64_benchmark() {
        bench_and_write(PopCountU64);
    }
}
