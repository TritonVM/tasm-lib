use std::collections::HashMap;

use num::One;
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct IncrU64;

impl Snippet for IncrU64 {
    fn inputs() -> Vec<&'static str> {
        vec!["value"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["value + 1"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["value == u64::MAX"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let values = vec![
            U32s::new([u32::MAX, 0]),
            U32s::new([0, u32::MAX]),
            U32s::new([u32::MAX, u32::MAX - 1]),
            // U32s::new([u32::MAX, u32::MAX])
            U32s::<2>::try_from(rng.next_u32()).unwrap(),
            U32s::<2>::try_from(rng.next_u64()).unwrap(),
        ];
        values
            .into_iter()
            .map(|value| {
                let mut stack = get_init_tvm_stack();
                push_hashable(&mut stack, &value);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }

    fn stack_diff() -> isize {
        0
    }

    fn entrypoint(&self) -> &'static str {
        "incr_u64"
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        const TWO_POW_32: &str = "4294967296";
        format!(
            "
            // Before: _ value_hi value_lo
            // After: _ (value + 1)_hi (value + 1)_lo
            {entrypoint}_carry:
                pop
                push 1
                add
                dup0
                push {TWO_POW_32}
                eq
                push 0
                eq
                assert
                push 0
                return

            {entrypoint}:
                push 1
                add
                dup0
                push {TWO_POW_32}
                eq
                skiz
                    call {entrypoint}_carry
                return
            ",
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let a: u32 = stack.pop().unwrap().try_into().unwrap();
        let b: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab = U32s::<2>::new([a, b]);
        let ab_incr = ab + U32s::one();
        let mut res = ab_incr.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;
    use crate::{get_init_tvm_stack, push_hashable};

    use super::*;

    #[test]
    fn incr_u64_test() {
        rust_tasm_equivalence_prop_new::<IncrU64>(IncrU64);
    }

    #[test]
    fn incr_u64_benchmark() {
        bench_and_write::<IncrU64>(IncrU64);
    }

    #[test]
    #[should_panic]
    fn incr_u64_negative_tasm_test() {
        let mut stack = get_init_tvm_stack();
        let u64_max = U32s::<2>::try_from(u64::MAX).unwrap();
        push_hashable(&mut stack, &u64_max);
        IncrU64.run_tasm_old(&mut stack, vec![], vec![], &mut HashMap::default(), 0);
    }

    #[test]
    #[should_panic]
    fn incr_u64_negative_rust_test() {
        let mut stack = get_init_tvm_stack();
        let u64_max = U32s::<2>::try_from(u64::MAX).unwrap();
        push_hashable(&mut stack, &u64_max);
        IncrU64::rust_shadowing(&mut stack, vec![], vec![], &mut HashMap::default());
    }
}
