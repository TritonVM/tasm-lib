use std::collections::HashMap;

use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

/// Consumes top element which is interpreted as exponent. Pushes a
/// U32<2> to the top of the stack. So grows the stack by 1.
pub struct Pow2StaticU64();

impl NewSnippet for Pow2StaticU64 {
    fn inputs() -> Vec<&'static str> {
        vec!["i"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["(2^i)_hi", "(2^i)_lo"]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        (0..64)
            .map(|i: u32| {
                let mut stack = get_init_tvm_stack();
                push_hashable(&mut stack, &i);
                ExecutionState::with_stack(stack)
            })
            .collect()
    }
}

impl Snippet for Pow2StaticU64 {
    fn stack_diff() -> isize {
        1
    }

    fn entrypoint() -> &'static str {
        "pow2_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let the_big_skiz: String = (0..64)
            .map(|j| {
                format!(
                    "
                    dup0
                    push {j} eq
                    skiz call {entrypoint}_{j}
                    "
                )
            })
            .collect::<Vec<_>>()
            .concat();

        let the_big_lebowski: String = (0..64)
            .map(|j| {
                let two_pow_j = 2u64.pow(j % 32);
                let push_two_pow_j = if j < 32 {
                    // 2^j is pushed in reverse for `swap2` to move `i` back to top
                    format!(
                        "push {two_pow_j} push 0  // -> _ i lo hi
                        swap2  // -> _ hi lo i
                        "
                    )
                } else {
                    format!(
                        "push 0 push {two_pow_j}  // -> _ i lo hi
                        swap2  // -> _ hi lo i
                        "
                    )
                };

                format!(
                    "
                    // Before: _ i
                    // After: _ hi lo i
                    {entrypoint}_{j}:
                        {push_two_pow_j}
                        return
                "
                )
            })
            .collect::<Vec<_>>()
            .concat();

        format!(
            "
            // Before: _ i
            // After: _ hi lo
            {entrypoint}:
                {the_big_skiz}

                pop
                return

            {the_big_lebowski}
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // Find exponent
        let mut exponent: u32 = stack.pop().unwrap().try_into().unwrap();
        let mut res = U32s::<2>::one();

        while exponent > 0 {
            res.mul_two();
            exponent -= 1;
        }

        let mut res = res.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::One;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn pow2_static_test() {
        rust_tasm_equivalence_prop_new::<Pow2StaticU64>();
    }

    #[test]
    fn pow2_static_benchmark() {
        bench_and_write::<Pow2StaticU64>();
    }

    fn prop_exp_static(exponent: u8) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(exponent as u64));

        let expected = None;
        let mut execution_result = rust_tasm_equivalence_prop::<Pow2StaticU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );

        let a = execution_result.final_stack.pop().unwrap().value();
        assert!(a < u32::MAX as u64);
        let b = execution_result.final_stack.pop().unwrap().value();
        assert!(b < u32::MAX as u64);
        let actual_res = U32s::<2>::new([a as u32, b as u32]);
        let mut expected_res = U32s::<2>::one();
        for _ in 0..exponent {
            expected_res.mul_two();
        }

        assert_eq!(expected_res, actual_res);
    }

    #[test]
    fn all_exponents_static() {
        for i in 0..64 {
            prop_exp_static(i);
        }
    }
}
