use std::collections::HashMap;

use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::Snippet;

pub struct IncrU64();

impl Snippet for IncrU64 {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "incr_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
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
    use rand::Rng;

    use crate::get_init_tvm_stack;
    use crate::test_helpers::rust_tasm_equivalence_prop;

    use super::*;

    #[test]
    fn u32s_2_incr() {
        let mut rng = rand::thread_rng();

        prop_incr(U32s::new([u32::MAX, 0]));
        prop_incr(U32s::new([0, u32::MAX]));
        prop_incr(U32s::new([u32::MAX, u32::MAX - 1]));

        for _ in 0..10 {
            prop_incr(U32s::new([0, rng.gen()]));
            prop_incr(U32s::new([rng.gen(), rng.gen()]));
        }
    }

    #[test]
    #[should_panic]
    fn u32s_2_incr_negative_tasm() {
        let mut init_stack = get_init_tvm_stack();
        let max_value = U32s::new([u32::MAX, u32::MAX]);
        init_stack.push(max_value.as_ref()[1].into());
        init_stack.push(max_value.as_ref()[0].into());

        let mut tasm_stack = init_stack;
        IncrU64::run_tasm(&mut tasm_stack, vec![], vec![], &mut HashMap::default(), 0);
    }

    #[test]
    #[should_panic]
    fn u32s_2_incr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let max_value = U32s::new([u32::MAX, u32::MAX]);
        init_stack.push(max_value.as_ref()[1].into());
        init_stack.push(max_value.as_ref()[0].into());

        let mut rust_stack = init_stack;
        IncrU64::rust_shadowing(&mut rust_stack, vec![], vec![], &mut HashMap::default());
    }

    fn prop_incr(some_value: U32s<2>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in some_value.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<IncrU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
