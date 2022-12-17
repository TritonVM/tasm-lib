use std::collections::HashMap;

use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct U32s2Incr();

impl Snippet for U32s2Incr {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "u32_2_incr"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        const TWO_POW_32: &str = "4294967296";
        format!(
            "
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
        _init_memory: HashMap<BFieldElement, BFieldElement>,
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

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

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
        U32s2Incr::run_tasm(&mut tasm_stack, vec![], vec![], HashMap::default());
    }

    #[test]
    #[should_panic]
    fn u32s_2_incr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let max_value = U32s::new([u32::MAX, u32::MAX]);
        init_stack.push(max_value.as_ref()[1].into());
        init_stack.push(max_value.as_ref()[0].into());

        let mut rust_stack = init_stack;
        U32s2Incr::rust_shadowing(&mut rust_stack, vec![], vec![], HashMap::default());
    }

    fn prop_incr(some_value: U32s<2>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in some_value.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<U32s2Incr>(
            &init_stack,
            &[],
            &[],
            HashMap::default(),
            expected,
        );
    }
}
