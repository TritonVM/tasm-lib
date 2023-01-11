use std::collections::HashMap;

use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::library::Library;
use crate::snippet::Snippet;

pub struct DecrU64();

impl Snippet for DecrU64 {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "decr_u64"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        const U32_MAX: &str = "4294967295";

        format!(
            "
            {entrypoint}:
                push -1
                add
                dup0
                push -1
                eq
                skiz
                    call {entrypoint}_carry
                return

            {entrypoint}_carry:
                pop
                push -1
                add
                dup0
                push -1
                eq
                push 0
                eq
                assert
                push {U32_MAX}
                return
            "
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
        let ab_incr = ab - U32s::one();
        let mut res = ab_incr.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;

    use crate::{get_init_tvm_stack, snippet::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_tasm() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let _execution_result =
            DecrU64::run_tasm(&mut tasm_stack, vec![], vec![], &mut HashMap::default(), 0);
    }

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        DecrU64::rust_shadowing(&mut tasm_stack, vec![], vec![], &mut HashMap::default());
    }

    #[test]
    fn u32s_2_decr_simple_tasm() {
        let some_value = U32s::<2>::new([0, 14]);
        let mut tasm_stack = get_init_tvm_stack();
        tasm_stack.push(some_value.as_ref()[1].into());
        tasm_stack.push(some_value.as_ref()[0].into());

        let _execution_result =
            DecrU64::run_tasm(&mut tasm_stack, vec![], vec![], &mut HashMap::default(), 0);

        let expected_res = U32s::<2>::new([u32::MAX, 13]);
        let mut expected_stack = get_init_tvm_stack();
        expected_stack.push(expected_res.as_ref()[1].into());
        expected_stack.push(expected_res.as_ref()[0].into());
        assert_eq!(expected_stack, tasm_stack);
    }

    #[test]
    fn u32s_2_decr_prop() {
        prop_decr(U32s::new([u32::MAX, 0]));
        prop_decr(U32s::new([0, u32::MAX]));
        prop_decr(U32s::new([u32::MAX, u32::MAX - 1]));
        prop_decr(U32s::new([0, 1]));

        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            prop_decr(U32s::new([0, rng.gen()]));
            prop_decr(U32s::new([rng.gen(), rng.gen()]));
        }
    }

    fn prop_decr(some_value: U32s<2>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in some_value.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected = None;
        let _execution_result = rust_tasm_equivalence_prop::<DecrU64>(
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
