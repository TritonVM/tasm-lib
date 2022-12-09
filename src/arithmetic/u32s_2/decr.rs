use num::One;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::snippet_trait::Snippet;

const SNIPPET_NAME: &str = "u32_2_decr";

pub struct U32s2Decr();

impl Snippet for U32s2Decr {
    const STACK_DIFF: isize = 0;
    const NAME: &'static str = SNIPPET_NAME;

    fn get_function() -> String {
        const U32_MAX: &str = "4294967295";
        let code: &str = &format!(
            "
        {SNIPPET_NAME}_carry:
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

        {SNIPPET_NAME}:
            push -1
            add
            dup0
            push -1
            eq
            skiz
                call {SNIPPET_NAME}_carry
            return
    "
        );
        code.to_string()
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
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

    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_tasm() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let _execution_result = U32s2Decr::run_tasm(&mut tasm_stack, vec![], vec![]);
    }

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        U32s2Decr::rust_shadowing(&mut tasm_stack, vec![], vec![]);
    }

    #[test]
    fn u32s_2_decr_simple_tasm() {
        let some_value = U32s::<2>::new([0, 14]);
        let mut tasm_stack = get_init_tvm_stack();
        tasm_stack.push(some_value.as_ref()[1].into());
        tasm_stack.push(some_value.as_ref()[0].into());

        let _execution_result = U32s2Decr::run_tasm(&mut tasm_stack, vec![], vec![]);

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
        init_stack.push(some_value.as_ref()[1].into());
        init_stack.push(some_value.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let execution_result = U32s2Decr::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `u32s_2_decr`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `u32s_2_decr`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        U32s2Decr::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `hash`"
        );
    }
}
