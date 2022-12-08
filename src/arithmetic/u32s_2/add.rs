use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::{arithmetic::u32::is_u32::IsU32, snippet_trait::Snippet};

pub struct U32Add();

const SNIPPET_NAME: &str = "u32_2_add";

impl Snippet for U32Add {
    const STACK_DIFF: isize = -2;
    const NAME: &'static str = SNIPPET_NAME;

    /// Four top elements of stack are assumed to be valid u32s. So to have
    /// a value that's less than 2^32.
    fn get_function() -> String {
        let is_u32_code = IsU32::get_function();
        let is_u32_function_name = IsU32::NAME;
        const MINUS_2_POW_32: &str = "18446744065119617025";
        let code: &str = &format!(
            "
        // Import the `is_u32` function
        {is_u32_code}
        {SNIPPET_NAME}_carry:
            push {MINUS_2_POW_32}
            add
            swap2
            push 1
            add
            swap2
            return

        {SNIPPET_NAME}:
            swap1
            swap2
            add
            dup0
            call {is_u32_function_name}
            push 0
            eq
            skiz
                call {SNIPPET_NAME}_carry
            swap2
            add
            dup0
            call {is_u32_function_name}
            assert
            swap1
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
        // top element on stack
        let a0: u32 = stack.pop().unwrap().try_into().unwrap();
        let b0: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab0 = U32s::<2>::new([a0, b0]);

        // second element on stack
        let a1: u32 = stack.pop().unwrap().try_into().unwrap();
        let b1: u32 = stack.pop().unwrap().try_into().unwrap();
        let ab1 = U32s::<2>::new([a1, b1]);
        let ab0_plus_ab1 = ab0 + ab1;
        let mut res = ab0_plus_ab1.to_sequence();
        for _ in 0..res.len() {
            stack.push(res.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::{One, Zero};
    use rand::RngCore;

    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn u32s_2_add_no_overflow() {
        // 127 + 129 = 256
        let mut expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::zero(), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 0]),
            U32s::new([129, 0]),
            Some(&expected_end_stack),
        );

        // 127 + 129 + 45 * 2^32 + 1000 * 2^32 = 256 + 1045*2^32
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::new(1045), BFieldElement::new(256)],
        ]
        .concat();
        prop_add(
            U32s::new([127, 45]),
            U32s::new([129, 1000]),
            Some(&expected_end_stack),
        );

        // (2^32 - 1) + 0 + 0 * 2^32 + 2004 * 2^32 = (2^32 - 1) + 2004*2^32
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![
                BFieldElement::new(2004),
                BFieldElement::new(u32::MAX as u64),
            ],
        ]
        .concat();
        prop_add(
            U32s::new([u32::MAX, 0]),
            U32s::new([0, 2004]),
            Some(&expected_end_stack),
        );

        // (2^31 - 1) + 2^31 + 14 * 2^32 + 10^9 * 2^32 = (2^32 - 1) + (10^9 + 14) * 2^32
        expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![
                BFieldElement::new(1_000_000_014),
                BFieldElement::new(u32::MAX as u64),
            ],
        ]
        .concat();
        prop_add(
            U32s::new([(1 << 31) - 1, 14]),
            U32s::new([1 << 31, 1_000_000_000]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_add_with_overflow_in_least_significant_u32() {
        // 2 ^ 31 + 2 ^ 31 = 0 + 1 * 2 ^32
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::one(), BFieldElement::zero()],
        ]
        .concat();
        prop_add(
            U32s::new([1 << 31, 0]),
            U32s::new([1 << 31, 0]),
            Some(&expected_end_stack),
        );

        // 2 ^ 32 + 2 ^ 32 - 1 - 2 = (1^32 - 3) + 1 * 2^32
        let expected_end_stack = vec![
            get_init_tvm_stack(),
            vec![BFieldElement::one(), BFieldElement::new((1 << 32) - 3)],
        ]
        .concat();
        prop_add(
            U32s::new([((1u64 << 32) - 1) as u32, 0]),
            U32s::new([((1u64 << 32) - 2) as u32, 0]),
            Some(&expected_end_stack),
        );
    }

    #[test]
    fn u32s_2_add_pbt() {
        let mut rng = rand::thread_rng();
        for _ in 0..100 {
            prop_add(
                U32s::new([rng.next_u32(), rng.next_u32() / 2]),
                U32s::new([rng.next_u32(), rng.next_u32() / 2]),
                None,
            );
        }
    }

    fn prop_add(lhs: U32s<2>, rhs: U32s<2>, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        for elem in rhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }
        for elem in lhs.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let mut tasm_stack = init_stack.clone();
        let execution_result = U32Add::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `_u32s_2_add_tasm`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `_u32s_2_add_tasm`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        U32Add::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(tasm_stack, rust_stack, "Rust code must match TVM for `add`");
        if let Some(expected) = expected {
            assert_eq!(
                tasm_stack, expected,
                "TVM must produce expected stack. lhs: {lhs}, rhs: {rhs}"
            );
        }
    }
}
