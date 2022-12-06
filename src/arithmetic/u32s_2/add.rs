use triton_vm::op_stack::OP_STACK_REG_COUNT;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::{arithmetic::u32::is_u32::IsU32, execute, snippet_trait::Snippet, ExecutionResult};

fn _u32s_2_add_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let is_u32_code = IsU32::get_code();
    const MINUS_2_POW_32: &str = "18446744065119617025";
    let code: &str = &format!(
        "
        call u32s_2_add_start
        halt

        u32s_2_add_start:
            swap1
            swap2
            add
            dup0
            {is_u32_code}
            push 0
            eq
            skiz
                call u32s_2_add_carry
            swap2
            add
            dup0
            {is_u32_code}
            assert
            swap1
            return

        u32s_2_add_carry:
            push {MINUS_2_POW_32}
            add
            swap2
            push 1
            add
            swap2
            return
    "
    );
    execute(code, stack, OP_STACK_REG_COUNT + 2, vec![], vec![])
}

fn _u32s_2_add_rust(stack: &mut Vec<BFieldElement>) {
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
        init_stack.push(rhs.as_ref()[1].into());
        init_stack.push(rhs.as_ref()[0].into());
        init_stack.push(lhs.as_ref()[1].into());
        init_stack.push(lhs.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let execution_result = _u32s_2_add_tasm(&mut tasm_stack);
        println!(
            "Cycle count for `_u32s_2_add_tasm`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `_u32s_2_add_tasm`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        _u32s_2_add_rust(&mut rust_stack);

        assert_eq!(tasm_stack, rust_stack, "Rust code must match TVM for `add`");
        if let Some(expected) = expected {
            assert_eq!(
                tasm_stack, expected,
                "TVM must produce expected stack. lhs: {lhs}, rhs: {rhs}"
            );
        }
    }
}
