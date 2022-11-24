use num::One;
use triton_vm::op_stack::OP_STACK_REG_COUNT;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::{execute, ExecutionResult};

type _H = RescuePrimeRegular;

fn _u32s_2_incr_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let code: &str = "
        call u32s_2_incr
        halt

        u32s_2_incr:
            push 1
            add
            dup0
            push 4294967296
            eq
            skiz
                call u32s_2_carry_incr
            return

        u32s_2_carry_incr:
            pop
            push 1
            add
            dup0
            push 4294967296
            eq
            push 0
            eq
            assert
            push 0
            return
    ";
    execute(code, stack, OP_STACK_REG_COUNT + 2, vec![], vec![])
}

fn _u32s_2_incr_rust(stack: &mut Vec<BFieldElement>) {
    let a: u32 = stack.pop().unwrap().try_into().unwrap();
    let b: u32 = stack.pop().unwrap().try_into().unwrap();
    let ab = U32s::<2>::new([a, b]);
    let ab_incr = ab + U32s::one();
    let mut res = ab_incr.to_sequence();
    for _ in 0..res.len() {
        stack.push(res.pop().unwrap());
    }
}

fn _u32s_2_decr_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    const MINUS_ONE: &str = "18446744069414584320";
    const U32_MAX: &str = "4294967295";
    let code: &str = &format!(
        "
        call u32s_2_decr
        halt

        u32s_2_decr:
            push {MINUS_ONE}
            add
            dup0
            push {MINUS_ONE}
            eq
            skiz
                call u32s_2_carry_decr
            return

            u32s_2_carry_decr:
            pop
            push {MINUS_ONE}
            add
            dup0
            push {MINUS_ONE}
            eq
            push 0
            eq
            assert
            push {U32_MAX}
            return
    "
    );

    execute(code, stack, OP_STACK_REG_COUNT + 2, vec![], vec![])
}

fn _u32s_2_decr_rust(stack: &mut Vec<BFieldElement>) {
    let a: u32 = stack.pop().unwrap().try_into().unwrap();
    let b: u32 = stack.pop().unwrap().try_into().unwrap();
    let ab = U32s::<2>::new([a, b]);
    let ab_incr = ab - U32s::one();
    let mut res = ab_incr.to_sequence();
    for _ in 0..res.len() {
        stack.push(res.pop().unwrap());
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;

    use crate::get_init_tvm_stack;

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

        let mut tasm_stack = init_stack.clone();
        let _execution_result = _u32s_2_incr_tasm(&mut tasm_stack);
    }

    #[test]
    #[should_panic]
    fn u32s_2_incr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let max_value = U32s::new([u32::MAX, u32::MAX]);
        init_stack.push(max_value.as_ref()[1].into());
        init_stack.push(max_value.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        _u32s_2_incr_rust(&mut tasm_stack);
    }

    fn prop_incr(some_value: U32s<2>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(some_value.as_ref()[1].into());
        init_stack.push(some_value.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let execution_result = _u32s_2_incr_tasm(&mut tasm_stack);
        println!(
            "Cycle count for `u32s_2_incr`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `u32s_2_incr`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        _u32s_2_incr_rust(&mut rust_stack);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `hash`"
        );
    }

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_tasm() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        let _execution_result = _u32s_2_decr_tasm(&mut tasm_stack);
    }

    #[test]
    #[should_panic]
    fn u32s_2_decr_negative_rust() {
        let mut init_stack = get_init_tvm_stack();
        let zero = U32s::new([0, 0]);
        init_stack.push(zero.as_ref()[1].into());
        init_stack.push(zero.as_ref()[0].into());

        let mut tasm_stack = init_stack.clone();
        _u32s_2_decr_rust(&mut tasm_stack);
    }

    #[test]
    fn u32s_2_decr() {
        let mut rng = rand::thread_rng();

        prop_decr(U32s::new([u32::MAX, 0]));
        prop_decr(U32s::new([0, u32::MAX]));
        prop_decr(U32s::new([u32::MAX, u32::MAX - 1]));
        prop_decr(U32s::new([0, 1]));

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
        let execution_result = _u32s_2_decr_tasm(&mut tasm_stack);
        println!(
            "Cycle count for `u32s_2_decr`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `u32s_2_decr`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        _u32s_2_decr_rust(&mut rust_stack);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `hash`"
        );
    }
}
