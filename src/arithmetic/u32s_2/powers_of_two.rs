use num::One;
use twenty_first::{
    amount::u32s::U32s, shared_math::b_field_element::BFieldElement,
    util_types::algebraic_hasher::Hashable,
};

use crate::snippet_trait::Snippet;

const SNIPPET_NAME: &str = "u32_2_powers_of_two";

/// Consumes top element which is interpreted as exponent. Pushes a
/// U32<2> to the top of the stack. So grows the stack by 1.
pub struct U322PowersOfTwoMemory();
pub struct U322PowersOfTwoLookup();

impl Snippet for U322PowersOfTwoMemory {
    const STACK_DIFF: isize = 1;
    const NAME: &'static str = SNIPPET_NAME;

    fn get_function() -> String {
        // Assumes that the top stack element is below 64. Otherwise undefined.
        let two_pow_32: &str = "4294967296";
        let init_mem_code: String = init_mem_code();

        let _tmp = format!(
            "
            {SNIPPET_NAME}:
                push 1
                push 0
                swap2
                dup0
                push 0
                eq
                skiz
                    call u32_2_powers_of_two_end
                call u32_2_powers_of_two_do_while
            u32_2_powers_of_two_do_while:
                push -1
                add
                swap2
                swap1
                push 2
                mul
                dup0
                push {two_pow_32}
                eq
                skiz
                    call u32_2_powers_of_two_carry
                swap1
                swap2
                dup0
                skiz
                    recurse
            u32_2_powers_of_two_end:
                pop
                push 0
                swap2
                push 1
                eq
                skiz
                    swap1
                return

            {SNIPPET_NAME}_carry:
                pop
                pop
                push 1
                push 1
                return
        "
        );

        format!(
            "
            {SNIPPET_NAME}:
                call {SNIPPET_NAME}_init_mem
                push 0    // will get overwritten
                read_mem  // _ i (2^i)
                swap1
                pop
                return

            {SNIPPET_NAME}_init_mem:
                {init_mem_code}
                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
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

fn init_mem_code() -> String {
    (0..32)
        .map(|i| {
            let two_pow_i = 2u64.pow(i);
            format!(
                "
                push {i} push {two_pow_i} write_mem pop pop\n"
            )
        })
        .collect::<Vec<_>>()
        .concat()
}

impl Snippet for U322PowersOfTwoLookup {
    const STACK_DIFF: isize = -1;

    const NAME: &'static str = "u32_2_powers_of_two_lookup";

    fn get_function() -> String {
        todo!()
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
    ) {
        U322PowersOfTwoMemory::rust_shadowing(stack, std_in, secret_in);
    }
}

#[cfg(test)]
mod tests {
    use num::One;

    use crate::get_init_tvm_stack;

    use super::*;

    fn prop_exp(exponent: u8) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(BFieldElement::new(exponent as u64));

        let mut tasm_stack = init_stack.clone();
        let execution_result = U322PowersOfTwoMemory::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for {} ({})`: {}",
            U322PowersOfTwoMemory::NAME,
            exponent,
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `{}`: {}",
            U322PowersOfTwoMemory::NAME,
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        U322PowersOfTwoMemory::rust_shadowing(&mut rust_stack, vec![], vec![]);
        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `U322PowersOfTwo`"
        );
        let a = tasm_stack.pop().unwrap().value();
        assert!(a < u32::MAX as u64);
        let b = tasm_stack.pop().unwrap().value();
        assert!(b < u32::MAX as u64);
        let actual_res = U32s::<2>::new([a as u32, b as u32]);
        let mut expected_res = U32s::<2>::one();
        for _ in 0..exponent {
            expected_res.mul_two();
        }

        assert_eq!(expected_res, actual_res);
    }

    #[test]
    fn two_pow_zero_equals_one() {
        prop_exp(0);
    }

    #[test]
    fn all_exponents() {
        prop_exp(2);
        // for i in 1..64 {
        //     prop_exp(i);
        // }
    }
}
