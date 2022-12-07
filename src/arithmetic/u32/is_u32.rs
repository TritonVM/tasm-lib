use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet_trait::Snippet;

pub struct IsU32();

impl Snippet for IsU32 {
    const STACK_DIFF: isize = 0;

    fn get_name() -> String {
        "is_u32".to_string()
    }

    /// Place 1 on stack iff top element is less than $2^32$. Otherwise
    /// place 0 on stack. Consumes top element of stack, leaves a boolean
    /// on top of stack. So this subroutine does not change the height
    /// of the stack
    fn get_function() -> String {
        let mut unrolled_loop: String = String::default();
        for _ in 0..32 {
            unrolled_loop.push_str("lsb\n");
            unrolled_loop.push_str("pop\n");
        }
        let code: &str = &format!(
            "
        {unrolled_loop}
        push 0
        eq
    "
        );

        code.to_string()
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
    ) {
        let top = stack.pop().unwrap();
        stack.push(if top.value() < (1 << 32) {
            BFieldElement::one()
        } else {
            BFieldElement::zero()
        });
    }
}

#[cfg(test)]
mod tests {
    use rand::RngCore;

    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn is_u32_simple() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        prop_is_u32(BFieldElement::zero(), &stack_true);
        prop_is_u32(BFieldElement::one(), &stack_true);
        prop_is_u32(BFieldElement::new(1 << 10), &stack_true);
        prop_is_u32(BFieldElement::new(1 << 20), &stack_true);
        prop_is_u32(BFieldElement::new(1 << 30), &stack_true);
        prop_is_u32(BFieldElement::new(1 << 40), &stack_false);
        prop_is_u32(BFieldElement::new(1 << 50), &stack_false);
        prop_is_u32(BFieldElement::new(1 << 60), &stack_false);
        prop_is_u32(BFieldElement::new((1 << 63) + (1 << 42)), &stack_false);
    }

    #[test]
    fn is_u32_pbt() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            prop_is_u32(BFieldElement::new(rng.next_u32() as u64), &stack_true);
            prop_is_u32(
                BFieldElement::new((rng.next_u32() as u64) + (1u64 << 32)),
                &stack_false,
            );
        }
    }

    fn prop_is_u32(some_value: BFieldElement, expected: &[BFieldElement]) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(some_value);

        let mut tasm_stack = init_stack.clone();
        let execution_result = IsU32::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!(
            "Cycle count for `u32_is_u32`: {}",
            execution_result.cycle_count
        );
        println!(
            "Hash table height for `u32_is_u32`: {}",
            execution_result.hash_table_height
        );

        let mut rust_stack = init_stack;
        IsU32::rust_shadowing(&mut rust_stack, vec![], vec![]);

        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `is_u32`"
        );
        assert_eq!(expected, tasm_stack, "End TASM stack must match expected state. input: {some_value}\n expected: {expected:?}");
    }
}
