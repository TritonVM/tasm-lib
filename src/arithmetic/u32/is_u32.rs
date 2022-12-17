use std::collections::HashMap;

use num::{One, Zero};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet_trait::Snippet;

pub struct IsU32();

impl Snippet for IsU32 {
    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "is_u32"
    }

    /// Place 1 on stack iff top element is less than $2^32$. Otherwise
    /// place 0 on stack. Consumes top element of stack, leaves a boolean
    /// on top of stack. So this subroutine does not change the height
    /// of the stack
    fn function_body(_library: &mut Library) -> String {
        let mut unrolled_loop: String = String::default();
        let entrypoint = Self::entrypoint();
        for _ in 0..32 {
            unrolled_loop.push_str("lsb\n");
            unrolled_loop.push_str("pop\n");
        }
        let code: &str = &format!(
            "
            {entrypoint}:
                {unrolled_loop}
                push 0
                eq
                return
            "
        );

        code.to_string()
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _init_memory: HashMap<BFieldElement, BFieldElement>,
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

    use crate::{get_init_tvm_stack, snippet_trait::rust_tasm_equivalence_prop};

    use super::*;

    #[test]
    fn is_u32_simple() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        prop_is_u32(BFieldElement::zero(), Some(&stack_true));
        prop_is_u32(BFieldElement::one(), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 10), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 20), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 30), Some(&stack_true));
        prop_is_u32(BFieldElement::new(1 << 40), Some(&stack_false));
        prop_is_u32(BFieldElement::new(1 << 50), Some(&stack_false));
        prop_is_u32(BFieldElement::new(1 << 60), Some(&stack_false));
        prop_is_u32(
            BFieldElement::new((1 << 63) + (1 << 42)),
            Some(&stack_false),
        );
    }

    #[test]
    fn is_u32_pbt() {
        let stack_true: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();

        let mut rng = rand::thread_rng();
        for _ in 0..10 {
            prop_is_u32(BFieldElement::new(rng.next_u32() as u64), Some(&stack_true));
            prop_is_u32(
                BFieldElement::new((rng.next_u32() as u64) + (1u64 << 32)),
                Some(&stack_false),
            );
        }
    }

    fn prop_is_u32(some_value: BFieldElement, expected: Option<&[BFieldElement]>) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(some_value);

        let _execution_result = rust_tasm_equivalence_prop::<IsU32>(
            &init_stack,
            &[],
            &[],
            HashMap::default(),
            expected,
        );
    }
}
