use std::collections::HashMap;

use num::{One, Zero};
use rand::RngCore;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct IsU32;

impl Snippet for IsU32 {
    fn inputs(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        vec!["value < 2^32".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U32]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Bool]
    }

    fn crash_conditions() -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let n: u32 = rand::thread_rng().next_u32();

        let mut true_stack = get_init_tvm_stack();
        push_hashable(&mut true_stack, &n);

        let mut false_stack = get_init_tvm_stack();
        push_hashable(&mut false_stack, &(u32::MAX));

        vec![
            ExecutionState::with_stack(true_stack),
            ExecutionState::with_stack(false_stack),
        ]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_is_u32".to_string()
    }

    /// Place 1 on stack iff top element is less than $2^32$. Otherwise
    /// place 0 on stack. Consumes top element of stack, leaves a boolean
    /// on top of stack. So this subroutine does not change the height
    /// of the stack
    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            {entrypoint}:
                        // _ a
                split   // _ hi lo
                pop     // _ hi
                push 0  // _ hi 0
                eq      // _ (hi == 0)
                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
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
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn is_u32_test() {
        rust_tasm_equivalence_prop_new::<IsU32>(IsU32);
    }

    #[test]
    fn is_u32_benchmark() {
        bench_and_write::<IsU32>(IsU32);
    }

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
            IsU32,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            expected,
        );
    }
}
