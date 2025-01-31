use std::collections::HashMap;

use num::One;
use num::Zero;
use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct IsU32;

impl DeprecatedSnippet for IsU32 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_is_u32".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["value < 2^32".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    /// Place 1 on stack iff top element is less than $2^32$. Otherwise
    /// place 0 on stack. Consumes top element of stack, leaves a boolean
    /// on top of stack. So this subroutine does not change the height
    /// of the stack
    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            {entrypoint}:
                        // _ a
                split   // _ hi lo
                pop 1   // _ hi
                push 0  // _ hi 0
                eq      // _ (hi == 0)
                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let n: u32 = rand::thread_rng().next_u32();

        let mut true_stack = empty_stack();
        push_encodable(&mut true_stack, &n);

        let mut false_stack = empty_stack();
        push_encodable(&mut false_stack, &(u32::MAX));

        vec![
            InitVmState::with_stack(true_stack),
            InitVmState::with_stack(false_stack),
        ]
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack([empty_stack(), vec![BFieldElement::new(1 << 16)]].concat())
    }

    fn worst_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack([empty_stack(), vec![BFieldElement::new((1 << 32) - 1)]].concat())
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
    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn is_u32_test() {
        test_rust_equivalence_multiple_deprecated(&IsU32, true);
    }

    #[test]
    fn is_u32_simple() {
        let stack_true: Vec<BFieldElement> = [empty_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> = [empty_stack(), vec![BFieldElement::zero()]].concat();

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
        let stack_true: Vec<BFieldElement> = [empty_stack(), vec![BFieldElement::one()]].concat();
        let stack_false: Vec<BFieldElement> = [empty_stack(), vec![BFieldElement::zero()]].concat();

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
        let mut init_stack = empty_stack();
        init_stack.push(some_value);

        test_rust_equivalence_given_input_values_deprecated::<IsU32>(
            &IsU32,
            &init_stack,
            &[],
            HashMap::default(),
            expected,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn is_u32_benchmark() {
        bench_and_write(IsU32);
    }
}
