use std::collections::HashMap;

use rand::prelude::*;
use triton_vm::prelude::*;

use crate::empty_stack;
use crate::prelude::*;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct IsOdd;

impl DeprecatedSnippet for IsOdd {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_is_odd".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["value % 2".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        // pops a u32 from the stack and pushes a bool
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ value
                // AFTER:  _ (value % 2)
                {entrypoint}:
                    push 2
                    swap 1
                    div_mod
                    swap 1
                    pop 1
                    return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["if `value` is not a u32".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let n: u32 = rand::thread_rng().next_u32();

        let mut even_stack = empty_stack();
        let even_value = n - (n & 1);
        push_encodable(&mut even_stack, &even_value);

        let mut odd_stack = empty_stack();
        let odd_value = n | 1;
        push_encodable(&mut odd_stack, &odd_value);

        vec![
            InitVmState::with_stack(even_stack),
            InitVmState::with_stack(odd_stack),
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
        let value: u64 = stack.pop().unwrap().value();
        stack.push(BFieldElement::new(value % 2));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn is_odd_u32_test() {
        test_rust_equivalence_multiple_deprecated(&IsOdd, true);
    }

    #[test]
    fn u32_is_odd_test() {
        prop_is_odd(0);
        prop_is_odd(1);
        prop_is_odd(2);
        prop_is_odd(3);
        prop_is_odd(4);
        prop_is_odd(5);
        prop_is_odd(6);
        prop_is_odd(7);
        prop_is_odd(8);
        prop_is_odd(u32::MAX);

        let mut rng = thread_rng();
        for _ in 0..100 {
            let value = rng.next_u32();
            prop_is_odd(value);
        }
    }

    fn prop_is_odd(value: u32) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(value as u64));
        let mut expected_stack = empty_stack();
        expected_stack.push(BFieldElement::new((value % 2) as u64));

        test_rust_equivalence_given_input_values_deprecated(
            &IsOdd,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn is_odd_u32_benchmark() {
        bench_and_write(IsOdd);
    }
}
