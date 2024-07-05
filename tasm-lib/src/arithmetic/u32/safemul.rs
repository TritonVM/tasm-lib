use rand::prelude::*;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

/// If the inputs, are valid u32s, then the output is guaranteed to be to.
/// Crashes on overflow.
#[derive(Clone, Debug)]
pub struct Safemul;

impl DeprecatedSnippet for Safemul {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u32_safemul".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["lhs".to_string(), "rhs".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U32, DataType::U32]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["lhs * rhs".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U32]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
                // BEFORE: _ rhs lhs
                // AFTER:  _ (lhs * rhs)
                {entrypoint}:
                    mul
                    dup 0  // _ (lhs * rhs) (lhs * rhs)
                    split  // _ (lhs * rhs) hi lo
                    pop 1  // _ (lhs * rhs) hi
                    push 0 // _ (lhs * rhs) hi 0
                    eq     // _ (lhs * rhs) (hi == 0)
                    assert // _ (lhs * rhs)
                    return
                    "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["result overflows u32".to_string()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut ret: Vec<InitVmState> = vec![];
        for _ in 0..10 {
            let mut stack = empty_stack();
            let lhs = thread_rng().gen_range(0..(1 << 16));
            let rhs = thread_rng().gen_range(0..(1 << 16));
            let lhs = BFieldElement::new(lhs as u64);
            let rhs = BFieldElement::new(rhs as u64);
            stack.push(lhs);
            stack.push(rhs);
            ret.push(InitVmState::with_stack(stack));
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::new(1 << 8), BFieldElement::new(1 << 9)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![
                    BFieldElement::new((1 << 15) - 1),
                    BFieldElement::new((1 << 16) - 1),
                ],
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let lhs: u32 = stack.pop().unwrap().try_into().unwrap();
        let rhs: u32 = stack.pop().unwrap().try_into().unwrap();

        let prod = lhs * rhs;
        stack.push(BFieldElement::new(prod as u64));
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num::Zero;

    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn snippet_test() {
        test_rust_equivalence_multiple_deprecated(&Safemul, true);
    }

    #[test]
    fn safe_sub_simple_test() {
        prop_safe_mul(1000, 1, Some(1000));
        prop_safe_mul(10_000, 900, Some(9_000_000));
        prop_safe_mul(1, 1, Some(1));
        prop_safe_mul(10_000, 10_000, Some(100_000_000));
        prop_safe_mul(u32::MAX, 1, Some(u32::MAX));
        prop_safe_mul(1, u32::MAX, Some(u32::MAX));
    }

    #[should_panic]
    #[test]
    fn overflow_test() {
        prop_safe_mul(1 << 16, 1 << 16, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_2() {
        prop_safe_mul(1 << 31, 2, None);
    }

    #[should_panic]
    #[test]
    fn overflow_test_3() {
        prop_safe_mul(2, 1 << 31, None);
    }

    fn prop_safe_mul(lhs: u32, rhs: u32, _expected: Option<u32>) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(rhs as u64));
        init_stack.push(BFieldElement::new(lhs as u64));

        let expected = lhs.checked_mul(rhs);
        let expected = [
            empty_stack(),
            vec![expected
                .map(|x| BFieldElement::new(x as u64))
                .unwrap_or_else(BFieldElement::zero)],
        ]
        .concat();

        test_rust_equivalence_given_input_values_deprecated(
            &Safemul,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected),
        );
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn safe_mul_benchmark() {
        bench_and_write(Safemul);
    }
}
