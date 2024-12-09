use std::collections::HashMap;

use num::Zero;
use rand::prelude::*;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct Div2;

impl DeprecatedSnippet for Div2 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_div2".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["value_hi".to_string(), "value_lo".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(value / 2)_hi".to_string(), "(value / 2)_lo".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let two_pow_31 = 1u64 << 31;

        format!(
            "
                // BEFORE: _ value_hi value_lo
                // AFTER:  _ (value / 2)_hi (value / 2)_lo
                {entrypoint}:
                    // Divide the lower number
                    push 2
                    swap 1
                    div_mod
                    pop 1
                    // stack: _ value_hi (value_lo / 2)

                    // Divide the upper number and carry its least significant bit into the lower number
                    swap 1
                    // stack: _ (value_lo / 2) value_hi

                    push 2
                    swap 1
                    div_mod
                    // stack: _ (value_lo / 2) (value_hi / 2) (value_hi % 2)

                    push {two_pow_31}
                    mul
                    // stack: _ (value_lo / 2) (value_hi / 2) carry

                    swap 1
                    swap 2
                    // stack: _ (value_hi / 2) carry (value_lo / 2)

                    add
                    // stack: _ (value / 2)_hi (value / 2)_lo

                    return
                "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![
            "If value_hi is not a u32".to_string(),
            "If value_lo is not a u32".to_string(),
        ]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let n: u64 = rand::thread_rng().next_u64();
        let n: U32s<2> = n.try_into().unwrap();
        let mut input_stack = empty_stack();

        push_encodable(&mut input_stack, &n);

        vec![InitVmState::with_stack(input_stack)]
    }

    fn common_case_input_state(&self) -> InitVmState {
        InitVmState::with_stack(
            [
                empty_stack(),
                vec![BFieldElement::zero(), BFieldElement::new(1 << 31)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> InitVmState {
        let big_number = 1 << 31;
        let worst_case_input = [big_number + 1, big_number].map(BFieldElement::new);
        let worst_case_stack = [empty_stack(), worst_case_input.to_vec()].concat();

        InitVmState::with_stack(worst_case_stack)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let value_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let value_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let value: u64 = ((value_hi as u64) << 32) + value_lo as u64;
        let result: u64 = value / 2;

        stack.push(BFieldElement::new(result >> 32));
        stack.push(BFieldElement::new(result & u32::MAX as u64));
    }
}

#[cfg(test)]
mod tests {
    use rand::prelude::*;
    use BFieldElement;

    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn div2_u64_test() {
        test_rust_equivalence_multiple_deprecated(&Div2, true);
    }

    #[should_panic]
    #[test]
    fn lo_is_not_u32() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(16));
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));

        test_rust_equivalence_given_input_values_deprecated::<Div2>(
            &Div2,
            &init_stack,
            &[],
            HashMap::default(),
            None,
        );
    }

    #[should_panic]
    #[test]
    fn hi_is_not_u32() {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(u32::MAX as u64 + 1));
        init_stack.push(BFieldElement::new(16));

        test_rust_equivalence_given_input_values_deprecated::<Div2>(
            &Div2,
            &init_stack,
            &[],
            HashMap::default(),
            None,
        );
    }

    #[test]
    fn div_2_test() {
        prop_div_2(0);
        prop_div_2(1);
        prop_div_2(2);
        prop_div_2(3);
        prop_div_2(4);
        prop_div_2(5);
        prop_div_2(6);
        prop_div_2(7);
        prop_div_2(8);
        prop_div_2(1 << 32);
        prop_div_2((1 << 32) + 1);
        prop_div_2((1 << 32) + 2);
        prop_div_2((1 << 32) + 3);
        prop_div_2((1 << 32) + 4);
        prop_div_2((1 << 63) + 4);
        prop_div_2((1 << 63) + 4);
        prop_div_2((1 << 63) + (1 << 31));
        prop_div_2((1 << 63) + (1 << 33) + (1 << 32) + (1 << 31));

        let mut rng = thread_rng();
        for _ in 0..100 {
            let value = rng.next_u64();
            prop_div_2(value);
        }
    }

    fn prop_div_2(value: u64) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(value >> 32));
        init_stack.push(BFieldElement::new(value & u32::MAX as u64));
        let mut expected_stack = empty_stack();
        let res = value / 2;
        expected_stack.push(BFieldElement::new(res >> 32));
        expected_stack.push(BFieldElement::new(res & u32::MAX as u64));

        test_rust_equivalence_given_input_values_deprecated::<Div2>(
            &Div2,
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
    fn div2_u64_benchmark() {
        bench_and_write(Div2);
    }
}
