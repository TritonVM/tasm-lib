use std::collections::HashMap;

use num::One;
use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::rust_shadowing_helper_functions::unsafe_list::unsafe_insert_random_list;
use crate::snippet::{DataType, Snippet};
use crate::snippet_state::SnippetState;
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone, Debug)]
pub struct UnsafePop(pub DataType);

impl Snippet for UnsafePop {
    fn inputs(&self) -> Vec<String> {
        vec!["*list".to_string()]
    }

    fn outputs(&self) -> Vec<String> {
        let mut ret: Vec<String> = vec![];
        let element_size = self.0.get_size();
        for i in 0..element_size {
            ret.push(format!("element_{}", element_size - 1 - i));
        }

        ret
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(self.0.clone()))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![self.0.clone()]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["stack underflow".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![prepare_state(&self.0)]
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize - 1
    }

    fn entrypoint(&self) -> String {
        format!("tasm_list_unsafe_u32_pop_{}", self.0)
    }

    /// Pop last element from list. Does *not* actually delete the last
    /// element but instead leaves it in memory.
    fn function_body(&self, _library: &mut SnippetState) -> String {
        let entry_point = self.entrypoint();

        let mut code_to_read_elements = String::default();
        // Start and end at loop: Stack: _  [elems], address_for_last_unread_element
        for i in 0..self.0.get_size() {
            code_to_read_elements.push_str("read_mem\n");
            // stack: _  address_for_last_unread_element, elem_{{N - 1 - i}}

            code_to_read_elements.push_str("swap 1\n");
            // stack: _  [..., elem_{{N - 1 - i}}], address_for_last_unread_element
            if i != self.0.get_size() - 1 {
                // Update offset for last unread element
                code_to_read_elements.push_str("push -1\n");
                code_to_read_elements.push_str("add\n");
            }
        }

        let element_size = self.0.get_size();
        let mul_with_size = if element_size != 1 {
            format!("push {element_size}\n mul\n")
        } else {
            String::default()
        };
        format!(
            // Before: _ *list
            // After: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}}
            "{entry_point}:
                read_mem
                // stack : _  *list, length

                // Assert that length is over 0
                dup 0
                push 0
                eq
                push 0
                eq
                assert
                // stack : _  *list, length

                // Decrease length value by one and write back to memory
                swap 1
                dup 1
                push -1
                add
                write_mem
                swap 1
                // stack : _  *list, initial_length

                {mul_with_size}
                // stack : _  *list, offset_for_last_element = (N * initial_length)

                add
                // stack : _  address_for_last_element

                {code_to_read_elements}
                // Stack: _  [elements], address_for_last_unread_element

                pop
                // Stack: _  [elements]

                return
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_address = stack.pop().unwrap();
        let initial_list_length = memory[&list_address];

        // Update length indicator
        memory.insert(list_address, initial_list_length - BFieldElement::one());

        let mut last_used_address =
            list_address + initial_list_length * BFieldElement::new(self.0.get_size() as u64);

        for _ in 0..self.0.get_size() {
            let elem = memory[&last_used_address];
            stack.push(elem);
            last_used_address -= BFieldElement::one();
        }
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state(&self.0)
    }
}

fn prepare_state(data_type: &DataType) -> ExecutionState {
    let list_pointer: u32 = random();
    let list_pointer = BFieldElement::new(list_pointer as u64);
    let old_length: usize = thread_rng().gen_range(1..30);
    let mut stack = get_init_tvm_stack();
    stack.push(list_pointer);
    let mut memory = HashMap::default();
    unsafe_insert_random_list(list_pointer, old_length, &mut memory, data_type.get_size());
    ExecutionState::with_stack_and_memory(stack, memory, 0)
}

#[cfg(test)]
mod tests_pop {
    use itertools::Itertools;
    use num::Zero;
    use rand::{thread_rng, RngCore};
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<UnsafePop>(UnsafePop(DataType::U32));
        rust_tasm_equivalence_prop_new::<UnsafePop>(UnsafePop(DataType::U64));
        rust_tasm_equivalence_prop_new::<UnsafePop>(UnsafePop(DataType::XFE));
        rust_tasm_equivalence_prop_new::<UnsafePop>(UnsafePop(DataType::Digest));
    }

    #[test]
    fn unsafe_pop_benchmark() {
        bench_and_write(UnsafePop(DataType::Digest));
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_1() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::BFE, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_2() {
        let list_address = BFieldElement::new(48);
        prop_pop::<2>(DataType::U64, list_address, 0);
    }

    #[test]
    #[should_panic]
    fn panic_if_pop_on_empty_list_3() {
        let list_address = BFieldElement::new(48);
        prop_pop::<3>(DataType::XFE, list_address, 0);
    }

    #[test]
    fn list_u32_n_is_n_pop() {
        let list_address = BFieldElement::new(48);
        prop_pop::<1>(DataType::BFE, list_address, 24);
        prop_pop::<2>(DataType::U64, list_address, 48);
        prop_pop::<3>(DataType::XFE, list_address, 3);
        // prop_pop::<4>(list_address, 4);
        prop_pop::<5>(DataType::Digest, list_address, 20);
        // prop_pop::<6>(list_address, 20);
        // prop_pop::<7>(list_address, 20);
        // prop_pop::<8>(list_address, 20);
        // prop_pop::<9>(list_address, 20);
        // prop_pop::<10>(list_address, 1);
        // prop_pop::<11>(list_address, 33);
        // prop_pop::<12>(list_address, 20);
        // prop_pop::<13>(list_address, 20);
        // prop_pop::<14>(list_address, 20);
        // prop_pop::<15>(list_address, 20);
        // prop_pop::<16>(list_address, 20);
    }

    fn prop_pop<const N: usize>(
        data_type: DataType,
        list_address: BFieldElement,
        init_list_length: u32,
    ) {
        let mut init_stack = get_init_tvm_stack();
        init_stack.push(list_address);

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        vm_memory.insert(list_address, BFieldElement::new(init_list_length as u64));

        // Insert random values for the elements in the list
        let mut rng = thread_rng();
        let mut last_element: [BFieldElement; N] = [BFieldElement::zero(); N];
        let mut j = 1;
        for _ in 0..init_list_length {
            last_element = (0..N)
                .map(|_| BFieldElement::new(rng.next_u64()))
                .collect_vec()
                .try_into()
                .unwrap();
            for elem in last_element.iter() {
                vm_memory.insert(list_address + BFieldElement::new(j), *elem);
                j += 1;
            }
        }

        let mut expected_end_stack = get_init_tvm_stack();

        for i in 0..N {
            expected_end_stack.push(last_element[N - 1 - i]);
        }

        let _execution_result = rust_tasm_equivalence_prop::<UnsafePop>(
            UnsafePop(data_type),
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length is now indicated to be `init_list_length - 1`
        assert_eq!(
            BFieldElement::new(init_list_length as u64) - BFieldElement::one(),
            vm_memory[&list_address]
        );
    }
}
