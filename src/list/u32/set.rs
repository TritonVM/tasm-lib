use std::collections::HashMap;

use rand::{random, thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;

use crate::library::Library;
use crate::rust_shadowing_helper_functions::insert_random_list;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

pub struct Set<const N: usize>;

impl<const N: usize> NewSnippet for Set<N> {
    fn inputs() -> Vec<&'static str> {
        // See: https://github.com/TritonVM/tasm-snippets/issues/13
        // _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
        vec![vec!["element"; N], vec!["*list", "index"]].concat()
    }

    fn outputs() -> Vec<&'static str> {
        vec![]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        fn prepare_state<const N: usize>() -> ExecutionState {
            let list_length: usize = thread_rng().gen_range(1..100);
            let index: usize = thread_rng().gen_range(0..list_length);
            let mut stack = get_init_tvm_stack();
            let mut push_value: Vec<BFieldElement> = random_elements(N);
            while let Some(element) = push_value.pop() {
                stack.push(element);
            }

            let list_pointer: BFieldElement = random();
            stack.push(list_pointer);
            stack.push(BFieldElement::new(index as u64));

            let mut memory = HashMap::default();
            insert_random_list::<N>(list_pointer, list_length, &mut memory);
            ExecutionState::with_stack_and_memory(stack, memory, 0)
        }

        vec![
            prepare_state::<N>(),
            prepare_state::<N>(),
            prepare_state::<N>(),
        ]
    }
}

impl<const N: usize> Snippet for Set<N> {
    fn stack_diff() -> isize {
        assert!(N < 17, "Max element size supported for list is 16");

        -2 - N as isize
    }

    fn entrypoint() -> &'static str {
        assert!(N < 17, "Max element size supported for list is 16");
        "list_set_element"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();

        let mut write_elements_to_memory_code = String::default();
        for i in 0..N {
            write_elements_to_memory_code.push_str("swap1\n");
            write_elements_to_memory_code.push_str("write_mem\n");
            write_elements_to_memory_code.push_str("pop\n");
            if i != N - 1 {
                // Prepare for next write. Not needed for last iteration.
                write_elements_to_memory_code.push_str("push 1\n");
                write_elements_to_memory_code.push_str("add\n");
            }
        }

        format!(
            "
                // BEFORE: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list index
                // AFTER: _
                {entrypoint}:
                    push {N}
                    mul
                    push 1
                    add
                    add

                    // stack: _ elem{{N - 1}}, elem{{N - 2}}, ..., elem{{0}} *list + offset

                    {write_elements_to_memory_code}
                    // stack: _ *list + offset
                    pop

                    return
                    "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let index: u32 = stack.pop().unwrap().try_into().unwrap();
        let list_pointer = stack.pop().unwrap();
        let mut element: [BFieldElement; N] = [BFieldElement::new(0); N];
        for ee in element.iter_mut() {
            *ee = stack.pop().unwrap();
        }
        rust_shadowing_helper_functions::list_set(list_pointer, index as usize, element, memory);
    }
}

#[cfg(test)]
mod list_set_tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::rust_shadowing_helper_functions::insert_random_list;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        rust_tasm_equivalence_prop_new::<Set<1>>();
        rust_tasm_equivalence_prop_new::<Set<2>>();
        rust_tasm_equivalence_prop_new::<Set<3>>();
        rust_tasm_equivalence_prop_new::<Set<4>>();
        rust_tasm_equivalence_prop_new::<Set<5>>();
        rust_tasm_equivalence_prop_new::<Set<6>>();
        rust_tasm_equivalence_prop_new::<Set<7>>();
        rust_tasm_equivalence_prop_new::<Set<8>>();
        rust_tasm_equivalence_prop_new::<Set<9>>();
        rust_tasm_equivalence_prop_new::<Set<10>>();
        rust_tasm_equivalence_prop_new::<Set<11>>();
        rust_tasm_equivalence_prop_new::<Set<12>>();
        rust_tasm_equivalence_prop_new::<Set<13>>();
        rust_tasm_equivalence_prop_new::<Set<14>>();
        rust_tasm_equivalence_prop_new::<Set<15>>();
        rust_tasm_equivalence_prop_new::<Set<16>>();
    }

    #[test]
    fn list_u32_n_is_one_set() {
        let list_address = BFieldElement::new(48);
        let insert_value = [BFieldElement::new(1337)];
        prop_set(list_address, 20, insert_value, 2);
    }

    #[test]
    fn list_u32_n_is_two_set() {
        let list_address = BFieldElement::new(1841);
        let push_value = [BFieldElement::new(133700), BFieldElement::new(32)];
        prop_set(list_address, 20, push_value, 0);
    }

    #[test]
    fn list_u32_n_is_five_set() {
        let list_address = BFieldElement::new(558);
        let push_value = [
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
        ];
        prop_set(list_address, 2313, push_value, 589);
    }

    #[test]
    fn list_u32_n_is_sixteen_set() {
        let list_address = BFieldElement::new(558);
        let push_value = [
            BFieldElement::new(133700),
            BFieldElement::new(32),
            BFieldElement::new(133700),
            BFieldElement::new(19990),
            BFieldElement::new(88888888),
            BFieldElement::new(1337001),
            BFieldElement::new(321),
            BFieldElement::new(1337001),
            BFieldElement::new(199901),
            BFieldElement::new(888888881),
            BFieldElement::new(1337002),
            BFieldElement::new(322),
            BFieldElement::new(1337002),
            BFieldElement::new(199902),
            BFieldElement::new(888888882),
            BFieldElement::new(888888883),
        ];
        prop_set(list_address, 231, push_value, 69);
    }

    fn prop_set<const N: usize>(
        list_address: BFieldElement,
        init_list_length: u32,
        push_value: [BFieldElement; N],
        index: u32,
    ) {
        let expected_end_stack = vec![get_init_tvm_stack()].concat();
        let mut init_stack = get_init_tvm_stack();

        for i in 0..N {
            init_stack.push(push_value[N - 1 - i]);
        }
        init_stack.push(list_address);
        init_stack.push(BFieldElement::new(index as u64));

        let mut vm_memory = HashMap::default();

        // Insert length indicator of list, lives on offset = 0 from `list_address`
        insert_random_list::<N>(list_address, init_list_length as usize, &mut vm_memory);

        let _execution_result = rust_tasm_equivalence_prop::<Set<N>>(
            &init_stack,
            &[],
            &[],
            &mut vm_memory,
            0,
            Some(&expected_end_stack),
        );

        // Verify that length indicator is unchanged
        assert_eq!(
            BFieldElement::new((init_list_length) as u64),
            vm_memory[&list_address]
        );

        // verify that value was inserted at expected place
        for i in 0..N {
            assert_eq!(
                push_value[i],
                vm_memory[&BFieldElement::new(
                    list_address.value() + 1 + N as u64 * index as u64 + i as u64
                )]
            );
        }
    }
}
