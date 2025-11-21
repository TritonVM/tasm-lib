use std::collections::HashMap;

use itertools::Itertools;
use rand::prelude::*;
use triton_vm::isa::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::InitVmState;
use crate::empty_stack;
use crate::list::LIST_METADATA_SIZE;
use crate::list::new::New;
use crate::prelude::*;
use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::function::*;

/// Zips two lists of equal length, returning a new list of pairs of elements.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Zip {
    pub left_type: DataType,
    pub right_type: DataType,
}

impl Zip {
    pub fn new(left_type: DataType, right_type: DataType) -> Self {
        Self {
            left_type,
            right_type,
        }
    }
}

impl BasicSnippet for Zip {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list = |data_type| DataType::List(Box::new(data_type));

        let left_list = (list(self.left_type.clone()), "*left_list".to_string());
        let right_list = (list(self.right_type.clone()), "*right_list".to_string());
        vec![left_list, right_list]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        let list = |data_type| DataType::List(Box::new(data_type));

        let tuple_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);
        let output_list = (list(tuple_type), "*output_list".to_string());
        vec![output_list]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_list_higher_order_u32_zip_{}_with_{}",
            self.left_type.label_friendly_name(),
            self.right_type.label_friendly_name()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let output_type = DataType::Tuple(vec![self.left_type.clone(), self.right_type.clone()]);

        let new_output_list = library.import(Box::new(New));

        let entrypoint = self.entrypoint();
        let main_loop_label = format!("{entrypoint}_loop");

        let right_size = self.right_type.stack_size();
        let left_size = self.left_type.stack_size();
        let read_left_element = self.left_type.read_value_from_memory_leave_pointer();
        let read_right_element = self.right_type.read_value_from_memory_leave_pointer();
        let write_output_element = output_type.write_value_to_memory_leave_pointer();
        let left_size_plus_one = left_size + 1;
        let left_size_plus_three = left_size + 3;
        let sum_of_size = left_size + right_size;
        let sum_of_size_plus_two = sum_of_size + 2;
        assert!(
            sum_of_size_plus_two <= NUM_OP_STACK_REGISTERS,
            "zip only works for an output element size less than or equal to the available \
                op-stack words"
        );
        let minus_two_times_sum_of_size = -(2 * sum_of_size as i32);

        let mul_with_size = |n| match n {
            0 => triton_asm!(pop 1 push 0),
            1 => triton_asm!(),
            n => triton_asm!(
                push {n}
                mul
            ),
        };

        let main_loop = triton_asm!(
            // INVARIANT: _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word
            {main_loop_label}:
                // test return condition: *l == *l_elem_last_word
                dup 3
                dup 3
                eq

                skiz return
                // _*l *l_elem_last_word *r_elem_last_word *pair_elem_first_word

                dup 2
                {&read_left_element}
                // _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word [left_element] *l_elem_last_word_prev

                swap {left_size_plus_three}
                pop 1
                // _ *l *l_elem_last_word_prev *r_elem_last_word *pair_elem_first_word [left_element]

                dup {left_size_plus_one}
                // _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word [left_element] *r_elem_last_word

                {&read_right_element}
                // _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word [left_element] [right_element] *r_elem_last_word_prev

                swap {sum_of_size_plus_two}
                pop 1
                // _ *l *l_elem_last_word_prev *r_elem_last_word_prev *pair_elem_first_word [left_element] [right_element]

                dup {sum_of_size}
                // _ *l *l_elem_last_word_prev *r_elem_last_word_prev *pair_elem_first_word [right_element] [left_element] *pair_elem_first_word

                {&write_output_element}
                // _ *l *l_elem_last_word_prev *r_elem_last_word_prev *pair_elem_first_word *pair_elem_first_word_next

                push {minus_two_times_sum_of_size}
                add
                // _ *l *l_elem_last_word_prev *r_elem_last_word_prev *pair_elem_first_word *pair_elem_first_word_prev

                swap 1
                pop 1
                // _ *l *l_elem_last_word_prev *r_elem_last_word_prev *pair_elem_first_word_prev

                recurse
        );

        triton_asm!(
            // BEFORE: _ *left_list *right_list
            // AFTER:  _ *pair_list
            {entrypoint}:
            // get lengths
            dup 1                   // _ *left_list *right_list *left_list
            read_mem 1 pop 1        // _ *left_list *right_list left_len

            dup 1                   // _ *left_list *right_list left_len *right_list
            read_mem 1 pop 1        // _ *left_list *right_list left_len right_len

            // assert equal lengths
            dup 1                   // _ *left_list *right_list left_len right_len left_len
            eq assert               // _ *left_list *right_list len

            // create object for pair list and set length
            call {new_output_list}  // _ *left_list *right_list len *pair_list

            // Write length of *pair_list
            dup 1
            swap 1
            write_mem 1
            // _ *left_list *right_list len *pair_list_first_word

            // Change all pointers to point to end of lists, in preparation for loop
            dup 1
            push -1
            add
            // _ *left_list *right_list len *pair_list_first_word (len - 1)

            {&mul_with_size(sum_of_size)}
            add
            // _ *left_list *right_list len *pair_list_last_element_first_word

            swap 2
            // _ *left_list *pair_list_last_element_first_word len *right_list

            dup 1
            {&mul_with_size(right_size)}
            add
            // _ *left_list *pair_list_last_element_first_word len *r_list_last_elem_last_word

            swap 1
            // _ *left_list *pair_list_last_element_first_word *r_list_last_elem_last_word len

            {&mul_with_size(left_size)}
            // _ *left_list *pair_list_last_element_first_word *r_list_last_elem_last_word left_offset

            dup 3
            // _ *left_list *pair_list_last_element_first_word *r_list_last_elem_last_word left_offset *left_list

            add
            // _ *left_list *pair_list_last_element_first_word *r_list_last_elem_last_word *l_list_last_elem_last_word

            swap 2
            // _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word

            call {main_loop_label}
            // _ *l *l_elem_last_word *r_elem_last_word *pair_elem_first_word

            // Adjust *pair to point to list instead of element in list
            push {sum_of_size - 1}
            add

            swap 3

            pop 3

            return

            {&main_loop}
        )
    }
}

impl Function for Zip {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        use crate::rust_shadowing_helper_functions::dyn_malloc;
        use crate::rust_shadowing_helper_functions::list;

        let right_pointer = stack.pop().unwrap();
        let left_pointer = stack.pop().unwrap();

        let left_length = list::list_get_length(left_pointer, memory);
        let right_length = list::list_get_length(right_pointer, memory);
        assert_eq!(left_length, right_length);
        let len = left_length;

        let output_pointer = dyn_malloc::dynamic_allocator(memory);
        list::list_new(output_pointer, memory);
        list::list_set_length(output_pointer, len, memory);

        for i in 0..len {
            let left_item = list::list_get(left_pointer, i, memory, self.left_type.stack_size());
            let right_item = list::list_get(right_pointer, i, memory, self.right_type.stack_size());

            let pair = right_item.into_iter().chain(left_item).collect_vec();
            list::list_set(output_pointer, i, pair, memory);
        }

        stack.push(output_pointer);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng = StdRng::from_seed(seed);
        let list_len = rng.random_range(0..20);
        let execution_state = self.generate_input_state(list_len, list_len);
        FunctionInitialState {
            stack: execution_state.stack,
            memory: execution_state.nondeterminism.ram,
        }
    }
}

impl Zip {
    fn generate_input_state(&self, left_length: usize, right_length: usize) -> InitVmState {
        let fill_with_random_elements =
            |data_type: &DataType, list_pointer, list_len, memory: &mut _| {
                untyped_insert_random_list(list_pointer, list_len, memory, data_type.stack_size())
            };

        let left_pointer = BFieldElement::new(0);
        let left_size = LIST_METADATA_SIZE + left_length * self.left_type.stack_size();
        let right_pointer = left_pointer + BFieldElement::new(left_size as u64);

        let mut memory = HashMap::default();
        fill_with_random_elements(&self.left_type, left_pointer, left_length, &mut memory);
        fill_with_random_elements(&self.right_type, right_pointer, right_length, &mut memory);

        let stack = [empty_stack(), vec![left_pointer, right_pointer]].concat();

        InitVmState::with_stack_and_memory(stack, memory)
    }
}

#[cfg(test)]
mod tests {
    use proptest::collection::vec;

    use super::*;
    use crate::rust_shadowing_helper_functions::list;
    use crate::test_prelude::*;

    #[test]
    fn prop_test_xfe_digest() {
        ShadowedFunction::new(Zip::new(DataType::Xfe, DataType::Digest)).test();
    }

    #[test]
    fn list_prop_test_more_types() {
        ShadowedFunction::new(Zip::new(DataType::Bfe, DataType::Bfe)).test();
        ShadowedFunction::new(Zip::new(DataType::U64, DataType::U32)).test();
        ShadowedFunction::new(Zip::new(DataType::Bool, DataType::Digest)).test();
        ShadowedFunction::new(Zip::new(DataType::U128, DataType::VoidPointer)).test();
        ShadowedFunction::new(Zip::new(DataType::U128, DataType::Digest)).test();
        ShadowedFunction::new(Zip::new(DataType::U128, DataType::U128)).test();
        ShadowedFunction::new(Zip::new(DataType::Digest, DataType::Digest)).test();
    }

    #[proptest]
    fn zipping_u32s_with_x_field_elements_correspond_to_bfieldcodec(
        left_list: Vec<u32>,
        #[strategy(vec(arb(), #left_list.len()))] right_list: Vec<XFieldElement>,
    ) {
        let left_pointer = bfe!(0);
        let right_pointer = bfe!(1_u64 << 60); // far enough

        let mut ram = HashMap::default();
        write_list_to_ram(&mut ram, left_pointer, &left_list);
        write_list_to_ram(&mut ram, right_pointer, &right_list);

        let mut stack = [empty_stack(), vec![left_pointer, right_pointer]].concat();

        let zip = Zip::new(DataType::U32, DataType::Xfe);
        zip.rust_shadow(&mut stack, &mut ram);
        let output_list_pointer = stack.pop().unwrap();
        let tasm_zipped = *Vec::decode_from_memory(&ram, output_list_pointer).unwrap();

        let rust_zipped = left_list.into_iter().zip_eq(right_list).collect_vec();
        prop_assert_eq!(rust_zipped, tasm_zipped);
    }

    fn write_list_to_ram<T: BFieldCodec + Copy>(
        ram: &mut HashMap<BFieldElement, BFieldElement>,
        list_pointer: BFieldElement,
        list: &[T],
    ) {
        list::list_new(list_pointer, ram);
        for &item in list {
            list::list_push(list_pointer, item.encode(), ram);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Zip::new(DataType::Xfe, DataType::Digest)).bench();
    }
}
