use std::collections::HashMap;

use num_traits::One;
use rand::prelude::*;
use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::list::LIST_METADATA_SIZE;
use crate::rust_shadowing_helper_functions::list::insert_random_list;
use crate::rust_shadowing_helper_functions::list::list_get;
use crate::rust_shadowing_helper_functions::list::list_set;
use crate::traits::algorithm::Algorithm;
use crate::traits::algorithm::AlgorithmInitialState;
use crate::traits::basic_snippet::BasicSnippet;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SwapUnchecked {
    element_type: DataType,
}

impl SwapUnchecked {
    pub fn new(element_type: DataType) -> Self {
        Self { element_type }
    }
}

impl BasicSnippet for SwapUnchecked {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let self_type = DataType::List(Box::new(self.element_type.to_owned()));

        vec![
            (self_type, "self".to_owned()),
            (DataType::U32, "a".to_owned()),
            (DataType::U32, "b".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_list_swap_{}",
            self.element_type.label_friendly_name()
        )
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let metadata_size = LIST_METADATA_SIZE;
        let element_size = self.element_type.stack_size();
        assert!(
            element_size + 2 < NUM_OP_STACK_REGISTERS,
            "This implementation can only handle swap up to element size 13"
        );

        let mul_with_size = if element_size == 1 {
            triton_asm!()
        } else {
            triton_asm!(
                push {element_size}
                mul
            )
        };

        let get_offset_for_last_word_in_element = if element_size == 1 {
            triton_asm!(
                // _ i

                push {metadata_size}
                add
                // _ i_offset_last_word
            )
        } else {
            triton_asm!(
                // _ i

                {&mul_with_size}
                // _ i_offset_internal

                push {metadata_size}
                add
                // _ i_offset

                push {element_size - 1}
                add
                // _ i_offset_last_word
            )
        };

        triton_asm!(
                // BEFORE: _ *list a b
                // AFTER:  _
                {entrypoint}:

                    // calculate *list[b]
                    // _ *list a b

                    {&get_offset_for_last_word_in_element}
                    // _ *list a b_offset_last_word

                    dup 2
                    add
                    // _ *list a *list[b]_last_word

                    {&self.element_type.read_value_from_memory_leave_pointer()}
                    // _ *list a [list[b]] (*list[b] - 1)

                    push 1
                    add
                    // _ *list a [list[b]] *list[b]

                    dup {element_size + 2}
                    dup {element_size + 2}
                    // _ *list a [list[b]] *list[b] *list a

                    {&get_offset_for_last_word_in_element}
                    // _ *list a [list[b]] *list[b] *list a_offset_last_word

                    add
                    // _ *list a [list[b]] *list[b] *list[a]_last_word

                    {&self.element_type.read_value_from_memory_leave_pointer()}
                    // _ *list a [list[b]] *list[b] [list[a]] (*list[a] - 1)

                    push 1
                    add
                    // _ *list a [list[b]] *list[b] [list[a]] *list[a]

                    swap {element_size + 1}
                    // _ *list a [list[b]] *list[a] [list[a]] *list[b]

                    {&self.element_type.write_value_to_memory_pop_pointer()}
                    // _ *list a [list[b]] *list[a]

                    // We leave pointer here, since it's more efficient to just
                    // pop all garbage in one fell swoop at the end.
                    {&self.element_type.write_value_to_memory_leave_pointer()}
                    // _ *list a *some_pointer

                    pop 3

                    return
        )
    }
}

impl Algorithm for SwapUnchecked {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism,
    ) {
        let b_index = stack.pop().unwrap().value() as usize;
        let a_index = stack.pop().unwrap().value() as usize;
        let list_pointer = stack.pop().unwrap();
        let element_size = self.element_type.stack_size();

        let a = list_get(list_pointer, a_index, memory, element_size);
        let b = list_get(list_pointer, b_index, memory, element_size);
        list_set(list_pointer, a_index, b, memory);
        list_set(list_pointer, b_index, a, memory);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> AlgorithmInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let list_pointer = BFieldElement::new(rng.gen());
        let list_length = rng.gen_range(1..200);
        let a = rng.gen_range(0..list_length);
        let b = rng.gen_range(0..list_length);
        self.initial_state(list_pointer, list_length, a, b)
    }

    fn corner_case_initial_states(&self) -> Vec<AlgorithmInitialState> {
        vec![
            self.initial_state(BFieldElement::one(), 5, 0, 0),
            self.initial_state(BFieldElement::one(), 1, 0, 0),
            self.initial_state(BFieldElement::one(), 2, 1, 1),
        ]
    }
}

impl SwapUnchecked {
    fn initial_state(
        &self,
        list_pointer: BFieldElement,
        list_length: usize,
        a: usize,
        b: usize,
    ) -> AlgorithmInitialState {
        let mut init_memory = HashMap::default();
        insert_random_list(
            &self.element_type,
            list_pointer,
            list_length,
            &mut init_memory,
        );

        AlgorithmInitialState {
            stack: [
                empty_stack(),
                vec![
                    list_pointer,
                    BFieldElement::new(a as u64),
                    BFieldElement::new(b as u64),
                ],
            ]
            .concat(),
            nondeterminism: NonDeterminism::default().with_ram(init_memory),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn test() {
        for data_type in [
            DataType::Bfe,
            DataType::Bool,
            DataType::U64,
            DataType::Xfe,
            DataType::U128,
            DataType::Digest,
            DataType::Tuple(vec![DataType::Xfe, DataType::Xfe]),
            DataType::Tuple(vec![DataType::Digest, DataType::U64]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Digest]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Xfe, DataType::Xfe]),
            DataType::Tuple(vec![DataType::Digest, DataType::Digest]),
            DataType::Tuple(vec![DataType::Xfe, DataType::Xfe, DataType::Digest]),
            DataType::Tuple(vec![
                DataType::Xfe,
                DataType::Xfe,
                DataType::Xfe,
                DataType::Xfe,
            ]),
            DataType::Tuple(vec![DataType::Digest, DataType::Digest, DataType::Xfe]),
        ] {
            ShadowedAlgorithm::new(SwapUnchecked::new(data_type)).test();
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench() {
        ShadowedAlgorithm::new(SwapUnchecked::new(DataType::Xfe)).bench();
    }
}
