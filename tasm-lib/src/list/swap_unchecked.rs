use std::collections::HashMap;

use num_traits::One;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{
    instruction::LabelledInstruction, op_stack::NUM_OP_STACK_REGISTERS, triton_asm, BFieldElement,
    NonDeterminism,
};

use super::ListType;
use crate::traits::algorithm::{Algorithm, AlgorithmInitialState};
use crate::traits::basic_snippet::BasicSnippet;
use crate::{data_type::DataType, empty_stack, library::Library};

pub struct SwapUnchecked {
    list_type: ListType,
    element_type: DataType,
}

impl BasicSnippet for SwapUnchecked {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(self.element_type.to_owned())),
                "self".to_owned(),
            ),
            (DataType::U32, "a".to_owned()),
            (DataType::U32, "b".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_{}_swap_{}",
            self.list_type,
            self.element_type.label_friendly_name()
        )
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let metadata_size = self.list_type.metadata_size();
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
                // AFTER: _
                {entrypoint}:

                    // calculate *list[b]
                    // _ *list a b

                    {&get_offset_for_last_word_in_element}
                    // _ *list a b_offset_last_word

                    dup 2
                    add
                    // _ *list a *list[b]_last_word

                    {&self.element_type.read_value_from_memory()}
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

                    {&self.element_type.read_value_from_memory()}
                    // _ *list a [list[b]] *list[b] [list[a]] (*list[a] - 1)

                    push 1
                    add
                    // _ *list a [list[b]] *list[b] [list[a]] *list[a]

                    swap {element_size + 1}
                    // _ *list a [list[b]] *list[a] [list[a]] *list[b]

                    {&self.element_type.write_value_to_memory()}
                    // _ *list a [list[b]] *list[a] *list[b+1]

                    pop 1
                    // _ *list a [list[b]] *list[a]

                    {&self.element_type.write_value_to_memory()}
                    // _ *list a (*list[a+1])

                    pop 3

                    return
        )
    }
}

impl Algorithm for SwapUnchecked {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
    ) {
        let b_index = stack.pop().unwrap().value() as usize;
        let a_index = stack.pop().unwrap().value() as usize;
        let list_pointer = stack.pop().unwrap();
        let element_size = self.element_type.stack_size();

        let a = self
            .list_type
            .rust_shadowing_get(list_pointer, a_index, memory, element_size);
        let b = self
            .list_type
            .rust_shadowing_get(list_pointer, b_index, memory, element_size);
        self.list_type
            .rust_shadowing_set(list_pointer, a_index, b, memory);
        self.list_type
            .rust_shadowing_set(list_pointer, b_index, a, memory);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> AlgorithmInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let list_pointer = BFieldElement::new(rng.gen());
        let list_length = rng.gen_range(0..200);
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
        self.list_type.rust_shadowing_insert_random_list(
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
    use super::*;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

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
            ShadowedAlgorithm::new(SwapUnchecked {
                list_type: ListType::Unsafe,
                element_type: data_type.clone(),
            })
            .test();
            ShadowedAlgorithm::new(SwapUnchecked {
                list_type: ListType::Safe,
                element_type: data_type.clone(),
            })
            .test();
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::algorithm::ShadowedAlgorithm;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn bench() {
        ShadowedAlgorithm::new(SwapUnchecked {
            list_type: ListType::Unsafe,
            element_type: DataType::Xfe,
        })
        .bench();
        ShadowedAlgorithm::new(SwapUnchecked {
            list_type: ListType::Safe,
            element_type: DataType::Xfe,
        })
        .bench();
    }
}
