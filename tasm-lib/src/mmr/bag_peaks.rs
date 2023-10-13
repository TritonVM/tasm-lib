use std::collections::HashMap;

use itertools::Itertools;
use rand::random;
use triton_vm::{BFieldElement, NonDeterminism};
use twenty_first::{shared_math::other::random_elements, util_types::shared::bag_peaks};

use crate::{
    empty_stack,
    list::unsafe_u32::{get::UnsafeGet, length::UnsafeLength},
    rust_shadowing_helper_functions,
    snippet::{DataType, DeprecatedSnippet},
    Digest, ExecutionState, VmHasher, DIGEST_LENGTH,
};

pub struct BagPeaks;

impl BagPeaks {
    fn input_state(num_peaks: usize) -> ExecutionState {
        let peaks: Vec<Digest> = random_elements(num_peaks);
        let address: BFieldElement = random();
        let mut stack = empty_stack();
        stack.push(address);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        // Insert list into memory
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
            address,
            peaks,
            &mut memory,
        );

        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
    }
}

impl DeprecatedSnippet for BagPeaks {
    fn entrypoint_name(&self) -> String {
        "tasm_mmr_bag_peaks".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["*peaks".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::Digest))]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "d4".to_string(),
            "d3".to_string(),
            "d2".to_string(),
            "d1".to_string(),
            "d0".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize {
        4
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        let get_element = library.import(Box::new(UnsafeGet(DataType::Digest)));
        let get_length = library.import(Box::new(UnsafeLength(DataType::Digest)));

        format!(
            "
        // BEFORE: _ *peaks
        // AFTER: _ d4 d3 d2 d1 d0
        {entrypoint}:

            dup 0  // _ *peaks *peaks
            call {get_length} // _ *peaks length

            // special case 0
            push 1 dup 1 push 0 eq // _ *peaks length 1 (length==0)
            skiz call {entrypoint}_length_is_zero
            skiz call {entrypoint}_length_is_not_zero

            // _ [digest_elements]
            return

        // BEFORE: _ *peaks length 1
        // AFTER: _ d4 d3 d2 d1 d0 0
        {entrypoint}_length_is_zero:
            pop
            pop pop
            push 0 push 0 push 0 push 0 push 0
            push 1 push 0 push 0 push 0 push 0
            // 0 0 0 0 0 1 0 0 0 0
            absorb_init
            squeeze
            swap 5 pop
            swap 5 pop
            swap 5 pop
            swap 5 pop
            swap 5 pop
            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER: _ d4 d3 d2 d1 d0
        {entrypoint}_length_is_not_zero:
            // special case 1
            push 1 dup 1 push 1 eq // _ *peaks length 1 length==1
            skiz call {entrypoint}_length_is_one
            skiz call {entrypoint}_length_is_not_one
            return

        // BEFORE: _ *peaks length 1
        // AFTER: _ d4 d3 d2 d1 d0 0
        {entrypoint}_length_is_one:
            pop
            push -1 add // _ *peaks 0
            call {get_element} // _ d4 d3 d2 d1 d0
            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER: _ d4 d3 d2 d1 d0
        {entrypoint}_length_is_not_one:
            // base case
            push -1 add // _ *peaks length-1
            dup 1 dup 1 // _ *peaks length-1 *peaks length-1
            call {get_element} // _ *peaks length-1 l4 l3 l2 l1 l0
            dup 6 dup 6 // _*peaks length-1 l4 l3 l2 l1 l0 *peaks length-1
            push -1 add // _*peaks length-1 l4 l3 l2 l1 l0 *peaks length-2
            call {get_element} // _ *peaks length-1 l4 l3 l2 l1 l0 p4 p3 p2 p1 p0
            hash

            swap 10
            push -1 add
            swap 10
            // _ *peaks length-2 d4 d3 d2 d1 d0 g4 g3 g2 g1 g0

            call {entrypoint}_loop // *peaks 0 d4 d3 d2 d1 d0 g4 g3 g2 g1 g0
            pop pop pop pop pop // *peaks 0 d4 d3 d2 d1 d0

            swap 2 // *peaks 0 d4 d3 d0 d1 d2
            swap 4 // *peaks 0 d2 d3 d0 d1 d4
            swap 6 // d4 0 d2 d3 d0 d1 *peaks
            pop // d4 0 d2 d3 d0 d1
            swap 2 // d4 0 d2 d1 d0 d3
            swap 4 // d4 d3 d2 d1 d0 0
            pop // d4 d3 d2 d1 d0

            return

        // INVARIANT: _ *peaks elements_left d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
        {entrypoint}_loop:
            // evaluate termination condition
            dup 10 // _ *peaks elements_left d9 d8 d7 d6 d5 d4 d3 d2 d1 d0 elements_left
            push 0 eq // _ *peaks elements_left d9 d8 d7 d6 d5 d4 d3 d2 d1 d0 elements_left==0
            skiz return
            // _ *peak elements_left d9 d8 d7 d6 d5 d4 d3 d2 d1 d0

            // body
            swap 10
            push -1
            add
            swap 10
            // _ *peaks elements_left-1 d9 d8 d7 d6 d5 d4 d3 d2 d1 d0

            pop pop pop pop pop
            dup 6
            dup 6
            // _ *peaks elements_left-1 d9 d8 d7 d6 d5 *peaks elements_left

            call {get_element}
            // _*peaks elements_left-1 d9 d8 d7 d6 d5 d4 d3 d2 d1 d0

            hash

            recurse
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        (0..30).map(Self::input_state).collect()
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::input_state(30)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::input_state(60)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let address = stack.pop().unwrap();

        let length = memory.get(&address).unwrap().value() as usize;
        let safety_offset = 1; // unsafe lists

        let mut bfes: Vec<BFieldElement> = Vec::with_capacity(length * DIGEST_LENGTH);

        for i in 0..length * DIGEST_LENGTH {
            bfes.push(
                *memory
                    .get(
                        &(address
                            + BFieldElement::new(safety_offset)
                            + BFieldElement::new(i as u64)),
                    )
                    .unwrap(),
            );
        }

        let peaks = bfes
            .chunks(5)
            .map(|ch| Digest::new(ch.try_into().unwrap()))
            .collect_vec();

        let bag = bag_peaks::<VmHasher>(&peaks);

        let mut bag_bfe = bag.values().to_vec();

        while let Some(element) = bag_bfe.pop() {
            stack.push(element);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::BagPeaks;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple_deprecated(&BagPeaks, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn bag_peaks_benchmark() {
        bench_and_write(BagPeaks);
    }
}
