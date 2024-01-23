use std::collections::HashMap;

use itertools::Itertools;
use rand::random;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;
use twenty_first::shared_math::other::random_elements;
use twenty_first::util_types::shared::bag_peaks;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::list::unsafeimplu32::get::UnsafeGet;
use crate::list::unsafeimplu32::length::Length as UnsafeLength;
use crate::rust_shadowing_helper_functions;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::VmHasher;

pub struct BagPeaks;

impl BagPeaks {
    fn input_state(num_peaks: usize) -> FunctionInitialState {
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

        FunctionInitialState { stack, memory }
    }
}

impl BasicSnippet for BagPeaks {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Digest)),
            "*peaks".to_string(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_mmr_bag_peaks".to_string()
    }

    fn code(&self, library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let get_element = library.import(Box::new(UnsafeGet {
            data_type: DataType::Digest,
        }));
        let get_length = library.import(Box::new(UnsafeLength {
            data_type: DataType::Digest,
        }));

        let length_is_zero_label = format!("{entrypoint}_length_is_zero");
        let length_is_not_zero_label = format!("{entrypoint}_length_is_not_zero");
        let length_is_one_label = format!("{entrypoint}_length_is_one");
        let length_is_not_zero_or_one = format!("{entrypoint}_length_is_not_zero_or_one");
        let loop_label = format!("{entrypoint}_loop");

        triton_asm!(
        // BEFORE: _ *peaks
        // AFTER:  _ d4 d3 d2 d1 d0
        {entrypoint}:
            dup 0  // _ *peaks *peaks
            call {get_length} // _ *peaks length

            // special case 0
            push 1 dup 1 push 0 eq // _ *peaks length 1 (length==0)
            skiz call {length_is_zero_label}
            skiz call {length_is_not_zero_label}

            // _ [digest_elements]
            return

        // BEFORE: _ *peaks length 1
        // AFTER:  _ d4 d3 d2 d1 d0 0
        {length_is_zero_label}:
            pop 3
            push 0 push 0 push 0 push 0 push 0
            push 1 push 0 push 0 push 0 push 0
            // 0 0 0 0 0 1 0 0 0 0
            sponge_init
            sponge_absorb
            sponge_squeeze
            swap 5 pop 1
            swap 5 pop 1
            swap 5 pop 1
            swap 5 pop 1
            swap 5 pop 1
            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER:  _ d4 d3 d2 d1 d0
        {length_is_not_zero_label}:
            // special case 1
            push 1 dup 1 push 1 eq // _ *peaks length 1 length==1
            skiz call {length_is_one_label}
            skiz call {length_is_not_zero_or_one}
            return

        // BEFORE: _ *peaks length 1
        // AFTER:  _ d4 d3 d2 d1 d0 0
        {length_is_one_label}:
            pop 2
            push 0
            // _ *peaks 0
            call {get_element} // _ d4 d3 d2 d1 d0
            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER:  _ d4 d3 d2 d1 d0
        {length_is_not_zero_or_one}:
            // base case
            push -1 add             // _ *peaks length-1
            dup 1 dup 1             // _ *peaks length-1 *peaks length-1
            call {get_element}      // _ *peaks length-1 l4 l3 l2 l1 l0
            dup 6 dup 6             // _*peaks length-1 l4 l3 l2 l1 l0 *peaks length-1
            push -1 add             // _*peaks length-1 l4 l3 l2 l1 l0 *peaks length-2
            call {get_element}      // _ *peaks length-1 l4 l3 l2 l1 l0 p4 p3 p2 p1 p0
            hash
            // _ *peaks length-1 [acc]

            swap 5
            push -1 add
            swap 5
            // _ *peaks length-2 [acc]

            call {loop_label}
            // *peaks 0 [acc]
            // *peaks 0 d4 d3 d2 d1 d0

            swap 2 // *peaks 0 d4 d3 d0 d1 d2
            swap 4 // *peaks 0 d2 d3 d0 d1 d4
            swap 6 // d4 0 d2 d3 d0 d1 *peaks
            pop 1  // d4 0 d2 d3 d0 d1
            swap 2 // d4 0 d2 d1 d0 d3
            swap 4 // d4 d3 d2 d1 d0 0
            pop 1  // d4 d3 d2 d1 d0

            return

        // INVARIANT: _ *peaks elements_left [acc]
        {loop_label}:
            // evaluate termination condition
            dup 5     // _ *peaks elements_left [acc] elements_left
            push 0 eq // _ *peaks elements_left [acc] elements_left==0
            skiz return
            // _ *peaks elements_left [acc]

            // body
            swap 5
            push -1
            add
            swap 5
            // _ *peaks elements_left' [acc]

            dup 6
            dup 6
            // _ *peaks elements_left' [acc] *peaks elements_left'

            call {get_element}
            // _*peaks elements_left' [acc] [peaks[elements_left - 1]]

            hash
            // _*peaks elements_left' [acc']

            recurse
        )
    }
}

impl Function for BagPeaks {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let address = stack.pop().unwrap();
        let length = memory.get(&address).unwrap().value() as usize;
        let safety_offset = BFieldElement::new(1); // unsafe lists

        let mut bfes: Vec<BFieldElement> = Vec::with_capacity(length * tip5::DIGEST_LENGTH);

        for i in 0..length * tip5::DIGEST_LENGTH {
            let element_index = address + safety_offset + BFieldElement::new(i as u64);
            bfes.push(*memory.get(&(element_index)).unwrap());
        }

        let peaks = bfes
            .chunks(5)
            .map(|ch| Digest::new(ch.try_into().unwrap()))
            .collect_vec();

        let bag = bag_peaks::<VmHasher>(&peaks);

        let mut bag_bfes = bag.values().to_vec();

        while let Some(element) = bag_bfes.pop() {
            stack.push(element);
        }
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> FunctionInitialState {
        match bench_case {
            Some(BenchmarkCase::CommonCase) => Self::input_state(30),
            Some(BenchmarkCase::WorstCase) => Self::input_state(60),
            None => {
                let mut rng: StdRng = SeedableRng::from_seed(seed);
                Self::input_state(rng.gen_range(0..=63))
            }
        }
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        vec![
            Self::input_state(0),
            Self::input_state(1),
            Self::input_state(2),
            Self::input_state(3),
            Self::input_state(63),
        ]
    }
}

#[cfg(test)]
mod tests {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::BagPeaks;

    #[test]
    fn prop() {
        ShadowedFunction::new(BagPeaks).test()
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::BagPeaks;

    #[test]
    fn bag_peaks_benchmark() {
        ShadowedFunction::new(BagPeaks).bench();
    }
}
