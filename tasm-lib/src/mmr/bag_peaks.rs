use std::collections::HashMap;

use itertools::Itertools;
use rand::prelude::*;
use triton_vm::prelude::*;
use twenty_first::math::other::random_elements;
use twenty_first::util_types::shared::bag_peaks;

use crate::data_type::DataType;
use crate::list::LIST_METADATA_SIZE;
use crate::mmr::MAX_MMR_HEIGHT;
use crate::rust_shadowing_helper_functions;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::VmHasher;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct BagPeaks;

impl BagPeaks {
    fn input_state(&self, num_peaks: usize) -> FunctionInitialState {
        let peaks: Vec<Digest> = random_elements(num_peaks);
        let address: BFieldElement = random();
        let mut stack = self.init_stack_for_isolated_run();
        stack.push(address);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        // Insert list into memory
        rust_shadowing_helper_functions::list::list_insert(address, peaks, &mut memory);

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
        "tasmlib_mmr_bag_peaks".to_string()
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let length_is_zero_label = format!("{entrypoint}_length_is_zero");
        let length_is_not_zero_label = format!("{entrypoint}_length_is_not_zero");
        let length_is_one_label = format!("{entrypoint}_length_is_one");
        let length_is_not_zero_or_one = format!("{entrypoint}_length_is_not_zero_or_one");
        let loop_label = format!("{entrypoint}_loop");

        triton_asm!(
        // BEFORE: _ *peaks
        // AFTER:  _ d4 d3 d2 d1 d0
        {entrypoint}:
            dup 0 read_mem 1 pop 1
            // _ *peaks length

            /* Verify that number of peaks does not exceed 63 */
            push {MAX_MMR_HEIGHT}
            dup 1
            lt
            assert

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

            push {Digest::LEN} add
            // _ *peaks[0]_lw

            read_mem {Digest::LEN}
            pop 1

            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER:  _ d4 d3 d2 d1 d0
        {length_is_not_zero_or_one}:

            /* Get pointer to last word of peaks list */
            push {Digest::LEN}
            mul
            // _ *peaks offset

            push 0
            swap 1
            dup 2
            add
            // _ *peaks 0 *peaks[last]_lw

            read_mem {Digest::LEN}
            // _ *peaks 0 [peaks[last]] *peaks[last-1]_lw

            swap 6
            pop 1
            // _ *peaks *peaks[last-2]_lw [peaks[last]]
            // _ *peaks *peaks[last-2]_lw [acc]

            call {loop_label}
            // *peaks *peaks [acc]
            // *peaks *peaks d4 d3 d2 d1 d0

            swap 2 // *peaks *peaks d4 d3 d0 d1 d2
            swap 4 // *peaks *peaks d2 d3 d0 d1 d4
            swap 6 // d4 *peaks d2 d3 d0 d1 *peaks
            pop 1  // d4 *peaks d2 d3 d0 d1
            swap 2 // d4 *peaks d2 d1 d0 d3
            swap 4 // d4 d3 d2 d1 d0 *peaks
            pop 1  // d4 d3 d2 d1 d0

            return

        // INVARIANT: _ *peaks *peaks[i]_lw [acc]
        {loop_label}:
            // evaluate termination condition
            // _ *peaks *peaks[i]_lw [acc]

            dup 5
            read_mem {Digest::LEN}
            swap 11
            pop 1
            // _*peaks *peaks[i-1] [acc] [peaks[i - 1]]

            hash
            // _*peaks *peaks[i-1] [acc']

            recurse_or_return
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
        let safety_offset = BFieldElement::new(LIST_METADATA_SIZE as u64);

        let mut bfes: Vec<BFieldElement> = Vec::with_capacity(length * tip5::Digest::LEN);

        for i in 0..length * tip5::Digest::LEN {
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
            Some(BenchmarkCase::CommonCase) => self.input_state(30),
            Some(BenchmarkCase::WorstCase) => self.input_state(60),
            None => {
                let mut rng: StdRng = SeedableRng::from_seed(seed);
                self.input_state(rng.gen_range(0..=63))
            }
        }
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        vec![
            self.input_state(0),
            self.input_state(1),
            self.input_state(2),
            self.input_state(3),
            self.input_state(4),
            self.input_state(5),
            self.input_state(63),
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
