use std::collections::HashMap;

use triton_vm::prelude::*;
use twenty_first::util_types::shared::bag_peaks;

use crate::mmr::MAX_MMR_HEIGHT;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bag peaks](bag_peaks) into a single [`Digest`].
///
/// # Behavior
///
/// ```text
/// BEFORE: _ *peaks
/// AFTER:  _ [bagged_peaks: Digest]
/// ```
///
/// # Preconditions
///
/// - the input argument points to a properly [`BFieldCodec`]-encoded list of
///   [`Digest`]s in memory
/// - the pointed-to list contains fewer than [`MAX_MMR_HEIGHT`] elements
///
/// # Postconditions
///
/// - the output is a single [`Digest`] computed like in [`bag_peaks`]
/// - the output is properly [`BFieldCodec`] encoded
///
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BagPeaks;

impl BasicSnippet for BagPeaks {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let list_of_digests = DataType::List(Box::new(DataType::Digest));

        vec![(list_of_digests, "*peaks".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_bag_peaks".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let length_is_zero = format!("{entrypoint}_length_is_zero");
        let length_is_not_zero = format!("{entrypoint}_length_is_not_zero");
        let length_is_one = format!("{entrypoint}_length_is_one");
        let length_is_gt_one = format!("{entrypoint}_length_is_gt_one");
        let bagging_loop = format!("{entrypoint}_loop");

        let Digest(zero_digest) = bag_peaks(&[]);

        triton_asm!(
        // BEFORE: _ *peaks
        // AFTER:  _ [bagged_peaks: Digest]
        {entrypoint}:
            dup 0 read_mem 1 pop 1
            // _ *peaks length

            push {MAX_MMR_HEIGHT}
            dup 1
            lt
            assert

            /* special case: length is 0 */
            push 1
            dup 1 push 0 eq
            // _ *peaks length 1 (length==0)

            skiz call {length_is_zero}
            skiz call {length_is_not_zero}

            // _ [bagged_peaks: Digest]
            return

        // BEFORE: _ *peaks length 1
        // AFTER:  _ [bagged_peaks: Digest] 0
        {length_is_zero}:
            pop 3

            push {zero_digest[4]}
            push {zero_digest[3]}
            push {zero_digest[2]}
            push {zero_digest[1]}
            push {zero_digest[0]}
            hint bag_of_no_peaks: Digest = stack[0..5]

            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER:  _ [bagged_peaks: Digest]
        {length_is_not_zero}:
            /* special case: length is 1 */
            push 1
            dup 1 push 1 eq
            // _ *peaks length 1 (length==1)

            skiz call {length_is_one}
            skiz call {length_is_gt_one}
            return

        // BEFORE: _ *peaks length 1
        // AFTER:  _ [bagged_peaks: Digest] 0
        {length_is_one}:
            pop 2

            push {Digest::LEN} add
            // _ *peaks[0]_lw

            read_mem {Digest::LEN}
            pop 1

            push 0
            return

        // BEFORE: _ *peaks length
        // AFTER:  _ [bagged_peaks: Digest]
        {length_is_gt_one}:
            /* Get pointer to last word of peaks list */
            push {Digest::LEN}
            mul
            // _ *peaks offset

            dup 1
            add
            // _ *peaks *peaks[last]_lw

            read_mem {Digest::LEN}
            // _ *peaks [peaks[last]: Digest] *peaks[last-1]_lw

            place 5
            // _ *peaks *peaks[last-1]_lw [peaks[last]: Digest]
            // _ *peaks *peaks[last-1]_lw [acc: Digest]

            call {bagging_loop}
            // *peaks *peaks [acc: Digest]
            // *peaks *peaks [bagged_peaks: Digest]

            pick 6
            pick 6
            pop 2

            return

        // INVARIANT: _ *peaks *peaks[i]_lw [acc: Digest]
        {bagging_loop}:
            pick 5
            read_mem {Digest::LEN}
            place 10
            // _*peaks *peaks[i-1]_lw [acc: Digest] [peaks[i - 1]: Digest]

            hash
            // _*peaks *peaks[i-1]_lw [acc': Digest]

            recurse_or_return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0xaf79abb21cb46bf.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use twenty_first::math::other::random_elements;

    use super::*;
    use crate::test_prelude::*;

    impl BagPeaks {
        fn set_up_initial_state(&self, num_peaks: usize) -> FunctionInitialState {
            let address = random();
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &address);

            let mut memory = HashMap::new();
            encode_to_memory(&mut memory, address, &random_elements::<Digest>(num_peaks));

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for BagPeaks {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let address = pop_encodable(stack);
            let peaks = *Vec::<Digest>::decode_from_memory(memory, address).unwrap();
            push_encodable(stack, &bag_peaks(&peaks));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let num_peaks = match bench_case {
                Some(BenchmarkCase::CommonCase) => 30,
                Some(BenchmarkCase::WorstCase) => 60,
                None => StdRng::from_seed(seed).gen_range(0..=63),
            };

            self.set_up_initial_state(num_peaks)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            (0..=5)
                .chain([63])
                .map(|num_peaks| self.set_up_initial_state(num_peaks))
                .collect()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(BagPeaks).test()
    }
}

#[cfg(test)]
mod benches {
    use super::BagPeaks;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(BagPeaks).bench();
    }
}
