use std::collections::HashMap;

use triton_vm::prelude::*;
use triton_vm::twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;

use crate::arithmetic;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// [Bag the peaks][bag] of an MMR into a single [`Digest`].
///
/// # Behavior
///
/// ```text
/// BEFORE: _ *mmr_accumulator
/// AFTER:  _ [bagged_peaks: Digest]
/// ```
///
/// # Preconditions
///
/// - the input argument points to a properly [`BFieldCodec`]-encoded list of
///   [`Digest`]s in memory
/// - the pointed-to MMR accumulator is consistent, *i.e.*, the number of peaks
///   matches with the number of set bits in the leaf count.
///
/// # Postconditions
///
/// - the output is a single [`Digest`] computed like in [`bag_peaks`][bag]
/// - the output is properly [`BFieldCodec`] encoded
///
/// # Crashes
///
///  - if the MMR accumulator is inconsistent, *i.e.*, if the number of peaks
///    does not match the number of set bits in the leaf count
///
/// [bag]: twenty_first::util_types::mmr::mmr_trait::Mmr::bag_peaks
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BagPeaks;

impl BagPeaks {
    const INCONSISTENT_NUM_PEAKS_ERROR_ID: usize = 560;
}

impl BasicSnippet for BagPeaks {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let mmr_accumulator = DataType::List(Box::new(DataType::Digest));

        vec![(mmr_accumulator, "*mmra".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_bag_peaks".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let bagging_loop = format!("{entrypoint}_loop");

        let destructure_mmra = MmrAccumulator::destructure();
        let pop_count = library.import(Box::new(arithmetic::u64::popcount::PopCount));

        triton_asm!(
        // BEFORE: _ *mmra
        // AFTER:  _ [bagged_peaks: Digest]
        {entrypoint}:
            {&destructure_mmra}
            // _ *peaks *leaf_count

            addi 1 read_mem 2 pop 1
            hint leaf_count : u64 = stack[0..2]
            // _ *peaks [leaf_count]

            dup 1 dup 1
            call {pop_count}
            // _ *peaks [leaf_count] popcount

            dup 3 read_mem 1 pop 1
            // _ *peaks [leaf_count] popcount num_peaks

            dup 1 eq
            // _ *peaks [leaf_count] pop_count (num_peaks==pop_count)

            assert error_id {Self::INCONSISTENT_NUM_PEAKS_ERROR_ID}
            hint num_peaks: u32 = stack[0]
            // _ *peaks [leaf_count] num_peaks

            place 2
            // _ *peaks num_peaks [leaf_count]
            // _ *peaks len [leaf_count] <-- rename

            push 0
            push 0
            push 0
            push 0
            push 0
            push 0
            push 0
            push 0
            pick 9
            pick 9
            // _ *peaks len 0 0 0 0 0 0 0 0 [leaf_count; 2]

            hash
            // _ *peaks len [hash_of_leaf_count]

            pick 5
            push {Digest::LEN}
            mul
            // _ *peaks [hash_of_leaf_count] size_of_peaks_list

            dup 6
            add
            // _ *peaks [hash_of_leaf_count] *peaks[last]_lw

            place 5
            // _ *peaks *peaks[last]_lw [hash_of_leaf_count]

            dup 6 dup 6 eq push 0 eq
            // _ *peaks *peaks[last]_lw [hash_of_leaf_count] (num_peaks == 0)

            skiz call {bagging_loop}
            // _ *peaks *peaks [bag_hash]

            pick 6 pick 6 pop 2
            // _ [bagged_peaks: Digest]

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
        [].into()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use triton_vm::twenty_first::prelude::Mmr;
    use twenty_first::math::other::random_elements;

    use super::*;
    use crate::test_prelude::*;

    impl BagPeaks {
        fn set_up_initial_state(&self, leaf_count: u64) -> FunctionInitialState {
            let address = rand::random();
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &address);

            let mut memory = HashMap::new();
            let num_peaks = leaf_count.count_ones();
            let mmra =
                MmrAccumulator::init(random_elements::<Digest>(num_peaks as usize), leaf_count);
            encode_to_memory(&mut memory, address, &mmra);

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
            let mmra = *MmrAccumulator::decode_from_memory(memory, address).unwrap();

            fn bag_peaks(peaks: &[Digest], leaf_count: u64) -> Digest {
                // use `hash_10` over `hash` or `hash_varlen` to simplify hashing in Triton VM
                let [lo_limb, hi_limb] = leaf_count.encode()[..] else {
                    panic!("internal error: unknown encoding of type `u64`")
                };
                let padded_leaf_count = bfe_array![lo_limb, hi_limb, 0, 0, 0, 0, 0, 0, 0, 0];
                let hashed_leaf_count = Digest::new(Tip5::hash_10(&padded_leaf_count));

                peaks
                    .iter()
                    .rev()
                    .fold(hashed_leaf_count, |acc, &peak| Tip5::hash_pair(peak, acc))
            }

            let bag = bag_peaks(&mmra.peaks(), mmra.num_leafs());
            println!("bag: {bag}");
            push_encodable(stack, &bag);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let num_leafs = match bench_case {
                Some(BenchmarkCase::CommonCase) => 348753,
                Some(BenchmarkCase::WorstCase) => 843759843768,
                None => StdRng::from_seed(seed).random_range(0u64..(u64::MAX >> 1)),
            };

            self.set_up_initial_state(num_leafs)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            (0..=5)
                .chain([63])
                .map(|num_peaks| self.set_up_initial_state((1 << num_peaks) - 1))
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
