use triton_vm::prelude::*;

use crate::arithmetic::u64::incr::Incr;
use crate::arithmetic::u64::trailing_zeros::TrailingZeros;
use crate::list::new::New;
use crate::list::pop::Pop;
use crate::list::push::Push;
use crate::prelude::*;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CalculateNewPeaksFromAppend;

impl BasicSnippet for CalculateNewPeaksFromAppend {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Digest));

        vec![
            (DataType::U64, "old_num_leafs".to_string()),
            (list_type, "peaks".to_string()),
            (DataType::Digest, "new_leaf".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::Digest));

        vec![
            (list_type.clone(), "*new_peaks".to_string()),
            (list_type, "*auth_path".to_string()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_calculate_new_peaks_from_append".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let while_loop_label = format!("{entrypoint}_while");

        let new_list = library.import(Box::new(New));
        let push = library.import(Box::new(Push::new(DataType::Digest)));
        let pop = library.import(Box::new(Pop::new(DataType::Digest)));
        let u64incr = library.import(Box::new(Incr));
        let right_lineage_count = library.import(Box::new(TrailingZeros));

        triton_asm!(
            // BEFORE: _ [old_num_leafs: u64] *peaks [new_leaf: Digest]
            // AFTER:  _ *new_peaks *auth_path
            {entrypoint}:
                dup 5
                place 5
                call {push}
                // _ [old_num_leafs: u64] *peaks

                /* create auth_path return value */
                call {new_list}
                // _ [old_num_leafs: u64] *peaks *auth_path

                pick 3 pick 3
                // _ *peaks *auth_path [old_num_leafs: u64]

                call {u64incr}
                call {right_lineage_count}
                // _ *peaks *auth_path right_lineage_count

                call {while_loop_label}
                // _ *peaks *auth_path 0

                pop 1
                // _ *peaks *auth_path

                return

            // INVARIANT: _ *peaks *auth_path rlc
            {while_loop_label}:
                dup 0
                push 0
                eq
                skiz
                    return
                // _ *peaks *auth_path rlc

                dup 2
                dup 0
                call {pop}
                // _ *peaks *auth_path rlc *peaks [new_hash: Digest]

                dup 5
                // _ *peaks *auth_path rlc *peaks [new_hash: Digest] *peaks

                call {pop}
                // _ *peaks *auth_path rlc *peaks [new_hash: Digest] [previous_peak: Digest]

                /* update authentication path with latest previous_peak */
                dup 12
                dup 5 dup 5 dup 5 dup 5 dup 5
                call {push}
                // _ *peaks *auth_path rlc *peaks [new_hash: Digest] [previous_peak: Digest]

                hash
                // _ *peaks *auth_path rlc *peaks [new_peak: Digest]

                call {push}
                // _ *peaks *auth_path rlc

                addi -1
                // _ *auth_path *peaks (rlc - 1)

                recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;

    use super::*;
    use crate::memory::FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;
    use crate::memory::dyn_malloc::DYN_MALLOC_FIRST_ADDRESS;
    use crate::rust_shadowing_helper_functions::list::list_pop;
    use crate::rust_shadowing_helper_functions::list::list_push;
    use crate::test_prelude::*;
    use crate::twenty_first::prelude::Mmr;
    use crate::twenty_first::util_types::mmr::shared_basic::calculate_new_peaks_from_append;

    impl CalculateNewPeaksFromAppend {
        fn set_up_initial_state(
            &self,
            mmr: MmrAccumulator,
            new_leaf: Digest,
        ) -> FunctionInitialState {
            let peaks_pointer = FIRST_NON_DETERMINISTICALLY_INITIALIZED_MEMORY_ADDRESS;

            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &mmr.num_leafs());
            push_encodable(&mut stack, &peaks_pointer);
            push_encodable(&mut stack, &new_leaf.reversed());

            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, peaks_pointer, &mmr.peaks());

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for CalculateNewPeaksFromAppend {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let new_leaf = pop_encodable::<Digest>(stack);
            let peaks_pointer = stack.pop().unwrap();
            let old_num_leafs = pop_encodable::<u64>(stack);
            let old_peaks = *Vec::decode_from_memory(memory, peaks_pointer).unwrap();

            // Mimic all potential artifacts of the snippet.
            // This is _not_ shadowing the actual behavior, only intermediate results.
            list_push(peaks_pointer, new_leaf.encode(), memory);
            let old_num_peaks = old_num_leafs.count_ones();
            for _ in 0..old_num_peaks {
                let left = list_pop(peaks_pointer, memory, Digest::LEN);
                let right = list_pop(peaks_pointer, memory, Digest::LEN);
                let new = Tip5::hash_pair(right.try_into().unwrap(), left.try_into().unwrap());
                list_push(peaks_pointer, new.encode(), memory);
            }

            // actually shadow the snippet
            let (new_peaks, proof) =
                calculate_new_peaks_from_append(old_num_leafs, old_peaks, new_leaf);

            let auth_path_pointer = DYN_MALLOC_FIRST_ADDRESS;
            encode_to_memory(memory, peaks_pointer, &new_peaks);
            encode_to_memory(memory, auth_path_pointer, &proof.authentication_path);
            stack.push(peaks_pointer);
            stack.push(auth_path_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);

            let old_num_leafs: u64 = match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 31) - 1,
                Some(BenchmarkCase::WorstCase) => (1 << 62) - 1,
                None => rng.random_range(0..1 << 63),
            };
            let peaks = (0..old_num_leafs.count_ones())
                .map(|_| rng.random())
                .collect();
            let mmr = MmrAccumulator::init(peaks, old_num_leafs);

            self.set_up_initial_state(mmr, rng.random())
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let mut states = vec![];
            for num_leafs in (0_u64..=17).chain([100, 1000, 1 << 31, 1 << 32, 1 << 33]) {
                let num_peaks = num_leafs.count_ones();
                let peaks = (0..num_peaks).map(|i| Tip5::hash(&i)).collect();
                let mmr = MmrAccumulator::init(peaks, num_leafs);
                let new_leaf = Tip5::hash(&num_leafs);
                states.push(self.set_up_initial_state(mmr, new_leaf));
            }

            states
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(CalculateNewPeaksFromAppend).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn calculate_new_peaks_from_append_benchmark() {
        ShadowedFunction::new(CalculateNewPeaksFromAppend).bench();
    }
}
