use num::Zero;
use rand::random;
use std::collections::HashMap;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::rescue_prime_digest::{Digest, DIGEST_LENGTH};
use twenty_first::shared_math::rescue_prime_regular::RescuePrimeRegular;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::mmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use super::data_index_to_node_index::DataIndexToNodeIndex;
use super::right_lineage_length::MmrRightLineageLength;
use super::MAX_MMR_HEIGHT;
use crate::library::Library;
use crate::list::unsafe_u32::pop::Pop;
use crate::list::unsafe_u32::push::Push;
use crate::list::unsafe_u32::set_length::SetLength;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

#[derive(Clone)]
pub struct CalculateNewPeaksFromAppend;

impl Snippet for CalculateNewPeaksFromAppend {
    fn inputs() -> Vec<&'static str> {
        vec![
            "old_leaf_count_hi",
            "old_leaf_count_lo",
            "*peaks",
            "digest_elem_4",
            "digest_elem_3",
            "digest_elem_2",
            "digest_elem_1",
            "digest_elem_0",
        ]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["*new_peaks", "*auth_path"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::U64,
            DataType::List(Box::new(DataType::Digest)),
            DataType::Digest,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec!["Snippet arguments are not a valid MMR accumulator"]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        fn prepare_state_with_mmra(
            mut start_mmr: MmrAccumulator<RescuePrimeRegular>,
            new_leaf: Digest,
        ) -> ExecutionState {
            // We assume that the peaks can safely be stored in memory on address 0
            let peaks_pointer = BFieldElement::zero();

            let mut stack = get_init_tvm_stack();
            let old_leaf_count: u64 = start_mmr.count_leaves() as u64;
            stack.push(BFieldElement::new(old_leaf_count >> 32));
            stack.push(BFieldElement::new(old_leaf_count & u32::MAX as u64));
            stack.push(peaks_pointer);

            // push digests such that element 0 of digest is on top of stack
            for value in new_leaf.values().iter().rev() {
                stack.push(*value);
            }

            // Initialize memory
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
            rust_shadowing_helper_functions::list_new(peaks_pointer, &mut memory);
            for peak in start_mmr.get_peaks() {
                rust_shadowing_helper_functions::list_push(
                    peaks_pointer,
                    peak.values(),
                    &mut memory,
                );
            }

            ExecutionState::with_stack_and_memory(stack, memory, MAX_MMR_HEIGHT * DIGEST_LENGTH + 1)
        }

        let digests: Vec<Digest> = random_elements(10);
        let new_leaf: Digest = random();
        let mmra = MmrAccumulator::new(digests);
        let ret0 = prepare_state_with_mmra(mmra, new_leaf);

        vec![ret0]
    }

    fn stack_diff() -> isize {
        // pops: `old_leaf_count` (u32s<2>); old_peaks (*list); [digests (new_leaf)]
        // pushes: *list (new peaks); *auth_path_of_newly_added_leaf
        -6
    }

    fn entrypoint(&self) -> &'static str {
        "calculate_new_peaks_from_append"
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let data_index_to_node_index = library.import(Box::new(DataIndexToNodeIndex));
        let right_lineage_length = library.import(Box::new(MmrRightLineageLength));
        let push = library.import(Box::new(Push::<DIGEST_LENGTH>(DataType::Digest)));
        let pop = library.import(Box::new(Pop::<DIGEST_LENGTH>(DataType::Digest)));
        let set_length = library.import(Box::new(SetLength(DataType::Digest)));

        // Allocate memory for the returned auth path for the newly inserted element
        // Warning: This auth path is only allocated *once* even though the code is called multiple times.
        // So if this function is called multiple times, the auth_paths will be overwritten.
        let static_auth_path_pointer = library.kmalloc(DIGEST_LENGTH * MAX_MMR_HEIGHT);

        format!(
            "
                // BEFORE: _ old_leaf_count_hi old_leaf_count_lo *peaks [digests (new_leaf)]
                // AFTER: _ *new_peaks *auth_path
                {entrypoint}:
                    call {push}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *peaks

                    // Create auth_path return value (vector living in RAM)
                    // Notice that this will always allocate to the same pointer in memory, even
                    // if this function is called multiple times.
                    push {static_auth_path_pointer}
                    push 0
                    call {set_length}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *peaks *auth_path

                    swap1
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks

                    dup3 dup3
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks old_leaf_count_hi old_leaf_count_lo

                    call {data_index_to_node_index}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks new_ni_hi new_ni_lo

                    call {right_lineage_length}
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rll

                    call {entrypoint}_while
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks (rll = 0)

                    pop
                    swap3 pop swap1 pop
                    // stack: _ *peaks *auth_path

                    return

                // Stack start and end: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rll
                {entrypoint}_while:
                    dup0
                    push 0
                    eq
                    skiz
                        return
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rll

                    swap2 swap1
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks

                    dup0
                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digest (new_hash)]

                    dup5
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digest (new_hash)] *peaks

                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    // Update authentication path with latest previous_peak
                    dup11
                    dup5 dup5 dup5 dup5 dup5
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digest (new_hash)] [digests (previous_peak)] *auth_path [digests (previous_peak)]

                    call {push}
                    pop
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    hash
                    pop pop pop pop pop
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks [digests (new_peak)]

                    call {push}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rll *auth_path *peaks

                    swap1 swap2
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rll

                    push -1
                    add
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks (rll - 1)

                    recurse
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        type H = RescuePrimeRegular;

        // BEFORE: _ old_leaf_count_hi old_leaf_count_lo *peaks [digests (new_leaf)]
        // AFTER: _ *new_peaks *auth_path
        let new_leaf: Digest = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let peaks_pointer = stack.pop().unwrap();
        let old_leaf_count_lo = stack.pop().unwrap().value();
        let old_leaf_count_hi = stack.pop().unwrap().value();
        let old_leaf_count = (old_leaf_count_hi << 32) | old_leaf_count_lo;

        let mut old_peaks: Vec<Digest> = vec![];
        let peak_count = memory[&peaks_pointer].value() as u32;

        for i in 0..peak_count {
            old_peaks.push(Digest::new(rust_shadowing_helper_functions::list_read(
                peaks_pointer,
                i as usize,
                memory,
            )));
        }

        // Run the actual `calculate_new_peaks_from_append` algorithm. This function
        // is inlined here to make it manipulate memory the same way that the TASM code
        // does.
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
        rust_shadowing_helper_functions::list_new(auth_path_pointer, memory);
        rust_shadowing_helper_functions::list_push(peaks_pointer, new_leaf.values(), memory);
        let new_node_index = mmr::shared::leaf_index_to_node_index(old_leaf_count as u128);
        let (mut right_lineage_count, _height) =
            mmr::shared::right_lineage_length_and_own_height(new_node_index);
        while right_lineage_count != 0 {
            let new_hash = Digest::new(rust_shadowing_helper_functions::list_pop::<DIGEST_LENGTH>(
                peaks_pointer,
                memory,
            ));
            let previous_peak = Digest::new(rust_shadowing_helper_functions::list_pop::<
                DIGEST_LENGTH,
            >(peaks_pointer, memory));
            rust_shadowing_helper_functions::list_push(
                auth_path_pointer,
                previous_peak.values(),
                memory,
            );
            rust_shadowing_helper_functions::list_push(
                peaks_pointer,
                H::hash_pair(&previous_peak, &new_hash).values(),
                memory,
            );
            right_lineage_count -= 1;
        }

        // Pop return values to stack
        stack.push(peaks_pointer);
        stack.push(auth_path_pointer); // Can this be done in a more dynamic way?
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use twenty_first::shared_math::b_field_element::BFieldElement;
    use twenty_first::shared_math::other::random_elements;
    use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
    use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
    use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
    use twenty_first::util_types::mmr::mmr_trait::Mmr;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn calculate_new_peaks_from_append_test() {
        rust_tasm_equivalence_prop_new::<CalculateNewPeaksFromAppend>(CalculateNewPeaksFromAppend);
    }

    #[test]
    fn calculate_new_peaks_from_append_benchmark() {
        bench_and_write::<CalculateNewPeaksFromAppend>(CalculateNewPeaksFromAppend);
    }

    #[test]
    fn mmra_append_test_empty() {
        type H = RescuePrimeRegular;
        type Mmr = MmrAccumulator<H>;
        let mmra: Mmr = MmrAccumulator::new(vec![]);
        let digest = H::hash(&BFieldElement::zero());
        let expected_final_mmra = MmrAccumulator::new(vec![digest]);
        prop_calculate_new_peaks_from_append(mmra, digest, expected_final_mmra);
    }

    #[test]
    fn mmra_append_test_single() {
        type H = RescuePrimeRegular;
        type Mmr = MmrAccumulator<H>;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let digest1 = H::hash(&BFieldElement::new(12345));
        let mmra: Mmr = MmrAccumulator::new(vec![digest0]);
        let expected_final_mmra = MmrAccumulator::new(vec![digest0, digest1]);
        prop_calculate_new_peaks_from_append(mmra, digest1, expected_final_mmra);
    }

    #[test]
    fn mmra_append_test_two_leaves() {
        type H = RescuePrimeRegular;
        type Mmr = MmrAccumulator<H>;
        let digest0 = H::hash(&BFieldElement::new(4545));
        let digest1 = H::hash(&BFieldElement::new(12345));
        let digest2 = H::hash(&BFieldElement::new(55488));
        let mmra: Mmr = MmrAccumulator::new(vec![digest0, digest1]);
        let expected_final_mmra = MmrAccumulator::new(vec![digest0, digest1, digest2]);
        prop_calculate_new_peaks_from_append(mmra, digest2, expected_final_mmra);
    }

    #[test]
    fn mmra_append_pbt() {
        type H = RescuePrimeRegular;
        type Mmr = MmrAccumulator<H>;

        let inserted_digest: Digest = H::hash(&BFieldElement::new(1337));
        for init_size in 0..40 {
            println!("init_size = {init_size}");
            let leaf_digests: Vec<Digest> = random_elements(init_size);
            let init_mmra: Mmr = MmrAccumulator::new(leaf_digests.clone());
            let expected_final_mmra: Mmr =
                MmrAccumulator::new(vec![leaf_digests, vec![inserted_digest]].concat());
            prop_calculate_new_peaks_from_append(init_mmra, inserted_digest, expected_final_mmra);
        }
    }

    #[test]
    fn mmra_append_big_mmr() {
        type H = RescuePrimeRegular;
        type Mmr = MmrAccumulator<H>;

        // Set MMR to be with 2^32 - 1 leaves and 32 peaks. Prepending one leaf should then reduce the number of leaves to 1.
        let inserted_digest: Digest = H::hash(&BFieldElement::new(1337));
        let init_mmra: Mmr =
            MmrAccumulator::init(vec![H::hash(&BFieldElement::zero()); 32], (1 << 32) - 1);
        let mut expected_final_mmr = init_mmra.clone();
        expected_final_mmr.append(inserted_digest);
        prop_calculate_new_peaks_from_append(init_mmra, inserted_digest, expected_final_mmr);

        // Set MMR to be with 2^33 - 1 leaves and 33 peaks. Prepending one leaf should then reduce the number of leaves to 1.
        let inserted_digest: Digest = H::hash(&BFieldElement::new(1337));
        let init_mmra: Mmr =
            MmrAccumulator::init(vec![H::hash(&BFieldElement::zero()); 33], (1 << 33) - 1);
        let mut expected_final_mmr = init_mmra.clone();
        expected_final_mmr.append(inserted_digest);
        prop_calculate_new_peaks_from_append(init_mmra, inserted_digest, expected_final_mmr);
    }

    fn prop_calculate_new_peaks_from_append<
        H: AlgebraicHasher + std::cmp::PartialEq + std::fmt::Debug,
    >(
        mut start_mmr: MmrAccumulator<H>,
        new_leaf: Digest,
        expected_mmr: MmrAccumulator<H>,
    ) {
        // We assume that the peaks can safely be stored in memory on address 0
        let peaks_pointer = BFieldElement::zero();

        // BEFORE: _ old_leaf_count_hi old_leaf_count_lo *peaks [digests (new_leaf)]
        // AFTER: _ *new_peaks *auth_path
        let mut init_stack = get_init_tvm_stack();
        let old_leaf_count: u64 = start_mmr.count_leaves() as u64;
        init_stack.push(BFieldElement::new(old_leaf_count >> 32));
        init_stack.push(BFieldElement::new(old_leaf_count & u32::MAX as u64));
        init_stack.push(peaks_pointer);

        // push digests such that element 0 of digest is on top of stack
        for value in new_leaf.values().iter().rev() {
            init_stack.push(*value);
        }

        // Initialize memory
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
        rust_shadowing_helper_functions::list_new(peaks_pointer, &mut memory);
        for peak in start_mmr.get_peaks() {
            rust_shadowing_helper_functions::list_push(peaks_pointer, peak.values(), &mut memory);
        }

        let auth_paths_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
        let mut expected_final_stack = get_init_tvm_stack();
        expected_final_stack.push(peaks_pointer);
        expected_final_stack.push(auth_paths_pointer);

        let _execution_result = rust_tasm_equivalence_prop::<CalculateNewPeaksFromAppend>(
            CalculateNewPeaksFromAppend,
            &init_stack,
            &[],
            &[],
            &mut memory,
            MAX_MMR_HEIGHT * DIGEST_LENGTH + 1,
            Some(&expected_final_stack),
        );

        // Find produced MMR
        let peaks_count = memory[&peaks_pointer].value();
        let mut produced_peaks = vec![];
        for i in 0..peaks_count {
            let peak = Digest::new(rust_shadowing_helper_functions::list_read(
                peaks_pointer,
                i as usize,
                &memory,
            ));
            produced_peaks.push(peak);
        }

        let mut produced_mmr =
            MmrAccumulator::<H>::init(produced_peaks, start_mmr.count_leaves() + 1);

        // Verify that both code paths produce the same MMR
        assert_eq!(expected_mmr, produced_mmr);

        // Verify that produced auth paths are valid
        let auth_path_element_count = memory[&auth_paths_pointer].value();
        let mut produced_auth_path = vec![];
        for i in 0..auth_path_element_count {
            produced_auth_path.push(Digest::new(rust_shadowing_helper_functions::list_read(
                auth_paths_pointer,
                i as usize,
                &memory,
            )));
        }

        let produced_mp = MmrMembershipProof::<H> {
            leaf_index: start_mmr.count_leaves(),
            authentication_path: produced_auth_path,
            _hasher: std::marker::PhantomData,
        };
        assert!(
            produced_mp
                .verify(
                    &produced_mmr.get_peaks(),
                    &new_leaf,
                    produced_mmr.count_leaves(),
                )
                .0,
            "TASM-produced authentication path must be valid"
        );
    }
}
