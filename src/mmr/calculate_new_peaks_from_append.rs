use std::collections::HashMap;

use num::One;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;
use twenty_first::util_types::mmr;

use crate::library::Library;
use crate::list::u32::pop::Pop;
use crate::list::u32::push::Push;
use crate::list::u32::set_length::SetLength;
use crate::rust_shadowing_helper_functions;
use crate::snippet::Snippet;

use super::data_index_to_node_index::DataIndexToNodeIndex;
use super::right_ancestor_count_and_own_height::MmrRightAncestorCountAndHeight;

pub const MAX_MMR_HEIGHT: usize = 64;

pub struct CalculateNewPeaksFromAppend;

impl Snippet for CalculateNewPeaksFromAppend {
    fn stack_diff() -> isize {
        // pops: `old_leaf_count` (u32s<2>); old_peaks (*list); [digests (new_leaf)]
        // pushes: *list (new peaks); *auth_path_of_newly_added_leaf
        -6
    }

    fn entrypoint() -> &'static str {
        "calculate_new_peaks_from_append"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let data_index_to_node_index = library.import::<DataIndexToNodeIndex>();
        let right_ancestor_count_and_own_height =
            library.import::<MmrRightAncestorCountAndHeight>();
        let push = library.import::<Push<DIGEST_LENGTH>>();
        let pop = library.import::<Pop<DIGEST_LENGTH>>();
        let set_length = library.import::<SetLength>();

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

                    call {right_ancestor_count_and_own_height}
                    pop
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rac

                    call {entrypoint}_while
                    // stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks (rac = 0)

                    pop
                    swap3 pop swap1 pop
                    // stack: _ *peaks *auth_path

                    return

                // Stack start and end: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rac
                {entrypoint}_while:
                    dup0
                    push 0
                    eq
                    skiz
                        return
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rac

                    swap2 swap1
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks

                    dup0
                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digest (new_hash)]

                    dup5
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digest (new_hash)] *peaks

                    call {pop}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    // Update authentication path with latest previous_peak
                    dup11
                    dup5 dup5 dup5 dup5 dup5
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digest (new_hash)] [digests (previous_peak)] *auth_path [digests (previous_peak)]

                    call {push}
                    pop
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digest (new_hash)] [digests (previous_peak)]

                    hash
                    pop pop pop pop pop
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks [digests (new_peak)]

                    call {push}
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo rac *auth_path *peaks

                    swap1 swap2
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks rac

                    push -1
                    add
                    // Stack: _ old_leaf_count_hi old_leaf_count_lo *auth_path *peaks (rac - 1)

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

        // TODO: Remove this when twenty-first is updated
        fn right_ancestor_count_and_own_height(node_index: u128) -> (u32, u32) {
            let (mut candidate, mut candidate_height) = mmr::shared::leftmost_ancestor(node_index);

            // leftmost ancestor is always a left node, so count starts at 0.
            let mut right_ancestor_count = 0;

            loop {
                if candidate == node_index {
                    return (right_ancestor_count, candidate_height as u32);
                }

                let left_child = mmr::shared::left_child(candidate, candidate_height);
                let candidate_is_right_child = left_child < node_index;
                if candidate_is_right_child {
                    candidate = mmr::shared::right_child(candidate);
                    right_ancestor_count += 1;
                } else {
                    candidate = left_child;
                    right_ancestor_count = 0;
                };

                candidate_height -= 1;
            }
        }

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
            let offset = BFieldElement::new((i as usize * DIGEST_LENGTH) as u64);
            old_peaks.push(Digest::new([
                memory[&(peaks_pointer + offset + BFieldElement::one())],
                memory[&(peaks_pointer + offset + BFieldElement::new(2))],
                memory[&(peaks_pointer + offset + BFieldElement::new(3))],
                memory[&(peaks_pointer + offset + BFieldElement::new(4))],
                memory[&(peaks_pointer + offset + BFieldElement::new(5))],
            ]));
        }

        // Run the actual `calculate_new_peaks_from_append` algorithm. This function
        // is inlined here to make it manipulate memory the same way that the TASM code
        // does.
        let auth_path_pointer = BFieldElement::new((MAX_MMR_HEIGHT * DIGEST_LENGTH + 1) as u64);
        rust_shadowing_helper_functions::list_new(auth_path_pointer, memory);
        rust_shadowing_helper_functions::list_push(peaks_pointer, new_leaf.values(), memory);
        let new_node_index = mmr::shared::data_index_to_node_index(old_leaf_count as u128);
        let (mut right_lineage_count, _height) =
            right_ancestor_count_and_own_height(new_node_index);
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

    use crate::{get_init_tvm_stack, snippet::rust_tasm_equivalence_prop};

    use super::*;

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
            let offset = BFieldElement::new((i as usize * DIGEST_LENGTH) as u64);
            let peak: Digest = Digest::new([
                memory[&(peaks_pointer + offset + BFieldElement::one())],
                memory[&(peaks_pointer + offset + BFieldElement::new(2))],
                memory[&(peaks_pointer + offset + BFieldElement::new(3))],
                memory[&(peaks_pointer + offset + BFieldElement::new(4))],
                memory[&(peaks_pointer + offset + BFieldElement::new(5))],
            ]);
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
            let offset = BFieldElement::new((i as usize * DIGEST_LENGTH) as u64);
            let auth_path_element: Digest = Digest::new([
                memory[&(auth_paths_pointer + offset + BFieldElement::one())],
                memory[&(auth_paths_pointer + offset + BFieldElement::new(2))],
                memory[&(auth_paths_pointer + offset + BFieldElement::new(3))],
                memory[&(auth_paths_pointer + offset + BFieldElement::new(4))],
                memory[&(auth_paths_pointer + offset + BFieldElement::new(5))],
            ]);
            produced_auth_path.push(auth_path_element);
        }

        let produced_mp = MmrMembershipProof::<H> {
            data_index: start_mmr.count_leaves(),
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
