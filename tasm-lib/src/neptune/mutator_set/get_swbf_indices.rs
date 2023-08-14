use itertools::Itertools;
use num_traits::{One, Zero};
use rand::random;
use std::{cell::RefCell, collections::HashMap};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{
    shared_math::{
        bfield_codec::BFieldCodec,
        tip5::{Tip5, Tip5State},
    },
    util_types::algebraic_hasher::{AlgebraicHasher, Domain, SpongeHasher},
};

use crate::{
    arithmetic::u128::{shift_left_static_u128, shift_right_static_u128},
    get_init_tvm_stack,
    hashing::sample_indices::SampleIndices,
    list::{
        higher_order::{
            inner_function::{InnerFunction, RawCode},
            map::Map,
        },
        ListType,
    },
    rust_shadowing_helper_functions,
    snippet::{DataType, DepracatedSnippet},
    Digest, ExecutionState, VmHasher, DIGEST_LENGTH,
};

/// Derives the indices that make up the removal record from the item
/// (a digest), the sender randomness (also a digest), receiver
/// preimage (ditto), and the item's aocl leaf index.
pub struct GetSwbfIndices {
    pub window_size: u32,
    pub num_trials: usize,
}

impl GetSwbfIndices {
    fn generate_input_state(stochastic: bool) -> ExecutionState {
        let mut stack = get_init_tvm_stack();
        let (item, sender_randomness, receiver_preimage, aocl_leaf_index): (
            Digest,
            Digest,
            Digest,
            u64,
        ) = if stochastic {
            (random(), random(), random(), random())
        } else {
            (
                VmHasher::hash_varlen(&[BFieldElement::new(0)]),
                VmHasher::hash_varlen(&[BFieldElement::new(1)]),
                VmHasher::hash_varlen(&[BFieldElement::new(2)]),
                ((u32::MAX as u64) << 3) + 2,
            )
        };
        stack.push(BFieldElement::new(aocl_leaf_index >> 32));
        stack.push(BFieldElement::new(aocl_leaf_index & u32::MAX as u64));
        stack.push(receiver_preimage.values()[4]);
        stack.push(receiver_preimage.values()[3]);
        stack.push(receiver_preimage.values()[2]);
        stack.push(receiver_preimage.values()[1]);
        stack.push(receiver_preimage.values()[0]);
        stack.push(sender_randomness.values()[4]);
        stack.push(sender_randomness.values()[3]);
        stack.push(sender_randomness.values()[2]);
        stack.push(sender_randomness.values()[1]);
        stack.push(sender_randomness.values()[0]);
        stack.push(item.values()[4]);
        stack.push(item.values()[3]);
        stack.push(item.values()[2]);
        stack.push(item.values()[1]);
        stack.push(item.values()[0]);
        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory: HashMap::new(),
            words_allocated: 0,
        }
    }
}

impl DepracatedSnippet for GetSwbfIndices {
    fn entrypoint_name(&self) -> String {
        format!(
            "tasm_neptune_mutator_get_swbf_indices_{}_{}",
            self.window_size, self.num_trials
        )
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "aocl_leaf_hi".to_string(),
            "aocl_leaf_lo".to_string(),
            "receiver_preimage4".to_string(),
            "receiver_preimage3".to_string(),
            "receiver_preimage2".to_string(),
            "receiver_preimage1".to_string(),
            "receiver_preimage0".to_string(),
            "sender_randomness4".to_string(),
            "sender_randomness3".to_string(),
            "sender_randomness2".to_string(),
            "sender_randomness1".to_string(),
            "sender_randomness0".to_string(),
            "item4".to_string(),
            "item3".to_string(),
            "item2".to_string(),
            "item1".to_string(),
            "item0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::U64,
            DataType::Digest,
            DataType::Digest,
            DataType::Digest,
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::List(Box::new(DataType::U32))]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["index_list".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -16
    }

    fn function_code(&self, library: &mut crate::library::Library) -> String {
        let num_trials = self.num_trials;
        let window_size = self.window_size;
        let sample_indices = library.import(Box::new(SampleIndices {
            list_type: ListType::Unsafe,
        }));

        let entrypoint = self.entrypoint_name();

        let rawcode_for_inner_function_u128_plus_u32 = RawCode::new_with_shadowing(
            triton_asm!(
                u32_to_u128_add_another_u128:
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index input_u32
                dup 4
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index input_u32 x_0
                add
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index (input_u32 + x_0)
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index carry_to_1 output_0
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_1
                dup 6
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_1 x_1
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 carry_to_2 output_1
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 carry_to_2
                dup 8
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 carry_to_3 output_2
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 carry_to_3
                dup 10
                add
                split
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 overflow output_3
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 output_3 overflow

                // verify no overflow
                push 0
                eq
                assert
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_0 output_1 output_2 output_3
                swap 3
                swap 1
                swap 2
                swap 1
                // stack:  _ [x_3, x_2, x_1, x_0] input_list output_list index output_3 output_2 output_1 output_0
                return
            ),
            vec![DataType::U128, DataType::U32],
            vec![DataType::U128],
            Box::new(RefCell::new(|vec: &mut Vec<BFieldElement>| {
                let list_element = vec.pop().unwrap().value();
                let u128_value = vec[vec.len() - 1].value() as u128
                    + ((vec[vec.len() - 2].value() as u128) << 32)
                    + ((vec[vec.len() - 3].value() as u128) << 64)
                    + ((vec[vec.len() - 4].value() as u128) << 96);

                let new_value = list_element as u128 + u128_value;
                let encoded = new_value.encode();
                for elem in encoded.into_iter().rev() {
                    vec.push(elem);
                }
            })),
        );
        let map_add_batch_offset = library.import(Box::new(Map {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(rawcode_for_inner_function_u128_plus_u32),
        }));

        // TODO: This can be replaced by a bit-mask to save some clock cycles
        let divide_by_batch_size = library.import(Box::new(
            shift_right_static_u128::ShiftRightStaticU128::<LOG2_BATCH_SIZE>,
        ));
        let mul_by_chunk_size = library.import(Box::new(
            shift_left_static_u128::ShiftLeftStaticU128::<LOG2_CHUNK_SIZE>,
        ));

        format!(
            "
        // BEFORE: _ li_hi li_lo r4 r3 r2 r1 r0 s4 s3 s2 s1 s0 i4 i3 i2 i1 i0
        // AFTER: _ index_list
        {entrypoint}:

            absorb_init
            pop pop pop pop pop
            pop pop pop pop pop
            // _ li_hi li_lo r4 r3 r2 r1 r0

            push 1 push 0 push 0
            // _ li_hi li_lo r4 r3 r2 r1 r0 1 0 0

            swap 9 // _ 0 li_lo r4 r3 r2 r1 r0 1 0 li_hi
            swap 6 // _ 0 li_lo r4 li_hi r2 r1 r0 1 0 r3
            swap 3 // _ 0 li_lo r4 li_hi r2 r1 r3 1 0 r0

            swap 1 // _ 0 li_lo r4 li_hi r2 r1 r3 1 r0 0
            swap 8 // _ 0 0 r4 li_hi r2 r1 r3 1 r0 li_lo
            swap 5 // _ 0 0 r4 li_hi li_lo r1 r3 1 r0 r2
            swap 2 // _ 0 0 r4 li_hi li_lo r1 r3 r2 r0 1
            swap 7 // _ 0 0 1 li_hi li_lo r1 r3 r2 r0 r4
            swap 4 // _ 0 0 1 li_hi li_lo r4 r3 r2 r0 r1
            swap 1 // _ 0 0 1 li_hi li_lo r4 r3 r2 r1 r0

            absorb
            pop pop pop pop pop
            // _ 0 0 1 li_hi li_lo

            swap 2 // _ 0 0 li_lo li_hi 1
            pop    // _ 0 0 li_lo li_hi
            swap 1
            call {divide_by_batch_size}
            call {mul_by_chunk_size}
            // _ batch_offset_3 batch_offset_2 batch_offset_1 batch_offset_0

            push {num_trials} // _ [batch_offset_u128] number
            push {window_size} // _ [batch_offset_u128] number upper_bound
            call {sample_indices} // _ [batch_offset_u128] list_of_indices_as_u32s
            call {map_add_batch_offset}
            // _ list_of_absolute_indices_as_u128s

            return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![Self::generate_input_state(true)]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::generate_input_state(false)
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::generate_input_state(false)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let item = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let sender_randomness = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let receiver_preimage = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let aocl_leaf_index_lo: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let aocl_leaf_index_hi: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let aocl_leaf_index_u64: u64 =
            ((aocl_leaf_index_hi as u64) << 32) + aocl_leaf_index_lo as u64;

        let mut vector = vec![];
        for i in item.values() {
            vector.push(i);
        }
        for s in sender_randomness.values() {
            vector.push(s);
        }
        for s in receiver_preimage.values() {
            vector.push(s);
        }
        vector.push(BFieldElement::new(aocl_leaf_index_lo as u64));
        vector.push(BFieldElement::new(aocl_leaf_index_hi as u64));
        vector.push(BFieldElement::one());
        vector.push(BFieldElement::zero());
        vector.push(BFieldElement::zero());

        let mut sponge = Tip5State::new(Domain::VariableLength);
        Tip5::absorb_repeatedly(&mut sponge, vector.iter());

        let mut u32_indices = vec![];
        let mut squeezed_elements = vec![];
        while u32_indices.len() != self.num_trials {
            if squeezed_elements.is_empty() {
                squeezed_elements = Tip5::squeeze(&mut sponge).into_iter().rev().collect_vec();
            }
            let element = squeezed_elements.pop().unwrap();
            if element != BFieldElement::new(BFieldElement::MAX) {
                u32_indices.push(element.value() as u32 % self.window_size);
            }
        }

        let u32_list_size_in_words = self.num_trials + 1;
        let u32_list_pointer = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            u32_list_size_in_words,
            memory,
        );
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(u32_list_pointer, memory);

        rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
            u32_list_pointer,
            self.num_trials as u32,
            memory,
        );

        for (i, index) in u32_indices.iter().enumerate() {
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_set(
                u32_list_pointer,
                i,
                vec![BFieldElement::new(*index as u64)],
                memory,
                1,
            );
        }

        // Compare derived indices to actual implementation (copy-pasted from
        // mutator set implementaion.
        let indices_from_mutator_set = get_swbf_indices::<VmHasher>(
            &item,
            &sender_randomness,
            &receiver_preimage,
            aocl_leaf_index_u64,
        );

        // let batch_offset = aocl_leaf_index_u64 as u128 /  (1 << LOG2_BATCH_SIZE)  as u128;
        let batch_index: u128 = aocl_leaf_index_u64 as u128 / (1 << LOG2_BATCH_SIZE) as u128;
        let batch_offset: u128 = batch_index * (1 << LOG2_CHUNK_SIZE) as u128;
        let u128_indices = u32_indices
            .into_iter()
            .map(|x| (x as u128) + batch_offset)
            .collect_vec();

        // Sanity check that this RUST-shadowing agrees with the real deal
        assert_eq!(
            indices_from_mutator_set.to_vec(),
            u128_indices,
            "VM-calculated indices must match that from mutator set module"
        );

        let u128_list_size_in_words = self.num_trials * 4 + 1;
        let u128_list_pointer = rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(
            u128_list_size_in_words,
            memory,
        );
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
            u128_list_pointer,
            u128_indices,
            memory,
        );

        stack.push(u128_list_pointer);
    }
}

// Copy-pasted from mutator set implementation
const NUM_TRIALS: usize = 45;
const LOG2_BATCH_SIZE: u8 = 3;
const LOG2_CHUNK_SIZE: u8 = 12;
const LOG2_WINDOW_SIZE: u32 = 20;
fn get_swbf_indices<H: AlgebraicHasher>(
    item: &Digest,
    sender_randomness: &Digest,
    receiver_preimage: &Digest,
    aocl_leaf_index: u64,
) -> [u128; 45_usize] {
    let batch_index: u128 = aocl_leaf_index as u128 / (1 << LOG2_BATCH_SIZE) as u128;
    let batch_offset: u128 = batch_index * (1 << LOG2_CHUNK_SIZE) as u128;
    let leaf_index_bfes = aocl_leaf_index.encode();
    let input = [
        item.encode(),
        sender_randomness.encode(),
        receiver_preimage.encode(),
        leaf_index_bfes,
        // Pad with zeros until length is a multiple of RATE; according to spec
        vec![
            BFieldElement::one(),
            BFieldElement::zero(),
            BFieldElement::zero(),
        ],
    ]
    .concat();
    assert_eq!(input.len() % DIGEST_LENGTH, 0);
    let mut sponge = <H as SpongeHasher>::init();
    H::absorb_repeatedly(&mut sponge, input.iter());
    H::sample_indices(&mut sponge, 1 << LOG2_WINDOW_SIZE, NUM_TRIALS)
        .into_iter()
        .map(|sample_index| sample_index as u128 + batch_offset)
        .collect_vec()
        .try_into()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::GetSwbfIndices;

    #[test]
    fn new_prop_test() {
        test_rust_equivalence_multiple(
            &GetSwbfIndices {
                window_size: 1048576,
                num_trials: 45,
            },
            true,
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn commit_benchmark_unsafe() {
        bench_and_write(GetSwbfIndices {
            window_size: 1048576,
            num_trials: 45,
        });
    }
}
