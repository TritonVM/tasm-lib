use triton_vm::prelude::*;

use crate::arithmetic::u128::shift_left_static;
use crate::arithmetic::u128::shift_right_static;
use crate::hashing::algebraic_hasher::sample_indices::SampleIndices;
use crate::list::higher_order::inner_function::InnerFunction;
use crate::list::higher_order::inner_function::RawCode;
use crate::list::higher_order::map::Map;
use crate::prelude::*;

const LOG2_BATCH_SIZE: u8 = 3;
const LOG2_CHUNK_SIZE: u8 = 12;

/// Derives the indices that make up the removal record from the item
/// (a digest), the sender randomness (also a digest), receiver
/// preimage (ditto), and the item's aocl leaf index.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct GetSwbfIndices {
    pub window_size: u32,
    pub num_trials: usize,
}

impl BasicSnippet for GetSwbfIndices {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "aocl_leaf".to_string()),
            (DataType::Digest, "receiver_preimage".to_string()),
            (DataType::Digest, "sender_randomness".to_string()),
            (DataType::Digest, "item".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::VoidPointer, "*index_list".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_neptune_mutator_get_swbf_indices_{}_{}",
            self.window_size, self.num_trials
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let num_trials = self.num_trials;
        let window_size = self.window_size;
        let sample_indices = library.import(Box::new(SampleIndices));

        let entrypoint = self.entrypoint();

        let map_add_batch_offset = library.import(Box::new(Map::new(InnerFunction::RawCode(
            u32_to_u128_add_another_u128(),
        ))));

        // TODO: This can be replaced by a bit-mask to save some clock cycles
        let divide_by_batch_size = library.import(Box::new(
            shift_right_static::ShiftRightStatic::<LOG2_BATCH_SIZE>,
        ));
        let mul_by_chunk_size = library.import(Box::new(
            shift_left_static::ShiftLeftStatic::<LOG2_CHUNK_SIZE>,
        ));

        triton_asm!(
        // BEFORE: _ li_hi li_lo r4 r3 r2 r1 r0 s4 s3 s2 s1 s0 i4 i3 i2 i1 i0
        // AFTER:  _ index_list
        {entrypoint}:

            sponge_init
            sponge_absorb
            // _ li_hi li_lo r4 r3 r2 r1 r0

            push 0
            push 0
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0

            dup 8 dup 8
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0 li_hi li_lo

            push 0
            push 0
            push 1
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0 li_hi li_lo {0 0 1

            dup 4 dup 4
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0 li_hi li_lo {0 0 1 li_hi li_lo

            dup 13 dup 13 dup 13 dup 13 dup 13
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0 li_hi li_lo {0 0 1 li_hi li_lo r4 r3 r2 r1 r0}

            sponge_absorb
            // _ li_hi li_lo r4 r3 r2 r1 r0 0 0 li_hi li_lo

            call {divide_by_batch_size}
            // _ li_hi li_lo r4 r3 r2 r1 r0 (li / bs)_0 (li / bs)_1 (li / bs)_2 (li / bs)_3

            call {mul_by_chunk_size}
            // _ li_hi li_lo r4 r3 r2 r1 r0 [batch_offset_u128]

            push {num_trials} // _ li_hi li_lo r4 r3 r2 r1 r0 [batch_offset_u128] number
            push {window_size} // _ li_hi li_lo r4 r3 r2 r1 r0 [batch_offset_u128] number upper_bound
            call {sample_indices} // _ li_hi li_lo r4 r3 r2 r1 r0 [batch_offset_u128] *list_of_indices_as_u32s


            call {map_add_batch_offset}
            // _ li_hi li_lo r4 r3 r2 r1 r0 [batch_offset_u128] *list_of_absolute_indices_as_u128s

            swap 11 pop 5 pop 5 pop 1
            // *list_of_absolute_indices_as_u128s

            return
        )
    }
}

/// ```text
/// BEFORE: _ [x_3, x_2, x_1, x_0] [bu ff er] input_u32
/// AFTER:  _ [x_3, x_2, x_1, x_0] [bu ff er] output_3 output_2 output_1 output_0
/// ```
pub(crate) fn u32_to_u128_add_another_u128() -> RawCode {
    let buffer_len = Map::NUM_INTERNAL_REGISTERS;
    let assembly = triton_asm!(
        u32_to_u128_add_another_u128:
        dup {buffer_len + 1}
        add     // _ [x_3, x_2, x_1, x_0] [bu ff er] (input_u32 + x_0)
        split   // _ [x_3, x_2, x_1, x_0] [bu ff er] carry_to_1 output_0
        pick 1  // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 carry_to_1
        dup {buffer_len + 3}
        add
        split   // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 carry_to_2 output_1
        pick 1  // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 carry_to_2
        dup {buffer_len + 5}
        add
        split   // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 carry_to_3 output_2
        pick 1  // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 output_2 carry_to_3
        dup {buffer_len + 7}
        add
        split   // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 output_2 overflow output_3
        pick 1  // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 output_2 output_3 overflow

        // verify no overflow
        push 0
        eq
        assert  // _ [x_3, x_2, x_1, x_0] [bu ff er] output_0 output_1 output_2 output_3
        place 3
        place 2
        place 1 // _ [x_3, x_2, x_1, x_0] [bu ff er] output_3 output_2 output_1 output_0
        return
    );
    RawCode::new(assembly, DataType::U32, DataType::U128)
}

#[cfg(test)]
mod tests {
    use twenty_first::prelude::Sponge;

    use super::*;
    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;

    const NUM_TRIALS: usize = 45;
    const LOG2_WINDOW_SIZE: u32 = 20;

    // Copy-pasted from mutator set implementation
    // Was there no other way besides code duplication? 😩
    fn get_swbf_indices(
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
        ]
        .concat();
        let mut sponge = Tip5::init();
        sponge.pad_and_absorb_all(&input);
        sponge
            .sample_indices(1 << LOG2_WINDOW_SIZE, NUM_TRIALS)
            .into_iter()
            .map(|sample_index| sample_index as u128 + batch_offset)
            .collect_vec()
            .try_into()
            .unwrap()
    }

    impl Function for GetSwbfIndices {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let item = pop_encodable::<Digest>(stack);
            let sender_randomness = pop_encodable::<Digest>(stack);
            let receiver_preimage = pop_encodable::<Digest>(stack);
            let aocl_leaf_index = pop_encodable::<u64>(stack);

            let mut sponge_seed = [item, sender_randomness, receiver_preimage]
                .map(|d| d.values())
                .concat();
            sponge_seed.extend(aocl_leaf_index.encode());

            let mut sponge = Tip5::init();
            sponge.pad_and_absorb_all(&sponge_seed);

            let mut u32_indices = vec![];
            let mut squeezed_elements = vec![];
            while u32_indices.len() != self.num_trials {
                if squeezed_elements.is_empty() {
                    squeezed_elements = sponge.squeeze().into_iter().rev().collect_vec();
                }
                let element = squeezed_elements.pop().unwrap();
                if element != BFieldElement::new(BFieldElement::MAX) {
                    u32_indices.push(element.value() as u32 % self.window_size);
                }
            }

            let u32_list_pointer =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            rust_shadowing_helper_functions::list::list_new(u32_list_pointer, memory);

            rust_shadowing_helper_functions::list::list_set_length(
                u32_list_pointer,
                self.num_trials,
                memory,
            );

            for (i, index) in u32_indices.iter().enumerate() {
                rust_shadowing_helper_functions::list::list_set(
                    u32_list_pointer,
                    i,
                    vec![BFieldElement::new(*index as u64)],
                    memory,
                );
            }

            // Compare derived indices to actual implementation (copy-pasted from
            // mutator set implementaion.
            // Why would you ever want to copy-paste code like this? 😫
            let indices_from_mutator_set = get_swbf_indices(
                &item,
                &sender_randomness,
                &receiver_preimage,
                aocl_leaf_index,
            );

            // let batch_offset = aocl_leaf_index_u64 as u128 /  (1 << LOG2_BATCH_SIZE)  as u128;
            let batch_index: u128 = aocl_leaf_index as u128 / (1 << LOG2_BATCH_SIZE) as u128;
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

            let u128_list_pointer =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            rust_shadowing_helper_functions::list::list_insert(
                u128_list_pointer,
                u128_indices,
                memory,
            );

            stack.push(u128_list_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let mut stack = empty_stack();
            let (item, sender_randomness, receiver_preimage, aocl_leaf_index): (
                Digest,
                Digest,
                Digest,
                u64,
            ) = (rng.random(), rng.random(), rng.random(), rng.random());
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

            FunctionInitialState {
                memory: HashMap::new(),
                stack,
            }
        }
    }

    #[test]
    fn test() {
        ShadowedFunction::new(GetSwbfIndices {
            window_size: 1048576,
            num_trials: 45,
        })
        .test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(GetSwbfIndices {
            window_size: 1048576,
            num_trials: 45,
        })
        .bench();
    }
}
