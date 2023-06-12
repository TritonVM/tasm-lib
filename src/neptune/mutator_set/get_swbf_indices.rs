use std::collections::HashMap;

use itertools::Itertools;
use num_traits::{One, Zero};
use rand::random;
use triton_vm::BFieldElement;
use twenty_first::{
    shared_math::tip5::{Tip5, Tip5State},
    util_types::algebraic_hasher::{Domain, SpongeHasher},
};

use crate::{
    get_init_tvm_stack,
    hashing::sample_indices::SampleIndices,
    list::ListType,
    rust_shadowing_helper_functions,
    snippet::{DataType, Snippet},
    Digest, ExecutionState,
};

/// Derives the indices that make up the removal record from the item
/// (a digest), the sender randomness (also a digest), receiver
/// preimage (ditto), and the item's aocl leaf index.
pub struct GetSwbfIndices {
    pub window_size: u32,
    pub num_trials: usize,
}

impl GetSwbfIndices {
    fn generate_input_state() -> ExecutionState {
        let mut stack = get_init_tvm_stack();
        let item: Digest = random();
        let sender_randomness: Digest = random();
        let receiver_preimage: Digest = random();
        let aocl_leaf_index: u64 = random();
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
            secret_in: vec![],
            memory: HashMap::new(),
            words_allocated: 0,
        }
    }
}

impl Snippet for GetSwbfIndices {
    fn entrypoint(&self) -> String {
        format!(
            "tasm_neptune_mutator_get_swbf_indices_{}_{}",
            self.window_size, self.num_trials
        )
    }

    fn inputs(&self) -> Vec<String> {
        vec![
            "aocl_leaf_index1".to_string(),
            "aocl_leaf_index0".to_string(),
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

    fn outputs(&self) -> Vec<String> {
        vec!["index_list".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -16
    }

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let num_trials = self.num_trials;
        let window_size = self.window_size;
        let sample_indices = _library.import(Box::new(SampleIndices {
            list_type: ListType::Unsafe,
        }));
        let entrypoint = self.entrypoint();

        format!(
            "
        // BEFORE: _ index1 index0 r4 r3 r2 r1 r0 s4 s3 s2 s1 s0 i4 i3 i2 i1 i0
        // AFTER: _ index_list
        {entrypoint}:

            absorb_init
            pop pop pop pop pop
            pop pop pop pop pop
            // _ index1 index0 r4 r3 r2 r1 r0

            push 1 push 0 push 0
            // _ index1 index0 r4 r3 r2 r1 r0 1 0 0

            swap 9 // _ 0 index0 r4 r3 r2 r1 r0 1 0 index1
            swap 6 // _ 0 index0 r4 index1 r2 r1 r0 1 0 r3
            swap 3 // _ 0 index0 r4 index1 r2 r1 r3 1 0 r0

            swap 1 // _ 0 index0 r4 index1 r2 r1 r3 1 r0 0
            swap 8 // _ 0 0 r4 index1 r2 r1 r3 1 r0 index0
            swap 5 // _ 0 0 r4 index1 index0 r1 r3 1 r0 r2
            swap 2 // _ 0 0 r4 index1 index0 r1 r3 r2 r0 1
            swap 7 // _ 0 0 1 index1 index0 r1 r3 r2 r0 r4
            swap 4 // _ 0 0 1 index1 index0 r4 r3 r2 r0 r1
            swap 1 // _ 0 0 1 index1 index0 r4 r3 r2 r1 r0

            absorb
            pop pop pop pop pop
            pop pop pop pop pop

            push {num_trials} // _ number
            push {window_size} // _ number upper_bound
            call {sample_indices} // _ list_of_indices
        
            return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![Self::generate_input_state()]
    }

    fn common_case_input_state(&self) -> crate::ExecutionState {
        Self::generate_input_state()
    }

    fn worst_case_input_state(&self) -> crate::ExecutionState {
        Self::generate_input_state()
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
        let aocl_leaf_index_hi: u32 = stack.pop().unwrap().value().try_into().unwrap();
        let aocl_leaf_index_lo: u32 = stack.pop().unwrap().value().try_into().unwrap();

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
        vector.push(BFieldElement::new(aocl_leaf_index_hi as u64));
        vector.push(BFieldElement::new(aocl_leaf_index_lo as u64));
        vector.push(BFieldElement::one());
        vector.push(BFieldElement::zero());
        vector.push(BFieldElement::zero());

        let mut sponge = Tip5State::new(Domain::VariableLength);
        Tip5::absorb_repeatedly(&mut sponge, vector.iter());

        let mut indices = vec![];
        let mut scalars = Tip5::squeeze(&mut sponge).into_iter().rev().collect_vec();
        while indices.len() != self.num_trials {
            if scalars.is_empty() {
                scalars = Tip5::squeeze(&mut sponge).into_iter().rev().collect_vec();
            }
            let scalar = scalars.pop().unwrap();
            if scalar == -BFieldElement::one() {
                continue;
            }
            indices.push(scalar.value() as u32 % self.window_size);
        }

        let size_in_words = self.num_trials + 1;
        let list_pointer =
            rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(size_in_words, memory);
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(list_pointer, memory);

        rust_shadowing_helper_functions::unsafe_list::unsafe_list_set_length(
            list_pointer,
            self.num_trials as u32,
            memory,
        );

        for (i, index) in indices.into_iter().enumerate() {
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_set(
                list_pointer,
                i,
                vec![BFieldElement::new(index as u64)],
                memory,
                1,
            );
        }

        stack.push(list_pointer);
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::GetSwbfIndices;

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(
            &GetSwbfIndices {
                window_size: 1048576,
                num_trials: 45,
            },
            true,
        );
    }

    #[test]
    fn commit_benchmark_unsafe() {
        bench_and_write(GetSwbfIndices {
            window_size: 1048576,
            num_trials: 45,
        });
    }
}
