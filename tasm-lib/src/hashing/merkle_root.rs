use std::collections::HashMap;

use itertools::Itertools;
use num_traits::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::tip5::DIGEST_LENGTH;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::AlgebraicHasher;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::encode_to_memory;
use crate::snippet_bencher::BenchmarkCase;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::Digest;
use crate::VmHasher;

/// Compute the Merkle root of a slice of `Digest`s
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MerkleRoot;

impl MerkleRoot {
    pub fn call(leafs: &[Digest], start: usize, stop: usize) -> Digest {
        let result: Digest = if stop == start + 1usize {
            leafs[start]
        } else {
            let half: usize = (stop - start) / 2;
            let left: Digest = Self::call(leafs, start, stop - half);
            let right: Digest = Self::call(leafs, start + half, stop);
            VmHasher::hash_pair(left, right)
        };

        result
    }
}

impl BasicSnippet for MerkleRoot {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Digest)),
                "*leafs".to_string(),
            ),
            (DataType::U32, "start".to_string()),
            (DataType::U32, "stop".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let dyn_malloc = library.import(Box::new(DynMalloc));

        let calculate_parent_digests_label = format!("{entrypoint}_calculate_parent_digests");

        let calculate_parent_digests_code = triton_asm!(
            // Populate the `*next` diges list
            // START: _ *curr *next_last_elem_first_word *curr_last_word
            // INVARIANT: _ *curr *next_elem *curr_elem
            // END: _ *curr *next *curr
            {calculate_parent_digests_label}:
                dup 2
                dup 1
                eq
                skiz
                    return
                // _ *curr *next_elem *curr_elem[n]

                dup 0
                read_mem {DIGEST_LENGTH}
                read_mem {DIGEST_LENGTH}
                // _ *curr *next_elem *curr_elem [right] [left] (*curr_elem[n] - 10)
                // _ *curr *next_elem *curr_elem [right] [left] *curr_elem[n - 2]
                // _ *curr *next_elem *curr_elem [right] [left] *curr_elem'


                swap 11
                pop 1
                // _ *curr *next_elem *curr_elem' [right] [left]

                hash
                // _ *curr *next_elem *curr_elem' [parent_digest]

                dup 6
                // _ *curr *next_elem *curr_elem' [parent_digest] *next_elem

                write_mem {DIGEST_LENGTH}
                // _ *curr *next_elem *curr_elem' (*next_elem + 5)

                push -10
                add
                // _ *curr *next_elem *curr_elem' (*next_elem - 5)
                // _ *curr *next_elem *curr_elem' *next_elem[n-1]
                // _ *curr *next_elem *curr_elem' *next_elem'

                swap 2
                pop 1
                // _ *curr *next_elem' *curr_elem'

                recurse
        );

        let next_layer_label = format!("{entrypoint}_next_layer");
        let next_layer_code = triton_asm!(
            // _ *digests
            {next_layer_label}:
                // _ *current_level

                dup 0
                // _ *current_level *current_level

                read_mem 1
                push 1
                add
                // _ *current_level len *current_level

                /* Allocate and calculate end of `next_level`, preserve `len` */
                call {dyn_malloc}
                // _ *current_level len *current_level *next_level

                push 2
                dup 3
                div_mod
                pop 1
                // _ *current_level len *current_level *next_level (len / 2)

                dup 0
                swap 2
                // _ *current_level len *current_level (len / 2) (len / 2) *next_level

                write_mem 1
                // _ *current_level len *current_level (len / 2) (*next_level+1)

                swap 1
                // _ *current_level len *current_level (*next_level+1) (len / 2)

                push -1
                add
                // _ *current_level len *current_level (*next_level+1) (len / 2 - 1)

                push {DIGEST_LENGTH}
                mul
                add
                // _ *current_level len *current_level *next_level_last_elem_first_word

                swap 2
                // _ *current_level *next_level_last_elem_first_word *current_level len

                push {DIGEST_LENGTH}
                mul
                add
                // _ *current_level *next_level_last_elem_first_word *current_level_last_digest

                call {calculate_parent_digests_label}
                // _ *curr (*next_elem-DIGEST_LENGTH+1) *curr_elem

                /* Cleanup stack */
                pop 1
                push {DIGEST_LENGTH - 1}
                add
                swap 1
                pop 1
                // _ *next
        );

        triton_asm!(
                {entrypoint}:
                    // *leafs

                   return
        )
    }
}

impl Function for MerkleRoot {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let stop = stack.pop().unwrap().value() as usize;
        let start = stack.pop().unwrap().value() as usize;
        let leafs_pointer = stack.pop().unwrap();
        let leafs = *Vec::<Digest>::decode_from_memory(memory, leafs_pointer).unwrap();

        let root: Digest = Self::call(&leafs, start, stop);

        stack.push(root.0[4]);
        stack.push(root.0[3]);
        stack.push(root.0[2]);
        stack.push(root.0[1]);
        stack.push(root.0[0]);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_leafs = match bench_case {
            Some(BenchmarkCase::CommonCase) => 512,
            Some(BenchmarkCase::WorstCase) => 1024,
            None => 1 << rng.gen_range(0..=8),
        };
        let leafs = (0..num_leafs).map(|_| rng.gen::<Digest>()).collect_vec();

        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let address = BFieldElement::zero();
        encode_to_memory(&mut memory, address, leafs);
        let mut stack = empty_stack();
        stack.push(address);
        stack.push(BFieldElement::new(0)); // start
        stack.push(BFieldElement::new(num_leafs)); // stop

        FunctionInitialState { stack, memory }
    }
}

#[cfg(test)]
mod test {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::MerkleRoot;

    #[test]
    fn test() {
        ShadowedFunction::new(MerkleRoot).test()
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::MerkleRoot;

    #[test]
    fn merkle_root_bench() {
        ShadowedFunction::new(MerkleRoot).bench()
    }
}
