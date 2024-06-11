use std::collections::HashMap;

use itertools::Itertools;
use num_traits::Zero;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::tip5::DIGEST_LENGTH;
use triton_vm::prelude::*;
use twenty_first::util_types::merkle_tree::CpuParallel;
use twenty_first::util_types::merkle_tree::MerkleTree;

use crate::data_type::DataType;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::encode_to_memory;
use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
use crate::snippet_bencher::BenchmarkCase;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::Digest;

/// Compute the Merkle root of a slice of `Digest`s
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MerkleRoot;

impl BasicSnippet for MerkleRoot {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Digest)),
            "*leafs".to_string(),
        )]
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

        let calculate_parent_digests = format!("{entrypoint}_calculate_parent_digests");
        let next_layer_loop = format!("{entrypoint}_next_layer_loop");

        triton_asm!(
                {entrypoint}:
                    // _ *leafs

                    read_mem 1
                    push 1
                    add
                    // _ leafs_len *leafs

                    call {dyn_malloc}
                    // _ leafs_len *leafs *parent_level

                    /* Adjust pointers to point to last element in both lists */
                    /* Adjust `*parent_level` pointer to point to 1st word in
                       its last element */
                    dup 2
                    push -1
                    add
                    push {DIGEST_LENGTH}
                    mul
                    add
                    // _ leafs_len *leafs (*parent_level + (leafs_len - 1)*DIGEST_LENGTH)
                    // _ leafs_len *leafs *parent_level'

                    swap 1
                    // _ leafs_len *parent_level' *leafs

                    /* Adjust `*leafs` to point to last element, last word */
                    dup 2
                    push {DIGEST_LENGTH}
                    mul
                    add
                    // _ leafs_len *parent_level' (*leafs + leafs_len * DIGEST_LENGTH)
                    // _ leafs_len *parent_level' *leafs'

                    call {next_layer_loop}
                    // _ 1 *address (*root + DIGEST_LENGTH)

                    swap 2
                    pop 2
                    // _ (*root + DIGEST_LENGTH - 1)

                    read_mem {DIGEST_LENGTH}
                    // _ [root; 5] (*root - 1)

                    pop 1
                    // _ [root; 5]

                   return

                // INVARIANT:  _ current_len *next_level[last]_first_word *current_level[last]_last_word
                {next_layer_loop}:
                    // _ current_len *next_level *current_level

                    /* end loop when `current_len == 1` */
                    dup 2
                    push 1
                    eq
                    skiz
                        return
                    // _ current_len *next_level *current_level

                    /*Update `current_len` */
                    swap 2
                    log_2_floor
                    push -1
                    add
                    push 2
                    pow
                    swap 2
                    // _ (current_len / 2) *next_level *current_level
                    // _ current_len' *next_level *current_level

                    // What is the stop-condition for `*next_level`?
                    // It must be `*next_level - current_len / 2 * DIGEST_LENGTH`
                    dup 1
                    dup 3
                    push {-(DIGEST_LENGTH as isize)}
                    mul
                    add
                    // _ current_len' *next_level *current_level *next_level_stop

                    swap 1
                    // _ current_len' *next_level *next_level_stop *current_level

                    dup 2
                    swap 1
                    // _ current_len' *next_level *next_level_stop *next_level *current_level

                    push 0
                    push 0
                    push 0
                    push 0

                    // _ current_len' *next_level *next_elem_stop *next_elem *curr_elem 0 0 0 0
                    call {calculate_parent_digests}
                    // _ current_len' *next_level *next_elem_stop *next_elem_stop *curr_elem_stop 0 0 0 0

                    pop 5
                    pop 1
                    // _ current_len' *next_level *next_elem_stop

                    /* Update `*current_level` based on `*next_level` */
                    swap 1
                    // _ current_len' *next_level' *next_level

                    push {DIGEST_LENGTH - 1}
                    add
                    // _ (current_len / 2) *next_level' *current_level'

                    recurse

                // Populate the `*next` digest list
                // INVARIANT: _ *next_elem_stop *next_elem *curr_elem 0 0 0 0
                {calculate_parent_digests}:

                    dup 4
                    read_mem {DIGEST_LENGTH}
                    read_mem {DIGEST_LENGTH}
                    // _ *next_elem_stop *next_elem *curr_elem 0 0 0 0 [right] [left] (*curr_elem[n] - 10)
                    // _ *next_elem_stop *next_elem *curr_elem 0 0 0 0 [right] [left] *curr_elem[n - 2]
                    // _ *next_elem_stop *next_elem *curr_elem 0 0 0 0 [right] [left] *curr_elem'

                    swap 15
                    pop 1
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 [right] [left]

                    hash
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 [parent_digest]

                    dup 10
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 [parent_digest] *next_elem

                    write_mem {DIGEST_LENGTH}
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 (*next_elem + 5)

                    push -10
                    add
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 (*next_elem - 5)
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 *next_elem[n-1]
                    // _ *next_elem_stop *next_elem *curr_elem' 0 0 0 0 *next_elem'

                    swap 6
                    pop 1
                    // _ *next_elem_stop *next_elem' *curr_elem' 0 0 0 0

                    recurse_or_return
        )
    }
}

impl Function for MerkleRoot {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let leafs_pointer = stack.pop().unwrap();
        let leafs = *Vec::<Digest>::decode_from_memory(memory, leafs_pointer).unwrap();
        let mt = MerkleTree::<Tip5>::new::<CpuParallel>(&leafs).unwrap();

        // Write entire Merkle tree to memory, because that's what the VM does
        let pointer = dynamic_allocator(memory);
        let num_non_leaf_nodes = leafs.len();

        // skip dummy digest at index 0
        for (node_index, &node) in (0..num_non_leaf_nodes).zip(mt.nodes()).skip(1) {
            let node_address = pointer + bfe!(node_index as u32) * bfe!(DIGEST_LENGTH as u32);
            encode_to_memory(memory, node_address, node);
        }

        stack.extend(mt.root().reversed().values());
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

        let digests_pointer = rng.gen();

        let leafs = (0..num_leafs).map(|_| rng.gen::<Digest>()).collect_vec();

        self.init_state(leafs, digests_pointer)
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let height_0_a = self.init_state(vec![Digest::default()], BFieldElement::zero());
        let height_0_b = self.init_state(
            vec![Digest::new([bfe!(6), bfe!(5), bfe!(4), bfe!(3), bfe!(2)])],
            bfe!(1u64 << 44),
        );
        let height_1 = self.init_state(
            vec![Digest::default(), Digest::default()],
            BFieldElement::zero(),
        );

        vec![height_0_a, height_0_b, height_1]
    }
}

impl MerkleRoot {
    fn init_state(
        &self,
        leafs: Vec<Digest>,
        digests_pointer: BFieldElement,
    ) -> FunctionInitialState {
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        encode_to_memory(&mut memory, digests_pointer, leafs);
        let mut stack = self.init_stack_for_isolated_run();
        stack.push(digests_pointer);

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
