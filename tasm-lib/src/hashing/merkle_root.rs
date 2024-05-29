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
use twenty_first::util_types::merkle_tree_maker::MerkleTreeMaker;

use crate::data_type::DataType;
use crate::library::Library;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::encode_to_memory;
use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
use crate::rust_shadowing_helper_functions::list::list_new;
use crate::rust_shadowing_helper_functions::list::list_push;
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

                    call {next_layer_loop}
                    // _ *root_list len *root_list

                    pop 2
                    // _ *root_list

                    push {DIGEST_LENGTH}
                    add
                    read_mem {DIGEST_LENGTH}
                    // _ [root; 5] *root_list

                    pop 1
                    // _ [root; 5]

                   return

                    // _ *digests
                {next_layer_loop}:
                    // _ *current_level

                    dup 0
                    // _ *current_level *current_level

                    read_mem 1
                    push 1
                    add
                    // _ *current_level len *current_level

                    /* end loop when `len == 1` */
                    dup 1
                    push 1
                    eq
                    skiz
                        return
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

                    call {calculate_parent_digests}
                    // _ *curr (*next_elem-DIGEST_LENGTH+1) *curr_elem

                    /* Cleanup stack */
                    pop 1
                    push {DIGEST_LENGTH - 1}
                    add
                    swap 1
                    pop 1

                    // _ *next

                    recurse

                // Populate the `*next` diges list
                // START: _ *curr *next_last_elem_first_word *curr_last_word
                // INVARIANT: _ *curr *next_elem *curr_elem
                // END: _ *curr *next *curr
                {calculate_parent_digests}:
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

        let mt: MerkleTree<Tip5> = CpuParallel::from_digests(&leafs).unwrap();
        let root = mt.root();

        // Write entire Merkle tree to memory, because that's what the VM does
        for layer in 1..(mt.height() + 1) {
            let digests_in_this_layer_pointer = dynamic_allocator(memory);
            list_new(digests_in_this_layer_pointer, memory);
            for node_count in 0..(leafs.len() >> layer) {
                let node_index = node_count + (1 << (mt.height() - layer));
                let node = mt.node(node_index).unwrap();
                list_push(
                    digests_in_this_layer_pointer,
                    node.values().to_vec(),
                    memory,
                    DIGEST_LENGTH,
                )
            }
        }

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

        let digests_pointer = BFieldElement::zero();

        let leafs = (0..num_leafs).map(|_| rng.gen::<Digest>()).collect_vec();

        self.init_state(leafs, digests_pointer)
    }

    fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
        let height_0 = self.init_state(vec![Digest::default()], BFieldElement::zero());
        let height_1 = self.init_state(
            vec![Digest::default(), Digest::default()],
            BFieldElement::zero(),
        );

        vec![height_0, height_1]
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
