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
        // #[allow(unused_assignments)]
        // let mut result: Digest = Digest::default();
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

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let read_digest = DataType::Digest.read_value_from_memory_pop_pointer();

        let then_label = format!("{entrypoint}_then");
        let else_label = format!("{entrypoint}_else");
        triton_asm!(
                {entrypoint}:
                    // *leafs start stop

                    push 1

                    dup 2
                    push 1
                    add
                    // *leafs start stop 1 (start + 1)

                    dup 2
                    eq
                    // *leafs start stop 1 (start + 1 == stop)

                    skiz
                        call {then_label}
                    skiz
                        call {else_label}
                    // _ *leafs start stop garbage [digest; 5]
                    // _ *leafs start stop garbage d4 d3 d2 d1 d0

                    swap 4
                    swap 8
                    pop 1
                    // _ d4 start stop garbage d0 d3 d2 d1

                    swap 4
                    pop 1
                    // _ d4 start stop d1 d0 d3 d2

                    swap 4
                    pop 1
                    // _ d4 start d2 d1 d0 d3

                    swap 4
                    pop 1
                    // _ d4 d3 d2 d1 d0

                    return

                {then_label}:
                    // *leafs start stop 1

                    dup 2
                    push {DIGEST_LENGTH}
                    mul
                    dup 4
                    add
                    push {DIGEST_LENGTH}
                    add
                    {&read_digest}
                    // *leafs start stop 1 [digest; 5]

                    push 0
                    return

                {else_label}:
                        // _ *leafs start stop

                        push 2
                        dup 1
                        dup 3
                        push -1
                        mul
                        add
                        // _ *leafs start stop 2 (stop - start)

                        div_mod
                        pop 1
                        // _ *leafs start stop ((stop - start) / 2)
                        // _ *leafs start stop half

                        dup 3
                        dup 3
                        dup 2
                        add
                        // _ *leafs start stop half *leafs (start + half)

                        dup 3
                        // _ *leafs start stop half *leafs (start + half) stop

                        call {entrypoint}
                        // _ *leafs start stop half [right; 5]

                        dup 8
                        dup 8
                        dup 8
                        dup 8
                        push -1
                        mul
                        add
                        // _ *leafs start stop half [right; 5] *leafs start (stop - half)

                        call {entrypoint}
                        // _ *leafs start stop half [right; 5] [left; 5]

                        hash

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
            Some(BenchmarkCase::CommonCase) => 32,
            Some(BenchmarkCase::WorstCase) => 128,
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
