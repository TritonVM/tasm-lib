use std::collections::HashMap;

use itertools::Itertools;
use num_traits::Zero;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::data_type::DataType;
use crate::{
    empty_stack,
    function::Function,
    snippet::BasicSnippet,
    snippet_bencher::BenchmarkCase,
    structure::tasm_object::{encode_to_memory, TasmObject},
    Digest, VmHasher,
};

/// Compute the Merkle root of a slice of `Digest`s
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
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Digest)),
                "*leafs".to_string(),
            ),
            (DataType::U32, "start".to_string()),
            (DataType::U32, "stop".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![(DataType::Digest, "root".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasm_hashing_merkle_root".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let tasm_arithmetic_u32_safe_add_u32 =
            library.import(Box::new(crate::arithmetic::u32::safeadd::Safeadd));
        let tasm_arithmetic_u32_safe_sub_u32 =
            library.import(Box::new(crate::arithmetic::u32::safesub::Safesub));
        #[allow(non_snake_case)]
        let _leafs_start__LDigestR_Digest_10_then =
            format!("{entrypoint}_leafs_start__LDigestR_Digest_10_then");
        #[allow(non_snake_case)]
        let _fn_call__LDigestR_Digest_30_else =
            format!("{entrypoint}_fn_call__LDigestR_Digest_30_else");

        // kudos to tasm-lang compiler
        triton_asm! {
        {_leafs_start__LDigestR_Digest_10_then}:
        pop 1
        dup 2
        dup 2
        push 5
        mul
        push 5
        add
        add
        read_mem 5
        pop 1
        push 0
        return

        {_fn_call__LDigestR_Digest_30_else}:
        dup 0
        dup 2
        swap 1
        call {tasm_arithmetic_u32_safe_sub_u32}
        push 2
        swap 1
        div_mod
        pop 1
        dup 3
        dup 3
        dup 3
        dup 3
        swap 1
        call {tasm_arithmetic_u32_safe_sub_u32}
        call {entrypoint}
        dup 8
        dup 8
        dup 7
        call {tasm_arithmetic_u32_safe_add_u32}
        dup 8
        call {entrypoint}
        dup 4
        dup 4
        dup 4
        dup 4
        dup 4
        dup 14
        dup 14
        dup 14
        dup 14
        dup 14
        hash
        swap 11
        pop 1
        swap 11
        pop 1
        swap 11
        pop 1
        swap 11
        pop 1
        swap 11
        pop 5
        pop 2
        return

        {entrypoint}:
        dup 0
        dup 2
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        eq
        push 1
        swap 1
        skiz
        call {_leafs_start__LDigestR_Digest_10_then}
        skiz
        call {_fn_call__LDigestR_Digest_30_else}
        swap 4
        swap 7
        pop 1
        swap 2
        swap 5
        pop 1
        swap 3
        pop 1
        swap 1
        return
        }
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
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_leafs = match bench_case {
            Some(BenchmarkCase::CommonCase) => 32,
            Some(BenchmarkCase::WorstCase) => 128,
            None => 64,
        };
        let leafs = (0..num_leafs).map(|_| rng.gen::<Digest>()).collect_vec();

        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let address = BFieldElement::zero();
        encode_to_memory(&mut memory, address, leafs);
        let mut stack = empty_stack();
        stack.push(address);
        stack.push(BFieldElement::new(0)); // start
        stack.push(BFieldElement::new(num_leafs)); // stop

        (stack, memory)
    }
}

#[cfg(test)]
mod test {
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    use super::MerkleRoot;

    #[test]
    fn test() {
        ShadowedFunction::new(MerkleRoot).test()
    }
}

#[cfg(test)]
mod benches {
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    use super::*;

    #[test]
    fn merkle_root_bench() {
        ShadowedFunction::new(MerkleRoot).bench()
    }
}
