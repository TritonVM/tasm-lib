use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::{
    function::Function,
    get_init_tvm_stack,
    snippet::{BasicSnippet, DataType},
    snippet_bencher::BenchmarkCase,
    structure::tasm_object::{load_to_memory, TasmObject},
    Digest, VmHasher,
};

struct MerkleRoot;

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
            VmHasher::hash_pair(&left, &right)
        };

        return result;
    }
}

impl BasicSnippet for MerkleRoot {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Digest)),
                "*leafs".to_string(),
            ),
            (DataType::U32, "start".to_string()),
            (DataType::U32, "stop".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
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
            library.import(Box::new(crate::arithmetic::u32::safe_add::SafeAdd));
        let tasm_arithmetic_u32_safe_sub_u32 =
            library.import(Box::new(crate::arithmetic::u32::safe_sub::SafeSub));
        let hash_pair = triton_asm!(
                hash
                pop
                pop
                pop
                pop
                pop);
        #[allow(non_snake_case)]
        let branch_binop_Eq__LboolR_bool_8_then =
            format!("{entrypoint}_binop_Eq__LboolR_bool_8_then");
        #[allow(non_snake_case)]
        let branch_binop_Eq__LboolR_bool_8_else =
            format!("{entrypoint}_binop_Eq__LboolR_bool_8_else");

        // kudos to tasm-lang compiler
        triton_asm! {
        {branch_binop_Eq__LboolR_bool_8_then}:
        pop
        dup 7
        dup 7
        push 5
        mul
        push 1
        add
        add
        push 4
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
        push 1
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        push 0
        return

        {branch_binop_Eq__LboolR_bool_8_else}:
        dup 5
        dup 7
        swap 1
        call {tasm_arithmetic_u32_safe_sub_u32}
        push 2
        swap 1
        div
        pop
        dup 8
        dup 8
        dup 8
        dup 3
        swap 1
        call {tasm_arithmetic_u32_safe_sub_u32}
        call {entrypoint}
        dup 13
        dup 13
        dup 7
        call {tasm_arithmetic_u32_safe_add_u32}
        dup 13
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
        {&hash_pair}
        push 1
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        return

        {entrypoint}:
        push 0
        push 0
        push 0
        push 0
        push 0
        push 1
        dup 1
        write_mem
        push 1
        add
        dup 2
        write_mem
        push 1
        add
        dup 3
        write_mem
        push 1
        add
        dup 4
        write_mem
        push 1
        add
        dup 5
        write_mem
        pop
        dup 5
        dup 7
        push 1
        call tasm_arithmetic_u32_safe_add_u32
        eq
        push 1
        swap 1
        skiz
        call {branch_binop_Eq__LboolR_bool_8_then}
        skiz
        call {branch_binop_Eq__LboolR_bool_8_else}
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        push 5
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
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
        _bench_case: Option<BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_leafs = 64;
        let leafs = (0..num_leafs).map(|_| rng.gen::<Digest>()).collect_vec();

        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let pointer = load_to_memory(&mut memory, leafs);
        let mut stack = get_init_tvm_stack();
        stack.push(pointer);
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
