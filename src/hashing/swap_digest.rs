use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_digest::DIGEST_LENGTH;

use crate::library::Library;
use crate::snippet::DataType;
use crate::snippet::Snippet;
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct SwapDigest;

impl Snippet for SwapDigest {
    fn inputs() -> Vec<&'static str> {
        vec!["b4", "b3", "b2", "b1", "b0", "a4", "a3", "a2", "a1", "a0"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["a4", "a3", "a2", "a1", "a0", "b4", "b3", "b2", "b1", "b0"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn crash_conditions() -> Vec<&'static str> {
        vec![]
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let digest_a: Digest = rng.gen();
        let digest_b: Digest = rng.gen();

        let mut stack = get_init_tvm_stack();
        push_hashable(&mut stack, &digest_b);
        push_hashable(&mut stack, &digest_a);

        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff() -> isize {
        0
    }

    fn entrypoint() -> &'static str {
        "swap_digest"
    }

    fn function_body(_library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        format!(
            "
            // Before: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // After:  _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0
            {entrypoint}:
                // a4 <-> b4:
                swap4 // _ b4 b3 b2 b1 b0 a0 a3 a2 a1 a4
                swap9 // _ a4 b3 b2 b1 b0 a0 a3 a2 a1 b4
                swap4 // _ a4 b3 b2 b1 b0 b4 a3 a2 a1 a0

                // a3 <-> b3:
                swap3 // _ a4 b3 b2 b1 b0 b4 a0 a2 a1 a3
                swap8 // _ a4 a3 b2 b1 b0 b4 a0 a2 a1 b3
                swap3 // _ a4 a3 b2 b1 b0 b4 b3 a2 a1 a0

                // a2 <-> b2
                swap2 // _ a4 a3 b2 b1 b0 b4 b3 a0 a1 a2
                swap7 // _ a4 a3 a2 b1 b0 b4 b3 a0 a1 b2
                swap2 // _ a4 a3 a2 b1 b0 b4 b3 b2 a1 a0

                // a1 <-> b1
                swap1 // _ a4 a3 a2 b1 b0 b4 b3 b2 a0 a1
                swap6 // _ a4 a3 a2 a1 b0 b4 b3 b2 a0 b1
                swap1 // _ a4 a3 a2 a1 b0 b4 b3 b2 b1 a0

                // a0 <-> b0
                swap5 // _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0

                return
            "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let h = stack.len();
        for i in 0..DIGEST_LENGTH {
            let ai_pos = h - i - 1;
            let bi_pos = h - i - DIGEST_LENGTH - 1;
            stack.swap(ai_pos, bi_pos);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn swap_digest_test() {
        rust_tasm_equivalence_prop_new::<SwapDigest>(SwapDigest);
    }

    #[test]
    fn swap_digest_benchmark() {
        bench_and_write::<SwapDigest>();
    }
}
