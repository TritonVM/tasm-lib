use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;
use crate::snippet::DataType;
use crate::snippet::DepracatedSnippet;
use crate::Digest;
use crate::DIGEST_LENGTH;
use crate::{get_init_tvm_stack, push_encodable, ExecutionState};

#[derive(Clone, Debug)]
pub struct SwapDigest;

impl DepracatedSnippet for SwapDigest {
    fn input_field_names(&self) -> Vec<String> {
        vec![
            "b4".to_string(),
            "b3".to_string(),
            "b2".to_string(),
            "b1".to_string(),
            "b0".to_string(),
            "a4".to_string(),
            "a3".to_string(),
            "a2".to_string(),
            "a1".to_string(),
            "a0".to_string(),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "a4".to_string(),
            "a3".to_string(),
            "a2".to_string(),
            "a1".to_string(),
            "a0".to_string(),
            "b4".to_string(),
            "b3".to_string(),
            "b2".to_string(),
            "b1".to_string(),
            "b0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        let mut rng = rand::thread_rng();
        let digest_a: Digest = rng.gen();
        let digest_b: Digest = rng.gen();

        let mut stack = get_init_tvm_stack();
        push_encodable(&mut stack, &digest_b);
        push_encodable(&mut stack, &digest_a);

        vec![ExecutionState::with_stack(stack)]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn entrypoint_name(&self) -> String {
        "tasm_hashing_swap_digest".to_string()
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // Before: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // After:  _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0
            {entrypoint}:
                // a4 <-> b4:
                swap 4 // _ b4 b3 b2 b1 b0 a0 a3 a2 a1 a4
                swap 9 // _ a4 b3 b2 b1 b0 a0 a3 a2 a1 b4
                swap 4 // _ a4 b3 b2 b1 b0 b4 a3 a2 a1 a0

                // a3 <-> b3:
                swap 3 // _ a4 b3 b2 b1 b0 b4 a0 a2 a1 a3
                swap 8 // _ a4 a3 b2 b1 b0 b4 a0 a2 a1 b3
                swap 3 // _ a4 a3 b2 b1 b0 b4 b3 a2 a1 a0

                // a2 <-> b2
                swap 2 // _ a4 a3 b2 b1 b0 b4 b3 a0 a1 a2
                swap 7 // _ a4 a3 a2 b1 b0 b4 b3 a0 a1 b2
                swap 2 // _ a4 a3 a2 b1 b0 b4 b3 b2 a1 a0

                // a1 <-> b1
                swap 1 // _ a4 a3 a2 b1 b0 b4 b3 b2 a0 a1
                swap 6 // _ a4 a3 a2 a1 b0 b4 b3 b2 a0 b1
                swap 1 // _ a4 a3 a2 a1 b0 b4 b3 b2 b1 a0

                // a0 <-> b0
                swap 5 // _ a4 a3 a2 a1 a0 b4 b3 b2 b1 b0

                return
            "
        )
    }

    fn rust_shadowing(
        &self,
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

    fn common_case_input_state(&self) -> ExecutionState {
        let mut rng = rand::thread_rng();
        let mut stack = get_init_tvm_stack();
        push_encodable(&mut stack, &rng.gen::<Digest>());
        push_encodable(&mut stack, &rng.gen::<Digest>());
        ExecutionState::with_stack(stack)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        self.common_case_input_state()
    }
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::test_rust_equivalence_multiple;

    use super::*;

    #[test]
    fn swap_digest_test() {
        test_rust_equivalence_multiple(&SwapDigest, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn swap_digest_benchmark() {
        bench_and_write(SwapDigest);
    }
}
