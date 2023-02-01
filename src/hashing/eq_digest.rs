use std::collections::HashMap;

use rand::Rng;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::rescue_prime_digest::Digest;

use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct EqDigest;

impl Snippet for EqDigest {
    fn inputs() -> Vec<&'static str> {
        vec!["b4", "b3", "b2", "b1", "b0", "a4", "a3", "a2", "a1", "a0"]
    }

    fn outputs() -> Vec<&'static str> {
        vec!["(a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)"]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Bool]
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
        -9
    }

    fn entrypoint(&self) -> &'static str {
        "eq_digest"
    }

    fn function_body(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        format!(
            "
            // Before: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // After: _ (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)
            {entrypoint}:
                swap6  // _ b4 b3 b2 a0 b0 a4 a3 a2 a1 b1
                eq     // _ b4 b3 b2 a0 b0 a4 a3 a2 (a1 = b1)
                swap6  // _ b4 b3 (a1 = b1) a0 b0 a4 a3 a2 b2
                eq     // _ b4 b3 (a1 = b1) a0 b0 a4 a3 (a2 = b2)
                swap6  // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 a3 b3
                eq     // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 (a3 = b3)
                swap6  // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 a4 b4
                eq     // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 (a4 = b4)
                swap2  // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) b0 a0
                eq     // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) (b0 = a0)
                
                mul
                mul
                mul
                mul    // (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)

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
        let digest_a = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        let digest_b = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        stack.push(BFieldElement::new((digest_a == digest_b) as u64));
    }
}

#[cfg(test)]
mod tests {
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::rust_tasm_equivalence_prop_new;

    use super::*;

    #[test]
    fn swap_digest_test() {
        rust_tasm_equivalence_prop_new::<EqDigest>(EqDigest);
    }

    #[test]
    fn swap_digest_benchmark() {
        bench_and_write::<EqDigest>(EqDigest);
    }
}
