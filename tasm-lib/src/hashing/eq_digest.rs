use std::collections::HashMap;

use rand::Rng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::Digest;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct EqDigest;

impl DeprecatedSnippet for EqDigest {
    fn entrypoint_name(&self) -> String {
        "tasmlib_hashing_eq_digest".to_string()
    }

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

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::Digest, DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["(a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        -9
    }

    fn function_code(&self, _library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        format!(
            "
            // BEFORE: _ b4 b3 b2 b1 b0 a4 a3 a2 a1 a0
            // AFTER:  _ (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)
            {entrypoint}:
                swap 6  // _ b4 b3 b2 a0 b0 a4 a3 a2 a1 b1
                eq      // _ b4 b3 b2 a0 b0 a4 a3 a2 (a1 = b1)
                swap 6  // _ b4 b3 (a1 = b1) a0 b0 a4 a3 a2 b2
                eq      // _ b4 b3 (a1 = b1) a0 b0 a4 a3 (a2 = b2)
                swap 6  // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 a3 b3
                eq      // _ b4 (a2 = b2) (a1 = b1) a0 b0 a4 (a3 = b3)
                swap 6  // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 a4 b4
                eq      // _ (a3 = b3) (a2 = b2) (a1 = b1) a0 b0 (a4 = b4)
                swap 2  // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) b0 a0
                eq      // _ (a3 = b3) (a2 = b2) (a1 = b1) (a4 = b4) (b0 = a0)

                mul
                mul
                mul
                mul     // (a3 = b3)·(a2 = b2)·(a1 = b1)·(a4 = b4)·(b0 = a0)

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut rng = rand::thread_rng();
        let digest_a: Digest = rng.gen();
        let digest_b: Digest = rng.gen();

        let mut stack = empty_stack();
        push_encodable(&mut stack, &digest_b);
        push_encodable(&mut stack, &digest_a);

        vec![InitVmState::with_stack(stack)]
    }

    fn common_case_input_state(&self) -> InitVmState {
        let mut stack = empty_stack();
        push_encodable(&mut stack, &Digest::default());
        push_encodable(&mut stack, &Digest::default());

        InitVmState::with_stack(stack)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        let mut rng = rand::thread_rng();
        let digest_a: Digest = rng.gen();
        let digest_b: Digest = rng.gen();

        let mut stack = empty_stack();
        push_encodable(&mut stack, &digest_b);
        push_encodable(&mut stack, &digest_a);

        InitVmState::with_stack(stack)
    }

    fn rust_shadowing(
        &self,
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
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn swap_digest_test() {
        test_rust_equivalence_multiple_deprecated(&EqDigest, true);
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn eq_digest_benchmark() {
        bench_and_write(EqDigest);
    }
}
