use rand::prelude::*;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::prelude::Digest;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

/// Reverse the order of elements in a digest: [d4, d3, d2, d1, d0] -> [d0, d1, d2, d3, d4]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ReverseDigest;

impl ReverseDigest {
    fn get_input_state() -> InitVmState {
        let digest: Digest = random();
        let stack = [empty_stack(), digest.values().to_vec()].concat();
        InitVmState::with_stack(stack)
    }
}

impl DeprecatedSnippet for ReverseDigest {
    fn entrypoint_name(&self) -> String {
        "tasmlib_hashing_reverse_digest".to_owned()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "d4".to_owned(),
            "d3".to_owned(),
            "d2".to_owned(),
            "d1".to_owned(),
            "d0".to_owned(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::Digest]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "d0".to_owned(),
            "d1".to_owned(),
            "d2".to_owned(),
            "d3".to_owned(),
            "d4".to_owned(),
        ]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Digest]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();

        format!(
            "
            // BEFORE: _ d4 d3 d2 d1 d0
            // AFTER:  _ d0 d1 d2 d3 d4
            {entrypoint}:
                swap 4
                // _ d0 d3 d2 d1 d4
                swap 1
                // _ d0 d3 d2 d4 d1
                swap 3
                // _ d0 d1 d2 d4 d3

                swap 1
                // _ d0 d1 d2 d3 d4

                // _ d0 d1 d2 d3 d4

                return
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        vec![Self::get_input_state()]
    }

    fn common_case_input_state(&self) -> InitVmState {
        Self::get_input_state()
    }

    fn worst_case_input_state(&self) -> InitVmState {
        Self::get_input_state()
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut elems = vec![];
        for _ in 0..Digest::LEN {
            elems.push(stack.pop().unwrap())
        }

        for elem in elems {
            stack.push(elem);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn reverse_digest_test() {
        test_rust_equivalence_multiple_deprecated(&ReverseDigest, true);
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn reverse_digest_benchmark() {
        bench_and_write(ReverseDigest);
    }
}
