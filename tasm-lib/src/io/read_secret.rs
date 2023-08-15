use std::collections::HashMap;

use triton_vm::NonDeterminism;
use twenty_first::shared_math::other::random_elements;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

/// Move an element of type `DataType` from secret in to the stack
#[derive(Clone, Debug)]
pub struct ReadSecret(pub DataType);

impl DeprecatedSnippet for ReadSecret {
    fn entrypoint_name(&self) -> String {
        format!("tasm_io_read_secret_{}", self.0)
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![self.0.clone()]
    }

    fn output_field_names(&self) -> Vec<String> {
        // This function returns element_0 on the top of the stack and the other elements below it. E.g.: _ elem_2 elem_1 elem_0
        let mut ret: Vec<String> = vec![];
        let size = self.0.get_size();
        for i in 0..size {
            ret.push(format!("element_{}", size - 1 - i));
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        self.0.get_size() as isize
    }

    fn function_code(&self, _library: &mut crate::library::Library) -> String {
        let entrypoint = self.entrypoint_name();
        let read_an_element = "divine\n".repeat(self.0.get_size());

        format!(
            "
            {entrypoint}:
                {read_an_element}
                return
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["secret input too short".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        vec![ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            nondeterminism: NonDeterminism::new(random_elements(self.0.get_size())),
            memory: HashMap::default(),
            words_allocated: 0,
        }]
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) {
        for elem in secret_in.iter().take(self.0.get_size()) {
            stack.push(*elem)
        }
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            nondeterminism: NonDeterminism::new(random_elements(self.0.get_size())),
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        self.common_case_input_state()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn new_snippet_test() {
        for _ in 0..10 {
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::Bool), true);
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::U32), true);
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::U64), true);
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::BFE), true);
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::XFE), true);
            test_rust_equivalence_multiple_deprecated(&ReadSecret(DataType::Digest), true);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn read_secret_benchmark() {
        bench_and_write(ReadSecret(DataType::Digest));
    }
}
