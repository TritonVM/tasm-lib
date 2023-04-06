use std::collections::HashMap;

use twenty_first::shared_math::{b_field_element::BFieldElement, other::random_elements};

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone)]
pub struct ReadSecret(pub DataType);

impl Snippet for ReadSecret {
    fn entrypoint(&self) -> String {
        format!("tasm_io_read_secret_{}", self.0)
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![self.0.clone()]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        // This function returns element_0 on the top of the stack and the other elements below it. E.g.: _ elem_2 elem_1 elem_0
        let mut ret: Vec<String> = vec![];
        let size = self.0.get_size();
        for i in 0..size {
            ret.push(format!("element_{}", size - 1 - i));
        }

        ret
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        self.0.get_size() as isize
    }

    fn function_body(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let read_an_element = "divine\n".repeat(self.0.get_size());

        format!(
            "
            {entrypoint}:
                {read_an_element}
                return
        "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec!["secret input too short".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        let secret_in: Vec<BFieldElement> = random_elements(self.0.get_size());
        vec![ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            secret_in,
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
    ) where
        Self: Sized,
    {
        for elem in secret_in.iter().take(self.0.get_size()) {
            stack.push(*elem)
        }
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut secret_in = vec![];
        secret_in.append(&mut random_elements(self.0.get_size()));
        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in: vec![],
            secret_in,
            memory: HashMap::default(),
            words_allocated: 0,
        }
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        self.common_case_input_state()
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_snippet_test() {
        for _ in 0..10 {
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::Bool));
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::U32));
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::U64));
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::BFE));
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::XFE));
            rust_tasm_equivalence_prop_new(ReadSecret(DataType::Digest));
        }
    }

    #[test]
    fn read_secret_benchmark() {
        bench_and_write(ReadSecret(DataType::Digest));
    }
}
