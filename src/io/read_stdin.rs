use std::collections::HashMap;

use twenty_first::shared_math::{b_field_element::BFieldElement, other::random_elements};

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
};

#[derive(Clone, Debug)]
pub struct ReadStdIn(pub DataType);

impl Snippet for ReadStdIn {
    fn entrypoint(&self) -> String {
        format!("tasm_io_read_stdin_{}", self.0)
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

    fn function_code(&self, _library: &mut crate::snippet_state::SnippetState) -> String {
        let entrypoint = self.entrypoint();
        let read_an_element = "read_io\n".repeat(self.0.get_size());

        format!(
            "
            {entrypoint}:
                {read_an_element}
                return
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["std input too short".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState>
    where
        Self: Sized,
    {
        let std_in: Vec<BFieldElement> = random_elements(self.0.get_size());
        vec![ExecutionState {
            stack: get_init_tvm_stack(),
            std_in,
            secret_in: vec![],
            memory: HashMap::default(),
            words_allocated: 0,
        }]
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        std_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _secret_in: Vec<twenty_first::shared_math::b_field_element::BFieldElement>,
        _memory: &mut std::collections::HashMap<
            twenty_first::shared_math::b_field_element::BFieldElement,
            twenty_first::shared_math::b_field_element::BFieldElement,
        >,
    ) where
        Self: Sized,
    {
        for elem in std_in.iter().take(self.0.get_size()) {
            stack.push(*elem)
        }
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        let mut std_in = vec![];
        std_in.append(&mut random_elements(self.0.get_size()));
        ExecutionState {
            stack: get_init_tvm_stack(),
            std_in,
            secret_in: vec![],
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
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::Bool), true);
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::U32), true);
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::U64), true);
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::BFE), true);
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::XFE), true);
            rust_tasm_equivalence_prop_new(&ReadStdIn(DataType::Digest), true);
        }
    }

    #[test]
    fn read_stdin_benchmark() {
        bench_and_write(ReadStdIn(DataType::Digest));
    }
}
