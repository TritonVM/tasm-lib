use num_traits::Zero;
use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::VmHasher;

pub trait BasicSnippet {
    fn inputs(&self) -> Vec<(DataType, String)>;
    fn outputs(&self) -> Vec<(DataType, String)>;
    fn entrypoint(&self) -> String;
    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction>;

    fn annotated_code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let code = self.code(library);
        let Some((entrypoint, snippet_body)) = code.split_first() else {
            return code;
        };
        if entrypoint.to_string() != self.entrypoint() {
            return code;
        }

        let mut input_hints = vec![];
        let mut stack_depth = 0;
        for (data_type, name) in self.inputs().into_iter().rev() {
            let stack_size = data_type.stack_size();
            let data_name = data_type.label_friendly_name();
            input_hints.push(format!(
                "hint {name}: {data_name} = stack[{stack_depth}..{}]",
                stack_depth + stack_size
            ));
            stack_depth += stack_size;
        }

        triton_asm! {
            {entrypoint}:
                {&input_hints}
                {&snippet_body}
        }
    }

    fn link_for_isolated_run(&self) -> Vec<LabelledInstruction> {
        let mut library = Library::empty();
        let entrypoint = self.entrypoint();
        let function_body = self.annotated_code(&mut library);
        let library_code = library.all_imports();

        // The TASM code is always run through a function call, so the 1st instruction is a call to the
        // function in question.
        let code = triton_asm!(
            call {entrypoint}
            halt

            {&function_body}
            {&library_code}
        );

        code
    }

    fn init_stack_for_isolated_run(&self) -> Vec<BFieldElement> {
        let code = self.link_for_isolated_run();
        let program = Program::new(&code);
        let program_digest = program.hash::<VmHasher>();
        [
            program_digest.reversed().values().to_vec(),
            vec![BFieldElement::zero(); NUM_OP_STACK_REGISTERS - tip5::DIGEST_LENGTH],
        ]
        .concat()
    }

    fn stack_diff(&self) -> isize {
        let mut diff = 0isize;
        for (dt, _name) in self.inputs() {
            diff -= dt.stack_size() as isize;
        }
        for (dt, _name) in self.outputs() {
            diff += dt.stack_size() as isize;
        }
        diff
    }
}

#[cfg(test)]
mod tests {
    use triton_vm::{program::Program, triton_asm};

    use crate::traits::basic_snippet::BasicSnippet;

    struct DummySnippet;

    impl BasicSnippet for DummySnippet {
        fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
            vec![]
        }

        fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
            vec![]
        }

        fn entrypoint(&self) -> String {
            "____dummy_snippet_test".to_string()
        }

        fn code(
            &self,
            _library: &mut crate::library::Library,
        ) -> Vec<triton_vm::prelude::LabelledInstruction> {
            triton_asm!(
                {self.entrypoint()}:
                    push 14
                    push 14
                    pop 2
                    return
            )
        }
    }

    #[test]
    fn init_stack_agrees_with_tvm() {
        // Verify that our assumptions about the initial stack at program start
        // agrees with Triton VM.
        let calculated_init_stack = DummySnippet.init_stack_for_isolated_run();
        let program = DummySnippet.link_for_isolated_run();
        let program = Program::new(&program);
        let init_vm_state =
            triton_vm::vm::VMState::new(&program, Default::default(), Default::default());

        assert_eq!(init_vm_state.op_stack.stack, calculated_init_stack);
    }
}
