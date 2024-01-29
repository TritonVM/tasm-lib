use triton_vm::prelude::*;

use crate::traits::basic_snippet::BasicSnippet;

/// Crash the VM if std in does not agree with own digest
/// Must be called as the first function in the program, as
/// it assumes that the bottom of the stack (stack[15..=11])
/// contains the digest of the running program
pub struct AssertStdInStartsWithOwnProgramDigest;

impl BasicSnippet for AssertStdInStartsWithOwnProgramDigest {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_assert_stdin_starts_with_own_program_digest".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                dup 15
                dup 15
                dup 15
                dup 15
                dup 15
                read_io 5
                assert_vector
                pop 5
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num_traits::Zero;
    use triton_vm::op_stack::NUM_OP_STACK_REGISTERS;
    use triton_vm::twenty_first::shared_math::tip5::DIGEST_LENGTH;

    use super::*;
    use crate::execute_with_terminal_state;
    use crate::library::Library;
    use crate::VmHasher;

    struct ProgramSetup {
        program: Program,
        program_digest: Digest,
        init_stack: Vec<BFieldElement>,
    }

    fn test_program() -> ProgramSetup {
        let snippet = AssertStdInStartsWithOwnProgramDigest;
        let snippet_code = snippet.code(&mut Library::empty());
        let code_for_test = triton_asm!(
            call {snippet.entrypoint()}
            halt
            {&snippet_code}
        );

        let program = Program::new(&code_for_test);
        let program_digest = program.hash::<VmHasher>();
        let init_stack = [
            program_digest.values().to_vec(),
            vec![BFieldElement::zero(); NUM_OP_STACK_REGISTERS - DIGEST_LENGTH],
        ]
        .concat();

        ProgramSetup {
            program,
            program_digest,
            init_stack,
        }
    }

    #[test]
    fn positive_test() {
        let test_setup = test_program();

        let std_in = test_setup.program_digest.encode();
        let vm_end_state = execute_with_terminal_state(
            &test_setup.program,
            &std_in,
            &test_setup.init_stack,
            &NonDeterminism::default(),
            None,
        )
        .unwrap();

        assert_eq!(test_setup.init_stack, vm_end_state.op_stack.stack);
        assert!(vm_end_state.jump_stack.is_empty());
    }

    #[test]
    fn expect_assertion_fail_due_to_bad_std_in() {
        let test_setup = test_program();

        let bad_std_in = Digest::default().encode();
        let execution_result = execute_with_terminal_state(
            &test_setup.program,
            &bad_std_in,
            &test_setup.init_stack,
            &NonDeterminism::default(),
            None,
        );
        let err = execution_result.unwrap_err();
        assert_eq!(InstructionError::VectorAssertionFailed(0), err);
    }
}
