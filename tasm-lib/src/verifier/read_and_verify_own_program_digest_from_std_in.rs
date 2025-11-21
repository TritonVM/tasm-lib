use triton_vm::prelude::*;

use crate::prelude::*;

/// Crash the VM if std-in does not agree with own program digest.
///
/// Crash the VM if std in does not agree with own digest
/// Must be called as the first function in the program, as
/// it assumes that the bottom of the stack (stack[15..=11])
/// contains the digest of the running program
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ReadAndVerifyOwnProgramDigestFromStdIn;

impl BasicSnippet for ReadAndVerifyOwnProgramDigestFromStdIn {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "own_program_digest".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_read_and_verify_own_program_digest_from_std_in".to_string()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
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
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute_with_terminal_state;

    #[derive(Debug, Clone, Eq, PartialEq)]
    struct ProgramSetup {
        program: Program,
        program_digest: Digest,
        init_stack: Vec<BFieldElement>,
    }

    fn test_program() -> ProgramSetup {
        let snippet = ReadAndVerifyOwnProgramDigestFromStdIn;
        let snippet_code = snippet.annotated_code(&mut Library::empty());
        let code_for_test = triton_asm!(
            call {snippet.entrypoint()}
            halt
            {&snippet_code}
        );

        let program = Program::new(&code_for_test);
        let program_digest = program.hash();
        let init_stack = snippet.init_stack_for_isolated_run();

        ProgramSetup {
            program,
            program_digest,
            init_stack,
        }
    }

    #[test]
    fn positive_test() {
        let test_setup = test_program();

        let std_in = test_setup.program_digest.reversed().values();
        let vm_end_state = execute_with_terminal_state(
            test_setup.program,
            &std_in,
            &test_setup.init_stack,
            &NonDeterminism::default(),
            None,
        )
        .unwrap();

        let expected_stack = [
            test_setup.init_stack.clone(),
            test_setup.init_stack[..Digest::LEN].to_vec(),
        ]
        .concat();
        assert_eq!(expected_stack, vm_end_state.op_stack.stack);
        assert!(vm_end_state.jump_stack.is_empty());
    }

    #[test]
    fn negative_test_expect_assertion_fail_due_to_bad_std_in() {
        let test_setup = test_program();

        let bad_std_in = Digest::default().encode();
        let execution_result = execute_with_terminal_state(
            test_setup.program,
            &bad_std_in,
            &test_setup.init_stack,
            &NonDeterminism::default(),
            None,
        );
        let err = execution_result.unwrap_err();
        assert!(matches!(err, InstructionError::VectorAssertionFailed(0, _)));
    }
}
