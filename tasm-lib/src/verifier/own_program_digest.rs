use triton_vm::prelude::*;

use crate::prelude::*;

/// Return own program digest. Must be called with a clean stack.
///
/// Move own program digest to the top of the stack
/// Must be called as the first function in the program, as
/// it assumes that the bottom of the stack (stack[15..=11])
/// contains the digest of the running program
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct OwnProgramDigest;

impl BasicSnippet for OwnProgramDigest {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "own_program_digest".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_own_program_digest".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                dup 15
                dup 15
                dup 15
                dup 15
                dup 15

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
        let snippet = OwnProgramDigest;
        let program = Program::new(&snippet.link_for_isolated_run());
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

        let vm_end_state = execute_with_terminal_state(
            test_setup.program,
            &[],
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
}
