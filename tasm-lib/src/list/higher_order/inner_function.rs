use std::collections::HashMap;
use triton_vm::instruction::AnInstruction;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::triton_asm;
use triton_vm::vm::VMState;
use triton_vm::BFieldElement;
use triton_vm::NonDeterminism;
use triton_vm::Program;
use triton_vm::PublicInput;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

/// A data structure for describing an inner function predicate to filter with,
/// or to map with.
pub struct RawCode {
    pub function: Vec<LabelledInstruction>,
    pub input_type: DataType,
    pub output_type: DataType,
}

impl RawCode {
    pub fn new(
        function: Vec<LabelledInstruction>,
        input_type: DataType,
        output_type: DataType,
    ) -> Self {
        // Verify that 1st line is a label
        assert!(
            function.len() >= 2,
            "Inner function must have at least two lines: a label and a return or recurse"
        );
        assert!(
            matches!(function[0], LabelledInstruction::Label(_)),
            "First line of inner function must be label. Got: {}",
            function[0]
        );
        assert!(
            matches!(
                function.last().unwrap(),
                LabelledInstruction::Instruction(AnInstruction::Return)
                    | LabelledInstruction::Instruction(AnInstruction::Recurse)
            ),
            "Last line of inner function must be either return or recurse. Got: {}",
            function.last().unwrap()
        );

        Self {
            function,
            input_type,
            output_type,
        }
    }
}

impl RawCode {
    /// Return the entrypoint, label, of the inner function. Used to make a call to this function.
    pub fn entrypoint(&self) -> String {
        let is_breakpoint = |x: &_| matches!(x, LabelledInstruction::Breakpoint);
        let first_non_breakpoint = self.function.iter().find(|&x| !is_breakpoint(x));
        let Some(labelled_instruction) = first_non_breakpoint else {
            panic!("Inner function must start with a label. Got only breakpoints.")
        };
        match labelled_instruction {
            LabelledInstruction::Instruction(instruction) => {
                panic!("Inner function must start with a label. Got: {instruction}")
            }
            LabelledInstruction::Label(label) => label.to_owned(),
            LabelledInstruction::Breakpoint => unreachable!(),
        }
    }
}

pub enum InnerFunction {
    RawCode(RawCode),
    DeprecatedSnippet(Box<dyn DeprecatedSnippet>),
    BasicSnippet(Box<dyn BasicSnippet>),

    // Used when a snippet is declared somewhere else, and it's not the responsibility of
    // the higher order function to import it.
    NoFunctionBody(NoFunctionBody),
}

pub struct NoFunctionBody {
    pub label_name: String,
    pub input_type: DataType,
    pub output_type: DataType,
}

impl InnerFunction {
    pub fn domain(&self) -> DataType {
        match self {
            InnerFunction::RawCode(raw) => raw.input_type.clone(),
            InnerFunction::DeprecatedSnippet(f) => f.input_types()[0].clone(),
            InnerFunction::NoFunctionBody(f) => f.input_type.clone(),
            InnerFunction::BasicSnippet(bs) => bs.inputs()[0].0.clone(),
        }
    }

    pub fn range(&self) -> DataType {
        match self {
            InnerFunction::RawCode(rc) => rc.output_type.clone(),
            InnerFunction::DeprecatedSnippet(sn) => sn.output_types()[0].clone(),
            InnerFunction::NoFunctionBody(lnat) => lnat.output_type.clone(),
            InnerFunction::BasicSnippet(bs) => bs.outputs()[0].0.clone(),
        }
    }

    /// Return the entrypoint, label, of the inner function. Used to make a call to this function.
    pub fn entrypoint(&self) -> String {
        match self {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::DeprecatedSnippet(sn) => sn.entrypoint_name(),
            InnerFunction::NoFunctionBody(sn) => sn.label_name.to_owned(),
            InnerFunction::BasicSnippet(bs) => bs.entrypoint(),
        }
    }

    /// Run the VM for on a given stack and memory to observe how it manipulates the
    /// stack. This is a helper function for [`apply`](apply), which in some cases just
    /// grabs the inner function's code and then needs a VM to apply it.
    fn run_vm(
        instructions: &[LabelledInstruction],
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
    ) {
        let Some(LabelledInstruction::Label(label)) = instructions.first() else {
            panic!();
        };
        let instructions = triton_asm!(
            call {label}
            halt
            {&instructions}
        );
        let program = Program::new(&instructions);
        let mut vmstate = VMState::new(&program, PublicInput::default(), NonDeterminism::default());
        vmstate.op_stack.stack = stack.clone();
        vmstate.ram = memory.clone();
        vmstate.run().unwrap();
        *stack = vmstate.op_stack.stack;
    }

    /// Computes the inner function and applies the resulting change to the given stack
    pub fn apply(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
    ) {
        match &self {
            InnerFunction::RawCode(rc) => {
                let instructions = rc.function.clone();
                Self::run_vm(&instructions, stack, memory);
            }
            InnerFunction::DeprecatedSnippet(sn) => {
                sn.rust_shadowing(stack, vec![], vec![], &mut memory.clone());
            }
            InnerFunction::NoFunctionBody(_lnat) => {
                panic!("Cannot apply inner function without function body")
            }
            InnerFunction::BasicSnippet(bs) => {
                let mut snippet_state: Library = Library::with_preallocated_memory(1);
                let entrypoint = bs.entrypoint();
                let function_body = bs.code(&mut snippet_state);
                let library_code = snippet_state.all_imports();

                // The TASM code is always run through a function call, so the 1st instruction
                // is a call to the function in question.
                let code = triton_asm!(
                    call {entrypoint}
                    halt

                    {&function_body}
                    {&library_code}
                );

                Self::run_vm(&code, stack, memory);
            }
        };
    }
}
