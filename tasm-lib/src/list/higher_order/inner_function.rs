use std::collections::HashMap;
use triton_vm::isa::instruction::AnInstruction;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::deprecated_snippet::DeprecatedSnippet;

const MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION: &str = "higher-order functions \
currently only work with *one* input element in inner function. \
Use a tuple data type to circumvent this.";

/// A data structure for describing an inner function predicate to filter with,
/// or to map with.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
        let is_label = |x: &_| matches!(x, LabelledInstruction::Label(_));
        let is_instruction = |x: &_| matches!(x, LabelledInstruction::Instruction(_));
        let first_label_or_instruction = self
            .function
            .iter()
            .find(|&x| is_label(x) || is_instruction(x));
        let Some(labelled_instruction) = first_label_or_instruction else {
            panic!("Inner function must start with a label. Got neither labels nor instructions.")
        };
        match labelled_instruction {
            LabelledInstruction::Instruction(instruction) => {
                panic!("Inner function must start with a label. Got: {instruction}")
            }
            LabelledInstruction::Label(label) => label.to_owned(),
            LabelledInstruction::Breakpoint => unreachable!(),
            LabelledInstruction::TypeHint(_) => unreachable!(),
        }
    }

    /// Returns `Some(code)` iff the raw code is a function that can be inlined
    pub fn inlined_body(&self) -> Option<Vec<LabelledInstruction>> {
        // If the RawCode contains a recurse, it cannot be inlined
        if self
            .function
            .iter()
            .any(|x| matches!(x, LabelledInstruction::Instruction(AnInstruction::Recurse)))
        {
            None
        } else if self.function.len() == 2 {
            Some(triton_asm!())
        } else {
            // Remove label and `return`
            Some(self.function[1..=self.function.len() - 2].to_vec())
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct NoFunctionBody {
    pub label_name: String,
    pub input_type: DataType,
    pub output_type: DataType,
}

impl InnerFunction {
    pub fn domain(&self) -> DataType {
        match self {
            InnerFunction::RawCode(raw) => raw.input_type.clone(),
            InnerFunction::DeprecatedSnippet(f) => {
                let [ref input] = f.input_types()[..] else {
                    panic!("{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}");
                };
                input.clone()
            }
            InnerFunction::NoFunctionBody(f) => f.input_type.clone(),
            InnerFunction::BasicSnippet(bs) => {
                let [(ref input, _)] = bs.inputs()[..] else {
                    panic!("{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}");
                };
                input.clone()
            }
        }
    }

    pub fn range(&self) -> DataType {
        match self {
            InnerFunction::RawCode(rc) => rc.output_type.clone(),
            InnerFunction::DeprecatedSnippet(sn) => {
                let [ref output] = sn.output_types()[..] else {
                    panic!("{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}");
                };
                output.clone()
            }
            InnerFunction::NoFunctionBody(lnat) => lnat.output_type.clone(),
            InnerFunction::BasicSnippet(bs) => {
                let [(ref output, _)] = bs.outputs()[..] else {
                    panic!("{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}");
                };
                output.clone()
            }
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
    /// stack. This is a helper function for [`apply`](Self::apply), which in some cases
    /// just grabs the inner function's code and then needs a VM to apply it.
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
        vmstate.op_stack.stack.clone_from(stack);
        vmstate.ram.clone_from(memory);
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
            InnerFunction::RawCode(rc) => Self::run_vm(&rc.function, stack, memory),
            InnerFunction::DeprecatedSnippet(sn) => {
                sn.rust_shadowing(stack, vec![], vec![], &mut memory.clone());
            }
            InnerFunction::NoFunctionBody(_lnat) => {
                panic!("Cannot apply inner function without function body")
            }
            InnerFunction::BasicSnippet(bs) => {
                let mut snippet_state = Library::new();
                let entrypoint = bs.entrypoint();
                let function_body = bs.annotated_code(&mut snippet_state);
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
