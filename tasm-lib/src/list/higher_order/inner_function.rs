use std::collections::HashMap;

use triton_vm::isa::instruction::AnInstruction;
use triton_vm::prelude::*;

use crate::prelude::*;

const MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION: &str = "higher-order functions \
currently only work with *one* input element in inner function. \
Use a tuple data type to circumvent this.";

/// A data structure for describing an inner function predicate to filter with,
/// or a function to map with.
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
        let is_label = |x: &_| matches!(x, LabelledInstruction::Label(_));
        let is_instruction = |x: &_| matches!(x, LabelledInstruction::Instruction(_));
        let labels_and_instructions = function.iter().filter(|i| is_label(i) || is_instruction(i));

        // Verify that 1st line is a label
        assert!(
            labels_and_instructions.count() >= 2,
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
                    | LabelledInstruction::Instruction(AnInstruction::RecurseOrReturn)
            ),
            "Last line of inner function must be either return, recurse, or recurse_or_return. Got: {}",
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
        let LabelledInstruction::Label(label) = labelled_instruction else {
            panic!("Inner function must start with a label. Got: {labelled_instruction}");
        };

        label.to_string()
    }

    /// Returns `Some(code)` iff the raw code is a function that can be inlined
    ///
    /// Type hints and breakpoints are stripped.
    pub fn inlined_body(&self) -> Option<Vec<LabelledInstruction>> {
        let is_label = |x: &_| matches!(x, LabelledInstruction::Label(_));
        let is_instruction = |x: &_| matches!(x, LabelledInstruction::Instruction(_));
        let is_recursive = |x: &_| {
            matches!(
                x,
                LabelledInstruction::Instruction(AnInstruction::Recurse)
                    | LabelledInstruction::Instruction(AnInstruction::RecurseOrReturn)
            )
        };

        if self.function.iter().any(is_recursive) {
            // recursion needs to be wrapped in a function
            return None;
        }

        let mut labels_and_instructions = self
            .function
            .iter()
            .filter(|i| is_label(i) || is_instruction(i));

        let Some(first_thing) = labels_and_instructions.next() else {
            return Some(triton_asm!());
        };
        let LabelledInstruction::Label(_) = first_thing else {
            panic!("Raw Code must start with a label.")
        };

        let Some(LabelledInstruction::Instruction(AnInstruction::Return)) =
            labels_and_instructions.next_back()
        else {
            panic!("Raw Code is probably buggy: too short, or doesn't end with `return`.");
        };

        Some(labels_and_instructions.cloned().collect())
    }
}

pub enum InnerFunction {
    RawCode(RawCode),
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
            InnerFunction::NoFunctionBody(f) => f.input_type.clone(),
            InnerFunction::BasicSnippet(bs) => {
                let [(ref input, _)] = bs.parameters()[..] else {
                    panic!("{MORE_THAN_ONE_INPUT_OR_OUTPUT_TYPE_IN_INNER_FUNCTION}");
                };
                input.clone()
            }
        }
    }

    pub fn range(&self) -> DataType {
        match self {
            InnerFunction::RawCode(rc) => rc.output_type.clone(),
            InnerFunction::NoFunctionBody(lnat) => lnat.output_type.clone(),
            InnerFunction::BasicSnippet(bs) => {
                let [(ref output, _)] = bs.return_values()[..] else {
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
            InnerFunction::NoFunctionBody(sn) => sn.label_name.to_owned(),
            InnerFunction::BasicSnippet(bs) => bs.entrypoint(),
        }
    }

    /// Computes the inner function and applies the resulting change to the given stack
    pub fn apply(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &HashMap<BFieldElement, BFieldElement>,
    ) {
        match &self {
            InnerFunction::RawCode(rc) => Self::run_vm(&rc.function, stack, memory),
            InnerFunction::NoFunctionBody(_lnat) => {
                panic!("Cannot apply inner function without function body")
            }
            InnerFunction::BasicSnippet(bs) => {
                let mut library = Library::new();
                let function = bs.annotated_code(&mut library);
                let imports = library.all_imports();
                let code = triton_asm!(
                    {&function}
                    {&imports}
                );

                Self::run_vm(&code, stack, memory);
            }
        };
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
        let mut vmstate = VMState::new(program, PublicInput::default(), NonDeterminism::default());
        vmstate.op_stack.stack.clone_from(stack);
        vmstate.ram.clone_from(memory);
        vmstate.run().unwrap();
        *stack = vmstate.op_stack.stack;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn breakpoint_does_not_influence_raw_code_inlining() {
        let raw_code = RawCode {
            function: triton_asm! { my_label: return break },
            input_type: DataType::VoidPointer,
            output_type: DataType::VoidPointer,
        };
        let inlined_code = raw_code.inlined_body().unwrap();
        assert_eq!(triton_asm!(), inlined_code);
    }

    #[test]
    fn type_hints_do_not_influence_raw_code_inlining() {
        let raw_code = RawCode {
            function: triton_asm! { my_label: hint a = stack[0] hint b = stack[1] return },
            input_type: DataType::VoidPointer,
            output_type: DataType::VoidPointer,
        };
        let inlined_code = raw_code.inlined_body().unwrap();
        assert_eq!(triton_asm!(), inlined_code);
    }

    #[test]
    fn allow_raw_code_with_recurse_or_return_instruction() {
        let raw_code = triton_asm!(
            please_help_me:
                hint im_falling = stack[0]
                hint in_love_with_you = stack[1]

                call close_the_door_to_temptation

                return

                close_the_door_to_temptation:
                    hint turn_away_from_me_darling = stack[5]
                    break
                    merkle_step_mem
                    recurse_or_return
        );
        let raw_code = RawCode::new(raw_code, DataType::VoidPointer, DataType::VoidPointer);
        assert!(
            raw_code.inlined_body().is_none(),
            "Disallow inling of code with `recurse_or_return` instruction"
        );
    }
}
