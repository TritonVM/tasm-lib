use std::{cell::RefCell, collections::HashMap};

use triton_opcodes::instruction::LabelledInstruction;
use triton_vm::BFieldElement;

use crate::snippet::{DataType, Snippet};

/// A data structure for describing an inner function predicate to filter with
pub struct RawCode {
    pub function: Vec<LabelledInstruction>,
    pub input_types: Vec<DataType>,
    pub output_types: Vec<DataType>,
    #[allow(clippy::type_complexity)]
    rust_shadowing: Option<Box<RefCell<dyn FnMut(&mut Vec<BFieldElement>)>>>,
}

impl RawCode {
    pub fn new(
        function: Vec<LabelledInstruction>,
        input_types: Vec<DataType>,
        output_types: Vec<DataType>,
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
                LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Return
                ) | LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Recurse
                )
            ),
            "Last line of inner function must be either return or recurse. Got: {}",
            function.last().unwrap()
        );

        Self {
            function,
            input_types,
            output_types,
            rust_shadowing: None,
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn new_with_shadowing(
        function: Vec<LabelledInstruction>,
        input_types: Vec<DataType>,
        output_types: Vec<DataType>,
        rust_shadowing: Box<RefCell<dyn FnMut(&mut Vec<BFieldElement>)>>,
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
                LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Return
                ) | LabelledInstruction::Instruction(
                    triton_opcodes::instruction::AnInstruction::Recurse
                )
            ),
            "Last line of inner function must be either return or recurse. Got: {}",
            function.last().unwrap()
        );

        Self {
            function,
            input_types,
            output_types,
            rust_shadowing: Some(rust_shadowing),
        }
    }
}

impl RawCode {
    pub fn entrypoint(&self) -> String {
        match &self.function[0] {
            LabelledInstruction::Instruction(inst) => {
                panic!("First line of inner function must be a label. Got: {inst}")
            }
            LabelledInstruction::Label(label) => label.to_owned(),
        }
    }
}

pub enum InnerFunction {
    RawCode(RawCode),
    Snippet(Box<dyn Snippet>),
}

impl InnerFunction {
    pub fn get_input_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(raw) => raw.input_types.clone(),
            InnerFunction::Snippet(f) => f.input_types(),
        }
    }

    pub fn get_output_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(rc) => rc.output_types.clone(),
            InnerFunction::Snippet(sn) => sn.output_types(),
        }
    }

    pub fn entrypoint(&self) -> String {
        match self {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => sn.entrypoint(),
        }
    }

    pub fn rust_shadowing(
        &self,
        std_in: &[BFieldElement],
        secret_in: &[BFieldElement],
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        match &self {
            InnerFunction::RawCode(rc) => {
                if let Some(func) = &rc.rust_shadowing {
                    let mut func = func.borrow_mut();
                    (*func)(stack)
                } else {
                    panic!("Raw code must have rust shadowing for equivalence testing")
                }
            }
            InnerFunction::Snippet(sn) => {
                sn.rust_shadowing(stack, std_in.to_vec(), secret_in.to_vec(), memory)
            }
        };
    }
}
