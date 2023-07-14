use std::{cell::RefCell, collections::HashMap};

use triton_vm::{instruction::LabelledInstruction, BFieldElement};

use crate::snippet::{DataType, Snippet};

/// A data structure for describing an inner function predicate to filter with,
/// or to map with.
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
                LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Return)
                    | LabelledInstruction::Instruction(
                        triton_vm::instruction::AnInstruction::Recurse
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
                LabelledInstruction::Instruction(triton_vm::instruction::AnInstruction::Return)
                    | LabelledInstruction::Instruction(
                        triton_vm::instruction::AnInstruction::Recurse
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
    /// Return the entrypoint, label, of the inner function. Used to make a call to this function.
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

    // Used when a snippet is declared somewhere else, and it's not the responsibility of
    // the higher order function to import it.
    NoFunctionBody(NoFunctionBody),
}

pub struct NoFunctionBody {
    pub label_name: String,
    pub input_types: Vec<DataType>,
    pub output_types: Vec<DataType>,
}

impl InnerFunction {
    /// Return the input types this inner function accepts
    pub fn get_input_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(raw) => raw.input_types.clone(),
            InnerFunction::Snippet(f) => f.input_types(),
            InnerFunction::NoFunctionBody(f) => f.input_types.clone(),
        }
    }

    /// Return the expected type of list element this function accepts
    pub fn input_list_element_type(&self) -> DataType {
        self.get_input_types().last().unwrap().to_owned()
    }

    /// Return all input types apart from the element type of the input list.
    /// May be the empty list.
    pub fn additional_inputs(&self) -> Vec<DataType> {
        let mut input_types = self.get_input_types();
        input_types.pop().unwrap();

        input_types
    }

    /// Return the size in words for the additional elements, all elements
    /// apart from the element from the input list.
    pub fn size_of_additional_inputs(&self) -> usize {
        self.additional_inputs().iter().map(|x| x.get_size()).sum()
    }

    /// Return types this function outputs.
    pub fn get_output_types(&self) -> Vec<DataType> {
        match self {
            InnerFunction::RawCode(rc) => rc.output_types.clone(),
            InnerFunction::Snippet(sn) => sn.output_types(),
            InnerFunction::NoFunctionBody(lnat) => lnat.output_types.clone(),
        }
    }

    /// Return the entrypoint, label, of the inner function. Used to make a call to this function.
    pub fn entrypoint(&self) -> String {
        match self {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::Snippet(sn) => sn.entrypoint(),
            InnerFunction::NoFunctionBody(sn) => sn.label_name.to_owned(),
        }
    }

    /// For testing purposes, this function can mirror what the TASM code does.
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
            InnerFunction::NoFunctionBody(_lnat) => {
                panic!("Cannot rust shadow inner function without function body")
            }
        };
    }
}
