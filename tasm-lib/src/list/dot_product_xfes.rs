use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;

pub struct DotProductXfes;

impl BasicSnippet for DotProductXfes {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::Xfe)), "lhs".to_owned()),
            (DataType::List(Box::new(DataType::Xfe)), "rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "dot_product".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_list_dot_product_xfes".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        todo!()
    }
}
