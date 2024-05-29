use crate::{data_type::DataType, traits::basic_snippet::BasicSnippet};

/// Compute the Merkle root from a list of XFEs where each XFE is converted to
/// a digest by appending two zero-words to it
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MerkleRootFromXfes;

impl BasicSnippet for MerkleRootFromXfes {
    fn inputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::Xfe)),
            "*xfe_leafs".to_string(),
        )]
    }

    fn outputs(&self) -> Vec<(crate::data_type::DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_root_from_xfes".to_owned()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::prelude::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        // Strategy: Let's use `recurse_or_return` for the loop end-conditions
        // First loop iteration, we have to append some zeros to the XFE list
        // Later iterations can *potentially* use `sponge_absorb_from_mem`

        // Let's say we already have a list of Digests. How do we calculate the
        // end condition?

        todo!()
    }
}
