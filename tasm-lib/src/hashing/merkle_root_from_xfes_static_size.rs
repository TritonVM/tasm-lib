use itertools::Itertools;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::prelude::*;
use twenty_first::prelude::*;
use x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::data_type::DataType;
use crate::hashing::merkle_root_static_size::MerkleRootStaticSize;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;
use crate::DIGEST_LENGTH;

pub struct MerkleRootFromXfesStaticSize {
    /// Aka `height` of the Merkle tree
    log2_length: u8,
}

impl MerkleRootFromXfesStaticSize {
    fn num_leaves(&self) -> usize {
        1 << self.log2_length
    }
}

impl BasicSnippet for MerkleRootFromXfesStaticSize {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::Array(Box::new(ArrayType {
                element_type: DataType::Xfe,
                length: self.num_leaves(),
            })),
            "*xfes".to_owned(),
        )]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_hashing_merkle_root_from_xfes_length_{}",
            self.num_leaves()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        assert!(self.log2_length >= 1, "need at least 1 actual layer");
        let entrypoint = self.entrypoint();

        // Allocate static memory for whole Merkle tree, except for the leaves
        let pointer = library.kmalloc((self.num_leaves() * EXTENSION_DEGREE) as u32);

        let merke_root_snippet = MerkleRootStaticSize {
            log2_length: self.log2_length - 1,
            nodes_pointer: pointer,
        };
        let merkle_root_snippet_label = library.import(Box::new(merke_root_snippet));

        let xfe_to_digest_on_stack = triton_asm!(
            push 0
            push 0
            swap 2
            read_mem {EXTENSION_DEGREE}
        );
        let all_xfes_as_digests_on_stack = vec![xfe_to_digest_on_stack; self.num_leaves()].concat();
        let xfes_pointer_last_word_offset = self.num_leaves() * EXTENSION_DEGREE - 1;

        let hash_one_leaf_and_write_to_ram = |node_index: u32| {
            triton_asm!(
                hash
                push {pointer + bfe!(node_index) * bfe!(DIGEST_LENGTH as u32)}
                write_mem {DIGEST_LENGTH}
                pop 1
            )
        };
        let hash_leaves_and_write_first_layer = (0..self.num_leaves())
            .map(|leaf_index| (leaf_index + self.num_leaves()) as u32)
            .map(hash_one_leaf_and_write_to_ram)
            .collect_vec()
            .concat();

        triton_asm!(
            {entrypoint}:
                // _ *xfes

                push {xfes_pointer_last_word_offset}
                add
                // _ *xfes_last_elem_last_word

                {&all_xfes_as_digests_on_stack}
                pop 1
                // _ [[leaf]; self.num_leaves()]

                {&hash_leaves_and_write_first_layer}
                // _

                call {merkle_root_snippet_label}
                return
        )
    }
}
