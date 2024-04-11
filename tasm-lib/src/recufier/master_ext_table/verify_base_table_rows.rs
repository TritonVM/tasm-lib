use crate::data_type::DataType;
use crate::hashing::algebraic_hasher::hash_static_size::HashStaticSize;
use crate::hashing::merkle_verify::MerkleVerify;
use crate::library::Library;
use crate::recufier::fri::verify::FriSnippet;
use crate::traits::basic_snippet::BasicSnippet;
use strum::Display;
use triton_vm::prelude::*;
use triton_vm::table::{NUM_BASE_COLUMNS, NUM_EXT_COLUMNS, NUM_QUOTIENT_SEGMENTS};
use triton_vm::twenty_first::shared_math::tip5::DIGEST_LENGTH;
use triton_vm::twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

#[derive(Debug, Copy, Clone, Display)]
pub enum ColumnType {
    Base,
    Extension,
    Quotient,
}

/// Crashes the VM is the base table rows do not authenticate against the provided Merkle root
/// First hashes the rows, then verifies that the digests belong in the Merkle tree.
pub struct VerifyBaseTableRows {
    pub column_type: ColumnType,
}

impl BasicSnippet for VerifyBaseTableRows {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "num_combination_codeword_checks".to_owned()),
            (DataType::U32, "merkle_tree_height".to_owned()),
            (DataType::VoidPointer, "*merkle_tree_root".to_owned()),
            (
                FriSnippet::indexed_leaves_list_type(),
                "*fri_revealed".to_owned(),
            ),
            // type of {base|ext|quot} table rows i
            // `Vec<[{BaseFieldElement, XFieldElement, XFieldElement}: COLUMN_COUNT]>` but encoded
            // in memory as a flat structure. So I'm not sure what type to use here. Anyway, it's
            // certainly a list.
            (DataType::VoidPointer, "*table_rows".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!("verify_{}_table_rows", self.column_type)
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let hash_static_size = match self.column_type {
            ColumnType::Base => NUM_BASE_COLUMNS,
            ColumnType::Extension => NUM_EXT_COLUMNS * EXTENSION_DEGREE,
            ColumnType::Quotient => NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE,
        };
        let hash_row = library.import(Box::new(HashStaticSize {
            size: hash_static_size,
        }));
        let merkle_root_verify = library.import(Box::new(MerkleVerify));

        let loop_label = format!("{entrypoint}_loop");
        let loop_code = triton_asm!(
            // Invariant: _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index *row_elem
            {loop_label}:
                // Check end-loop condition
                dup 4
                push 0
                eq
                skiz
                    return

                // 1. dup up *merkle_tree_root, and get digest
                // 2.
                //   a. Read revealed FRI leaf index
                //   b. Update revealed FRI leaf index to next value
                // 3. get digest through call to `hash_base_row`, and update pointer value to next item
                // 4. dup Merkle tree height to the top.

                /* 1. */
                dup 2
                push {DIGEST_LENGTH - 1}
                add
                read_mem {DIGEST_LENGTH}
                pop 1
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root]

                /* 2. */
                dup 6
                read_mem 1
                push {EXTENSION_DEGREE + 2}
                add
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root] fri_revealed_leaf_index (*fri_revealed_leaf_index + 4)
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root] fri_revealed_leaf_index *fri_revealed_leaf_index_next

                swap 8
                pop 1
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem [mt_root] fri_revealed_leaf_index

                /* 3. */
                dup 6
                call {hash_row}
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem [mt_root] fri_revealed_leaf_index [base_row_digest] *row_elem_next

                swap 12
                pop 1
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root] fri_revealed_leaf_index [base_row_digest]

                /* 4. */
                dup 14
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root] fri_revealed_leaf_index [base_row_digest] merkle_tree_height

                call {merkle_root_verify}
                // _ remaining_iterations merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next

                swap 4
                push -1
                add
                swap 4
                // _ (remaining_iterations - 1) merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next

                recurse
        );

        triton_asm!(
            // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root *fri_revealed *base_table_rows
            {entrypoint}:

                swap 1
                push {1 + EXTENSION_DEGREE}
                add
                swap 1
                // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root (*fri_revealed_first_elem.0) *base_table_rows

                // skip length indicator of row vector
                push 1
                add

                call {loop_label}
                // _ 0 merkle_tree_height *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next

                pop 5
                // _

                return

            {&loop_code}
        )
    }
}
