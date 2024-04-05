use crate::data_type::DataType;
use crate::hashing::absorb_multiple_static_size::AbsorbMultipleStaticSize;
use crate::hashing::algebraic_hasher::hash_static_size::HashStaticSize;
use crate::library::Library;
use crate::recufier::fri::verify::FriSnippet;
use crate::traits::basic_snippet::BasicSnippet;
use triton_vm::prelude::*;
use triton_vm::table::NUM_BASE_COLUMNS;

/// Crashes the VM is the base table rows do not authenticate against the provided Merkle root
/// First hashes the rows, then verifies that the digests belong in the Merkle tree.
pub struct VerifyBaseTableRows;

impl BasicSnippet for VerifyBaseTableRows {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "num_combination_codeword_checks".to_owned()),
            (DataType::U32, "merkle_tree_height".to_owned()),
            (DataType::VoidPointer, "*merkle_tree_root".to_owned()),
            // type of base table rows is `List<[BFieldElement: BASE_TABLE_COLUMNS]>` but encoded
            // in memory as a flat structure. So I'm not sure what type to use here. Anyway, it's
            // certainly a pointer.
            (DataType::VoidPointer, "*base_table_rows".to_owned()),
            (
                FriSnippet::indexed_leaves_list_type(),
                "*revealed_fri".to_owned(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "verify_base_table_rows".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoints = self.entrypoint();

        let hash_base_row = library.import(Box::new(HashStaticSize {
            size: NUM_BASE_COLUMNS,
        }));

        // I would caution against (but not deny) unrolling this loop, as the loop body is already very long
        let loop_code = triton_asm!(
            // Invariant: _ remaining_iterations merkle_tree_height *merkle_tree_root *base_table_rows_element

            // Check end-loop condition
            dup 3
            push 0
            eq
            return

            // _  remaining_iterations merkle_tree_height *merkle_tree_root *base_table_rows_element

            call {hash_base_row}



            recurse
        );

        triton_asm!(
            // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root *base_table_rows *revealed_fri
            {entrypoints}:

                return
        )
    }
}
