use strum::Display;
use strum::EnumIter;
use triton_vm::prelude::*;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::hashing::algebraic_hasher::hash_static_size::HashStaticSize;
use crate::prelude::*;

#[derive(Debug, Copy, Clone, Display, EnumIter)]
pub enum ColumnType {
    Main,
    Aux,
    Quotient,
}

/// Crashes the VM is the table rows do not authenticate against the provided Merkle root
/// First hashes the rows, then verifies that the digests belong in the Merkle tree.
#[derive(Debug, Copy, Clone)]
pub struct VerifyTableRows {
    pub column_type: ColumnType,
}

impl VerifyTableRows {
    pub fn new(column_type: ColumnType) -> Self {
        Self { column_type }
    }
}

impl VerifyTableRows {
    pub fn row_size(&self) -> usize {
        match self.column_type {
            ColumnType::Main => MasterMainTable::NUM_COLUMNS,
            ColumnType::Aux => MasterAuxTable::NUM_COLUMNS * EXTENSION_DEGREE,
            ColumnType::Quotient => NUM_QUOTIENT_SEGMENTS * EXTENSION_DEGREE,
        }
    }
}

impl BasicSnippet for VerifyTableRows {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "num_combination_codeword_checks".to_owned()),
            (DataType::U32, "merkle_tree_height".to_owned()),
            (DataType::VoidPointer, "*merkle_tree_root".to_owned()),
            (DataType::VoidPointer, "*fri_revealed".to_owned()),
            // type of {main|aux|quot} table rows i
            // `Vec<[{BaseFieldElement, XFieldElement, XFieldElement}: COLUMN_COUNT]>` but encoded
            // in memory as a flat structure. So I'm not sure what type to use here. Anyway, it's
            // certainly a list.
            (DataType::VoidPointer, "*table_rows".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasmlib_verifier_master_table_verify_{}_table_rows",
            self.column_type
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let hash_row = library.import(Box::new(HashStaticSize {
            size: self.row_size(),
        }));

        let loop_over_auth_path_digests_label =
            format!("{entrypoint}_loop_over_auth_path_elements");
        let loop_over_auth_paths_code = triton_asm!(
            {loop_over_auth_path_digests_label}:
                merkle_step                 // move up one level in the Merkle tree
                recurse_or_return           // break loop if node_index is 1
        );

        let loop_over_rows_label = format!("{entrypoint}_loop_over_rows");
        let loop_over_rows_code = triton_asm!(
            // Invariant: _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root]
            {loop_over_rows_label}:
                // Check end-loop condition
                dup 9
                push 0
                eq
                skiz
                    return

                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root]

                push 1
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root] 1

                /* 2. */
                dup 7
                read_mem 1
                push {EXTENSION_DEGREE + 2}
                add
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root] 1 fri_revealed_leaf_index (*fri_revealed_leaf_index + 4)
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index *row_elem [mt_root] 1 fri_revealed_leaf_index *fri_revealed_leaf_index_next

                swap 9
                pop 1
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem [mt_root] 1 fri_revealed_leaf_index

                dup 10
                add
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem [mt_root] 1 node_index

                /* 3. */
                dup 7
                call {hash_row}
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem [mt_root] 1 node_index [row_digest] *row_elem_next

                swap 13
                pop 1
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root] 1 node_index [row_digest]

                call {loop_over_auth_path_digests_label}
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root] 1 1 [calculated_root]

                swap 2
                swap 4
                swap 6
                pop 1
                swap 2
                swap 4
                pop 1
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root] [calculated_root]

                assert_vector error_id 40
                // _ remaining_iterations num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root]

                swap 9
                push -1
                add
                swap 9
                // _ (remaining_iterations - 1) num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root]

                recurse

                {&loop_over_auth_paths_code}
        );

        triton_asm!(
            // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root *fri_revealed *table_rows
            {entrypoint}:

                swap 1
                push {1 + EXTENSION_DEGREE}
                add
                swap 1
                // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root (*fri_revealed_first_elem.0) *table_rows

                // Verify length of `rows`
                read_mem 1
                push 2
                add
                swap 1
                // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root (*fri_revealed_first_elem.0) *table_rows[0] length

                dup 5
                eq
                assert error_id 41
                // _ num_combination_codeword_checks merkle_tree_height *merkle_tree_root (*fri_revealed_first_elem.0) *table_rows[0]

                swap 3
                push 2
                pow
                // _ num_combination_codeword_checks *table_rows[0] *merkle_tree_root (*fri_revealed_first_elem.0) (2^merkle_tree_height)
                // _ num_combination_codeword_checks *table_rows[0] *merkle_tree_root (*fri_revealed_first_elem.0) num_leaves

                swap 3
                // _ num_combination_codeword_checks num_leaves *merkle_tree_root (*fri_revealed_first_elem.0) *table_rows[0]

                dup 2
                push {Digest::LEN - 1}
                add
                read_mem {Digest::LEN}
                pop 1

                // _ num_combination_codeword_checks num_leaves *merkle_tree_root (*fri_revealed_first_elem.0) *table_rows[0] [mt_root]

                call {loop_over_rows_label}
                // _ 0 num_leaves *merkle_tree_root *fri_revealed_leaf_index_next *row_elem_next [mt_root]

                pop 5
                pop 5
                // _

                return

            {&loop_over_rows_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::other::random_elements;
    use twenty_first::prelude::*;
    use twenty_first::tip5::RATE;

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::test_prelude::*;

    #[test]
    fn verify_table_pbt_main() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Main,
        })
        .test()
    }

    #[test]
    fn verify_table_pbt_aux() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Aux,
        })
        .test()
    }

    #[test]
    fn verify_table_pbt_quot() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Quotient,
        })
        .test()
    }

    mod negative_tests {
        use strum::IntoEnumIterator;
        use tasm_lib::test_helpers::test_assertion_failure;
        use test_strategy::proptest;

        use super::*;

        #[proptest(cases = 50)]
        fn verify_bad_auth_path_crashes_vm(seed: [u8; 32]) {
            let mut rng = StdRng::from_seed(seed);
            let snippets = ColumnType::iter().map(|column_type| VerifyTableRows { column_type });

            for snippet in snippets {
                let mut init_state = snippet.pseudorandom_initial_state(seed, None);
                let num_digests = init_state.nondeterminism.digests.len();
                init_state.nondeterminism.digests[rng.random_range(0..num_digests)] = rng.random();

                test_assertion_failure(&ShadowedProcedure::new(snippet), init_state.into(), &[40]);
            }
        }

        #[proptest(cases = 50)]
        fn verify_bad_row_list_length(seed: [u8; 32]) {
            let snippets = ColumnType::iter().map(|column_type| VerifyTableRows { column_type });
            for snippet in snippets {
                let mut init_state = snippet.pseudorandom_initial_state(seed, None);

                // Mutate `num_combination_codeword_checks` to make it invalid
                let init_stack_length = init_state.stack.len();
                init_state.stack[init_stack_length - 5].increment();

                test_assertion_failure(&ShadowedProcedure::new(snippet), init_state.into(), &[41]);
            }
        }
    }

    // TODO: Add negative tests, to verify that VM crashes if fed a leaf index that's not a valid
    // u32.

    impl Procedure for VerifyTableRows {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            fn verify_one_row(
                leaf_index: u32,
                merkle_root: Digest,
                merkle_tree_height: u32,
                authentication_path: Vec<Digest>,
                row: &[BFieldElement],
                sponge: &mut Tip5,
            ) {
                // We define a local hash_varlen to be able to simulate what happens to the sponge,
                // as this is required by the test framework.
                fn local_hash_varlen(input: &[BFieldElement], sponge: &mut Tip5) -> Digest {
                    *sponge = Tip5::init();
                    sponge.pad_and_absorb_all(input);
                    let produce: [BFieldElement; RATE] = sponge.squeeze();

                    Digest::new((&produce[..Digest::LEN]).try_into().unwrap())
                }

                let leaf_digest = local_hash_varlen(row, sponge);
                let merkle_tree_inclusion_proof = MerkleTreeInclusionProof {
                    tree_height: merkle_tree_height,
                    indexed_leafs: vec![(leaf_index as usize, leaf_digest)],
                    authentication_structure: authentication_path,
                };

                assert!(merkle_tree_inclusion_proof.verify(merkle_root));
            }

            *sponge = Some(Tip5::init());
            let table_rows_pointer = stack.pop().unwrap();
            let fri_revealed_pointer = stack.pop().unwrap();
            let merkle_tree_root_pointer = stack.pop().unwrap();
            let merkle_tree_height: u32 = stack.pop().unwrap().try_into().unwrap();
            let num_combination_codeword_checks: u32 = stack.pop().unwrap().try_into().unwrap();

            let merkle_root = Digest::new(
                (0..Digest::LEN)
                    .map(|i| memory[&(merkle_tree_root_pointer + bfe!(i as u32))])
                    .collect_vec()
                    .try_into()
                    .unwrap(),
            );

            // Verify all rows
            let mut j = 0;
            for i in 0..num_combination_codeword_checks {
                // Read a row from memory
                let row = (0..self.row_size())
                    .map(|l| {
                        memory[&(table_rows_pointer
                            + bfe!(l as u64 + 1 + (self.row_size() as u64) * i as u64))]
                    })
                    .collect_vec();

                // Read leaf index as provided by the FRI verifier
                let leaf_index: u32 = nondeterminism.ram
                    [&(fri_revealed_pointer + bfe!(4) + BFieldElement::new(i as u64 * 4))]
                    .try_into()
                    .unwrap();
                let mut authentication_path = vec![];
                for _ in 0..merkle_tree_height {
                    authentication_path.push(nondeterminism.digests[j]);
                    j += 1;
                }

                verify_one_row(
                    leaf_index,
                    merkle_root,
                    merkle_tree_height,
                    authentication_path,
                    &row,
                    sponge.as_mut().unwrap(),
                )
            }

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let merkle_tree_height = match bench_case {
                Some(BenchmarkCase::CommonCase) => 17,
                Some(BenchmarkCase::WorstCase) => 22,
                None => rng.random_range(2..7),
            };
            let num_leafs = 1 << merkle_tree_height;
            let num_combination_codeword_checks = 3;
            let mut memory = HashMap::default();

            let rows: Vec<Vec<BFieldElement>> =
                vec![vec![rng.random(); self.row_size()]; num_combination_codeword_checks];
            let leaf_indices: Vec<usize> = (0..num_combination_codeword_checks)
                .map(|_| rng.random_range(0..num_leafs))
                .collect_vec();

            // Construct Merkle tree with specified rows as preimages to the leafs at the specified
            // indices.
            let mut leafs: Vec<Digest> = random_elements(1 << merkle_tree_height);
            for (leaf_index, leaf_preimage) in leaf_indices.iter().zip_eq(rows.iter()) {
                leafs[*leaf_index] = Tip5::hash_varlen(leaf_preimage);
            }

            let merkle_tree = MerkleTree::par_new(&leafs).unwrap();
            let merkle_root = merkle_tree.root();
            let merkle_root_pointer: BFieldElement = rng.random();
            encode_to_memory(&mut memory, merkle_root_pointer, &merkle_root);

            // Insert all rows into memory, as a list
            let row_pointer: BFieldElement = rng.random();
            memory.insert(row_pointer, bfe!(num_combination_codeword_checks as u64));
            let mut j: BFieldElement = bfe!(1);
            for row in rows {
                for word in row {
                    memory.insert(row_pointer + j, word);
                    j.increment();
                }
            }

            let mocked_fri_return_value: Vec<(u32, XFieldElement)> = leaf_indices
                .iter()
                .map(|x| *x as u32)
                .zip((0..num_combination_codeword_checks).map(|_| rng.random()))
                .collect_vec();
            let fri_return_value_pointer = rng.random();
            list_insert(
                fri_return_value_pointer,
                mocked_fri_return_value,
                &mut memory,
            );

            let mut authentication_paths: Vec<Digest> = vec![];
            for leaf_index in leaf_indices {
                authentication_paths
                    .extend(merkle_tree.authentication_structure(&[leaf_index]).unwrap());
            }

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![
                    bfe!(num_combination_codeword_checks as u64),
                    bfe!(merkle_tree_height),
                    merkle_root_pointer,
                    fri_return_value_pointer,
                    row_pointer,
                ],
            ]
            .concat();
            ProcedureInitialState {
                stack,
                nondeterminism: NonDeterminism::default()
                    .with_ram(memory)
                    .with_digests(authentication_paths),
                public_input: vec![],
                sponge: None,
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn verify_table_bench_main() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Main,
        })
        .bench()
    }

    #[test]
    fn verify_table_bench_aux() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Aux,
        })
        .bench()
    }

    #[test]
    fn verify_table_bench_quot() {
        ShadowedProcedure::new(VerifyTableRows {
            column_type: ColumnType::Quotient,
        })
        .bench()
    }
}
