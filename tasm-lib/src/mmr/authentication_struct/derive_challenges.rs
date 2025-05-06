use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::mmr::authentication_struct::shared;
use crate::prelude::BasicSnippet;
use crate::prelude::Library;

/// Derive and return the challenges that the authentication structure verification
/// program uses.
pub struct DeriveChallenges;

impl BasicSnippet for DeriveChallenges {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Digest)),
                "auth_struct".to_owned(),
            ),
            (
                DataType::List(Box::new(shared::indexed_leaf_element_type())),
                "indexed_leafs".to_owned(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "alpha".to_owned()),
            (DataType::Xfe, "-beta".to_owned()),
            (DataType::Xfe, "gamma".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_authentication_struct_derive_challenges".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let absorb_multiple = library.import(Box::new(AbsorbMultiple));

        let entrypoint = self.entrypoint();

        let indexed_leaf_element_size = shared::indexed_leaf_element_type().stack_size();
        triton_asm!(
            {entrypoint}:
                // _ *auth_struct *indexed_leafs

                sponge_init
                // _ *auth_struct *indexed_leafs

                read_mem 1
                push 1
                add
                // _ *auth_struct indexed_leafs_len *indexed_leafs

                swap 1
                // _ *auth_struct *indexed_leafs indexed_leafs_len

                push {indexed_leaf_element_size}
                mul
                push 1
                add
                // _ *auth_struct *indexed_leafs indexed_leafs_size

                call {absorb_multiple}
                // _ *auth_struct

                read_mem 1
                push 1
                add
                // _ auth_struct_len *auth_struct

                swap 1
                push {Digest::LEN}
                mul
                push 1
                add
                // _ *auth_struct auth_struct_size

                call {absorb_multiple}
                // _

                sponge_squeeze
                // _ w9 w8 w7 w6 w5 w4 w3 w2 w1 w0

                pop 1
                // _ w9 w8 w7 w6 w5 w4 w3 w2 w1
                // _ [gamma] [-beta] [alpha] <- rename

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use rand::Rng;
    use rand::SeedableRng;
    use rand::rngs::StdRng;
    use shared::AuthenticatedMerkleAuthStruct;
    use twenty_first::prelude::Sponge;
    use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;

    use crate::mmr::authentication_struct::shared::AuthStructIntegrityProof;
    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    const SIZE_OF_INDEXED_LEAFS_ELEMENT: usize = Digest::LEN + 2;

    #[test]
    fn test() {
        ShadowedProcedure::new(DeriveChallenges).test();
    }

    impl Procedure for DeriveChallenges {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _nondeterminism: &NonDeterminism,
            _public_input: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let indexed_leafs_pointer = stack.pop().unwrap();
            let auth_struct_pointer = stack.pop().unwrap();
            let bfes_to_indexed_leaf =
                |bfes: [BFieldElement; SIZE_OF_INDEXED_LEAFS_ELEMENT]| -> (u64, Digest) {
                    *<(u64, Digest)>::decode(&bfes).unwrap()
                };
            let bfes_to_digest = |bfes: [BFieldElement; Digest::LEN]| -> Digest {
                *Digest::decode(&bfes[0..Digest::LEN]).unwrap()
            };
            let indexed_leafs: Vec<[BFieldElement; SIZE_OF_INDEXED_LEAFS_ELEMENT]> =
                load_list_with_copy_elements(indexed_leafs_pointer, memory);
            let indexed_leafs = indexed_leafs
                .into_iter()
                .map(bfes_to_indexed_leaf)
                .collect_vec();
            let auth_struct: Vec<[BFieldElement; Digest::LEN]> =
                load_list_with_copy_elements(auth_struct_pointer, memory);
            let auth_struct = auth_struct.into_iter().map(bfes_to_digest).collect_vec();

            let sponge = sponge.as_mut().expect("sponge must be initialized");

            sponge.pad_and_absorb_all(&indexed_leafs.encode());
            sponge.pad_and_absorb_all(&auth_struct.encode());

            let sponge_output = sponge.squeeze();
            for elem in sponge_output.into_iter().skip(1).rev() {
                stack.push(elem);
            }

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let (tree_height, num_revealed_leafs) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (32, 10),
                Some(BenchmarkCase::WorstCase) => (62, 10),
                None => (rng.random_range(0..62), 10),
            };

            let leaf_count = 1 << tree_height;
            let revealed_leaf_indices = (0..num_revealed_leafs)
                .map(|_| rng.random_range(0..leaf_count))
                .unique()
                .collect_vec();
            let indexed_leafs = revealed_leaf_indices
                .into_iter()
                .map(|leaf_idx: u64| (leaf_idx, rng.random()))
                .collect_vec();
            let (mmra, mps) = mmra_with_mps(leaf_count, indexed_leafs.clone());
            let indexed_mmr_mps = indexed_leafs
                .into_iter()
                .zip_eq(mps)
                .map(|((leaf_idx, leaf), mp)| (leaf_idx, leaf, mp))
                .collect_vec();
            let authenticity_witnesses =
                AuthStructIntegrityProof::new_from_mmr_membership_proofs(&mmra, indexed_mmr_mps);
            assert!(
                authenticity_witnesses.len().is_one(),
                "All indices belong to first peak"
            );
            let AuthenticatedMerkleAuthStruct {
                auth_struct,
                indexed_leafs,
                ..
            } = &authenticity_witnesses[&0];

            let mut memory = HashMap::new();
            let authentication_structure_ptr = rng.random();
            let indexed_leafs_ptr = rng.random();

            list_insert(
                authentication_structure_ptr,
                auth_struct.to_owned(),
                &mut memory,
            );
            list_insert(indexed_leafs_ptr, indexed_leafs.to_owned(), &mut memory);

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![authentication_structure_ptr, indexed_leafs_ptr],
            ]
            .concat();

            let nondeterminism = NonDeterminism::default().with_ram(memory);
            ProcedureInitialState {
                stack,
                nondeterminism,
                public_input: vec![],
                sponge: Some(Tip5::init()),
            }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bag_peaks_benchmark() {
        ShadowedProcedure::new(DeriveChallenges).bench();
    }
}
