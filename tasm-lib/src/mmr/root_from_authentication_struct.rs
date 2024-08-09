use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::DataType;
use crate::hashing::absorb_multiple::AbsorbMultiple;
use crate::library::Library;
use crate::prelude::BasicSnippet;

pub struct RootFromAuthenticationStruct;

impl RootFromAuthenticationStruct {
    fn indexed_leaf_element_type() -> DataType {
        DataType::Tuple(vec![DataType::U64, DataType::Digest])
    }
}

impl BasicSnippet for RootFromAuthenticationStruct {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "tree_height".to_owned()),
            (
                DataType::List(Box::new(DataType::Digest)),
                "auth_struct".to_owned(),
            ),
            (
                DataType::List(Box::new(Self::indexed_leaf_element_type())),
                "indexed_leafs".to_owned(),
            ),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "root".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_mmr_root_from_authentication_struct".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let absorb_multiple = library.import(Box::new(AbsorbMultiple));
        let alpha_challenge_pointer_write = library.kmalloc(Digest::LEN as u32);
        let beta_challenge_pointer_write = library.kmalloc(Digest::LEN as u32);
        let gamma_challenge_pointer_write = library.kmalloc(Digest::LEN as u32);

        let indexed_leaf_element_size = Self::indexed_leaf_element_type().stack_size();
        let calculate_and_store_challenges = triton_asm!(
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
            // _ [gamma] [beta] [alpha] <- rename

            push {alpha_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _ [gamma] [beta]

            push {beta_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _ [gamma]

            push {gamma_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _
        );

        let digest_to_xfe = triton_asm!(
            // _ *digest_last_word

            read_mem {Digest::LEN}
            pop 1
            // _ [digest; 5]

            push 1
            // _ [digest; 5] 1

            push {alpha_challenge_pointer_write}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ [digest] 1 [challenge]
            // _ d4 d3 d2 d1 d0 1 [challenge]

            xx_mul
            // _ d4 d3 d2 [(d1 d0 1) * challenge]

            xx_add
            // _ [digest_as_xfe; 3]
        );

        let u64_size = DataType::U64.stack_size();
        const TWO_POW_32: u64 = 1 << 32;
        let node_index_to_bfe = triton_asm!(
            /* node-index is assumed to have type u64, which takes up two words */
            // _ *node_index_last_word

            read_mem {u64_size}
            pop 1
            // _ [node_index; 2]
            // _ hi lo

            swap 1
            push {TWO_POW_32}
            // _ lo (hi * 2^{32})

            add
            // _ node_index_as_bfe
        );

        let entrypoint = self.entrypoint();
        triton_asm!(
            {entrypoint}:
                // _ tree_height *auth_struct *indexed_leafs

                dup 1
                dup 1
                // _ tree_height *auth_struct *indexed_leafs *auth_struct *indexed_leafs

                {&calculate_and_store_challenges}
                // _ tree_height *auth_struct *indexed_leafs

                // TODO: REMOVE
                push 0
                push 0

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use hex::decode;
    use itertools::Itertools;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use twenty_first::prelude::CpuParallel;
    use twenty_first::prelude::MerkleTree;
    use twenty_first::prelude::Sponge;

    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::procedure::Procedure;
    use crate::traits::procedure::ProcedureInitialState;
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::VmHasher;

    use super::*;

    const SIZE_OF_INDEXED_LEAFS_ELEMENT: usize = Digest::LEN + 2;

    #[test]
    fn test() {
        ShadowedProcedure::new(RootFromAuthenticationStruct).test();
    }

    impl Procedure for RootFromAuthenticationStruct {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
            public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            assert_eq!(
                SIZE_OF_INDEXED_LEAFS_ELEMENT,
                Self::indexed_leaf_element_type().stack_size()
            );
            let indexed_leafs_pointer = stack.pop().unwrap();
            let auth_struct_pointer = stack.pop().unwrap();
            let tree_height = stack.pop().unwrap();

            let bfes_to_indexed_leaf =
                |bfes: [BFieldElement; SIZE_OF_INDEXED_LEAFS_ELEMENT]| -> (u64, Digest) {
                    println!("bfes_to_indexed_leaf:\nbfes: {bfes:?}");
                    // let mt_leaf_index = *u64::decode(&bfes[0..2]).unwrap();
                    // let digest = *Digest::decode(&bfes[2..SIZE_OF_INDEXED_LEAFS_ELEMENT]).unwrap();

                    // (mt_leaf_index, digest)
                    *<(u64, Digest)>::decode(&bfes).unwrap()
                };
            let bfes_to_digest = |bfes: [BFieldElement; Digest::LEN]| -> Digest {
                println!("bfes_to_digest:\nbfes: {bfes:?}");
                *Digest::decode(&bfes[0..Digest::LEN]).unwrap()
            };

            let indexed_leafs: Vec<[BFieldElement; SIZE_OF_INDEXED_LEAFS_ELEMENT]> =
                load_list_with_copy_elements(indexed_leafs_pointer, memory);
            println!("indexed_leafs: {indexed_leafs:?}");
            let indexed_leafs = indexed_leafs
                .into_iter()
                .map(bfes_to_indexed_leaf)
                .collect_vec();
            let auth_struct: Vec<[BFieldElement; Digest::LEN]> =
                load_list_with_copy_elements(auth_struct_pointer, memory);
            println!("auth_struct: {auth_struct:?}");
            let auth_struct = auth_struct.into_iter().map(bfes_to_digest).collect_vec();

            let sponge = sponge.as_mut().expect("sponge must be initialized");

            sponge.pad_and_absorb_all(&indexed_leafs.encode());
            sponge.pad_and_absorb_all(&auth_struct.encode());

            let sponge_output = sponge.squeeze();

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            // TODO: use real `mmr_authentication_struct` code here
            let tree_height = rng.gen_range(0..4);
            let leaf_count = 1 << tree_height;
            let num_revealed_leafs = rng.gen_range(0..leaf_count);
            let revealed_leaf_indices = (0..num_revealed_leafs)
                .map(|_| rng.gen_range(0..leaf_count))
                .unique()
                .collect_vec();
            let leafs = (0..leaf_count).map(|_| rng.gen()).collect_vec();
            let tree = MerkleTree::<Tip5>::new::<CpuParallel>(&leafs).unwrap();

            let proof = tree
                .inclusion_proof_for_leaf_indices(&revealed_leaf_indices)
                .unwrap();

            let mut memory = HashMap::new();
            let authentication_structure = proof.authentication_structure.clone();
            let authentication_structure_ptr = rng.gen();

            let indexed_leafs_ptr = rng.gen();
            let indexed_leafs = proof.indexed_leafs.clone();
            let indexed_leafs = indexed_leafs
                .into_iter()
                .map(|(index, leaf)| (index as u64, leaf))
                .collect_vec();

            println!("indexed_leafs.len(): {}", indexed_leafs.len());
            println!(
                "authentication_structure.len(): {}",
                authentication_structure.len()
            );

            list_insert(
                authentication_structure_ptr,
                authentication_structure,
                &mut memory,
            );
            list_insert(indexed_leafs_ptr, indexed_leafs, &mut memory);

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![
                    bfe!(tree_height),
                    authentication_structure_ptr,
                    indexed_leafs_ptr,
                ],
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

        fn corner_case_initial_states(&self) -> Vec<ProcedureInitialState> {
            vec![]
        }
    }
}
