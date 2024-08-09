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
        let alpha_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let alpha_challenge_pointer_read =
            alpha_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);
        let beta_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let beta_challenge_pointer_read =
            beta_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);
        let gamma_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let gamma_challenge_pointer_read =
            gamma_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);

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
            hint alpha: XFieldElement = stack[0..3]
            hint minus_beta: XFieldElement = stack[3..6]
            hint gamma: XFieldElement = stack[6..9]
            // _ [gamma] [-beta] [alpha] <- rename

            push {alpha_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _ [gamma] [-beta]

            push {beta_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _ [gamma]

            push {gamma_challenge_pointer_write}
            write_mem {EXTENSION_DEGREE}
            pop 1
            // _
        );

        let u64_size = DataType::U64.stack_size();
        const TWO_POW_32: u64 = 1 << 32;
        let u64_to_bfe = triton_asm!(
            // _ leaf_idx_hi leaf_idx_lo

            swap 1
            push {TWO_POW_32}
            mul
            // _ leaf_idx_lo (leaf_idx_hi << 32)

            add

            // _ leaf_idx_as_bfe
        );

        let digest_to_xfe = triton_asm!(
            // _ [digest]

            push 1
            push {alpha_challenge_pointer_read}
            read_mem {EXTENSION_DEGREE}
            pop 1
            // _ l4 l3 l2 l1 l0 1 [α; 3]

            xx_mul
            // _ l4 l3 l2 [(l1 l0 1) * α]

            xx_add
            // _ xfe
        );

        let entrypoint = self.entrypoint();
        let accumulate_indexed_leafs_loop_label = format!("{entrypoint}_acc_indexed_leafs");
        let accumulated_indexed_leafs_loop = triton_asm!(
            // INVARIANT: _ num_leafs *auth_struct *idx_leafs *idx_leafs[n]_lw [0; 2] [p; 3]
            {accumulate_indexed_leafs_loop_label}:
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n]_lw [0; 2] [p; 3]

                /* Read leaf-index, convert it to BFE, and multiply it with `gamma` challenge */
                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n]_lw [0; 2] [p; 3] [gamma]
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n]_lw [0; 2] [p; 3] [γ] <-- rename

                dup 8
                read_mem {u64_size}
                swap 11
                pop 1
                // _ num_leafs *auth_struct *idx_leafs (*idx_leafs[n]_lw - 2) [0; 2] [p; 3] [γ] [leaf_idx; 2]

                {&u64_to_bfe}
                // _ num_leafs *auth_struct *idx_leafs (*idx_leafs[n]_lw - 2) [0; 2] [p; 3] [γ] leaf_idx_bfe

                dup 12
                add
                // _ num_leafs *auth_struct *idx_leafs (*idx_leafs[n]_lw - 2) [0; 2] [p; 3] [γ] node_idx_bfe

                xb_mul
                // _ num_leafs *auth_struct *idx_leafs (*idx_leafs[n]_lw - 2) [0; 2] [p; 3] [γ * node_idx; 3]

                dup 8
                read_mem {Digest::LEN}
                swap 14
                pop 1
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] [leaf; 5]
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] l4 l3 l2 l1 l0

                /* Convert `leaf` to XFE, using challenge */
                {&digest_to_xfe}
                hint leaf_as_xfe: XFieldElement = stack[0..2]
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] [(l1 l0 1) * α + (l4 l3 l2)]
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] [leaf_as_xfe] <-- rename

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] [leaf_as_xfe] [-beta]

                xx_add
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx; 3] [leaf_as_xfe - beta]

                xx_add
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p; 3] [γ * node_idx + leaf_as_xfe - beta]

                xx_mul
                // _ num_leafs *auth_struct *idx_leafs *idx_leafs[n-1]_lw [0; 2] [p'; 3]

                recurse_or_return
        );
        let accumulate_indexed_leafs_from_public_data = triton_asm!(
            // _ num_leafs *auth_struct *indexed_leafs

            dup 0
            read_mem 1
            push 1
            add
            swap 1
            // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs indexed_leafs_len

            push {indexed_leaf_element_size}
            mul
            // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs (indexed_leafs_size - 1)

            add
            // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs_last_word

            push 0
            push 0
            push 0
            push 0
            push 1
            hint prev = stack[3..5]
            hint p: XFieldElement = stack[0..3]
            // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs_last_word [0u64; 2] [p; 3]

            dup 6
            dup 6
            eq
            push 0
            eq
            skiz
                call {accumulate_indexed_leafs_loop_label}
            // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs [0u64; 2] [p; 3]
        );

        let accumulate_auth_struct_leafs_from_public_data_label =
            format!("{entrypoint}_auth_struct_loop");

        // let u64_lt = library.import(Box::new(LtU64PreserveArgs));
        let u64_lt = triton_asm!(
            // _ lhs_hi lhs_lo rhs_hi rhs_lo

            /* calculate rhs_hi < lhs_hi || rhs_hi == lhs_hi && rhs_lo < lhs_lo */
            dup 2
            swap 1
            // _ lhs_hi lhs_lo rhs_hi lhs_lo rhs_lo

            lt
            // _ lhs_hi lhs_lo rhs_hi (lhs_lo > rhs_lo)
            // _ lhs_hi lhs_lo rhs_hi (rhs_lo < lhs_lo)

            dup 1
            dup 4
            eq
            // _ lhs_hi lhs_lo rhs_hi (rhs_lo < lhs_lo) (rhs_hi == lhs_hi)

            mul
            // _ lhs_hi lhs_lo rhs_hi (rhs_lo < lhs_lo && rhs_hi == lhs_hi)

            dup 3
            swap 1
            swap 2
            // _ lhs_hi lhs_lo (rhs_lo < lhs_lo && rhs_hi == lhs_hi) lhs_hi rhs_hi

            lt
            // _ lhs_hi lhs_lo (rhs_lo < lhs_lo && rhs_hi == lhs_hi) (lhs_hi > rhs_hi)
            // _ lhs_hi lhs_lo (rhs_lo < lhs_lo && rhs_hi == lhs_hi) (rhs_hi < lhs_hi)

            add
            // _ lhs_hi lhs_lo (rhs_lo < lhs_lo && rhs_hi == lhs_hi || rhs_hi < lhs_hi)
        );

        let accumulate_auth_struct_leafs_from_public_data = triton_asm!(
            // INVARIANT: _ *auth_struct *auth_struct[n]_lw [prev; 2] [p]
            {accumulate_auth_struct_leafs_from_public_data_label}:
                /* Divine in auth-struct node-index and verify ordering */

                divine 2
                hint auth_struct_elem_node_index: u64 = stack[0..2]
                // _ *auth_struct *auth_struct[n]_lw [prev; 2] [p] [node_index]


                /* Notice that the u64-lt snippet crashes if divined node-index
                   words are not valid u32s. So no need to check explicitly. */
                dup 6
                dup 6
                {&u64_lt}
                // _ *auth_struct *auth_struct[n]_lw [prev; 2] [p] [node_index] (prev < nodex_index)

                assert
                // _ *auth_struct *auth_struct[n]_lw [prev; 2] [p] [node_index]
                // _ *auth_struct *auth_struct[n]_lw prev_hi prev_lo p2 p1 p0 node_index_hi node_index_lo

                swap 5
                pop 1
                swap 5
                pop 1
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p]

                /* Calculate `node_index * challenge` */
                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [γ]

                dup 7
                push {TWO_POW_32}
                mul
                dup 7
                add
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [γ] node_index_bfe

                xb_mul
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [node_index * γ]

                /* Read auth-struct element and convert to XFE */
                dup 8
                read_mem {Digest::LEN}
                swap 14
                pop 1
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [node_index * γ] [auth_struct_digest]

                {&digest_to_xfe}
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [node_index * γ] [auth_struct_xfe]

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [node_index * γ] [auth_struct_xfe - beta]

                xx_add
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p] [node_index * γ + auth_struct_xfe - beta]

                xx_mul
                // _ *auth_struct *auth_struct[n]_lw [node_index] [p * (node_index * γ + auth_struct_xfe - beta)]
                // _ *auth_struct *auth_struct[n]_lw [prev] [p'] <-- rename

                recurse_or_return
        );

        let set_initial_t_value = triton_asm!(
            // _ num_leafs *indexed_leafs *auth_struct [p]

            /* Set `t` digest to according to the rules:
            let mut t = auth_struct
                .first()
                .copied()
                .unwrap_or_else(|| indexed_leafs.first().unwrap().1);
         */
            dup 3
            read_mem 1
            pop 1
            // _ num_leafs *indexed_leafs *auth_struct [p] auth_struct_len
        );

        triton_asm!(
            {entrypoint}:
                // _ tree_height *auth_struct *indexed_leafs

                dup 1
                dup 1
                // _ tree_height *auth_struct *indexed_leafs *auth_struct *indexed_leafs

                {&calculate_and_store_challenges}
                // _ tree_height *auth_struct *indexed_leafs

                /* Calculate number of leafs in Merkle tree */
                swap 2
                push 2
                pow
                swap 2
                hint num_leafs = stack[2]
                // _ num_leafs *auth_struct *indexed_leafs

                {&accumulate_indexed_leafs_from_public_data}
                // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs [garbage; 2] [p; 3]
                // _ num_leafs *auth_struct *indexed_leafs *indexed_leafs [garbage; 2] p2 p1 p0 <-- rename

                /* Prepare for next loop, absorption of auth-struct digests into accumulator */
                swap 7
                swap 6
                pop 1
                // _ num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1

                dup 5
                read_mem 1
                push 1
                add
                swap 1
                // _ num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1 *auth_struct auth_struct_len

                push {Digest::LEN}
                mul
                add
                // _ num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1 *auth_struct_last_word

                swap 5
                swap 7
                // _ num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [0; 2] p2 p1 p0
                // _ num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p] <-- rename

                dup 6
                dup 6
                eq
                push 0
                eq
                // _ num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p] (*auth_struct_last_word != *auth_struct)

                skiz
                    call {accumulate_auth_struct_leafs_from_public_data_label}
                // _ num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p]

                /* Cleanup stack before next loop */
                swap 3
                pop 1
                swap 3
                pop 1
                swap 3
                pop 1
                // _ num_leafs *indexed_leafs *auth_struct [p]

                return

                {&accumulated_indexed_leafs_loop}
                {&accumulate_auth_struct_leafs_from_public_data}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::collections::VecDeque;

    use itertools::Itertools;
    use num::One;
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
            fn digest_to_xfe(digest: Digest, challenge: XFieldElement) -> XFieldElement {
                let [l0, l1, l2, l3, l4] = digest.0;
                let leaf_xfe_lo = XFieldElement::new([BFieldElement::new(1), l0, l1]);
                let leaf_xfe_hi = XFieldElement::new([l2, l3, l4]);

                challenge * leaf_xfe_lo + leaf_xfe_hi
            }

            assert_eq!(
                SIZE_OF_INDEXED_LEAFS_ELEMENT,
                Self::indexed_leaf_element_type().stack_size()
            );
            let indexed_leafs_pointer = stack.pop().unwrap();
            let auth_struct_pointer = stack.pop().unwrap();
            let tree_height: u32 = stack.pop().unwrap().try_into().unwrap();

            let bfes_to_indexed_leaf =
                |bfes: [BFieldElement; SIZE_OF_INDEXED_LEAFS_ELEMENT]| -> (u64, Digest) {
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
            let alpha = XFieldElement::new([sponge_output[1], sponge_output[2], sponge_output[3]]);
            let beta = -XFieldElement::new([sponge_output[4], sponge_output[5], sponge_output[6]]);
            let gamma = XFieldElement::new([sponge_output[7], sponge_output[8], sponge_output[9]]);
            println!("alpha {alpha}");
            println!("-beta {}", -beta);
            println!("gamma {}", -gamma);

            let num_leafs = 1 << tree_height;
            let mut p = XFieldElement::one();
            for (leaf_idx, leaf) in indexed_leafs.iter().copied().rev() {
                let leaf_idx_as_bfe = bfe!(leaf_idx);
                let node_idx_as_bfe = leaf_idx_as_bfe + bfe!(num_leafs);

                let leaf_as_xfe = digest_to_xfe(leaf, alpha);

                println!("gamma * node_idx_as_bfe = {}", gamma * node_idx_as_bfe);
                println!("leaf_as_xfe = {}", leaf_as_xfe);

                let fact = leaf_as_xfe - beta + gamma * node_idx_as_bfe;
                println!("indexed-leaf fact: {fact}");
                p *= fact;
            }

            println!("Rust-shadow, p, after indexed-leafs absorption: {p}");

            let mut prev = 0u64;
            let mut individual_tokens: VecDeque<BFieldElement> =
                nondeterminism.individual_tokens.to_owned().into();
            for auth_struct_elem in auth_struct.iter().copied().rev() {
                let auth_struct_elem_node_index_hi: u32 =
                    individual_tokens.pop_front().unwrap().try_into().unwrap();
                let auth_struct_elem_node_index_lo: u32 =
                    individual_tokens.pop_front().unwrap().try_into().unwrap();
                let auth_struct_elem_node_index = ((auth_struct_elem_node_index_hi as u64) << 32)
                    + auth_struct_elem_node_index_lo as u64;
                println!("auth_struct_elem_node_index: {auth_struct_elem_node_index}");
                assert!(auth_struct_elem_node_index > prev);
                prev = auth_struct_elem_node_index;

                let auth_struct_index_as_bfe = bfe!(auth_struct_elem_node_index);

                let auth_struct_elem_xfe = digest_to_xfe(auth_struct_elem, alpha);
                let fact = auth_struct_elem_xfe - beta + gamma * auth_struct_index_as_bfe;

                println!("auth struct fact: {fact}");
                p *= fact;
            }

            println!("Rust-shadow, p, after auth-struct absorption: {p}");

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            // TODO: use real `mmr_authentication_struct` code here
            let tree_height = rng.gen_range(1..5);
            let leaf_count = 1 << tree_height;
            let num_revealed_leafs = rng.gen_range(1..leaf_count);
            let revealed_leaf_indices = (0..num_revealed_leafs)
                .map(|_| rng.gen_range(0..leaf_count))
                .unique()
                .collect_vec();
            let leafs = (0..leaf_count).map(|_| rng.gen()).collect_vec();
            let tree = MerkleTree::<Tip5>::new::<CpuParallel>(&leafs).unwrap();
            let mmr_authentication_struct =
                mmr_authentication_struct::AuthStructIntegrityProof::new_from_merkle_tree(
                    &tree,
                    revealed_leaf_indices,
                );

            let mut memory = HashMap::new();
            let authentication_structure_ptr = rng.gen();
            let indexed_leafs_ptr = rng.gen();

            println!(
                "indexed_leafs.len(): {}",
                mmr_authentication_struct.indexed_leafs.len()
            );
            println!(
                "authentication_structure.len(): {}",
                mmr_authentication_struct.auth_struct.len()
            );

            list_insert(
                authentication_structure_ptr,
                mmr_authentication_struct.auth_struct.clone(),
                &mut memory,
            );
            list_insert(
                indexed_leafs_ptr,
                mmr_authentication_struct.indexed_leafs,
                &mut memory,
            );

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![
                    bfe!(tree_height),
                    authentication_structure_ptr,
                    indexed_leafs_ptr,
                ],
            ]
            .concat();

            println!(
                "node indices: {}",
                mmr_authentication_struct
                    .witness
                    .nd_auth_struct_indices
                    .iter()
                    .join(", ")
            );
            let individual_tokens = mmr_authentication_struct
                .witness
                .nd_auth_struct_indices
                .into_iter()
                .rev()
                .flat_map(|node_index| node_index.encode().into_iter().rev().collect_vec())
                .collect_vec();
            println!("individual_tokens: {}", individual_tokens.iter().join(", "));
            let nondeterminism = NonDeterminism::new(individual_tokens).with_ram(memory);
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

// TODO: Use this logic from `twenty-first` instead!
mod mmr_authentication_struct {
    use std::collections::{HashMap, HashSet};

    use itertools::Itertools;
    use num::One;
    use twenty_first::{
        prelude::{AlgebraicHasher, Inverse, MerkleTree, Mmr, MmrMembershipProof, Sponge},
        util_types::mmr::{
            mmr_accumulator::MmrAccumulator, shared_advanced::get_peak_heights,
            shared_basic::leaf_index_to_mt_index_and_peak_index,
        },
    };

    use super::*;

    const ROOT_MT_INDEX: u64 = 1;

    /// A witness to facilitate the proving of the authenticity of a Merkle
    /// authentication struct.
    #[derive(Debug, Clone)]
    pub struct AuthStructIntegrityProof {
        // All indices are Merkle tree node indices
        pub nd_auth_struct_indices: Vec<u64>,
        pub nd_sibling_indices: Vec<(u64, u64)>,
        pub nd_siblings: Vec<(Digest, Digest)>,
    }

    /// An authentication structure that can be used to prove membership of a list
    /// of leaves in a Merkle tree, along with the indexed leaves in question, and
    /// the witness necessary to prove membership in a ZK program.
    #[derive(Debug, Clone)]
    pub struct AuthenticatedMerkleAuthStruct {
        pub auth_struct: Vec<Digest>,
        pub indexed_leafs: Vec<(u64, Digest)>,
        pub witness: AuthStructIntegrityProof,
    }

    impl AuthStructIntegrityProof {
        /// Return the Merkle tree node indices of the digests required to prove
        /// membership for the specified leaf indices, as well as the node indices
        /// that can be derived from the leaf indices and their authentication
        /// path.
        fn auth_struct_and_nd_indices(
            num_leafs: u64,
            leaf_indices: &[u64],
        ) -> (Vec<u64>, Vec<(u64, u64)>) {
            // The set of indices of nodes that need to be included in the authentications
            // structure. In principle, every node of every authentication path is needed.
            // The root is never needed. Hence, it is not considered below.
            let mut node_is_needed = HashSet::new();

            // The set of indices of nodes that can be computed from other nodes in the
            // authentication structure or the leafs that are explicitly supplied during
            // verification. Every node on the direct path from the leaf to the root can
            // be computed by the very nature of “authentication path”.
            let mut node_can_be_computed = HashSet::new();

            for &leaf_index in leaf_indices {
                assert!(num_leafs > leaf_index, "Leaf index must be less than number of leafs. Got leaf_index = {leaf_index}; num_leafs = {num_leafs}");

                let mut node_index = leaf_index + num_leafs;
                while node_index > ROOT_MT_INDEX {
                    let sibling_index = node_index ^ 1;
                    node_can_be_computed.insert(node_index);
                    node_is_needed.insert(sibling_index);
                    node_index /= 2;
                }
            }

            let set_difference = node_is_needed.difference(&node_can_be_computed).copied();
            let set_union = node_is_needed
                .union(&node_can_be_computed)
                .sorted_unstable()
                .rev();

            let mut set_union = set_union.peekable();

            let mut set_union_as_ordered_pairs = Vec::new();
            while set_union.peek().is_some() {
                let right_index = *set_union.next().unwrap();

                // Crashes on odd-length of input list, which is what we want, as
                // this acts as a sanity check.
                let left_index = *set_union.next().unwrap();
                set_union_as_ordered_pairs.push((left_index, right_index));
            }

            (
                set_difference.sorted_unstable().rev().collect(),
                set_union_as_ordered_pairs,
            )
        }

        pub fn root_from_authentication_struct(
            &self,
            tree_height: u32,
            auth_struct: Vec<Digest>,
            indexed_leafs: Vec<(u64, Digest)>,
        ) -> Digest {
            fn digest_to_xfe(digest: Digest, challenge: XFieldElement) -> XFieldElement {
                let leaf_xfe_lo = XFieldElement::new([digest.0[0], digest.0[1], digest.0[2]]);
                let leaf_xfe_hi = challenge
                    * XFieldElement::new([digest.0[3], digest.0[4], BFieldElement::one()]);

                leaf_xfe_lo + leaf_xfe_hi
            }

            fn node_index_to_bfe(node_index: u64) -> BFieldElement {
                BFieldElement::new(node_index)
            }

            // Sanity check
            assert_eq!(
                self.nd_auth_struct_indices.len(),
                auth_struct.len(),
                "Provided auth struct length must match that specified in receiver"
            );

            // Get challenges
            let (alpha, beta, gamma) = {
                let mut sponge = Tip5::init();
                sponge.pad_and_absorb_all(&indexed_leafs.encode());
                sponge.pad_and_absorb_all(&auth_struct.encode());
                let challenges = sponge.sample_scalars(3);
                (challenges[0], challenges[1], challenges[2])
            };

            // Accumulate `p` from public data
            let mut p = XFieldElement::one();
            for i in (0..indexed_leafs.len()).rev() {
                let node_index_as_bfe = node_index_to_bfe((1 << tree_height) ^ indexed_leafs[i].0);
                let leaf_as_xfe = digest_to_xfe(indexed_leafs[i].1, alpha);
                let fact = leaf_as_xfe - beta + gamma * node_index_as_bfe;
                p *= fact;
            }

            let mut prev = 0;
            for i in (0..auth_struct.len()).rev() {
                let auth_struct_index = self.nd_auth_struct_indices[i];

                // `auth_struct` must be sorted high-to-low by node-index. But since
                // we're traversing in reverse order, the inequality is flipped.
                assert!(auth_struct_index > prev);
                prev = auth_struct_index;

                let auth_struct_index_as_bfe = node_index_to_bfe(auth_struct_index);

                let auth_str_elem_as_xfe = digest_to_xfe(auth_struct[i], alpha);
                let fact = auth_str_elem_as_xfe - beta + gamma * auth_struct_index_as_bfe;
                p *= fact;
            }

            // Use secret data to invert `p` back and to calculate the root
            let mut t = auth_struct
                .first()
                .copied()
                .unwrap_or_else(|| indexed_leafs.first().unwrap().1);
            let mut t_xfe = digest_to_xfe(t, alpha);
            let mut parent_index_bfe = BFieldElement::one();
            for ((l, r), (left_index, right_index)) in self
                .nd_siblings
                .iter()
                .zip_eq(self.nd_sibling_indices.clone())
            {
                assert_eq!(left_index + 1, right_index);

                t = Tip5::hash_pair(*l, *r);

                let l_xfe = digest_to_xfe(*l, alpha);
                let r_xfe = digest_to_xfe(*r, alpha);
                t_xfe = digest_to_xfe(t, alpha);

                let left_index_bfe = node_index_to_bfe(left_index);
                let right_index_bfe = node_index_to_bfe(right_index);
                parent_index_bfe = left_index_bfe / bfe!(2);

                let fact1 = l_xfe - beta + gamma * left_index_bfe;
                let fact2 = r_xfe - beta + gamma * right_index_bfe;
                let fact_parent = t_xfe - beta + gamma * parent_index_bfe;

                p *= fact1.inverse() * fact2.inverse() * fact_parent;
            }

            assert_eq!(t_xfe - beta + gamma, p);
            assert!(parent_index_bfe.is_one());

            t
        }

        /// Return the authentication structure authenticity witness,
        /// authentication structure, and the (leaf-index, leaf-digest) pairs
        /// from a list of MMR membership proofs. All MMR membership proofs must
        /// belong under the same peak, i.e., be part of the same Merkle tree in
        /// the list of Merkle trees that the MMR contains.
        ///
        /// Panics if the input list of MMR-membership proofs is empty, or if they
        /// do not all belong under the same peak.
        pub fn new_from_mmr_membership_proofs(
            mmra: &MmrAccumulator,
            indexed_mmr_mps: Vec<(u64, Digest, MmrMembershipProof)>,
        ) -> HashMap<u32, AuthenticatedMerkleAuthStruct> {
            #[derive(Clone, Debug)]
            struct IndexedAuthenticatedMmrLeaf {
                merkle_tree_node_index: u64,
                merkle_tree_leaf_index: u64,
                leaf_digest: Digest,
                membership_proof: MmrMembershipProof,
            }

            // Split indexed MMR-mps into a hashmap with one entry for each
            // referenced peak in the MMR.
            let num_mmr_leafs = mmra.num_leafs();
            let mut peak_index_to_indexed_mmr_mp: HashMap<u32, Vec<IndexedAuthenticatedMmrLeaf>> =
                HashMap::default();
            let peak_heights = get_peak_heights(num_mmr_leafs);
            for (mmr_leaf_index, leaf, mmr_mp) in indexed_mmr_mps {
                let (mt_index, peak_index) =
                    leaf_index_to_mt_index_and_peak_index(mmr_leaf_index, num_mmr_leafs);
                let peak_index_as_usize: usize = peak_index.try_into().unwrap();
                let num_leafs_local_mt = 1 << peak_heights[peak_index_as_usize];
                let mt_leaf_index = mt_index - num_leafs_local_mt;
                peak_index_to_indexed_mmr_mp
                    .entry(peak_index)
                    .or_default()
                    .push(IndexedAuthenticatedMmrLeaf {
                        merkle_tree_node_index: mt_index,
                        merkle_tree_leaf_index: mt_leaf_index,
                        leaf_digest: leaf,
                        membership_proof: mmr_mp,
                    });
            }

            // Loop over all peaks and collect an authentication witness struct
            // for each peak.
            let mut peak_index_to_authenticated_auth_struct = HashMap::default();
            for (peak_index, indexed_mmr_mp_structs) in peak_index_to_indexed_mmr_mp {
                let peak_index_as_usize: usize = peak_index.try_into().unwrap();
                let num_leafs_in_local_mt = 1 << peak_heights[peak_index_as_usize];
                let local_mt_leaf_indices = indexed_mmr_mp_structs
                    .iter()
                    .map(|x| x.merkle_tree_leaf_index)
                    .collect_vec();

                let (nd_auth_struct_indices, nd_sibling_indices) =
                    Self::auth_struct_and_nd_indices(num_leafs_in_local_mt, &local_mt_leaf_indices);
                let peak = mmra.peaks()[peak_index_as_usize];

                let mut node_digests: HashMap<u64, Digest> = HashMap::default();
                node_digests.insert(ROOT_MT_INDEX, peak);

                // Loop over all indexed leafs for this peak
                for indexed_mmr_mp in indexed_mmr_mp_structs.iter() {
                    let mut mt_node_index = indexed_mmr_mp.merkle_tree_node_index;
                    let mut node = indexed_mmr_mp.leaf_digest;

                    // Loop over all authentication path elements for this indexed leaf
                    for ap_elem in indexed_mmr_mp.membership_proof.authentication_path.iter() {
                        node_digests.insert(mt_node_index, node);
                        node_digests.insert(mt_node_index ^ 1, *ap_elem);
                        node = if mt_node_index & 1 == 0 {
                            Tip5::hash_pair(node, *ap_elem)
                        } else {
                            Tip5::hash_pair(*ap_elem, node)
                        };

                        mt_node_index /= 2;
                    }

                    // Sanity check that MMR-MPs are valid
                    assert_eq!(peak, node, "Derived peak must match provided peak");
                }
                let nd_siblings = nd_sibling_indices
                    .iter()
                    .map(|(left_idx, right_idx)| (node_digests[left_idx], node_digests[right_idx]))
                    .collect_vec();
                let auth_struct = nd_auth_struct_indices
                    .iter()
                    .map(|idx| node_digests[idx])
                    .collect_vec();
                let indexed_leafs = indexed_mmr_mp_structs
                    .into_iter()
                    .map(|indexed_mmr_mp| {
                        (
                            indexed_mmr_mp.merkle_tree_leaf_index,
                            indexed_mmr_mp.leaf_digest,
                        )
                    })
                    .collect_vec();

                let witness = Self {
                    nd_auth_struct_indices,
                    nd_sibling_indices,
                    nd_siblings,
                };

                peak_index_to_authenticated_auth_struct.insert(
                    peak_index,
                    AuthenticatedMerkleAuthStruct {
                        auth_struct,
                        indexed_leafs,
                        witness,
                    },
                );
            }

            peak_index_to_authenticated_auth_struct
        }

        /// Return the authentication structure witness, authentication structure,
        /// and the (leaf-index, leaf-digest) pairs.
        pub fn new_from_merkle_tree(
            tree: &MerkleTree<Tip5>,
            mut revealed_leaf_indices: Vec<u64>,
        ) -> AuthenticatedMerkleAuthStruct {
            revealed_leaf_indices.sort_unstable();
            revealed_leaf_indices.dedup();
            revealed_leaf_indices.reverse();
            let num_leafs: u64 = tree.num_leafs() as u64;

            let (mut nd_auth_struct_indices, nd_sibling_indices) =
                Self::auth_struct_and_nd_indices(num_leafs, &revealed_leaf_indices);
            if revealed_leaf_indices.is_empty() {
                nd_auth_struct_indices = vec![ROOT_MT_INDEX];
            }

            let nd_siblings = nd_sibling_indices
                .iter()
                .map(|&(l, r)| {
                    let l: usize = l.try_into().unwrap();
                    let r: usize = r.try_into().unwrap();
                    (tree.node(l).unwrap(), tree.node(r).unwrap())
                })
                .collect_vec();

            let revealed_leafs = revealed_leaf_indices
                .iter()
                .map(|j| tree.node((*j + num_leafs) as usize).unwrap())
                .collect_vec();
            let indexed_leafs = revealed_leaf_indices
                .clone()
                .into_iter()
                .zip_eq(revealed_leafs)
                .collect_vec();

            let auth_struct = nd_auth_struct_indices
                .iter()
                .map(|node_index| tree.node(*node_index as usize).unwrap())
                .collect_vec();

            let witness = Self {
                nd_auth_struct_indices,
                nd_sibling_indices,
                nd_siblings,
            };

            AuthenticatedMerkleAuthStruct {
                auth_struct,
                indexed_leafs,
                witness,
            }
        }
    }
}
