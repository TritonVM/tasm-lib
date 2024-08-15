use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::prelude::Inverse;

use crate::data_type::DataType;
use crate::library::Library;
use crate::mmr::authentication_struct::derive_challenges::DeriveChallenges;
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
        let alpha_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let alpha_challenge_pointer_read =
            alpha_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);
        let beta_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let beta_challenge_pointer_read =
            beta_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);
        let gamma_challenge_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let gamma_challenge_pointer_read =
            gamma_challenge_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);
        let t_digest_pointer_write = library.kmalloc(Digest::LEN as u32);
        let t_digest_pointer_read = t_digest_pointer_write + bfe!(Digest::LEN as u64 - 1);
        let p_pointer_write = library.kmalloc(EXTENSION_DEGREE as u32);
        let p_pointer_read = p_pointer_write + bfe!(EXTENSION_DEGREE as u64 - 1);

        let indexed_leaf_element_size = Self::indexed_leaf_element_type().stack_size();
        let derive_challenges = library.import(Box::new(DeriveChallenges));
        let calculate_and_store_challenges = triton_asm!(
            // _ *auth_struct *indexed_leafs

            call {derive_challenges}
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

                // TODO: Assert that `leaf_idx < num_leafs`?

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

            /* Disallow empty list of indexed leafs */
            dup 0
            push 0
            eq
            push 0
            eq
            assert
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

        let nd_loop_label = format!("{entrypoint}_nd_loop");
        let dup_top_two_digests = triton_asm![dup 9; Digest::LEN * 2];
        let dup_top_digest = triton_asm![dup 4; Digest::LEN];
        let one_half = BFieldElement::new(2).inverse();
        let nd_loop = triton_asm!(
            // _ INVARIANT: _
            {nd_loop_label}:
                divine 2
                // _ left_index right_index

                dup 1
                push 1
                add
                eq
                // _ l_index_bfe (left_index + 1 == right_index)

                assert
                // _ l_index_bfe

                /* Update parent index */
                push {one_half}
                mul
                hint parent_index: BFieldElement = stack[0..1]
                // _ (l_index_bfe / 2)
                // _ parent_index <-- rename

                /* Calculate parent digest, preserving child digests */
                divine {Digest::LEN}
                hint right: Digest = stack[0..5]

                divine {Digest::LEN}
                hint left: Digest = stack[0..5]

                {&dup_top_two_digests}
                hash
                hint t: Digest = stack[0..5]
                // _ parent_index [right] [left] [t]

                {&dup_top_digest}
                push {t_digest_pointer_write}
                write_mem {Digest::LEN}
                pop 1
                // _ parent_index [right] [left] [t]

                {&digest_to_xfe}
                hint t_xfe: XFieldElement = stack[0..3]
                // _ parent_index [right] [left] [t_xfe]

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ parent_index [right] [left] [t_xfe - β]

                dup 13
                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index [right] [left] [t_xfe - β] parent_index [γ]

                swap 1
                swap 2
                swap 3
                xb_mul
                // _ parent_index [right] [left] [t_xfe - β] [γ * parent_index]

                xx_add
                // _ parent_index [right] [left] [t_xfe - β + γ * parent_index]
                // _ parent_index [right] [left] [fact_parent] <-- rename

                /* Accumulate `fact_parent` into `p` */
                push {p_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_mul
                push {p_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index [right] [left]

                /* Claculate `fact_1` */
                {&digest_to_xfe}
                // _ parent_index [right] [left_xfe]

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ parent_index [right] [left_xfe - β]

                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index [right] [left_xfe - β] [γ]

                dup 11
                push 2
                mul
                // _ parent_index [right] [left_xfe - β] [γ] left_index

                xb_mul
                // _ parent_index [right] [left_xfe - β] [γ * left_index]

                xx_add
                // _ parent_index [right] [left_xfe - β + γ * left_index]
                // _ parent_index [right] [fact_1] <-- rename

                x_invert
                // _ parent_index [right] [fact_1^{-1}]

                /* Divide `fact_1` out of `p` */
                push {p_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_mul
                push {p_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index [right]

                /* Calculate `fact_2` */
                {&digest_to_xfe}
                // _ parent_index [right_xfe]

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ parent_index [right_xfe - β]

                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index [right_xfe - β] [γ]

                dup 6
                push 2
                mul
                push 1
                add
                // _ parent_index [right_xfe - β] [γ] right_index

                xb_mul
                // _ parent_index [right_xfe - β] [right_index * γ]

                xx_add
                // _ parent_index [right_xfe - β + right_index * γ]
                // _ parent_index [fact_2] <-- rename

                x_invert
                // _ parent_index [fact_2^{-1}]

                /* Divide `fact_2` out of `p` */
                push {p_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_mul
                push {p_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 1
                // _ parent_index

                push 1
                eq
                skiz
                    return

                recurse
        );

        let compare_xfes = DataType::Xfe.compare();
        triton_asm!(
            {entrypoint}:
                // _ tree_height *auth_struct *indexed_leafs

                dup 1
                dup 1
                // _ tree_height *auth_struct *indexed_leafs *auth_struct *indexed_leafs

                {&calculate_and_store_challenges}
                // _ tree_height *auth_struct *indexed_leafs

                /* Calculate number of leafs in Merkle tree
                   Notice that `tree_num_leafs` is not necessarily a u32, but is a
                   BFE and a power of two whose log_2 value is in the range
                   [0,63] */
                swap 2
                push 2
                pow
                swap 2
                hint tree_num_leafs: BFieldElement = stack[2]
                // _ tree_num_leafs *auth_struct *indexed_leafs

                {&accumulate_indexed_leafs_from_public_data}
                // _ tree_num_leafs *auth_struct *indexed_leafs *indexed_leafs [garbage; 2] [p; 3]
                // _ tree_num_leafs *auth_struct *indexed_leafs *indexed_leafs [garbage; 2] p2 p1 p0 <-- rename

                /* Prepare for next loop, absorption of auth-struct digests into accumulator */
                swap 7
                swap 6
                pop 1
                // _ tree_num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1

                dup 5
                read_mem 1
                push 1
                add
                swap 1
                // _ tree_num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1 *auth_struct auth_struct_len

                push {Digest::LEN}
                mul
                add
                // _ tree_num_leafs p0 *auth_struct *indexed_leafs [0; 2] p2 p1 *auth_struct_last_word

                swap 5
                swap 7
                // _ tree_num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [0; 2] p2 p1 p0
                // _ tree_num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p] <-- rename

                dup 6
                dup 6
                eq
                push 0
                eq
                // _ tree_num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p] (*auth_struct_last_word != *auth_struct)

                skiz
                    call {accumulate_auth_struct_leafs_from_public_data_label}
                // _ tree_num_leafs *indexed_leafs *auth_struct *auth_struct_last_word [prev; 2] [p]

                /* Cleanup stack before next loop */
                swap 4
                pop 1
                swap 4
                pop 1
                swap 4
                pop 2
                // _ tree_num_leafs *indexed_leafs [p]

                /* Set initial t values, from indexed_leafs[0] */
                dup 3
                push {Digest::LEN}
                add
                read_mem {Digest::LEN}
                pop 1
                // _ tree_num_leafs *indexed_leafs [p] [t; 5]

                /* Write t value, and `p` to static memory */
                push {t_digest_pointer_write}
                write_mem {Digest::LEN}
                pop 1
                // _ tree_num_leafs *indexed_leafs [p]

                push {p_pointer_write}
                write_mem {EXTENSION_DEGREE}
                pop 2
                // _ tree_num_leafs

                /* Call the ND-loop if tree_num_leafs != 1 */
                push 1
                eq
                push 0
                eq
                // (tree_num_leafs != 1)

                skiz
                    call {nd_loop_label}
                // _

                /* Assert that p == t_xfe - beta + gamma */
                push {p_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                // _ [p]

                push {t_digest_pointer_read}
                read_mem {Digest::LEN}
                pop 1
                {&digest_to_xfe}
                // _ [p] [t_xfe]

                push {beta_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ [p] [t_xfe - β]

                push {gamma_challenge_pointer_read}
                read_mem {EXTENSION_DEGREE}
                pop 1
                xx_add
                // _ [p] [t_xfe - β + γ]

                {&compare_xfes}
                // _ (p == t_xfe - β + γ)

                assert
                // _

                /* Return `t` (digest) */
                push {t_digest_pointer_read}
                read_mem {Digest::LEN}
                pop 1
                // _ [t]

                return

                {&accumulated_indexed_leafs_loop}
                {&accumulate_auth_struct_leafs_from_public_data}
                {&nd_loop}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::min;
    use std::collections::HashMap;
    use std::collections::VecDeque;

    use itertools::Itertools;
    use num::One;
    use num::Zero;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use twenty_first::prelude::AlgebraicHasher;
    use twenty_first::prelude::Sponge;
    use twenty_first::util_types::mmr::mmr_accumulator::util::mmra_with_mps;

    use crate::mmr::authentication_struct::shared::AuthStructIntegrityProof;
    use crate::rust_shadowing_helper_functions::input::read_digest_from_input;
    use crate::rust_shadowing_helper_functions::list::list_insert;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::rust_shadowing_helper_functions::memory::write_to_memory;
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
            _public_input: &[BFieldElement],
            sponge: &mut Option<VmHasher>,
        ) -> Vec<BFieldElement> {
            fn digest_to_xfe(digest: Digest, challenge: XFieldElement) -> XFieldElement {
                let [l0, l1, l2, l3, l4] = digest.0;
                let leaf_xfe_lo = XFieldElement::new([BFieldElement::new(1), l0, l1]);
                let leaf_xfe_hi = XFieldElement::new([l2, l3, l4]);

                challenge * leaf_xfe_lo + leaf_xfe_hi
            }

            fn mimic_use_of_static_memory(
                memory: &mut HashMap<BFieldElement, BFieldElement>,
                alpha: XFieldElement,
                beta: XFieldElement,
                gamma: XFieldElement,
                t: Digest,
                p: XFieldElement,
            ) {
                const ALPHA_POINTER_WRITE: BFieldElement = BFieldElement::new(BFieldElement::P - 4);
                const BETA_POINTER_WRITE: BFieldElement = BFieldElement::new(BFieldElement::P - 7);
                const GAMMA_POINTER_WRITE: BFieldElement =
                    BFieldElement::new(BFieldElement::P - 10);
                const T_DIGEST_POINTER_WRITE: BFieldElement =
                    BFieldElement::new(BFieldElement::P - 15);
                const P_POINTER_WRITE: BFieldElement = BFieldElement::new(BFieldElement::P - 18);

                write_to_memory(ALPHA_POINTER_WRITE, alpha, memory);
                write_to_memory(BETA_POINTER_WRITE, beta, memory);
                write_to_memory(GAMMA_POINTER_WRITE, gamma, memory);
                write_to_memory(T_DIGEST_POINTER_WRITE, t, memory);
                write_to_memory(P_POINTER_WRITE, p, memory);
            }

            fn accumulate_indexed_leafs(
                indexed_leafs: &[(u64, Digest)],
                alpha: XFieldElement,
                beta: XFieldElement,
                gamma: XFieldElement,
                tree_num_leafs: u64,
            ) -> XFieldElement {
                let mut p = XFieldElement::one();
                for (leaf_idx, leaf) in indexed_leafs.iter().copied().rev() {
                    let leaf_idx_as_bfe = bfe!(leaf_idx);
                    let node_idx_as_bfe = leaf_idx_as_bfe + bfe!(tree_num_leafs);
                    let leaf_as_xfe = digest_to_xfe(leaf, alpha);
                    let fact = leaf_as_xfe - beta + gamma * node_idx_as_bfe;
                    p *= fact;
                }

                p
            }

            fn accumulate_auth_struct(
                mut p: XFieldElement,
                auth_struct: Vec<Digest>,
                individual_tokens: &mut VecDeque<BFieldElement>,
                alpha: XFieldElement,
                beta: XFieldElement,
                gamma: XFieldElement,
            ) -> XFieldElement {
                let mut prev = 0u64;

                for auth_struct_elem in auth_struct.iter().copied().rev() {
                    let auth_struct_elem_node_index_hi: u32 =
                        individual_tokens.pop_front().unwrap().try_into().unwrap();
                    let auth_struct_elem_node_index_lo: u32 =
                        individual_tokens.pop_front().unwrap().try_into().unwrap();
                    let auth_struct_elem_node_index = ((auth_struct_elem_node_index_hi as u64)
                        << 32)
                        + auth_struct_elem_node_index_lo as u64;
                    assert!(auth_struct_elem_node_index > prev);
                    prev = auth_struct_elem_node_index;

                    let auth_struct_index_as_bfe = bfe!(auth_struct_elem_node_index);

                    let auth_struct_elem_xfe = digest_to_xfe(auth_struct_elem, alpha);
                    let fact = auth_struct_elem_xfe - beta + gamma * auth_struct_index_as_bfe;

                    p *= fact;
                }

                p
            }

            assert_eq!(
                SIZE_OF_INDEXED_LEAFS_ELEMENT,
                Self::indexed_leaf_element_type().stack_size()
            );

            // declare input-arguments
            let indexed_leafs_pointer = stack.pop().unwrap();
            let auth_struct_pointer = stack.pop().unwrap();
            let tree_height: u32 = stack.pop().unwrap().try_into().unwrap();

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

            // Calculate challenges
            let sponge = sponge.as_mut().expect("sponge must be initialized");
            sponge.pad_and_absorb_all(&indexed_leafs.encode());
            sponge.pad_and_absorb_all(&auth_struct.encode());

            let sponge_output = sponge.squeeze();
            let alpha = XFieldElement::new([sponge_output[1], sponge_output[2], sponge_output[3]]);
            let beta = -XFieldElement::new([sponge_output[4], sponge_output[5], sponge_output[6]]);
            let gamma = XFieldElement::new([sponge_output[7], sponge_output[8], sponge_output[9]]);

            let tree_num_leafs = 1u64 << tree_height;

            // Accumulate into `p` from public data
            let mut p =
                accumulate_indexed_leafs(&indexed_leafs, alpha, beta, gamma, tree_num_leafs);

            let mut individual_tokens: VecDeque<BFieldElement> =
                nondeterminism.individual_tokens.to_owned().into();
            p = accumulate_auth_struct(p, auth_struct, &mut individual_tokens, alpha, beta, gamma);

            // "Unaccumulate" into `p` from secret data, and calculate Merkle root
            let mut t = indexed_leafs[0].1;
            let mut t_xfe = digest_to_xfe(t, alpha);
            if tree_num_leafs != 1 {
                loop {
                    let left_index = individual_tokens.pop_front().unwrap();
                    let right_index = individual_tokens.pop_front().unwrap();
                    assert_eq!(left_index + bfe!(1), right_index);

                    let parent_index = left_index / bfe!(2);

                    let right = read_digest_from_input(&mut individual_tokens);
                    let left = read_digest_from_input(&mut individual_tokens);

                    t = Tip5::hash_pair(left, right);
                    t_xfe = digest_to_xfe(t, alpha);
                    let l_xfe = digest_to_xfe(left, alpha);
                    let r_xfe = digest_to_xfe(right, alpha);
                    let fact1 = l_xfe - beta + gamma * left_index;
                    let fact2 = r_xfe - beta + gamma * right_index;
                    let fact_parent = t_xfe - beta + gamma * parent_index;

                    p *= fact1.inverse() * fact2.inverse() * fact_parent;

                    if parent_index.is_one() {
                        break;
                    }
                }
            }

            assert_eq!(t_xfe - beta + gamma, p);

            // Return the Merkle root on the stack
            for elem in t.encode().into_iter().rev() {
                stack.push(elem);
            }

            mimic_use_of_static_memory(memory, alpha, -beta, gamma, t, p);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let (tree_height, num_revealed_leafs) = match bench_case {
                None => {
                    let tree_height = rng.gen_range(0..32);
                    let num_leafs_in_merkle_tree = 1 << tree_height;
                    let num_revealed_leafs = rng.gen_range(1..=min(num_leafs_in_merkle_tree, 20));
                    (tree_height, num_revealed_leafs)
                }
                Some(BenchmarkCase::CommonCase) => (32, 20),
                Some(BenchmarkCase::WorstCase) => (62, 20),
            };
            let num_leafs_in_merkle_tree = 1 << tree_height;

            let revealed_leaf_indices = (0..num_revealed_leafs)
                .map(|_| rng.gen_range(0..num_leafs_in_merkle_tree))
                .unique()
                .collect_vec();
            let num_revealed_leafs = revealed_leaf_indices.len();
            assert!(!num_revealed_leafs.is_zero());

            let revealed_leafs: Vec<Digest> =
                (0..num_revealed_leafs).map(|_| rng.gen()).collect_vec();
            let indexed_leafs = revealed_leaf_indices
                .into_iter()
                .zip_eq(revealed_leafs)
                .collect_vec();

            let (mmra, mps) = mmra_with_mps(num_leafs_in_merkle_tree, indexed_leafs.clone());
            let indexed_mmr_mps = indexed_leafs
                .into_iter()
                .zip_eq(mps)
                .map(|((idx, leaf), mp)| (idx, leaf, mp))
                .collect_vec();

            let mmr_authentication_struct =
                AuthStructIntegrityProof::new_from_mmr_membership_proofs(&mmra, indexed_mmr_mps);
            assert!(mmr_authentication_struct.len().is_one());
            let mmr_authentication_struct = &mmr_authentication_struct[&0];

            let mut memory = HashMap::new();
            let authentication_structure_ptr = rng.gen();
            let indexed_leafs_ptr = rng.gen();

            list_insert(
                authentication_structure_ptr,
                mmr_authentication_struct.auth_struct.clone(),
                &mut memory,
            );
            list_insert(
                indexed_leafs_ptr,
                mmr_authentication_struct.indexed_leafs.clone(),
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

            let nd_auth_struct_indices = mmr_authentication_struct
                .witness
                .nd_auth_struct_indices
                .iter()
                .rev()
                .flat_map(|node_index| node_index.encode().into_iter().rev().collect_vec())
                .collect_vec();
            let nd_loop_nd = mmr_authentication_struct
                .witness
                .nd_sibling_indices
                .iter()
                .copied()
                .zip_eq(
                    mmr_authentication_struct
                        .witness
                        .nd_siblings
                        .iter()
                        .copied(),
                )
                .flat_map(|((left_index, right_index), (left_node, right_node))| {
                    [
                        vec![bfe!(left_index), bfe!(right_index)],
                        right_node.encode().into_iter().rev().collect_vec(),
                        left_node.encode().into_iter().rev().collect_vec(),
                    ]
                    .concat()
                })
                .collect_vec();

            let individual_tokens = [nd_auth_struct_indices, nd_loop_nd].concat();
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

#[cfg(test)]
mod benches {
    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn bench_root_from_auth_struct() {
        ShadowedProcedure::new(RootFromAuthenticationStruct).bench();
    }
}
