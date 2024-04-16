use rand::random;
use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::push_encodable;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::procedure::Procedure;
use crate::traits::procedure::ProcedureInitialState;

/// `divine_sibling` but for index of type `u64`
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct DivineSiblingU64Index;

impl BasicSnippet for DivineSiblingU64Index {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "Mekle tree index".to_owned()),
            (DataType::Digest, "previous digest".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "Parent Mekle tree index".to_owned()),
            (DataType::Digest, "Right child digest".to_owned()),
            (DataType::Digest, "Left child digest".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_divine_sibling_u64_index".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        const TWO_POW_31: u32 = 1u32 << 31;
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
                divine_sibling

                push 2
                push 0
                // _ mt_index_hi (mt_index_lo / 2) [digest (right_node)] [digest (left_node)] 2 0

                swap 13
                // _ 0 (mt_index_lo / 2) [digest (right_node)] [digest (left_node)] 2 mt_index_hi

                div_mod
                // _ 0 (mt_index_lo / 2) [digest (right_node)] [digest (left_node)] (mt_index_hi / 2) (mt_index_hi % 2)

                push {TWO_POW_31}
                mul
                // _ 0 (mt_index_lo / 2) [digest (right_node)] [digest (left_node)] (mt_index_hi / 2) carry

                swap 1
                swap 13
                // _ (mt_index_hi / 2) (mt_index_lo / 2) [digest (right_node)] [digest (left_node)] carry 0

                swap 12
                // _ (mt_index_hi / 2) 0 [digest (right_node)] [digest (left_node)] carry (mt_index_lo / 2)

                add
                // _ (mt_index_hi / 2) 0 [digest (right_node)] [digest (left_node)] (mt_index / 2)_lo

                swap 11
                pop 1
                // _ (mt_index_hi / 2) (mt_index / 2)_lo [digest (right_node)] [digest (left_node)]

                return
        )
    }
}

impl Procedure for DivineSiblingU64Index {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        _memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
        nondeterminism: &NonDeterminism,
        _public_input: &[BFieldElement],
        _sponge: &mut Option<Tip5>,
    ) -> Vec<BFieldElement> {
        let stack_digest: Digest = Digest::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let ap_digest: Digest = nondeterminism.digests[0];

        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) | (leaf_index_lo as u64);
        let (left_digest, right_digest) = if leaf_index % 2 == 1 {
            (ap_digest, stack_digest)
        } else {
            (stack_digest, ap_digest)
        };

        let parent_index = leaf_index / 2;
        stack.push(BFieldElement::new(parent_index >> 32));
        stack.push(BFieldElement::new(parent_index & u32::MAX as u64));

        push_encodable(stack, &right_digest);
        push_encodable(stack, &left_digest);

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<BenchmarkCase>,
    ) -> ProcedureInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let (stack, nondeterminism) = match bench_case {
            Some(BenchmarkCase::CommonCase) => self.prepare_stack_and_non_determinism(1 << 33),
            Some(BenchmarkCase::WorstCase) => self.prepare_stack_and_non_determinism(1 << 63),
            None => self.prepare_stack_and_non_determinism(rng.gen()),
        };

        ProcedureInitialState {
            stack,
            nondeterminism,
            public_input: vec![],
            sponge: None,
        }
    }
}

impl DivineSiblingU64Index {
    fn prepare_stack_and_non_determinism(
        &self,
        leaf_index: u64,
    ) -> (Vec<BFieldElement>, NonDeterminism) {
        let mut init_stack = empty_stack();
        init_stack.push(BFieldElement::new(leaf_index >> 32));
        init_stack.push(BFieldElement::new(leaf_index & u32::MAX as u64));

        let digest: Digest = random();
        for elem in digest.values() {
            init_stack.push(elem);
        }

        (
            init_stack,
            NonDeterminism::default().with_digests(vec![random()]),
        )
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::traits::procedure::ShadowedProcedure;
    use crate::traits::rust_shadow::RustShadow;
    use crate::DIGEST_LENGTH;

    use super::*;

    #[test]
    fn prop() {
        for _ in 0..10 {
            ShadowedProcedure::new(DivineSiblingU64Index).test();
        }
    }

    #[test]
    fn unit_test() {
        prop(1 << 32, 1 << 31);
        prop(1 << 33, 1 << 32);
        prop(1 << 34, 1 << 33);
        prop((1 << 34) + (1 << 32), (1 << 33) + (1 << 31));
        prop(u64::MAX, (1u64 << 63) - 1);

        fn prop(mt_index: u64, expected_parent_index: u64) {
            let shadowed_procedure = ShadowedProcedure::new(DivineSiblingU64Index);
            let (init_stack, non_determinism) =
                DivineSiblingU64Index.prepare_stack_and_non_determinism(mt_index);
            let tasm_final_state = crate::test_helpers::tasm_final_state(
                &shadowed_procedure,
                &init_stack,
                &[],
                non_determinism,
                &None,
            );
            println!(
                "final final_stack\n{}",
                tasm_final_state.op_stack.stack.iter().join(", ")
            );

            let mut final_stack = tasm_final_state.op_stack.stack;

            // Pop and ignore digests
            for _ in 0..2 * DIGEST_LENGTH {
                final_stack.pop();
            }

            // Verify expected value of parent index
            let calculated_parent_index_lo: u32 =
                final_stack.pop().unwrap().value().try_into().unwrap();
            let calculated_parent_index_hi: u32 =
                final_stack.pop().unwrap().value().try_into().unwrap();
            let calculated_parent_index: u64 =
                calculated_parent_index_lo as u64 + ((calculated_parent_index_hi as u64) << 32);
            assert_eq!(
                expected_parent_index, calculated_parent_index,
                "TASM-calculated parent MT index must match provided expected value"
            );
            assert_eq!(
                mt_index / 2,
                calculated_parent_index,
                "TASM-calculated parent MT index must match Rust-calculated expected value"
            );
        }
    }
}
