use triton_vm::prelude::*;

use crate::prelude::*;

/// `merkle_step` but for index of type `u64`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MerkleStepU64Index;

impl BasicSnippet for MerkleStepU64Index {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "Merkle tree node index".to_owned()),
            (DataType::Digest, "node digest".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U64, "Merkle tree node index".to_owned()),
            (DataType::Digest, "parent digest".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_merkle_step_u64_index".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                            // _ mt_index_hi mt_index_lo       [digest'; 5]
                merkle_step // _ mt_index_hi (mt_index_lo / 2) [digest; 5]
                push 2      // _ mt_index_hi (mt_index_lo / 2) [digest; 5] 2
                push 0      // _ mt_index_hi (mt_index_lo / 2) [digest; 5] 2 0
                swap 8      // _ 0 (mt_index_lo / 2) [digest; 5] 2 mt_index_hi
                div_mod     // _ 0 (mt_index_lo / 2) [digest; 5] (mt_index_hi / 2) (mt_index_hi % 2)

                push {1u32 << 31}
                hint two_pow_31: u32 = stack[0]
                mul         // _ 0 (mt_index_lo / 2) [digest; 5] (mt_index_hi / 2) carry
                swap 1      // _ 0 (mt_index_lo / 2) [digest; 5] carry (mt_index_hi / 2)
                swap 8      // _ (mt_index / 2)_hi (mt_index_lo / 2) [digest; 5] carry 0
                swap 7      // _ (mt_index / 2)_hi 0 [digest; 5] carry (mt_index_lo / 2)
                add         // _ (mt_index / 2)_hi 0 [digest; 5] (mt_index / 2)_lo
                swap 6      // _ (mt_index / 2)_hi (mt_index / 2)_lo [digest; 5] 0
                pop 1       // _ (mt_index / 2)_hi (mt_index / 2)_lo [digest; 5]
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    impl Procedure for MerkleStepU64Index {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            _: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
            _: &[BFieldElement],
            _: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let stack_digest = pop_encodable::<Digest>(stack);
            let ap_digest = nondeterminism.digests[0];

            let leaf_index = pop_encodable::<u64>(stack);
            let stack_digest_is_left_sibling = leaf_index % 2 == 0;
            let (left_digest, right_digest) = if stack_digest_is_left_sibling {
                (stack_digest, ap_digest)
            } else {
                (ap_digest, stack_digest)
            };
            let parent_digest = Tip5::hash_pair(left_digest, right_digest);

            let parent_index = leaf_index / 2;

            push_encodable(stack, &parent_index);
            push_encodable(stack, &parent_digest);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);

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

    impl MerkleStepU64Index {
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

    #[test]
    fn prop() {
        for _ in 0..10 {
            ShadowedProcedure::new(MerkleStepU64Index).test();
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
            let shadowed_procedure = ShadowedProcedure::new(MerkleStepU64Index);
            let (init_stack, non_determinism) =
                MerkleStepU64Index.prepare_stack_and_non_determinism(mt_index);
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

            // Pop and ignore digest
            for _ in 0..Digest::LEN {
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

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(MerkleStepU64Index).bench();
    }
}
