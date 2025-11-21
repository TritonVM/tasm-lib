use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::prelude::*;

/// Verify Merkle authentication paths in a FRI context.
///
/// Verify a batch of Merkle membership claims in a FRI context where only the
/// a-indices are known and the b-indices must be calculated on the fly. This
/// snippet can be used for both a and b-indices. For a-indices the
/// `xor_bit_mask` value must be set to the domain length, and for b indices,
/// `xor_bit_mask` must be set to 3/2 times the domain length. The
/// `xor_bit_mask` is used to convert a leaf index into a Merkle tree node
/// index.
///
/// Behavior: crashes the VM if just one of the authentication paths is
/// invalid. Goes into an infinite loop if a node index value is initialized to
/// 0 or 1 through wrong domain-length values. Also cannot handle empty lists,
/// so this snippet must verify at least one authentication path.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct VerifyFriAuthenticationPaths;

impl BasicSnippet for VerifyFriAuthenticationPaths {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "dom_len_minus_one".to_owned()),
            (DataType::U32, "xor_bitflag".to_owned()),
            (
                DataType::List(Box::new(DataType::Xfe)),
                "*values_last_word".to_owned(),
            ),
            (
                DataType::List(Box::new(DataType::U32)),
                "*a_indices".to_owned(),
            ),
            (
                DataType::List(Box::new(DataType::U32)),
                "*a_indices_last_word".to_owned(),
            ),
            (DataType::Digest, "root".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_verify_fri_authentication_paths".into()
    }

    fn code(&self, _library: &mut crate::library::Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");

        let loop_over_auth_paths_label = format!("{entrypoint}_loop_over_auth_path_elements");
        let loop_over_auth_paths_code = triton_asm!(
            {loop_over_auth_paths_label}:
                merkle_step                         // move up one level in the Merkle tree
                recurse_or_return                   // break loop if node_index is 1
        );

        triton_asm!(
            // BEFORE: _ dom_len_minus_one xor_bitflag *values_last_word *idx_end_cond *a_indices_last_word [root]
            // AFTER : _

            {entrypoint}:
                call {main_loop}
                // _ dom_len_minus_one xor_bitflag *values_last_word *a_indices *a_indices_last_word [root]

                /* Cleanup stack */
                pop 5
                pop 5
                // _

                return


            // Invariant: _ dom_len_minus_one xor_bitflag *value[n]_last_word *a_indices *a_indices[n] [root]
            {main_loop}:
                // _ dom_len_minus_one xor_bitflag *value[n] *a_indices *a_indices[n] [root]

                push 1
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n] [root] 1

                pick 6
                read_mem 1
                place 7
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root] 1 ia_0[n]

                dup 11
                and
                dup 10
                xor
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root] 1 ((ia_0[n] & dom_len_minus_one) ^ xor_bitflag)
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root] 1 (i_r[n] + dom_len)
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root] 1 node_index_i_r[n]

                push 0
                push 0
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root] 1 i_r[n] 0 0

                pick 11
                read_mem {EXTENSION_DEGREE}
                place 14
                // _ dom_len_minus_one xor_bitflag *value' *a_indices *a_indices[n]' [root] 1 i_r[n] 0 0 [xfe]

                call {loop_over_auth_paths_label}
                // _ dom_len_minus_one xor_bitflag *value' *a_indices *a_indices[n]' [root] 1 1 [calculated_root]
                // _ dom_len_minus_one xor_bitflag *value' *a_indices *a_indices[n]' [root] 1 1 cr4 cr3 cr2 cr1 cr0

                pick 5 pick 6
                pop 2
                // _ dom_len_minus_one xor_bitflag *value' *a_indices *a_indices[n]' [root] cr4 cr3 cr2 cr1 cr0
                // _ dom_len_minus_one xor_bitflag *value' *a_indices *a_indices[n]' [root] [calculated_root]

                assert_vector
                    error_id 30
                // _ dom_len_minus_one xor_bitflag *value *a_indices *a_indices[n]' [root]

                recurse_or_return

            {&loop_over_auth_paths_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use rand::distr::Distribution;
    use rand::distr::StandardUniform;
    use strum::EnumIter;
    use strum::IntoEnumIterator;
    use twenty_first::prelude::*;

    use super::*;
    use crate::U32_TO_USIZE_ERR;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;

    #[derive(Clone, Debug, EnumIter, Copy)]
    enum IndexType {
        A,
        B,
    }

    impl Distribution<IndexType> for StandardUniform {
        fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> IndexType {
            if rng.random() {
                IndexType::A
            } else {
                IndexType::B
            }
        }
    }

    impl Algorithm for VerifyFriAuthenticationPaths {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
        ) {
            let root = pop_encodable(stack);
            let idx_last_elem = pop_encodable(stack);
            let idx_end_condition = pop_encodable(stack);
            let leaf_last_element_pointer = pop_encodable(stack);
            let xor_bitflag = pop_encodable::<u32>(stack);
            let dom_len_minus_one = pop_encodable::<u32>(stack);

            let dom_len = dom_len_minus_one + 1;
            let tree_height = dom_len.ilog2();

            let mut auth_path_counter = 0;
            let mut idx_element_pointer = idx_last_elem;
            let mut leaf_pointer = leaf_last_element_pointer;
            while idx_element_pointer != idx_end_condition {
                let auth_path_len = usize::try_from(tree_height).expect(U32_TO_USIZE_ERR);
                let auth_path_start = auth_path_counter * auth_path_len;
                let auth_path_end = auth_path_start + auth_path_len;
                let authentication_path =
                    nondeterminism.digests[auth_path_start..auth_path_end].to_vec();

                let leaf_index_a_round_0: u32 = memory
                    .get(&idx_element_pointer)
                    .map(|x| x.value())
                    .unwrap_or_default()
                    .try_into()
                    .unwrap();
                let node_index = (leaf_index_a_round_0 & dom_len_minus_one) ^ xor_bitflag;
                let leaf_index = node_index ^ dom_len;

                let read_word_from_mem =
                    |pointer: BFieldElement| memory.get(&pointer).copied().unwrap_or_default();
                let leaf = XFieldElement::new([
                    read_word_from_mem(leaf_pointer - bfe!(2)),
                    read_word_from_mem(leaf_pointer - bfe!(1)),
                    read_word_from_mem(leaf_pointer),
                ]);
                let inclusion_proof = MerkleTreeInclusionProof {
                    tree_height,
                    indexed_leafs: vec![(leaf_index as usize, leaf.into())],
                    authentication_structure: authentication_path,
                };
                assert!(inclusion_proof.verify(root));

                idx_element_pointer.decrement();
                auth_path_counter += 1;
                leaf_pointer -= bfe!(EXTENSION_DEGREE as u64);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> AlgorithmInitialState {
            let mut rng = StdRng::from_seed(seed);

            // determine sizes
            let (height, num_indices) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (10, 80),
                Some(BenchmarkCase::WorstCase) => (20, 80),
                None => (rng.random_range(6..=15), rng.random_range(2..10) as usize),
            };

            let index_type = rng.random();

            self.prepare_state(&mut rng, height, num_indices, index_type)
        }

        fn corner_case_initial_states(&self) -> Vec<AlgorithmInitialState> {
            let mut rng = StdRng::from_seed([42u8; 32]);

            let mut test_cases = vec![];
            for index_type in IndexType::iter() {
                test_cases.push(self.prepare_state(&mut rng, 1, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 1, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 1, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 1, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 1, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 1, 2, index_type));
                test_cases.push(self.prepare_state(&mut rng, 2, 1, index_type));
                test_cases.push(self.prepare_state(&mut rng, 2, 2, index_type));
                test_cases.push(self.prepare_state(&mut rng, 2, 3, index_type));
                test_cases.push(self.prepare_state(&mut rng, 2, 4, index_type));
            }

            test_cases
        }
    }

    impl VerifyFriAuthenticationPaths {
        fn prepare_state(
            &self,
            rng: &mut StdRng,
            height: u32,
            num_indices: usize,
            index_type: IndexType,
        ) -> AlgorithmInitialState {
            // generate data structure
            let dom_len = 1 << height;
            let dom_len_minus_one = dom_len - 1;
            let dom_len_half: u32 = dom_len / 2;

            let xfe_leafs = (0..dom_len)
                .map(|_| rng.random::<XFieldElement>())
                .collect_vec();
            let leafs_as_digest: Vec<Digest> =
                xfe_leafs.iter().map(|&xfe| xfe.into()).collect_vec();
            let tree = MerkleTree::par_new(&leafs_as_digest).unwrap();
            let root = tree.root();

            let a_indices = (0..num_indices)
                .map(|_| rng.random_range(0..dom_len) as usize)
                .collect_vec();

            // TODO: Generalize for other values than round=0
            let indices_revealed = match index_type {
                IndexType::A => a_indices.clone(),
                IndexType::B => a_indices
                    .clone()
                    .into_iter()
                    .map(|x| (x + dom_len as usize / 2) & dom_len_minus_one as usize)
                    .collect_vec(),
            };
            let opened_leafs = indices_revealed.iter().map(|i| xfe_leafs[*i]).collect_vec();
            let authentication_paths = indices_revealed
                .iter()
                .rev()
                .map(|i| tree.authentication_structure(&[*i]).unwrap())
                .collect_vec();
            let a_indices: Vec<u32> = a_indices.into_iter().map(|idx| idx as u32).collect_vec();

            // prepare memory + stack + nondeterminism
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();

            let a_indices_pointer = BFieldElement::new(rng.next_u64() % (1 << 20));
            rust_shadowing_helper_functions::list::list_insert(
                a_indices_pointer,
                a_indices,
                &mut memory,
            );

            let leaf_pointer = BFieldElement::new(rng.next_u64() % (1 << 20) + (1 << 32));
            rust_shadowing_helper_functions::list::list_insert(
                leaf_pointer,
                opened_leafs,
                &mut memory,
            );

            let a_indices_last_word = a_indices_pointer + bfe!(num_indices as u64);
            let leaf_pointer_last_word =
                leaf_pointer + bfe!((EXTENSION_DEGREE * num_indices) as u64);
            let dom_len_minus_one: u32 = dom_len - 1;
            let xor_bitflag: u32 = match index_type {
                IndexType::A => dom_len,
                IndexType::B => dom_len_half + dom_len,
            };

            let mut stack = self.init_stack_for_isolated_run();
            stack.push(bfe!(dom_len_minus_one));
            stack.push(bfe!(xor_bitflag));
            stack.push(leaf_pointer_last_word);
            stack.push(a_indices_pointer);
            stack.push(a_indices_last_word);
            stack.push(root.0[4]);
            stack.push(root.0[3]);
            stack.push(root.0[2]);
            stack.push(root.0[1]);
            stack.push(root.0[0]);
            let nondeterminism = NonDeterminism::default()
                .with_digests(authentication_paths.into_iter().flatten().collect_vec())
                .with_ram(memory);

            AlgorithmInitialState {
                stack,
                nondeterminism,
            }
        }
    }

    #[test]
    fn test() {
        ShadowedAlgorithm::new(VerifyFriAuthenticationPaths).test();
    }

    #[proptest]
    fn fri_authentication_fails_if_root_is_disturbed_slightly(
        seed: [u8; 32],
        #[strategy(0_usize..5)] perturbation_index: usize,
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let mut initial_state = VerifyFriAuthenticationPaths.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        initial_state.stack[top_of_stack - perturbation_index] += bfe!(perturbation);

        test_assertion_failure(
            &ShadowedAlgorithm::new(VerifyFriAuthenticationPaths),
            initial_state.into(),
            &[30],
        );
    }

    #[proptest]
    fn fri_authentication_fails_if_xor_bitflag_is_disturbed_slightly(seed: [u8; 32]) {
        let mut initial_state = VerifyFriAuthenticationPaths.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;
        let xor_bitflag = initial_state.stack.get_mut(top_of_stack - 8).unwrap();
        *xor_bitflag *= bfe!(2); // todo: generalize this perturbation
        prop_assume!(u32::try_from(*xor_bitflag).is_ok());

        test_assertion_failure(
            &ShadowedAlgorithm::new(VerifyFriAuthenticationPaths),
            initial_state.into(),
            &[30],
        );
    }

    #[proptest]
    fn fri_authentication_fails_if_authentication_path_is_disturbed_slightly(
        seed: [u8; 32],
        digest_index: usize,
        #[strategy(0_usize..5)] perturbation_index: usize,
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let mut initial_state = VerifyFriAuthenticationPaths.pseudorandom_initial_state(seed, None);
        let auth_paths = &mut initial_state.nondeterminism.digests;
        let digest_index = digest_index % auth_paths.len();
        let Digest(ref mut auth_path_element_innards) = auth_paths[digest_index];
        auth_path_element_innards[perturbation_index] += bfe!(perturbation);

        test_assertion_failure(
            &ShadowedAlgorithm::new(VerifyFriAuthenticationPaths),
            initial_state.into(),
            &[30],
        );
    }

    #[proptest]
    fn fri_authentication_fails_if_a_index_is_disturbed_slightly(
        seed: [u8; 32],
        perturbation_index: usize,
        #[filter(#perturbation != 0)] perturbation: i8,
    ) {
        let perturbation = bfe!(perturbation);

        let mut initial_state = VerifyFriAuthenticationPaths.pseudorandom_initial_state(seed, None);
        let top_of_stack = initial_state.stack.len() - 1;

        let a_indices_pointer = initial_state.stack[top_of_stack - 6];
        let a_indices_len = initial_state.nondeterminism.ram[&a_indices_pointer].value() as usize;
        let perturbation_index = bfe!(perturbation_index % a_indices_len);

        let perturbation_pointer = a_indices_pointer + bfe!(1) + perturbation_index;
        let ram = &mut initial_state.nondeterminism.ram;
        let a_index = ram.get_mut(&perturbation_pointer).unwrap();

        let old_a_index = a_index.value();
        *a_index += perturbation;
        let new_a_index = a_index.value();
        prop_assume!(u32::try_from(*a_index).is_ok());

        // ensure meaningful perturbation
        let dom_len_minus_one = initial_state.stack[top_of_stack - 9].value();
        let xor_bitflag = initial_state.stack[top_of_stack - 8].value();
        let node_index = |i| (i & dom_len_minus_one) ^ xor_bitflag;
        prop_assume!(node_index(old_a_index) != node_index(new_a_index));

        test_assertion_failure(
            &ShadowedAlgorithm::new(VerifyFriAuthenticationPaths),
            initial_state.into(),
            &[30],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedAlgorithm::new(VerifyFriAuthenticationPaths).bench();
    }
}
