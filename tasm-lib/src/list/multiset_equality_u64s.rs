use triton_vm::prelude::*;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::hashing::algebraic_hasher::hash_varlen::HashVarlen;
use crate::prelude::*;

#[derive(Debug, Clone, Copy)]
pub struct MultisetEqualityU64s;

const U64_STACK_SIZE: usize = 2;

impl BasicSnippet for MultisetEqualityU64s {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::U64)), "list_a".to_owned()),
            (DataType::List(Box::new(DataType::U64)), "list_b".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "multisets_are_equal".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_multiset_equality_u64s".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        assert_eq!(U64_STACK_SIZE, DataType::U64.stack_size());

        let hash_varlen = library.import(Box::new(HashVarlen));
        let compare_xfes = DataType::Xfe.compare();

        let running_product_result_alloc = library.kmalloc(EXTENSION_DEGREE.try_into().unwrap());

        let compare_lengths = triton_asm!(
            // _ *a *b

            dup 1
            dup 1
            // _ *a *b *a *b

            read_mem 1
            pop 1
            // _ *a *b *a b_len

            swap 1
            read_mem 1
            pop 1
            // _ *a *b b_len a_len

            dup 1
            eq
            // _ *a *b b_len (b_len == a_len)
        );

        let not_equal_length_label = format!("{entrypoint}_not_equal_length");
        let not_equal_length_code = triton_asm!(
            {not_equal_length_label}:
            // _ *list_a *list_b len_b 1
            pop 4

            /* push snippet return value (false) */
            push 0 // _ 0

            /* ensure `else` branch is not taken */
            push 0

            return
        );

        let find_challenge_indeterminate = triton_asm!(
            // _ *a *b size

            /* Hash list `a` */
            dup 2
            dup 1
            // _ *a *b size *a size

            push 1 add
            // _ *a *b size *a (size + 1)
            // _ *a *b size *a full_size ; `full_size` includes length indicator

            call {hash_varlen}
            // _ *a *b size [a_digest]

            /* Hash list `b` */
            dup 6
            dup 6
            // _ *a *b size [a_digest] *b size

            push 1 add
            // _ *a *b size [a_digest] *b (size + 1)

            call {hash_varlen}
            // _ *a *b size [a_digest] [b_digest]

            /* Get challenge indeterminate */
            hash
            pop 2
            // _ *a *b size [-indeterminate]
        );

        let calculate_running_product_loop_label = format!("{entrypoint}_loop");
        let calculate_running_product_loop_code = triton_asm!(
            // INVARIANT: _ [-indeterminate] *list *list[i]_lw [garbage; 2] [running_product]
            {calculate_running_product_loop_label}:

                push 0
                dup 6
                read_mem {U64_STACK_SIZE}
                swap 9
                pop 1
                // _ [-indeterminate] *list *list[i-1]_lw [garbage; 2] [running_product] 0 u64_hi u64_lo
                // _ [-indeterminate] *list *list[i-1]_lw [garbage; 2] [running_product] [elem_as_xfe]

                dup 12
                dup 12
                dup 12
                xx_add
                xx_mul
                // _ [-indeterminate] *list *list[i-1]_lw [garbage; 2] [running_product * (elem_as_xfe -indeterminate)]
                // _ [-indeterminate] *list *list[i-1]_lw [garbage; 2] [running_product']

                recurse_or_return
        );

        let equal_length_label = format!("{entrypoint}_equal_length");
        let equal_length_code = triton_asm!(
            {equal_length_label}:
                // _ *a *b len

                push {U64_STACK_SIZE}
                mul
                // _ *a *b size

                // `size` is size of elements excluding length indicator
                // Notice that we also absorb the length indicator into the
                // sponge state.

                {&find_challenge_indeterminate}
                // _ *a *b size [-indeterminate]

                dup 5
                dup 6
                dup 5
                add
                // _ *a *b size [-indeterminate] *a (*a + size)

                push 0
                push 0
                // _ *a *b size [-indeterminate] *a (*a + size) [garbage; 2]

                push 0
                push 0
                push 1
                // _ *a *b size [-indeterminate] *a (*a + size) [garbage; 2] [1]
                // _ *a *b size [-indeterminate] *a (*a + size) [garbage; 2] [running_product]

                dup 6
                dup 6
                eq
                push 0
                eq
                skiz call {calculate_running_product_loop_label}
                // _ *a *b size [-indeterminate] *a *a [garbage; 2] [a_rp]

                /* store result in static memory and cleanup stack */
                push {running_product_result_alloc.write_address()}
                write_mem {running_product_result_alloc.num_words()}
                pop 5
                // _ *a *b size [-indeterminate]

                /* Prepare stack for loop */
                dup 4
                dup 5
                dup 5
                // _ *a *b size [-indeterminate] *b *b size

                add
                // _ *a *b size [-indeterminate] *b *b_lw

                push 0
                push 0
                push 0
                push 0
                push 1
                // _ *a *b size [-indeterminate] *b *b_lw [garbage; 2] [running_product]

                dup 6
                dup 6
                eq
                push 0
                eq
                // _ *a *b size [-indeterminate] *b *b_lw [garbage; 2] [running_product] (*b != *b_lw)

                skiz call {calculate_running_product_loop_label}
                // _ *a *b size [-indeterminate] *b *b_lw [garbage; 2] [b_rp]

                swap 10
                pop 1
                swap 10
                pop 1
                swap 10
                // _ [b_rp] [-indeterminate] *b *b_lw [garbage; 2] *a

                pop 5
                pop 3
                // _ [b_rp]

                push {running_product_result_alloc.read_address()}
                read_mem {running_product_result_alloc.num_words()}
                pop 1
                // _ [b_rp] [a_rp]

                {&compare_xfes}
                // _ (b_rp == a_rp)

                return
        );

        triton_asm!(
            // BEFORE: _ *a *b
            // AFTER: a == b (as multisets, or up to permutation)
            {entrypoint}:
                {&compare_lengths}
                // _ *a *b b_len (a_len == b_len)

                push 1
                swap 1
                push 0
                eq
                // _ *a *b b_len 1 (a_len != b_len)

                skiz call {not_equal_length_label}
                skiz call {equal_length_label}
                // _ multisets_are_equal

                return

            {&not_equal_length_code}
            {&equal_length_code}
            {&calculate_running_product_loop_code}
        )
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use num::Zero;

    use super::*;
    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::list::LIST_METADATA_SIZE;
    use crate::memory::encode_to_memory;
    use crate::rust_shadowing_helper_functions;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_prelude::*;

    #[test]
    fn returns_true_on_multiset_equality() {
        let snippet = MultisetEqualityU64s;
        let return_value_is_true = [
            snippet.init_stack_for_isolated_run(),
            vec![BFieldElement::one()],
        ]
        .concat();

        let mut rng = rand::rng();
        let mut seed = [0u8; 32];
        rng.fill_bytes(&mut seed);
        let mut rng = StdRng::from_seed(seed);

        for length in (0..10).chain(1000..1001) {
            let init_state = snippet.random_equal_multisets(length, &mut rng);
            let nd = NonDeterminism::default().with_ram(init_state.memory);
            test_rust_equivalence_given_complete_state(
                &ShadowedFunction::new(snippet),
                &init_state.stack,
                &[],
                &nd,
                &None,
                Some(&return_value_is_true),
            );
        }
    }

    #[test]
    fn returns_false_on_multiset_inequality() {
        let snippet = MultisetEqualityU64s;
        let return_value_is_false = [
            snippet.init_stack_for_isolated_run(),
            vec![BFieldElement::zero()],
        ]
        .concat();

        let mut rng = rand::rng();
        let mut seed = [0u8; 32];
        rng.fill_bytes(&mut seed);
        let mut rng = StdRng::from_seed(seed);

        for length in (1..10).chain(1000..1001) {
            let init_state = snippet.random_same_length_mutated_elements(length, 1, 1, &mut rng);
            let nd = NonDeterminism::default().with_ram(init_state.memory);
            test_rust_equivalence_given_complete_state(
                &ShadowedFunction::new(snippet),
                &init_state.stack,
                &[],
                &nd,
                &None,
                Some(&return_value_is_false),
            );
        }
    }

    #[test]
    fn multiset_equality_u64s_pbt() {
        ShadowedFunction::new(MultisetEqualityU64s).test()
    }

    impl Function for MultisetEqualityU64s {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let list_b_pointer = stack.pop().unwrap();
            let list_a_pointer = stack.pop().unwrap();

            let a: Vec<[BFieldElement; 2]> = load_list_with_copy_elements(list_a_pointer, memory);
            let b: Vec<[BFieldElement; 2]> = load_list_with_copy_elements(list_b_pointer, memory);

            if a.len() != b.len() {
                stack.push(BFieldElement::zero());
                return;
            }

            let len = a.len();

            // hash to get Fiat-Shamir challenge
            let a_digest = Tip5::hash(&a);
            let b_digest = Tip5::hash(&b);
            let indeterminate = Tip5::hash_pair(b_digest, a_digest);
            let indeterminate =
                -XFieldElement::new(indeterminate.values()[2..Digest::LEN].try_into().unwrap());

            // compute running products
            let mut running_product_a = XFieldElement::one();
            for i in 0..len as u64 {
                let u64_elem = list_get(list_a_pointer, i as usize, memory, U64_STACK_SIZE);
                let m = XFieldElement::new([u64_elem[0], u64_elem[1], BFieldElement::zero()]);
                let factor = m - indeterminate;
                running_product_a *= factor;
            }
            let mut running_product_b = XFieldElement::one();
            for i in 0..len as u64 {
                let u64_elem = list_get(list_b_pointer, i as usize, memory, U64_STACK_SIZE);
                let m = XFieldElement::new([u64_elem[0], u64_elem[1], BFieldElement::zero()]);
                let factor = m - indeterminate;
                running_product_b *= factor;
            }

            // Write to static memory, since that's what the TASM code does
            encode_to_memory(
                memory,
                STATIC_MEMORY_FIRST_ADDRESS - bfe!(EXTENSION_DEGREE as u64 - 1),
                &running_product_a,
            );

            stack.push(bfe!((running_product_a == running_product_b) as u64))
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);

            match bench_case {
                // Common case: 2 * 45 ~ 2 inputs
                // Common case: 8 inputs
                Some(BenchmarkCase::CommonCase) => self.random_equal_multisets(90, &mut rng),
                Some(BenchmarkCase::WorstCase) => self.random_equal_multisets(360, &mut rng),
                None => {
                    let length = rng.random_range(0..50);
                    let num_mutations = rng.random_range(0..=length);
                    let mutation_translation: u64 = rng.random();
                    let another_length = length + rng.random_range(1..10);
                    match rng.random_range(0..=5) {
                        0 => self.random_equal_multisets(length, &mut rng),
                        1 => self.random_equal_lists(length, &mut rng),
                        2 => self.random_equal_multisets_flipped_pointers(length, &mut rng),
                        3 => self.random_same_length_mutated_elements(
                            length,
                            num_mutations,
                            mutation_translation,
                            &mut rng,
                        ),
                        4 => self.random_unequal_length_lists(length, another_length, &mut rng),
                        5 => self.random_unequal_length_lists_trailing_zeros(
                            length,
                            another_length,
                            &mut rng,
                        ),
                        _ => unreachable!(),
                    }
                }
            }
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let seed = [111u8; 32];
            let mut rng = StdRng::from_seed(seed);

            let length_0_length_1 = self.random_unequal_length_lists(0, 1, &mut rng);
            let length_1_length_0 = self.random_unequal_length_lists(1, 0, &mut rng);
            let two_empty_lists = self.random_equal_multisets(0, &mut rng);
            let two_equal_singletons = self.random_equal_multisets(1, &mut rng);
            let two_equal_lists_length_2 = self.random_equal_lists(2, &mut rng);
            let two_equal_lists_flipped_order =
                self.random_equal_multisets_flipped_pointers(4, &mut rng);

            let unqual_lists_length_1_add_1 =
                self.random_same_length_mutated_elements(1, 1, 1, &mut rng);
            let unqual_lists_length_1_add_2pow32 =
                self.random_same_length_mutated_elements(1, 1, 1u64 << 32, &mut rng);

            let unqual_lists_length_2_add_1 =
                self.random_same_length_mutated_elements(2, 1, 1, &mut rng);
            let unqual_lists_length_2_add_2pow32 =
                self.random_same_length_mutated_elements(2, 1, 1u64 << 32, &mut rng);

            let equal_multisets_length_2s = (0..10)
                .map(|_| self.random_equal_multisets(2, &mut rng))
                .collect_vec();
            let equal_multisets_length_3s = (0..10)
                .map(|_| self.random_equal_multisets(3, &mut rng))
                .collect_vec();
            let equal_multisets_length_4s = (0..10)
                .map(|_| self.random_equal_multisets(4, &mut rng))
                .collect_vec();

            let different_lengths_same_initial_elements_1_2 =
                self.random_unequal_length_lists(1, 2, &mut rng);
            let different_lengths_same_initial_elements_2_1 =
                self.random_unequal_length_lists(2, 1, &mut rng);
            let different_lengths_trailing_zeros_1_2 =
                self.random_unequal_length_lists_trailing_zeros(1, 2, &mut rng);

            [
                vec![
                    length_0_length_1,
                    length_1_length_0,
                    two_empty_lists,
                    two_equal_singletons,
                    two_equal_lists_length_2,
                    two_equal_lists_flipped_order,
                    unqual_lists_length_1_add_1,
                    unqual_lists_length_1_add_2pow32,
                    unqual_lists_length_2_add_1,
                    unqual_lists_length_2_add_2pow32,
                    different_lengths_same_initial_elements_1_2,
                    different_lengths_same_initial_elements_2_1,
                    different_lengths_trailing_zeros_1_2,
                ],
                equal_multisets_length_2s,
                equal_multisets_length_3s,
                equal_multisets_length_4s,
            ]
            .concat()
        }
    }

    impl MultisetEqualityU64s {
        fn list_a_and_both_pointers(
            &self,
            length: usize,
            rng: &mut StdRng,
        ) -> (Vec<u64>, BFieldElement, BFieldElement) {
            let mut list_a: Vec<u64> = vec![0u64; length];
            for elem in list_a.iter_mut() {
                *elem = rng.random();
            }

            let pointer_a: BFieldElement = rng.random();

            // Avoid lists from overlapping in memory
            let list_size = length * U64_STACK_SIZE + LIST_METADATA_SIZE;
            let pointer_b_offset: u32 = rng.random_range(list_size as u32..u32::MAX);
            let pointer_b: BFieldElement =
                BFieldElement::new(pointer_a.value() + pointer_b_offset as u64);

            (list_a, pointer_a, pointer_b)
        }

        fn init_state(
            &self,
            pointer_a: BFieldElement,
            pointer_b: BFieldElement,
            a: Vec<u64>,
            b: Vec<u64>,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            rust_shadowing_helper_functions::list::list_insert(pointer_a, a, &mut memory);
            rust_shadowing_helper_functions::list::list_insert(pointer_b, b, &mut memory);

            let stack = [
                self.init_stack_for_isolated_run(),
                vec![pointer_a, pointer_b],
            ]
            .concat();
            FunctionInitialState { stack, memory }
        }

        fn random_equal_multisets(&self, length: usize, rng: &mut StdRng) -> FunctionInitialState {
            let (a, pointer_a, pointer_b) = self.list_a_and_both_pointers(length, rng);
            let mut b = a.clone();
            b.sort();

            self.init_state(pointer_a, pointer_b, a, b)
        }

        fn random_equal_lists(&self, length: usize, rng: &mut StdRng) -> FunctionInitialState {
            let (a, pointer_a, pointer_b) = self.list_a_and_both_pointers(length, rng);
            let b = a.clone();

            self.init_state(pointer_a, pointer_b, a, b)
        }

        fn random_equal_multisets_flipped_pointers(
            &self,
            length: usize,
            rng: &mut StdRng,
        ) -> FunctionInitialState {
            let (b, pointer_b, pointer_a) = self.list_a_and_both_pointers(length, rng);
            let mut a = b.clone();
            a.sort();

            // Generate testcase where `(*a)`.value() < `(*b).value`
            self.init_state(pointer_a, pointer_b, a, b)
        }

        fn random_same_length_mutated_elements(
            &self,
            length: usize,
            num_mutations: usize,
            mutation_translation: u64,
            rng: &mut StdRng,
        ) -> FunctionInitialState {
            let (a, pointer_a, pointer_b) = self.list_a_and_both_pointers(length, rng);
            let mut b = a.clone();
            b.sort();

            for _ in 0..num_mutations {
                let elem_mut_ref = b.choose_mut(rng).unwrap();
                *elem_mut_ref = elem_mut_ref.wrapping_add(mutation_translation);
            }

            self.init_state(pointer_a, pointer_b, a, b)
        }

        fn random_unequal_length_lists(
            &self,
            length_a: usize,
            length_b: usize,
            rng: &mut StdRng,
        ) -> FunctionInitialState {
            assert_ne!(length_a, length_b, "Don't do this");

            let (a, pointer_a, pointer_b) = self.list_a_and_both_pointers(length_a, rng);
            let mut b = a.clone();
            b.resize_with(length_b, || rng.random());

            self.init_state(pointer_a, pointer_b, a, b)
        }

        fn random_unequal_length_lists_trailing_zeros(
            &self,
            length_a: usize,
            length_b: usize,
            rng: &mut StdRng,
        ) -> FunctionInitialState {
            assert!(length_b > length_a);

            let (a, pointer_a, pointer_b) = self.list_a_and_both_pointers(length_a, rng);
            let mut b = a.clone();
            b.resize_with(length_b, || 0);

            self.init_state(pointer_a, pointer_b, a, b)
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(MultisetEqualityU64s).bench()
    }
}
