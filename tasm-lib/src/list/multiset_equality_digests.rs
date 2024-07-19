use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::hashing::algebraic_hasher::hash_varlen::HashVarlen;
use crate::library::Library;
use crate::list::length::Length;
use crate::traits::basic_snippet::BasicSnippet;
use crate::DIGEST_LENGTH;

/// Determine whether two lists are equal up to permutation. The
/// lists are given as lists of digests. This function uses hashing
/// to compute a challenge indeterminate, and then computes a running
/// products for both lists. In the future, the implementation of
/// function may be replaced by one that uses Triton VM's native
/// support for permutation checks instead of Fiat-Shamir and running
/// products.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MultisetEqualityDigests;

impl BasicSnippet for MultisetEqualityDigests {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::Digest)), "a".to_owned()),
            (DataType::List(Box::new(DataType::Digest)), "b".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "equal_multisets".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_multiset_equality_digests".to_owned()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let length_snippet = library.import(Box::new(Length::new(DataType::Digest)));
        let hash_varlen = library.import(Box::new(HashVarlen));
        const DIGEST_LENGTH_PLUS_ONE: usize = DIGEST_LENGTH + 1;

        let early_abort_label = format!("{entrypoint}_early_abort");
        let continue_label = format!("{entrypoint}_continue");
        let running_product_label = format!("{entrypoint}_running_product");
        let running_product_loop_label = format!("{entrypoint}_running_product_loop");

        triton_asm!(
            // BEFORE: _ *list_a *list_b
            // AFTER:  _ list_a==list_b (as multisets, or up to permutation)
            {entrypoint}:

                // read lengths of lists
                dup 1 dup 1             // _ *list_a *list_b *list_a *list_b
                call {length_snippet}   // _ *list_a *list_b *list_a len_b
                swap 1                  // _ *list_a *list_b len_b *list_a
                call {length_snippet}   // _ *list_a *list_b len_b len_a

                // equate lengths and return early if possible
                dup 1                   // _ *list_a *list_b len_b len_a len_b
                eq                      // _ *list_a *list_b len_b (len_a==len_b)
                push 0 eq               // _ *list_a *list_b len_b (len_a!=len_b)

                // early return if lengths mismatch
                // otherwise continue
                push 1 swap 1           // _ *list_a *list_b len_b 1 (len_a!=len_b)
                skiz call {early_abort_label}
                skiz call {continue_label}

                // _ (list_a == list_b) (as multisets, or up to permutation)
                return

            {early_abort_label}:
                // _ *list_a *list_b len_b 1
                pop 4

                // push return value (false)
                push 0 // _ 0

                // ensure `else` branch is not taken
                push 0
                return

            {continue_label}:
                // _ *list_a *list_b len

                // hash list_a
                dup 2                    // _ *list_a *list_b len *list_a
                push 1 add               // _ *list_a *list_b len *list_a[0]
                dup 1                    // _ *list_a *list_b len *list_a[0] len
                push {DIGEST_LENGTH} mul // _ *list_a *list_b len *list_a[0] (len*{DIGEST_LENGTH})
                call {hash_varlen}       // _ *list_a *list_b len da4 da3 da2 da1 da0

                // hash list_b
                dup 6                            // _ *list_a *list_b len da4 da3 da2 da1 da0 *list_b
                push 1 add                       // _ *list_a *list_b len *list_b[0]
                dup 6                            // _ *list_a *list_b len da4 da3 da2 da1 da0 *list_b[0] len
                push {DIGEST_LENGTH} mul         // _ *list_a *list_b len da4 da3 da2 da1 da0 *list_b[0] (len*{DIGEST_LENGTH})
                call {hash_varlen}               // _ *list_a *list_b len da4 da3 da2 da1 da0 db4 db3 db2 db1 db0

                // hash together
                hash  // _ *list_a *list_b len d4 d3 d2 d1 d0
                pop 2 // _ *list_a *list_b len [-indeterminate]

                call {running_product_label} // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0
                dup 8                        // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a
                dup 7                        // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len
                dup 7 dup 7 dup 7            // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate]
                call {running_product_label} // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa1 rpa0

                // test equality
                dup 8  // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa1 rpa0 rpb0
                eq     // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa1 rpa0==rpb0
                swap 1 // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa0==rpb0 rpa1
                dup 9  // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa0==rpb0 rpa1 rpb1
                eq mul // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa2 rpa0==rpb0&&rpa1==rpb1
                swap 1 // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa0==rpb0&&rpa1==rpb1 rpa2
                dup 9  // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa0==rpb0&&rpa1==rpb1 rpa2 rpb2
                eq mul // _ *list_a *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] rpa0==rpb0&&rpa1==rpb1&&rpa2==rpb2

                // clean up and return
                swap 14 // _ rpa0==rpb0&&rpa1==rpb1 rpa2 rpb2 *list_b len [-indeterminate] rpb2 rpb1 rpb0 *list_a len [-indeterminate] list_a
                pop 5 pop 5 pop 4
                // _ (rpa0==rpb0&&rpa1==rpb1)

                return

            // BEFORE: _ *list len [-indeterminate]
            // AFTER:  _ *list len [-indeterminate] rp2 rp1 rp0
            {running_product_label}:
                // initialize loop
                dup 4                // _ *list len [-indeterminate] *list
                push 1 add           // _ *list len [-indeterminate] addr
                dup 4                // _ *list len [-indeterminate] addr itrs_left
                push 0 push 0 push 1 // _ *list len [-indeterminate] addr itrs_left 0 0 1

                call {running_product_loop_label}
                // _ *list len [-indeterminate] addr* 0 rp2 rp1 rp0

                // clean up and return
                swap 2
                swap 4
                pop 1
                swap 2
                pop 1
                // _ *list len [-indeterminate] [rp]

                return

            // INVARIANT: _ *list len d2 d1 d0 addr itrs_left [rp]
            {running_product_loop_label}:
                // test termination condition
                dup 3       // _ *list len [-indeterminate] addr itrs_left [rp] itrs_left
                push 0 eq   // _ *list len [-indeterminate] addr itrs_left [rp] itrs_left==0
                skiz return // _ *list len [-indeterminate] addr itrs_left [rp]

                // read addr+2, addr+1, addr+0
                dup 4 push 2 add read_mem 3
                // _ *list len [-indeterminate] addr itrs_left [rp] [m] (addr - 1)

                // addr += DIGEST_LENGTH + 1
                push {DIGEST_LENGTH_PLUS_ONE} add // _ *list len [-indeterminate] addr itrs_left [rp] [m] addr+{DIGEST_LENGTH}
                swap 8                            // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} itrs_left [rp] [m] addr
                pop 1                             // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} itrs_left [rp] [m]

                // itrs_left -= 1
                swap 6 push -1 add swap 6         // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} (itrs_left-1) [rp] [m]

                // add indeterminate
                dup 10 dup 10 dup 10  // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} (itrs_left-1) [rp] m2 m1 m0 [-indeterminate]
                xx_add                // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} (itrs_left-1) [rp] [m - indeterminate]

                // multiply into running product
                xx_mul                // _ *list len [-indeterminate] addr+{DIGEST_LENGTH} (itrs_left-1) [rp']

                recurse
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use itertools::Itertools;
    use num::One;
    use rand::random;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use tip5::DIGEST_LENGTH;
    use twenty_first::math::other::random_elements;

    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions;
    use crate::rust_shadowing_helper_functions::list::load_list_with_copy_elements;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::{Function, FunctionInitialState, ShadowedFunction};
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn prop_test() {
        ShadowedFunction::new(MultisetEqualityDigests).test();
    }

    impl Function for MultisetEqualityDigests {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let list_b_pointer = stack.pop().unwrap();
            let list_a_pointer = stack.pop().unwrap();

            let a: Vec<[BFieldElement; DIGEST_LENGTH]> =
                load_list_with_copy_elements(list_a_pointer, memory);
            let mut a = a.into_iter().map(Digest::new).collect_vec();
            a.sort_unstable();
            let b: Vec<[BFieldElement; DIGEST_LENGTH]> =
                load_list_with_copy_elements(list_b_pointer, memory);
            let mut b = b.into_iter().map(Digest::new).collect_vec();
            b.sort_unstable();

            // equate and push result to stack
            let result = a == b;
            stack.push(BFieldElement::new(result as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => self.random_equal_lists(2),
                Some(BenchmarkCase::WorstCase) => self.random_equal_lists(100),
                None => {
                    let mut rng: StdRng = SeedableRng::from_seed(seed);
                    let length = rng.gen_range(1..50);
                    let index = rng.gen_range(0..length);
                    let digest_word_index = rng.gen_range(0..DIGEST_LENGTH);
                    let another_length = length + rng.gen_range(1..10);
                    match rng.gen_range(0..=3) {
                        0 => self.random_equal_lists(length),
                        1 => self.random_unequal_lists(length),
                        2 => self.random_unequal_length_lists(length, another_length),
                        3 => {
                            self.random_lists_one_element_flipped(length, index, digest_word_index)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let short_equal_multisets = (0..15).map(|i| self.random_equal_lists(i)).collect_vec();
            let short_unequal_multisets =
                (0..15).map(|i| self.random_unequal_lists(i)).collect_vec();
            let mut short_lists_one_element_flipped = vec![];
            for length in 1..15 {
                for manipulated_element in 0..length {
                    for manipulated_word in 0..DIGEST_LENGTH {
                        short_lists_one_element_flipped.push(
                            self.random_lists_one_element_flipped(
                                length,
                                manipulated_element,
                                manipulated_word,
                            ),
                        );
                    }
                }
            }

            let unequal_lengths = vec![
                self.random_unequal_length_lists(0, 5),
                self.random_unequal_length_lists(0, 1),
                self.random_unequal_length_lists(1, 0),
                self.random_unequal_length_lists(5, 0),
                self.random_unequal_length_lists(1, 2),
                self.random_unequal_length_lists(2, 1),
                self.random_unequal_length_lists(10, 17),
                self.random_unequal_length_lists(21, 0),
            ];

            [
                short_equal_multisets,
                short_unequal_multisets,
                short_lists_one_element_flipped,
                unequal_lengths,
            ]
            .concat()
        }
    }

    impl MultisetEqualityDigests {
        fn random_equal_lists(&self, length: usize) -> FunctionInitialState {
            let list_a: Vec<Digest> = random_elements(length);
            let mut list_b = list_a.clone();
            list_b.sort();
            let pointer_a: BFieldElement = random();
            let pointer_b: BFieldElement =
                BFieldElement::new(pointer_a.value() + random::<u32>() as u64);

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
            rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

            let stack = [empty_stack(), vec![pointer_a, pointer_b]].concat();
            FunctionInitialState { stack, memory }
        }

        fn random_unequal_lists(&self, length: usize) -> FunctionInitialState {
            let list_a: Vec<Digest> = random_elements(length);
            let list_b: Vec<Digest> = random_elements(length);
            let pointer_a: BFieldElement = random();
            let pointer_b: BFieldElement =
                BFieldElement::new(pointer_a.value() + random::<u32>() as u64);
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
            rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

            let stack = [empty_stack(), vec![pointer_a, pointer_b]].concat();
            FunctionInitialState { stack, memory }
        }

        fn random_unequal_length_lists(
            &self,
            length_a: usize,
            length_b: usize,
        ) -> FunctionInitialState {
            let list_a: Vec<Digest> = random_elements(length_a);
            let list_b: Vec<Digest> = random_elements(length_b);
            let pointer_a: BFieldElement = random();
            let pointer_b: BFieldElement =
                BFieldElement::new(pointer_a.value() + random::<u32>() as u64);
            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
            rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

            let stack = [empty_stack(), vec![pointer_a, pointer_b]].concat();
            FunctionInitialState { stack, memory }
        }

        fn random_lists_one_element_flipped(
            &self,
            length: usize,
            manipulated_index: usize,
            manipulated_digest_word_index: usize,
        ) -> FunctionInitialState {
            assert!(manipulated_index < length);
            assert!(manipulated_digest_word_index < DIGEST_LENGTH);
            let list_a: Vec<Digest> = random_elements(length);
            let mut list_b = list_a.clone();
            list_b.sort();
            list_b[manipulated_index].0[manipulated_digest_word_index] += BFieldElement::one();
            let pointer_a: BFieldElement = random();
            let pointer_b: BFieldElement =
                BFieldElement::new(pointer_a.value() + random::<u32>() as u64);

            let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

            rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
            rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

            let stack = [empty_stack(), vec![pointer_a, pointer_b]].concat();
            FunctionInitialState { stack, memory }
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn multiset_eq_digests_benchmark() {
        ShadowedFunction::new(MultisetEqualityDigests).bench();
    }
}
