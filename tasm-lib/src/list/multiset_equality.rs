use std::collections::HashMap;

use num::Zero;
use num_traits::One;
use rand::random;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::x_field_element;
use triton_vm::twenty_first::prelude::AlgebraicHasher;
use twenty_first::shared_math::other::random_elements;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::hashing::algebraic_hasher::hash_varlen::HashVarlen;
use crate::library::Library;
use crate::list::length::Length;
use crate::rust_shadowing_helper_functions;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::traits::procedure::Procedure;
use crate::ExecutionState;
use crate::VmHasher;
use crate::DIGEST_LENGTH;

use super::LIST_METADATA_SIZE;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MultisetEquality;

/// Determine whether two lists are equal up to permutation. The
/// lists are given as lists of digests. This function uses hashing
/// to compute a challenge indeterminate, and then computes a running
/// products for both lists. In the future, the implementation of
/// function may be replaced by one that uses Triton VM's native
/// support for permutation checks instead of Fiat-Shamir and running
/// products.
impl MultisetEquality {
    fn random_equal_lists(&self, length: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length);
        let mut list_b = list_a.clone();
        list_b.sort();
        let pointer_a: BFieldElement = random();
        let pointer_b: BFieldElement =
            BFieldElement::new(pointer_a.value() + random::<u32>() as u64);

        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
        rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ExecutionState {
            stack: [empty_stack(), vec![pointer_a, pointer_b]].concat(),
            std_in: vec![],
            nondeterminism,
        }
    }

    fn random_unequal_lists(&self, length: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length);
        let list_b: Vec<Digest> = random_elements(length);
        let pointer_a: BFieldElement = random();
        let pointer_b: BFieldElement =
            BFieldElement::new(pointer_a.value() + random::<u32>() as u64);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
        rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ExecutionState {
            stack: [empty_stack(), vec![pointer_a, pointer_b]].concat(),
            std_in: vec![],
            nondeterminism,
        }
    }

    fn random_unequal_length_lists(&self, length_a: usize, length_b: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length_a);
        let list_b: Vec<Digest> = random_elements(length_b);
        let pointer_a: BFieldElement = random();
        let pointer_b: BFieldElement =
            BFieldElement::new(pointer_a.value() + random::<u32>() as u64);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
        rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ExecutionState {
            stack: [empty_stack(), vec![pointer_a, pointer_b]].concat(),
            std_in: vec![],
            nondeterminism,
        }
    }

    fn random_lists_one_element_flipped(&self, length: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length);
        let mut list_b = list_a.clone();
        list_b.sort();
        let manipulated_index = random::<usize>() % length;
        let manipulated_digest_elem_index = random::<usize>() % x_field_element::EXTENSION_DEGREE;
        list_b[manipulated_index].0[manipulated_digest_elem_index] += BFieldElement::one();
        let pointer_a: BFieldElement = random();
        let pointer_b: BFieldElement =
            BFieldElement::new(pointer_a.value() + random::<u32>() as u64);

        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        rust_shadowing_helper_functions::list::list_insert(pointer_a, list_a, &mut memory);
        rust_shadowing_helper_functions::list::list_insert(pointer_b, list_b, &mut memory);

        let nondeterminism = NonDeterminism::default().with_ram(memory);
        ExecutionState {
            stack: [empty_stack(), vec![pointer_a, pointer_b]].concat(),
            std_in: vec![],
            nondeterminism,
        }
    }
}

impl DeprecatedSnippet for MultisetEquality {
    fn entrypoint_name(&self) -> String {
        "tasmlib_list_multiset_equality".into()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec!["list_a".to_string(), "list_b".to_string()]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["multisets_are_equal".to_string()]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bool]
    }

    fn stack_diff(&self) -> isize {
        -1
    }

    fn function_code(&self, library: &mut Library) -> String {
        let length_snippet = library.import(Box::new(Length::new(DataType::Digest)));
        let first_element_offset = LIST_METADATA_SIZE;
        let hash_varlen = library.import(Box::new(HashVarlen));
        let entrypoint = self.entrypoint_name();
        const DIGEST_LENGTH_PLUS_ONE: usize = DIGEST_LENGTH + 1;

        format!(
            "
            // BEFORE: _ list_a list_b
            // AFTER:  _ list_a==list_b (as multisets, or up to permutation)
            {entrypoint}:

                // read lengths of lists
                dup 1 dup 1             // _ list_a list_b list_a list_b
                call {length_snippet}   // _ list_a list_b list_a len_b
                swap 1                  // _ list_a list_b len_b list_a
                call {length_snippet}   // _ list_a list_b len_b len_a

                // equate lengths and return early if possible
                dup 1                   // _ list_a list_b len_b len_a len_b
                eq                      // _ list_a list_b len_b (len_a==len_b)
                push 0 eq               // _ list_a list_b len_b (len_a!=len_b)

                // early return if lengths mismatch
                // otherwise continue
                push 1 swap 1           // _ list_a list_b len_b 1 (len_a!=len_b)
                skiz call {entrypoint}_early_abort
                skiz call {entrypoint}_continue

                // _ (list_a == list_b) (as multisets, or up to permutation)
                return

            {entrypoint}_early_abort:
                // _ list_a list_b len_b 1
                pop 4

                // push return value (false)
                push 0 // _ 0

                // ensure `else` branch is not taken
                push 0
                return

            {entrypoint}_continue:
                // _ list_a list_b len

                // hash list_a
                dup 2 // _ list_a list_b len list_a
                push {first_element_offset} add // _ list_a list_b len (list_a+n)
                dup 1                    // _ list_a list_b len list_a len
                push {DIGEST_LENGTH} mul // _ list_a list_b len list_a) (len*{DIGEST_LENGTH})
                call {hash_varlen}       // _ list_a list_b len da4 da3 da2 da1 da0

                // hash list_b
                dup 6 // _ list_a list_b len da4 da3 da2 da1 da0 list_b
                push {first_element_offset} add // _ list_a list_b len (list_a+n)
                dup 6 // _ list_a list_b len da4 da3 da2 da1 da0 list_b len
                push {DIGEST_LENGTH} mul // _ list_a list_b len da4 da3 da2 da1 da0 list_b (len*{DIGEST_LENGTH})
                call {hash_varlen} // _ list_a list_b len da4 da3 da2 da1 da0 db4 db3 db2 db1 db0

                // hash together
                hash  // _ list_a list_b len d4 d3 d2 d1 d0
                pop 2 // _ list_a list_b len d4 d3 d2

                call {entrypoint}_running_product // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0
                dup 8 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a
                dup 7 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len
                dup 7 dup 7 dup 7 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2
                call {entrypoint}_running_product // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa1 rpa0

                // test equality
                dup 8 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa1 rpa0 rpb0
                eq //  _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa1 rpa0==rpb0
                swap 1 //  _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa0==rpb0 rpa1
                dup 9 //   _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa0==rpb0 rpa1 rpb1
                eq mul // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa2 rpa0==rpb0&&rpa1==rpb1
                swap 1 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa0==rpb0&&rpa1==rpb1 rpa2
                dup 9 // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa0==rpb0&&rpa1==rpb1 rpa2 rpb2
                eq mul // _ list_a list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 rpa0==rpb0&&rpa1==rpb1&&rpa2==rpb2

                // clean up and return
                swap 14 // _ rpa0==rpb0&&rpa1==rpb1 rpa2 rpb2 list_b len d4 d3 d2 rpb2 rpb1 rpb0 list_a len d4 d3 d2 list_a
                pop 5 pop 5 pop 4
                // _ (rpa0==rpb0&&rpa1==rpb1)

                return

            // BEFORE: _ list len d2 d1 d0
            // AFTER:  _ list len d2 d1 d0 rp2 rp1 rp0
            {entrypoint}_running_product:
                // initialize loop
                dup 4 // _ list len d2 d1 d0 list
                push {first_element_offset} add // _ list len d2 d1 d0 addr
                dup 4 // _ list len d2 d1 d0 addr itrs_left
                push 0 push 0 push 1 // _ list len d2 d1 d0 addr itrs_left 0 0 1

                call {entrypoint}_running_product_loop
                // _ list len d2 d1 d0 addr* 0 rp2 rp1 rp0

                // clean up and return
                swap 2 // _ list len d2 d1 d0 addr* 0 rp0 rp1 rp2
                swap 4 // _ list len d2 d1 d0 rp2 0 rp0 rp1 addr*
                pop 1  // _ list len d2 d1 d0 rp2 0 rp0 rp1
                swap 2 // _ list len d2 d1 d0 rp2 rp1 rp0 0
                pop 1  // _ list len d2 d1 d0 rp2 rp1 rp0

                return

            // INVARIANT: _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0
            {entrypoint}_running_product_loop:
                // test termination condition
                dup 3       // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 itrs_left
                push 0 eq   // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 itrs_left==0
                skiz return // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0

                // read addr+2, addr+1, addr+0
                dup 4 push 2 add read_mem 3
                // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 m1 m0 (addr - 1)

                // addr += DIGEST_LENGTH + 1
                push {DIGEST_LENGTH_PLUS_ONE} add // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 m1 m0 addr+{DIGEST_LENGTH}
                swap 8 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left rp2 rp1 rp0 m2 m1 m0 addr
                pop 1  // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left rp2 rp1 rp0 m2 m1 m0

                // itrs_left -= 1
                swap 6              // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} m0 rp2 rp1 rp0 m2 m1 itrs_left
                push -1 add swap 6  // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2 rp1 rp0 m2 m1 m0

                // subtract indeterminate
                dup 10 dup 10 dup 10 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2 rp1 rp0 m2 m1 m0 d2 d1 d0
                push -1 xbmul        // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2 rp1 rp0 m2 m1 m0 -d2 -d1 -d0
                xxadd                // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2 rp1 rp0 (m2-d2) (m1-d1) (m0-d0)
                push -1 xbmul        // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2 rp1 rp0 (d2-m2) (d1-m1) (d0-m0)

                // multiply into running product
                xxmul                // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} (itrs_left-1) rp2' rp1' rp0'

                recurse
            "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["Length exceeds u32::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        vec![
            self.random_equal_lists(0),
            self.random_equal_lists(1),
            self.random_equal_lists(2),
            self.random_equal_lists(3),
            self.random_equal_lists(4),
            self.random_equal_lists(5),
            self.random_equal_lists(6),
            self.random_equal_lists(7),
            self.random_equal_lists(8),
            self.random_equal_lists(9),
            self.random_equal_lists(10),
            self.random_equal_lists(11),
            self.random_equal_lists(12),
            self.random_equal_lists(13),
            self.random_equal_lists(14),
            self.random_equal_lists(15),
            self.random_equal_lists(21),
            self.random_unequal_lists(1),
            self.random_unequal_lists(2),
            self.random_unequal_lists(3),
            self.random_unequal_lists(10),
            self.random_unequal_lists(21),
            self.random_unequal_length_lists(0, 5),
            self.random_unequal_length_lists(1, 2),
            self.random_unequal_length_lists(2, 1),
            self.random_unequal_length_lists(10, 17),
            self.random_unequal_length_lists(21, 0),
            self.random_lists_one_element_flipped(1),
            self.random_lists_one_element_flipped(2),
            self.random_lists_one_element_flipped(3),
            self.random_lists_one_element_flipped(4),
            self.random_lists_one_element_flipped(5),
            self.random_lists_one_element_flipped(7),
            self.random_lists_one_element_flipped(20),
            self.random_lists_one_element_flipped(21),
        ]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        self.random_equal_lists(2)
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        self.random_equal_lists(100)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let list_b_pointer = stack.pop().unwrap();
        let list_a_pointer = stack.pop().unwrap();

        // compare lengths and return early if unequal
        let len_a: u32 = memory
            .get(&list_a_pointer)
            .unwrap()
            .value()
            .try_into()
            .unwrap();
        let len_b: u32 = memory
            .get(&list_b_pointer)
            .unwrap()
            .value()
            .try_into()
            .unwrap();

        if len_a != len_b {
            stack.push(BFieldElement::zero());
            return;
        }

        let len = len_a;

        // prepare lists for hashing
        let mut list_a_bfes = vec![];
        let mut list_b_bfes = vec![];

        let rust_shadowing_helper_list_read = rust_shadowing_helper_functions::list::list_get;

        for i in 0..len as usize {
            list_a_bfes.append(&mut rust_shadowing_helper_list_read(
                list_a_pointer,
                i,
                memory,
                DIGEST_LENGTH,
            ));
            list_b_bfes.append(&mut rust_shadowing_helper_list_read(
                list_b_pointer,
                i,
                memory,
                DIGEST_LENGTH,
            ));
        }

        // hash to get Fiat-Shamir challenge
        let first_element_offset = BFieldElement::new(LIST_METADATA_SIZE as u64);
        let list_a_hash = {
            stack.push(list_a_pointer + first_element_offset);
            stack.push(BFieldElement::new(len as u64 * DIGEST_LENGTH as u64));
            HashVarlen.rust_shadow(stack, memory, &NonDeterminism::default(), &[], &mut None);
            Digest::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ])
        };
        let list_b_hash = {
            stack.push(list_b_pointer + first_element_offset);
            stack.push(BFieldElement::new(len as u64 * DIGEST_LENGTH as u64));
            HashVarlen.rust_shadow(stack, memory, &NonDeterminism::default(), &[], &mut None);
            Digest::new([
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
                stack.pop().unwrap(),
            ])
        };

        let digest = VmHasher::hash_pair(list_a_hash, list_b_hash);
        let indeterminate =
            XFieldElement::new([digest.values()[0], digest.values()[1], digest.values()[2]]);

        // compute running products
        let mut running_product_a = XFieldElement::one();
        for i in 0..len as u64 {
            let digest_elems =
                rust_shadowing_helper_list_read(list_a_pointer, i as usize, memory, DIGEST_LENGTH);
            let m = XFieldElement::new([digest_elems[0], digest_elems[1], digest_elems[2]]);
            let factor = indeterminate - m;
            running_product_a *= factor;
        }
        let mut running_product_b = XFieldElement::one();
        for i in 0..len as u64 {
            let digest_elems =
                rust_shadowing_helper_list_read(list_b_pointer, i as usize, memory, DIGEST_LENGTH);
            let m = XFieldElement::new([digest_elems[0], digest_elems[1], digest_elems[2]]);
            let factor = indeterminate - m;
            running_product_b *= factor;
        }

        // equate and push result to stack
        let result = running_product_a == running_product_b;
        // println!("result: {}", result);
        stack.push(BFieldElement::new(result as u64));
    }
}

#[cfg(test)]
mod tests {
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    use super::*;

    #[test]
    fn prop_test() {
        test_rust_equivalence_multiple_deprecated(&MultisetEquality, true);
    }
}

#[cfg(test)]
mod benches {
    use crate::snippet_bencher::bench_and_write;

    use super::*;

    #[test]
    fn multiset_eq_benchmark() {
        bench_and_write(MultisetEquality);
    }
}
