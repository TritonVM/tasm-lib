use std::collections::HashMap;

use num::Zero;
use num_traits::One;
use rand::random;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::x_field_element::XFieldElement;
use twenty_first::{
    shared_math::b_field_element::BFieldElement, util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::hashing::hash_varlen::HashVarlen;
use crate::{get_init_tvm_stack, Digest, DIGEST_LENGTH};
use crate::{
    list::safe_u32::length::SafeLength,
    snippet::{DataType, Snippet},
    snippet_state::SnippetState,
    ExecutionState, VmHasher,
};

#[derive(Clone, Debug)]
pub struct MultisetEquality;

/// Determine whether two lists are equal up to permutation. The
/// lists are given as lists of digests. This function uses hashing
/// to compute a challenge indeterminate, and then computes a running
/// products for both lists. In the future, the implementation of
/// function may be replaced by one that uses Triton VM's native
/// support for permutation checks instead of Fiat-Shamir and running
/// products.
impl MultisetEquality {
    fn random_equal_lists(length: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length);
        let mut list_b = list_a.clone();
        list_b.sort();
        let memory_a: BFieldElement = random();
        let memory_b: BFieldElement = BFieldElement::new(memory_a.value() + random::<u32>() as u64);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        memory.insert(memory_a, BFieldElement::new(length as u64));
        memory.insert(memory_b, BFieldElement::new(length as u64));
        for i in 0..length {
            for j in 0..DIGEST_LENGTH {
                memory.insert(
                    memory_a + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_a[i].values()[j],
                );
                memory.insert(
                    memory_b + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_b[i].values()[j],
                );
            }
        }

        ExecutionState {
            stack: vec![get_init_tvm_stack(), vec![memory_a, memory_b]].concat(),
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 1,
        }
    }

    fn random_unequal_lists(length: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length);
        let list_b: Vec<Digest> = random_elements(length);
        let memory_a: BFieldElement = random();
        let memory_b: BFieldElement = BFieldElement::new(memory_a.value() + random::<u32>() as u64);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        memory.insert(memory_a, BFieldElement::new(length as u64));
        memory.insert(memory_b, BFieldElement::new(length as u64));
        for i in 0..length {
            for j in 0..DIGEST_LENGTH {
                memory.insert(
                    memory_a + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_a[i].values()[j],
                );
                memory.insert(
                    memory_b + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_b[i].values()[j],
                );
            }
        }

        ExecutionState {
            stack: vec![get_init_tvm_stack(), vec![memory_a, memory_b]].concat(),
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 1,
        }
    }

    fn random_unequal_length_lists(length_a: usize, length_b: usize) -> ExecutionState {
        let list_a: Vec<Digest> = random_elements(length_a);
        let list_b: Vec<Digest> = random_elements(length_b);
        let memory_a: BFieldElement = random();
        let memory_b: BFieldElement = BFieldElement::new(memory_a.value() + random::<u32>() as u64);
        let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        memory.insert(memory_a, BFieldElement::new(length_a as u64));
        memory.insert(memory_b, BFieldElement::new(length_b as u64));
        for (i, list_ai) in list_a.iter().enumerate() {
            for j in 0..DIGEST_LENGTH {
                memory.insert(
                    memory_a + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_ai.values()[j],
                );
            }
        }
        for (i, list_bi) in list_b.iter().enumerate() {
            for j in 0..DIGEST_LENGTH {
                memory.insert(
                    memory_b + BFieldElement::new((1 + i * DIGEST_LENGTH + j) as u64),
                    list_bi.values()[j],
                );
            }
        }

        ExecutionState {
            stack: vec![get_init_tvm_stack(), vec![memory_a, memory_b]].concat(),
            std_in: vec![],
            secret_in: vec![],
            memory,
            words_allocated: 1,
        }
    }
}

impl Snippet for MultisetEquality {
    fn entrypoint(&self) -> String {
        "tasm_list_multiset_equality".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["list_a".to_string(), "list_b".to_string()]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![
            DataType::List(Box::new(DataType::Digest)),
            DataType::List(Box::new(DataType::Digest)),
        ]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::Bool]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec!["multisets_are_equal".to_string()]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        -1
    }

    fn function_body(&self, library: &mut SnippetState) -> String {
        let safe_length = library.import(Box::new(SafeLength(DataType::Digest)));
        let hash_varlen = library.import(Box::new(HashVarlen));
        let entrypoint = self.entrypoint();

        format!(
            "
            // BEFORE: _ list_a list_b
            // AFTER: _ list_a==list_b (as multisets, or up to permutation)
            {entrypoint}:

                // read lengths of lists
                dup 1 dup 1 // _ list_a list_b list_a list_b
                call {safe_length} // _ list_a list_b list_a len_b
                swap 1 // _ list_a list_b len_b list_a
                call {safe_length} // _ list_a list_b len_b len_a

                // equate lengths and return early if possible
                dup 1 // _ list_a list_b len_b len_a len_b
                eq // _ list_a list_b len_b len_a==len_b
                push 0 eq // _ list_a list_b len_b len_a!=len_b

                // early return if lengths mismatch
                // otherwise continue
                push 1 swap 1 // _ list_a list_b len_b 1 len_a!=len_b
                skiz call {entrypoint}_early_abort
                skiz call {entrypoint}_continue
                return

            {entrypoint}_early_abort:
                pop // _ list_a list_b len_b
                pop pop pop
                push 0 // _ 0

                push 0
                return

            {entrypoint}_continue:
                // _ list_a list_b len

                // hash list_a
                dup 2 // _ list_a list_b len list_a
                push 1 add // _ list_a list_b len (list_a+1)
                dup 1 // _ list_a list_b len (list_a+1) len
                push {DIGEST_LENGTH} mul // _ list_a list_b len (list_a+1) (len*{DIGEST_LENGTH})
                call {hash_varlen} // _ list_a list_b len da4 da3 da2 da1 da0

                // hash list_b
                dup 6 // _ list_a list_b len da4 da3 da2 da1 da0 list_b
                push 1 add // _ list_a list_b len da4 da3 da2 da1 da0 (list_b+1)
                dup 6 // _ list_a list_b len da4 da3 da2 da1 da0 (list_b+1) len
                push {DIGEST_LENGTH} mul // _ list_a list_b len da4 da3 da2 da1 da0 (list_b+1) (len*{DIGEST_LENGTH})
                call {hash_varlen} // _ list_a list_b len da4 da3 da2 da1 da0 db4 db3 db2 db1 db0
                
                // hash together
                hash // _ list_a list_b len d4 d3 d2 d1 d0 0 0 0 0 0
                pop pop pop pop pop
                pop pop // _ list_a list_b len d4 d3 d2

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
                pop pop pop pop pop pop pop pop pop pop pop pop pop pop
                // _ rpa0==rpb0&&rpa1==rpb1

                return

            // BEFORE: _ list len d2 d1 d0
            // AFTER: _ list len d2 d1 d0 rp2 rp1 rp0
            {entrypoint}_running_product:
                // initialize loop
                dup 4 // _ list len d2 d1 d0 list
                push 1 add // _ list len d2 d1 d0 addr
                dup 4 // _ list len d2 d1 d0 addr itrs_left
                push 0 push 0 push 1 // _ list len d2 d1 d0 addr itrs_left 0 0 1

                call {entrypoint}_running_product_loop
                // _ list len d2 d1 d0 addr* 0 rp2 rp1 rp0

                // clean up and return
                swap 2 // _ list len d2 d1 d0 addr* 0 rp0 rp1 rp2
                swap 4 // _ list len d2 d1 d0 rp2 0 rp0 rp1 addr*
                pop // _ list len d2 d1 d0 rp2 0 rp0 rp1
                swap 2 // _ list len d2 d1 d0 rp2 rp1 rp0 0
                pop // _ list len d2 d1 d0 rp2 rp1 rp0

                return

            // INVARIANT: _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0
            {entrypoint}_running_product_loop:
                // test termination condition
                dup 3 // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 itrs_left
                push 0 eq // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 itrs_left==0
                skiz return // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0

                // read addr+2, addr+1, addr+0
                dup 4 push 2 add read_mem // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 addr+2 m2
                swap 1 push -1 add read_mem // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 addr+1 m1
                swap 1 push -1 add read_mem // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 m1 addr m0
                swap 1 // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 m1 m0 addr

                // addr += {DIGEST_LENGTH}
                push {DIGEST_LENGTH} add // _ list len d2 d1 d0 addr itrs_left rp2 rp1 rp0 m2 m1 m0 addr+{DIGEST_LENGTH}
                swap 8 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left rp2 rp1 rp0 m2 m1 m0 addr
                pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left rp2 rp1 rp0 m2 m1 m0

                // itrs_left -= 1
                swap 6 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} m0 rp2 rp1 rp0 m2 m1 itrs_left
                push -1 add swap 6 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 m0

                // subtract indeterminate
                dup 10 dup 10 dup 10 // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 m0 d2 d1 d0
                push -1 xbmul // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 m0 -d2 -d1 -d0
                xxadd // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 m0 m2-d2 m1-d1 m0-d0
                push -1 xbmul // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 m0 d2-m2 d1-m1 d0-m0
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 m1 d0-m0 d2-m2 d1-m1
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 m2 d1-m1 d0-m0 d2-m2
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 d2-m2 d1-m1 d0-m0

                // multiply into running product
                xxmul // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0 rp2* rp1* rp0*
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1 rp0* rp2* rp1*
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2 rp1* rp0* rp2*
                swap 3 pop // _ list len d2 d1 d0 addr+{DIGEST_LENGTH} itrs_left-1 rp2* rp1* rp0* rp2*

                recurse
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec!["Length exceeds u32::MAX".to_string()]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        vec![
            Self::random_equal_lists(0),
            Self::random_equal_lists(1),
            Self::random_equal_lists(2),
            Self::random_equal_lists(10),
            Self::random_equal_lists(21),
            Self::random_unequal_lists(1),
            Self::random_unequal_lists(2),
            Self::random_unequal_lists(10),
            Self::random_unequal_lists(21),
            Self::random_unequal_length_lists(0, 5),
            Self::random_unequal_length_lists(1, 2),
            Self::random_unequal_length_lists(2, 1),
            Self::random_unequal_length_lists(10, 17),
            Self::random_unequal_length_lists(21, 0),
        ]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        Self::random_equal_lists(2)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        Self::random_equal_lists(100)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        _std_in: Vec<triton_vm::BFieldElement>,
        _secret_in: Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) where
        Self: Sized,
    {
        let list_b = stack.pop().unwrap();
        let list_a = stack.pop().unwrap();

        // compare lengths and return early if unequal
        let len_a: u32 = memory.get(&list_a).unwrap().value().try_into().unwrap();
        let len_b: u32 = memory.get(&list_b).unwrap().value().try_into().unwrap();

        if len_a != len_b {
            stack.push(BFieldElement::zero());
            return;
        }

        let len = len_a;

        // prepare lists for hashing
        let mut list_a_bfes = vec![];
        let mut list_b_bfes = vec![];
        for i in 0..len as u64 {
            for j in 0..DIGEST_LENGTH as u64 {
                list_a_bfes.push(
                    memory
                        .get(&(list_a + BFieldElement::new(1u64 + i * DIGEST_LENGTH as u64 + j)))
                        .unwrap()
                        .to_owned(),
                );
                list_b_bfes.push(
                    memory
                        .get(&(list_b + BFieldElement::new(1u64 + i * DIGEST_LENGTH as u64 + j)))
                        .unwrap()
                        .to_owned(),
                );
            }
        }

        // hash to get Fiat-Shamir challenge
        let list_a_hash = VmHasher::hash_varlen(&list_a_bfes);
        let list_b_hash = VmHasher::hash_varlen(&list_b_bfes);
        let digest = VmHasher::hash_pair(&list_a_hash, &list_b_hash);
        let indeterminate =
            XFieldElement::new([digest.values()[0], digest.values()[1], digest.values()[2]]);

        // compute running products
        let mut running_product_a = XFieldElement::one();
        for i in 0..len as u64 {
            let m2 = memory
                .get(&(list_a + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m1 = memory
                .get(&(list_a + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m0 = memory
                .get(&(list_a + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m = XFieldElement::new([m0, m1, m2]);
            let factor = indeterminate - m;
            running_product_a *= factor;
        }
        let mut running_product_b = XFieldElement::one();
        for i in 0..len as u64 {
            let m2 = memory
                .get(&(list_b + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m1 = memory
                .get(&(list_b + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m0 = memory
                .get(&(list_b + BFieldElement::new(1 + i * DIGEST_LENGTH as u64 + 2)))
                .unwrap()
                .to_owned();
            let m = XFieldElement::new([m0, m1, m2]);
            let factor = indeterminate - m;
            running_product_b *= factor;
        }

        // equate and push result to stack
        stack.push(BFieldElement::new(
            (running_product_a == running_product_b) as u64,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{snippet_bencher::bench_and_write, test_helpers::rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn new_prop_test() {
        rust_tasm_equivalence_prop_new(MultisetEquality);
    }

    #[test]
    fn leaf_index_to_mt_index_benchmark() {
        bench_and_write(MultisetEquality);
    }
}
