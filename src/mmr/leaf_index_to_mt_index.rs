use std::collections::HashMap;

use num::BigUint;
use rand::{thread_rng, Rng};
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;
use twenty_first::util_types::mmr;

use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::lt_u64::LtU64;
use crate::arithmetic::u64::pow2_u64::Pow2U64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, ExecutionState};

#[derive(Clone)]
pub struct MmrLeafIndexToMtIndexAndPeakIndex;

impl Snippet for MmrLeafIndexToMtIndexAndPeakIndex {
    fn inputs(&self) -> Vec<String> {
        vec![
            "leaf_count_hi".to_string(),
            "leaf_count_lo".to_string(),
            "leaf_index_hi".to_string(),
            "leaf_index_lo".to_string(),
        ]
    }

    fn outputs(&self) -> Vec<String> {
        vec![
            "mt_index_hi".to_string(),
            "mt_index_lo".to_string(),
            "peak_index".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U32]
    }

    fn crash_conditions() -> Vec<String> {
        vec!["Input values are not valid u32s".to_string()]
    }

    fn gen_input_states(&self) -> Vec<crate::ExecutionState> {
        let mut ret: Vec<ExecutionState> = vec![];
        for _ in 0..10 {
            let leaf_count = thread_rng().gen_range(0..u64::MAX / 2);
            let leaf_index = thread_rng().gen_range(0..leaf_count);
            ret.push(prepare_state(leaf_count, leaf_index))
        }

        ret
    }

    fn stack_diff(&self) -> isize {
        // Consumes leaf_index: u64 and leaf_count: u64. Pushes MT index: u64 and peak index: u32
        -1
    }

    fn entrypoint(&self) -> String {
        "tasm_mmr_leaf_index_to_mt_index_and_peak_index".to_string()
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let log_2_floor_u64 = library.import(Box::new(Log2FloorU64));
        let lt_u64 = library.import(Box::new(LtU64));
        let add_u64 = library.import(Box::new(AddU64));
        let and_u64 = library.import(Box::new(AndU64));
        let pow2_u64 = library.import(Box::new(Pow2U64));
        let sub_u64 = library.import(Box::new(SubU64));
        let eq_u64 = library.import(Box::new(EqU64));

        format!(
            "
        // Before: _ leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo
        // After: _ mt_index_hi mt_index_lo peak_index
        {entrypoint}:
            // assert that leaf_index < leaf_count
            call {lt_u64}
            assert
            // stack: _ leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo

            dup3 dup3
            call {log_2_floor_u64}
            // stack: _ leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo max_tree_height

            // Rename: max_tree_height -> h, leaf_index -> ret
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h

            push 0
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index

            call {entrypoint}_loop
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo

            // ret -> ret + maybe_pow
            dup5 dup5 call {add_u64}
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index (ret + maybe_pow)_hi (ret + maybe_pow)_lo

            swap6 pop swap6 pop
            // stack: _ (ret + maybe_pow)_hi (ret + maybe_pow)_lo ret_hi ret_lo h peak_index

            swap3 pop pop pop
            // stack: _ (ret + maybe_pow)_hi (ret + maybe_pow)_lo peak_index

            // rename: (ret + maybe_pow) -> ret
            // stack: _ ret_hi ret_lo peak_index

            return

        // Start/end stack: // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index
        // On return: stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo
        {entrypoint}_loop:
            dup1
            call {pow2_u64}
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index pow_hi pow_lo

            dup7 dup7
            call {and_u64}
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo

            // If h == 0 || ret < maybe_pow, then return from loop
            dup5 dup5 call {lt_u64}
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo ret_hi ret_lo (ret < maybe_pow)

            swap2 pop pop
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo (ret < maybe_pow)

            dup4 push 0 eq
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo (ret < maybe_pow) (h == 0)

            add
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo (ret < maybe_pow || h == 0)

            skiz return
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo

            // update ret: ret -> ret - maybe_pow
            dup1 dup1 dup7 dup7 call {sub_u64}
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo (ret - maybe_pow)_hi (ret - maybe_pow)_lo

            swap6 pop swap6 pop
            // stack: _ leaf_count_hi leaf_count_lo (ret - maybe_pow)_hi (ret - maybe_pow)_lo h peak_index maybe_pow_hi maybe_pow_lo

            // rename (ret - maybe_pow) -> ret
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index maybe_pow_hi maybe_pow_lo

            push 0 push 0 call {eq_u64} push 0 eq
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index (maybe_pow != 0)

            add
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h (peak_index + (maybe_pow != 0))

            // rename: (peak_index + (maybe_pow != 0)) -> peak_index
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index

            // update h -> h - 1
            swap1 push -1 add swap1
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo (h - 1) peak_index

            // rename: h - 1 -> h
            // stack: _ leaf_count_hi leaf_count_lo ret_hi ret_lo h peak_index

            recurse
            "
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let leaf_index_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_index: u64 = ((leaf_index_hi as u64) << 32) + leaf_index_lo as u64;

        let leaf_count_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let leaf_count: u64 = ((leaf_count_hi as u64) << 32) + leaf_count_lo as u64;

        let (mt_index, peak_index) = mmr::shared::leaf_index_to_mt_index_and_peak_index(
            leaf_index as u128,
            leaf_count as u128,
        );
        let mt_index = mt_index as u64;
        let mt_index: U32s<2> = U32s::from(BigUint::from(mt_index));

        stack.append(&mut mt_index.to_sequence().into_iter().rev().collect());
        stack.push(BFieldElement::new(peak_index as u64));
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 31) - 1, (1 << 30) + 1)
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        prepare_state((1 << 63) - 1, (1 << 62) + 1)
    }
}

fn prepare_state(leaf_count: u64, leaf_index: u64) -> ExecutionState {
    let mut stack = get_init_tvm_stack();
    let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
    let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
    stack.push(leaf_count_hi);
    stack.push(leaf_count_lo);
    let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
    let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
    stack.push(leaf_index_hi);
    stack.push(leaf_index_lo);
    ExecutionState::with_stack(stack)
}

#[cfg(test)]
mod tests {
    use twenty_first::shared_math::b_field_element::BFieldElement;

    use crate::get_init_tvm_stack;
    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};

    use super::*;

    #[test]
    fn leaf_index_to_mt_index_test() {
        rust_tasm_equivalence_prop_new(MmrLeafIndexToMtIndexAndPeakIndex);
    }

    #[test]
    fn leaf_index_to_mt_index_benchmark() {
        bench_and_write(MmrLeafIndexToMtIndexAndPeakIndex);
    }

    #[test]
    fn leaf_index_to_mt_index_size_is_one_test() {
        // Leaf count = 1
        prop_leaf_index_to_mt_index_and_peak_index(0, 1, 1, 0);
    }

    #[test]
    fn leaf_index_to_mt_index_tests() {
        // Leaf count = 1
        prop_leaf_index_to_mt_index_and_peak_index(0, 1, 1, 0);

        // Leaf count = 2
        prop_leaf_index_to_mt_index_and_peak_index(0, 2, 2, 0);
        prop_leaf_index_to_mt_index_and_peak_index(1, 2, 3, 0);

        // Leaf count = 3
        prop_leaf_index_to_mt_index_and_peak_index(0, 3, 2, 0);
        prop_leaf_index_to_mt_index_and_peak_index(1, 3, 3, 0);
        prop_leaf_index_to_mt_index_and_peak_index(2, 3, 1, 1);

        // Leaf count = 4
        prop_leaf_index_to_mt_index_and_peak_index(0, 4, 4, 0);
        prop_leaf_index_to_mt_index_and_peak_index(1, 4, 5, 0);
        prop_leaf_index_to_mt_index_and_peak_index(2, 4, 6, 0);
        prop_leaf_index_to_mt_index_and_peak_index(3, 4, 7, 0);

        // Leaf count = 5
        prop_leaf_index_to_mt_index_and_peak_index(0, 5, 4, 0);
        prop_leaf_index_to_mt_index_and_peak_index(1, 5, 5, 0);
        prop_leaf_index_to_mt_index_and_peak_index(2, 5, 6, 0);
        prop_leaf_index_to_mt_index_and_peak_index(3, 5, 7, 0);
        prop_leaf_index_to_mt_index_and_peak_index(4, 5, 1, 1);

        // Leaf count = 14
        prop_leaf_index_to_mt_index_and_peak_index(0, 14, 8, 0);
        prop_leaf_index_to_mt_index_and_peak_index(1, 14, 9, 0);
        prop_leaf_index_to_mt_index_and_peak_index(2, 14, 10, 0);
        prop_leaf_index_to_mt_index_and_peak_index(3, 14, 11, 0);
        prop_leaf_index_to_mt_index_and_peak_index(4, 14, 12, 0);
        prop_leaf_index_to_mt_index_and_peak_index(5, 14, 13, 0);
        prop_leaf_index_to_mt_index_and_peak_index(6, 14, 14, 0);
        prop_leaf_index_to_mt_index_and_peak_index(7, 14, 15, 0);
        prop_leaf_index_to_mt_index_and_peak_index(8, 14, 4, 1);
        prop_leaf_index_to_mt_index_and_peak_index(9, 14, 5, 1);
        prop_leaf_index_to_mt_index_and_peak_index(10, 14, 6, 1);
        prop_leaf_index_to_mt_index_and_peak_index(11, 14, 7, 1);
        prop_leaf_index_to_mt_index_and_peak_index(12, 14, 2, 2);
        prop_leaf_index_to_mt_index_and_peak_index(13, 14, 3, 2);

        // Leaf count = 32
        let mut leaf_count = 32;
        for i in 0..leaf_count {
            prop_leaf_index_to_mt_index_and_peak_index(i, leaf_count, 32 + i, 0);
        }

        // Leaf count = 33
        leaf_count = 33;
        for i in 0..32 {
            prop_leaf_index_to_mt_index_and_peak_index(i, leaf_count, 32 + i, 0);
        }
        prop_leaf_index_to_mt_index_and_peak_index(32, leaf_count, 1, 1);

        // Leaf count = 35
        leaf_count = 35;
        for i in 0..32 {
            prop_leaf_index_to_mt_index_and_peak_index(i, leaf_count, 32 + i, 0);
        }
        prop_leaf_index_to_mt_index_and_peak_index(32, leaf_count, 2, 1);
        prop_leaf_index_to_mt_index_and_peak_index(33, leaf_count, 3, 1);
        prop_leaf_index_to_mt_index_and_peak_index(34, leaf_count, 1, 2);

        // Leaf count = 36
        leaf_count = 36;
        for i in 0..32 {
            prop_leaf_index_to_mt_index_and_peak_index(i, leaf_count, 32 + i, 0);
        }
        prop_leaf_index_to_mt_index_and_peak_index(32, leaf_count, 4, 1);
        prop_leaf_index_to_mt_index_and_peak_index(33, leaf_count, 5, 1);
        prop_leaf_index_to_mt_index_and_peak_index(34, leaf_count, 6, 1);
        prop_leaf_index_to_mt_index_and_peak_index(35, leaf_count, 7, 1);

        // Leaf count = 37
        leaf_count = 37;
        for i in 0..32 {
            prop_leaf_index_to_mt_index_and_peak_index(i, leaf_count, 32 + i, 0);
        }
        prop_leaf_index_to_mt_index_and_peak_index(32, leaf_count, 4, 1);
        prop_leaf_index_to_mt_index_and_peak_index(33, leaf_count, 5, 1);
        prop_leaf_index_to_mt_index_and_peak_index(34, leaf_count, 6, 1);
        prop_leaf_index_to_mt_index_and_peak_index(35, leaf_count, 7, 1);
        prop_leaf_index_to_mt_index_and_peak_index(36, leaf_count, 1, 2);

        // Bigger MMRs
        for i in 10..20 {
            prop_leaf_index_to_mt_index_and_peak_index(14, 1 << i, 14 + (1 << i), 0);
            prop_leaf_index_to_mt_index_and_peak_index((1 << i) + 9, (1 << i) + 11, 3, 2);
            prop_leaf_index_to_mt_index_and_peak_index((1 << i) + 10, (1 << i) + 11, 1, 3);
        }

        // 2^32
        // prop_leaf_index_to_mt_index_and_peak_index((1 << 32) - 1, 1 << 32, 1 << 33, 0);
        prop_leaf_index_to_mt_index_and_peak_index(5550001, 1 << 31, (1 << 31) + 5550001, 0);
        prop_leaf_index_to_mt_index_and_peak_index(
            5550001,
            (1 << 31) + (1 << 20),
            (1 << 31) + 5550001,
            0,
        );
        prop_leaf_index_to_mt_index_and_peak_index(
            (1 << 31) + (1 << 20) - 2,
            (1 << 31) + (1 << 20) - 1,
            1,
            20,
        );

        // 2^64
        prop_leaf_index_to_mt_index_and_peak_index(
            (1 << 63) + (1 << 62) - 2,
            (1 << 63) + (1 << 62) - 1,
            1,
            62,
        );
    }

    fn prop_leaf_index_to_mt_index_and_peak_index(
        leaf_index: u64,
        leaf_count: u64,
        expected_mt_index: u64,
        expected_peak_index: u32,
    ) {
        let mut init_stack = get_init_tvm_stack();
        let leaf_count_hi = BFieldElement::new(leaf_count >> 32);
        let leaf_count_lo = BFieldElement::new(leaf_count & u32::MAX as u64);
        init_stack.push(leaf_count_hi);
        init_stack.push(leaf_count_lo);

        let leaf_index_hi = BFieldElement::new(leaf_index >> 32);
        let leaf_index_lo = BFieldElement::new(leaf_index & u32::MAX as u64);
        init_stack.push(leaf_index_hi);
        init_stack.push(leaf_index_lo);

        // _ (right_lineage_count:u32) (height: u32)
        let mut expected = get_init_tvm_stack();
        expected.push(BFieldElement::new(expected_mt_index >> 32));
        expected.push(BFieldElement::new(expected_mt_index & u32::MAX as u64));
        expected.push(BFieldElement::new(expected_peak_index as u64));

        let _execution_result = rust_tasm_equivalence_prop::<MmrLeafIndexToMtIndexAndPeakIndex>(
            MmrLeafIndexToMtIndexAndPeakIndex,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected),
        );
    }
}
