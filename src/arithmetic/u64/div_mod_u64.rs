use std::collections::HashMap;

use num::Zero;
use rand::RngCore;
use twenty_first::amount::u32s::U32s;
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::Hashable;

use crate::arithmetic::u32::safe_add::SafeAdd;
use crate::arithmetic::u32::safe_sub::SafeSub;
use crate::arithmetic::u64::and_u64::{self, AndU64};
use crate::arithmetic::u64::leading_zeros_u64::{self, LeadingZerosU64};
use crate::arithmetic::u64::lt_u64::{LtStandardU64, LtU64};
use crate::arithmetic::u64::or_u64::{self, OrU64};
use crate::arithmetic::u64::shift_left_u64::{self, ShiftLeftU64};
use crate::arithmetic::u64::shift_right_u64::ShiftRightU64;
use crate::arithmetic::u64::sub_u64::{self, SubU64};
use crate::library::Library;
use crate::snippet::{DataType, Snippet};
use crate::{get_init_tvm_stack, push_hashable, ExecutionState};

#[derive(Clone)]
pub struct DivModU64;

impl Snippet for DivModU64 {
    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u64_div_mod".to_string()
    }

    fn inputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "rhs_hi".to_string(),
            "rhs_lo".to_string(),
            "lhs_hi".to_string(),
            "lhs_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<crate::snippet::DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn outputs(&self) -> Vec<String>
    where
        Self: Sized,
    {
        vec![
            "(lhs / rhs)_hi".to_string(),
            "(lhs / rhs)_lo".to_string(),
            "(lhs % rhs)_hi".to_string(),
            "(lhs % rhs)_lo".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize
    where
        Self: Sized,
    {
        0
    }

    fn function_body(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint();
        let shift_right_u64 = library.import(Box::new(ShiftRightU64));
        let shift_left_u64 = library.import(Box::new(ShiftLeftU64));
        let and_u64 = library.import(Box::new(AndU64));
        let lt_u64 = library.import(Box::new(LtStandardU64));
        let or_u64 = library.import(Box::new(OrU64));
        let sub_u64 = library.import(Box::new(SubU64));
        let sub_u32 = library.import(Box::new(SafeSub));
        let leading_zeros_u64 = library.import(Box::new(LeadingZerosU64));
        let add_u32 = library.import(Box::new(SafeAdd));

        format!(
            "
            // BEFORE: _ numerator_hi numerator_lo divisor_hi divisor_lo
            // AFTER: _ quotient_hi quotient_lo remainder_hi remainder_lo
            {entrypoint}:
            push 0
            dup 1
            write_mem
            push 1
            add
            dup 2
            write_mem
            pop
            dup 3
            dup 3
            push 32
            call {shift_right_u64}
            swap 1
            pop
            dup 4
            dup 4
            push 00000000004294967295
            push 0
            swap 1
            call {and_u64}
            swap 1
            pop
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            push 32
            call {shift_right_u64}
            swap 1
            pop
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            push 00000000004294967295
            push 0
            swap 1
            call {and_u64}
            swap 1
            pop
            push 0
            push 0
            push 0
            push 0
            dup 11
            dup 11
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            dup 3
            dup 3
            call {lt_u64}
            push 1
            swap 1
            skiz
            call _binop_Gt_bool_bool_26_then
            skiz
            call _binop_Gt_bool_bool_26_else
            pop
            pop
            swap 8
            pop
            swap 8
            pop
            swap 8
            pop
            swap 8
            pop
            pop
            pop
            pop
            pop
            return
            _binop_Eq_bool_bool_53_then:
            pop
            dup 8
            dup 7
            swap 1
            div
            pop
            push 0
            swap 1
            dup 10
            dup 9
            swap 1
            div
            swap 1
            pop
            push 0
            swap 1
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            push 0
            return
            _binop_Eq_bool_bool_53_else:
            return
            _binop_Eq_bool_bool_47_then:
            pop
            dup 1
            dup 1
            push 0
            push 0
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            push 0
            return
            _binop_Eq_bool_bool_47_else:
            dup 9
            push 0
            eq
            push 1
            swap 1
            skiz
            call _binop_Eq_bool_bool_53_then
            skiz
            call _binop_Eq_bool_bool_53_else
            return
            _lit_u64_u64_99_then:
            pop
            push 0
            push 0
            push 0
            return
            _lit_u64_u64_99_else:
            push 00000000004294967295
            push 00000000004294967295
            return
            _binop_Gt_bool_bool_81_while_loop:
            dup 4
            push 0
            lt
            push 0
            eq
            skiz
            return
            dup 3
            dup 3
            push 1
            call {shift_left_u64}
            dup 8
            dup 8
            push 63
            call {shift_right_u64}
            call {or_u64}
            swap 4
            pop
            swap 4
            pop
            dup 6
            dup 6
            push 1
            call {shift_left_u64}
            dup 3
            dup 3
            push 0
            push 1
            call {and_u64}
            call {or_u64}
            swap 7
            pop
            swap 7
            pop
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            dup 5
            dup 5
            call {lt_u64}
            push 1
            swap 1
            skiz
            call _lit_u64_u64_99_then
            skiz
            call _lit_u64_u64_99_else
            swap 2
            pop
            swap 2
            pop
            dup 3
            dup 3
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            dup 5
            dup 5
            call {and_u64}
            swap 3
            swap 1
            swap 3
            swap 2
            call {sub_u64}
            swap 4
            pop
            swap 4
            pop
            dup 4
            push 1
            swap 1
            call {sub_u32}
            swap 5
            pop
            recurse
            _binop_Or_bool_bool_44_then:
            pop
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            push 0
            push 1
            swap 3
            eq
            swap 2
            eq
            mul
            push 1
            swap 1
            skiz
            call _binop_Eq_bool_bool_47_then
            skiz
            call _binop_Eq_bool_bool_47_else
            push 0
            return
            _binop_Or_bool_bool_44_else:
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            call {leading_zeros_u64}
            dup 2
            dup 2
            call {leading_zeros_u64}
            swap 1
            call {sub_u32}
            push 1
            call {add_u32}
            dup 2
            dup 2
            dup 2
            call {shift_right_u64}
            dup 4
            dup 4
            push 64
            dup 5
            swap 1
            call {sub_u32}
            call {shift_left_u64}
            swap 5
            pop
            swap 5
            pop
            push 0
            push 0
            call _binop_Gt_bool_bool_81_while_loop
            dup 6
            dup 6
            push 1
            call {shift_left_u64}
            dup 3
            dup 3
            push 0
            push 1
            call {and_u64}
            call {or_u64}
            dup 5
            dup 5
            swap 11
            pop
            swap 11
            pop
            swap 11
            pop
            swap 11
            pop
            pop
            pop
            pop
            pop
            pop
            return
            _binop_Gt_bool_bool_26_then:
            pop
            push 0
            push 0
            dup 3
            dup 3
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            swap 6
            pop
            push 0
            return
            _binop_Gt_bool_bool_26_else:
            dup 7
            push 0
            eq
            push 1
            read_mem
            swap 1
            push -1
            add
            read_mem
            swap 1
            pop
            push 0
            push 1
            swap 3
            eq
            swap 2
            eq
            mul
            add
            push 2
            eq
            dup 8
            push 0
            eq
            dup 11
            push 0
            eq
            add
            push 2
            eq
            add
            push 0
            eq
            push 0
            eq
            push 1
            swap 1
            skiz
            call _binop_Or_bool_bool_44_then
            skiz
            call _binop_Or_bool_bool_44_else
            return
            "
        )
    }

    fn crash_conditions() -> Vec<String>
    where
        Self: Sized,
    {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState>
    where
        Self: Sized,
    {
        let mut rng = rand::thread_rng();
        let lhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let rhs = U32s::<2>::try_from(rng.next_u64()).unwrap();
        let mut stack = get_init_tvm_stack();
        push_hashable(&mut stack, &lhs);
        push_hashable(&mut stack, &rhs);
        vec![ExecutionState::with_stack(stack)]
    }

    fn common_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::zero(), BFieldElement::new((1 << 31) - 1)],
                vec![BFieldElement::zero(), BFieldElement::new((1 << 10) - 1)],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState
    where
        Self: Sized,
    {
        ExecutionState::with_stack(
            vec![
                get_init_tvm_stack(),
                vec![BFieldElement::new(1 << 31), BFieldElement::new(1 << 31)],
                vec![
                    BFieldElement::new(1 << 30),
                    BFieldElement::new((1 << 31) + 10),
                ],
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) where
        Self: Sized,
    {
        // top element on stack
        let divisor_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let divisor_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let divisor: u64 = divisor_lo as u64 + (divisor_hi as u64) << 32;

        // second element on stack
        let numerator_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator: u64 = numerator_lo as u64 + (numerator_hi as u64) << 32;
        let quotient = divisor / numerator;
        let remainder = divisor % numerator;

        let quotient = U32s::<2>::try_from(quotient).unwrap();
        let remainder = U32s::<2>::try_from(remainder).unwrap();
        let mut quotient = quotient.to_sequence();
        for _ in 0..quotient.len() {
            stack.push(quotient.pop().unwrap());
        }
        let mut remainder = remainder.to_sequence();
        for _ in 0..remainder.len() {
            stack.push(remainder.pop().unwrap());
        }
    }
}

#[cfg(test)]
mod tests {
    use num::Zero;
    use rand::Rng;

    use crate::snippet_bencher::bench_and_write;
    use crate::test_helpers::{rust_tasm_equivalence_prop, rust_tasm_equivalence_prop_new};
    use crate::{get_init_tvm_stack, push_hashable};

    use super::*;

    #[test]
    fn div_mod_u64_test() {
        rust_tasm_equivalence_prop_new(DivModU64);
    }

    fn prop_div_mod(numerator: u64, divisor: u64) {
        let mut init_stack = get_init_tvm_stack();

        let rhs_u32_2 =
            U32s::<2>::new([(divisor & u32::MAX as u64) as u32, (divisor >> 32) as u32]);
        for elem in rhs_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let lhs_u32_2 = U32s::<2>::new([
            (numerator & u32::MAX as u64) as u32,
            (numerator >> 32) as u32,
        ]);
        for elem in lhs_u32_2.to_sequence().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected_res: BigUint = (numerator & divisor).into();
        println!("Expected: {expected_res}");
        let expected_u32_2: U32s<2> = expected_res.into();
        let mut expected_end_stack = get_init_tvm_stack();
        for elem in expected_u32_2.to_sequence().into_iter().rev() {
            expected_end_stack.push(elem);
        }

        let _execution_result = rust_tasm_equivalence_prop::<AndU64>(
            AndU64,
            &init_stack,
            &[],
            &[],
            &mut HashMap::default(),
            0,
            Some(&expected_end_stack),
        );
    }
}
