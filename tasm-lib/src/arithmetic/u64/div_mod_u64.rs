use std::collections::HashMap;

use itertools::Itertools;
use num::One;
use rand::prelude::*;
use triton_vm::prelude::*;
use triton_vm::twenty_first::prelude::U32s;

use crate::arithmetic::u32::safeadd::Safeadd;
use crate::arithmetic::u32::safesub::Safesub;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::leading_zeros_u64::LeadingZerosU64;
use crate::arithmetic::u64::lt_u64::LtU64ConsumeArgs;
use crate::arithmetic::u64::or_u64::OrU64;
use crate::arithmetic::u64::shift_left_u64::ShiftLeftU64;
use crate::arithmetic::u64::shift_right_u64::ShiftRightU64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
use crate::traits::deprecated_snippet::DeprecatedSnippet;
use crate::InitVmState;

#[derive(Clone, Debug)]
pub struct DivModU64;

impl DeprecatedSnippet for DivModU64 {
    fn entrypoint_name(&self) -> String {
        "tasmlib_arithmetic_u64_div_mod".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "numerator_hi".to_string(),
            "numerator_lo".to_string(),
            "divisor_hi".to_string(),
            "divisor_lo".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::U64, DataType::U64]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec![
            "(numerator / divisor)_hi".to_string(),
            "(numerator / divisor)_lo".to_string(),
            "(numerator % divisor)_hi".to_string(),
            "(numerator % divisor)_lo".to_string(),
        ]
    }

    fn stack_diff(&self) -> isize {
        0
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let shift_right_u64 = library.import(Box::new(ShiftRightU64));
        let shift_left_u64 = library.import(Box::new(ShiftLeftU64));
        let and_u64 = library.import(Box::new(AndU64));
        let lt_u64 = library.import(Box::new(LtU64ConsumeArgs));
        let or_u64 = library.import(Box::new(OrU64));
        let sub_u64 = library.import(Box::new(SubU64));
        let sub_u32 = library.import(Box::new(Safesub));
        let leading_zeros_u64 = library.import(Box::new(LeadingZerosU64));
        let add_u32 = library.import(Box::new(Safeadd));
        let spilled_divisor_alloc = library.kmalloc(2);

        // The below code has been compiled from a Rust implementation of an LLVM function
        // called `divmoddi4` that can do u64 divmod with only access to u32 bit divmod and
        // some u64 arithmetic instructions or functions. The compiler used for this was the
        // `tasm-lang` compiler: https://github.com/TritonVM/tasm-lang
        // You could probably get a smaller cycle count if you hand-compiled the function.
        triton_asm!(
            // BEFORE: _ numerator_hi numerator_lo divisor_hi divisor_lo
            // AFTER:  _ quotient_hi quotient_lo remainder_hi remainder_lo
            {entrypoint}:
                dup 1
                dup 1
                push {spilled_divisor_alloc.write_address()}
                write_mem 2
                pop 1
                dup 3
                dup 3
                push 32
                call {shift_right_u64}
                swap 1
                pop 1
                dup 4
                dup 4
                push 00000000004294967295
                push 0
                swap 1
                call {and_u64}
                swap 1
                pop 1
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
                push 32
                call {shift_right_u64}
                swap 1
                pop 1
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
                push 00000000004294967295
                push 0
                swap 1
                call {and_u64}
                swap 1
                pop 1
                push 0
                push 0
                push 0
                push 0
                dup 11
                dup 11
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
                dup 3
                dup 3
                call {lt_u64}
                push 1
                swap 1
                skiz
                call _binop_Gt_bool_bool_26_then
                skiz
                call _binop_Gt_bool_bool_26_else
                pop 2
                swap 8
                pop 1
                swap 8
                pop 1
                swap 8
                pop 1
                swap 8
                pop 5
                return
                _binop_Eq_bool_bool_53_then:
                pop 1
                dup 8
                dup 7
                swap 1
                div_mod
                pop 1
                push 0
                swap 1
                dup 10
                dup 9
                swap 1
                div_mod
                swap 1
                pop 1
                push 0
                swap 1
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                push 0
                return
                _binop_Eq_bool_bool_53_else:
                return
                _binop_Eq_bool_bool_47_then:
                pop 1
                dup 1
                dup 1
                push 0
                push 0
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
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
                pop 1
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
                pop 1
                swap 4
                pop 1
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
                pop 1
                swap 7
                pop 1
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
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
                pop 1
                swap 2
                pop 1
                dup 3
                dup 3
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
                dup 5
                dup 5
                call {and_u64}
                swap 3
                swap 1
                swap 3
                swap 2
                call {sub_u64}
                swap 4
                pop 1
                swap 4
                pop 1
                dup 4
                push 1
                swap 1
                call {sub_u32}
                swap 5
                pop 1
                recurse
                _binop_Or_bool_bool_44_then:
                pop 1
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
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
                push 0
                push 0
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
                swap 3
                eq
                swap 2
                eq
                mul
                push 0
                eq
                assert
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
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
                pop 1
                swap 5
                pop 1
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
                pop 1
                swap 11
                pop 1
                swap 11
                pop 1
                swap 11
                pop 5
                pop 1
                return
                _binop_Gt_bool_bool_26_then:
                pop 1
                push 0
                push 0
                dup 3
                dup 3
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                swap 6
                pop 1
                push 0
                return
                _binop_Gt_bool_bool_26_else:
                dup 7
                push 0
                eq
                push {spilled_divisor_alloc.read_address()}
                read_mem {spilled_divisor_alloc.num_words()}
                pop 1
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
        )
        .iter()
        .join("\n")
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec!["inputs are not valid u32s".to_owned()]
    }

    fn gen_input_states(&self) -> Vec<InitVmState> {
        let mut rng = rand::thread_rng();

        let mut ret = vec![];
        for i in 0..32 {
            for j in 0..32 {
                for _ in 0..2 {
                    ret.push(prepare_state(
                        rng.next_u32() as u64 + (1 << i),
                        rng.next_u32() as u64 + (1 << j),
                    ))
                }
            }
        }

        ret
    }

    fn common_case_input_state(&self) -> InitVmState {
        prepare_state(u32::MAX as u64, 1 << 15)
    }

    fn worst_case_input_state(&self) -> InitVmState {
        prepare_state(u64::MAX, (1 << 32) + 45454545)
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        // top element on stack
        let divisor_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let divisor_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let divisor: u64 = divisor_lo as u64 + ((divisor_hi as u64) << 32);

        // second element on stack
        let numerator_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let numerator: u64 = numerator_lo as u64 + ((numerator_hi as u64) << 32);

        let quotient = numerator / divisor;
        let quotient_u32_2 = U32s::<2>::try_from(quotient).unwrap();
        for bfe in quotient_u32_2.encode().into_iter().rev() {
            stack.push(bfe);
        }

        let remainder = numerator % divisor;

        let remainder = U32s::<2>::try_from(remainder).unwrap();
        for bfe in remainder.encode().into_iter().rev() {
            stack.push(bfe);
        }

        // Because of spilling, the divisor is stored in memory. This spilling could probably be
        // avoided if the code didn't go through the tasm-lang compiler but was handcompiled
        // instead.
        let static_address_0 = STATIC_MEMORY_FIRST_ADDRESS;
        let static_address_1 = static_address_0 - BFieldElement::one();
        memory.insert(static_address_0, BFieldElement::from(divisor_hi));
        memory.insert(static_address_1, BFieldElement::from(divisor_lo));
    }
}

fn prepare_state(numerator: u64, divisor: u64) -> InitVmState {
    InitVmState::with_stack(
        [
            empty_stack(),
            vec![
                BFieldElement::new(numerator >> 32),
                BFieldElement::new(numerator & u32::MAX as u64),
            ],
            vec![
                BFieldElement::new(divisor >> 32),
                BFieldElement::new(divisor & u32::MAX as u64),
            ],
        ]
        .concat(),
    )
}

#[cfg(test)]
mod tests {
    use num::BigUint;

    use super::*;
    use crate::empty_stack;
    use crate::test_helpers::test_rust_equivalence_given_input_values_deprecated;
    use crate::test_helpers::test_rust_equivalence_multiple_deprecated;

    #[test]
    fn div_mod_u64_test() {
        test_rust_equivalence_multiple_deprecated(&DivModU64, true);
    }

    #[test]
    #[should_panic]
    fn fail_vm_execution_on_divide_by_zero_u32_numerator() {
        // Verify that division by zero stops the VM from executing
        // when numerator is small, `numerator < 1 ^ 32`.
        // TODO: `run_tasm` ought to return an error on failure instead of
        // crashing!
        let mut init_state = prepare_state(100, 0);
        DivModU64.link_and_run_tasm_from_state_for_test(&mut init_state);
    }

    #[test]
    #[should_panic]
    fn fail_vm_execution_on_divide_by_zero_u64_numerator() {
        // Verify that division by zero stops the VM from executing
        // when numerator is big, `numerator >= 1 ^ 32`
        // TODO: `run_tasm` ought to return an error on failure instead of
        // crashing!
        let mut init_state = prepare_state(1u64 << 33, 0);
        DivModU64.link_and_run_tasm_from_state_for_test(&mut init_state);
    }

    #[test]
    fn div_mod_u64_unit_test() {
        prop_div_mod(1000, 100);
        prop_div_mod(0, 1);
        prop_div_mod(0, 2);
        prop_div_mod(0, 3);
        prop_div_mod(0, 100);
        prop_div_mod(0, u32::MAX as u64);
        prop_div_mod(0, 0xFFFF_FFFF_0000_0000);
        prop_div_mod(0, 11428751156810088448);

        // Found in bug reports online
        prop_div_mod(6098312677908545536, 6098805452391317504);
        prop_div_mod(5373808693584330752, 11428751156810088448);
        prop_div_mod(8268416007396130816, 6204028719464448000);

        // Suggested by ChatGPT
        prop_div_mod(u64::MAX, 1);
        prop_div_mod(u64::MAX, 2);
        prop_div_mod(u64::MAX, u64::MAX);
        prop_div_mod(0x0000_0001_FFFF_FFFF, 0xFFFF_FFFF_0000_0000);
        prop_div_mod(0xFFFF_FFFF_0000_0000, 0x0000_0000_FFFF_FFFF);
        prop_div_mod(0xABCD_EF12_3456_789A, 0x1234_5678_9ABC_DEF0);

        // Edge cases around powers of two
        prop_div_mod(u64::MAX, (1 << 31) + 454545454);
        prop_div_mod(u64::MAX, (1 << 32) + 454545454);
        prop_div_mod(u64::MAX, (1 << 33) + 454545454);
        prop_div_mod(u64::MAX, (1 << 34) + 454545454);
        prop_div_mod(u64::MAX, (1 << 35) + 454545454);
        prop_div_mod(u64::MAX, (1 << 31) + 1);
        prop_div_mod(u64::MAX, (1 << 32) - 1);
        prop_div_mod(u64::MAX, 1 << 32);

        prop_div_mod(u64::MAX - 1, (1 << 32) - 2);
        prop_div_mod(u64::MAX - 1, (1 << 32) - 1);
        prop_div_mod(u64::MAX - 1, 1 << 32);
        prop_div_mod(u64::MAX - 1, (1 << 32) + 1);
        prop_div_mod(u64::MAX - 1, (1 << 32) + 2);
        prop_div_mod(u64::MAX - 1, (1 << 32) + 3);

        prop_div_mod(u64::MAX, (1 << 32) + 1);
        prop_div_mod(u64::MAX, (1 << 32) + 2);
        prop_div_mod(u64::MAX, (1 << 32) + 3);

        prop_div_mod(u64::MAX - 1, (1 << 33) - 1);
        prop_div_mod(u64::MAX - 1, 1 << 33);
        prop_div_mod(u64::MAX - 1, (1 << 33) + 1);
        prop_div_mod(u64::MAX, (1 << 33) - 1);
        prop_div_mod(u64::MAX, 1 << 33);
        prop_div_mod(u64::MAX, (1 << 33) + 1);
    }

    fn prop_div_mod(numerator: u64, divisor: u64) {
        let mut init_stack = empty_stack();

        let numerator_lo = (numerator & u32::MAX as u64) as u32;
        let numerator_hi = (numerator >> 32) as u32;

        let numerator = U32s::<2>::new([numerator_lo, numerator_hi]);
        for elem in numerator.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        let divisor_lo = (divisor & u32::MAX as u64) as u32;
        let divisor_hi = (divisor >> 32) as u32;
        let divisor = U32s::<2>::new([divisor_lo, divisor_hi]);
        for elem in divisor.encode().into_iter().rev() {
            init_stack.push(elem);
        }

        let expected_res: (BigUint, BigUint) =
            ((numerator / divisor).into(), (numerator % divisor).into());
        let expected_u32_2_quotient: U32s<2> = expected_res.0.into();
        let expected_u32_2_remainder: U32s<2> = expected_res.1.into();
        let mut expected_end_stack = empty_stack();
        for elem in expected_u32_2_quotient.encode().into_iter().rev() {
            expected_end_stack.push(elem);
        }
        for elem in expected_u32_2_remainder.encode().into_iter().rev() {
            expected_end_stack.push(elem);
        }

        test_rust_equivalence_given_input_values_deprecated(
            &DivModU64,
            &init_stack,
            &[],
            HashMap::default(),
            Some(&expected_end_stack),
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::snippet_bencher::bench_and_write;

    #[test]
    fn div_mod_u64_benchmark() {
        bench_and_write(DivModU64);
    }
}
