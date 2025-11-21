use triton_vm::prelude::*;

use crate::arithmetic::u32::safe_add::SafeAdd;
use crate::arithmetic::u32::safe_sub::SafeSub;
use crate::arithmetic::u64::and::And;
use crate::arithmetic::u64::leading_zeros::LeadingZeros;
use crate::arithmetic::u64::lt::Lt;
use crate::arithmetic::u64::or::Or;
use crate::arithmetic::u64::shift_left::ShiftLeft;
use crate::arithmetic::u64::shift_right::ShiftRight;
use crate::arithmetic::u64::sub::Sub;
use crate::prelude::*;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DivMod;

impl DivMod {
    pub const DIVISION_BY_ZERO_ERROR_ID: i128 = 420;
}

impl BasicSnippet for DivMod {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["numerator", "denominator"]
            .map(|name| (DataType::U64, name.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        ["quotient", "remainder"]
            .map(|name| (DataType::U64, name.to_string()))
            .to_vec()
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_div_mod".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let shift_right_u64 = library.import(Box::new(ShiftRight));
        let shift_left_u64 = library.import(Box::new(ShiftLeft));
        let and_u64 = library.import(Box::new(And));
        let lt_u64 = library.import(Box::new(Lt));
        let or_u64 = library.import(Box::new(Or));
        let sub_u64 = library.import(Box::new(Sub));
        let sub_u32 = library.import(Box::new(SafeSub));
        let leading_zeros_u64 = library.import(Box::new(LeadingZeros));
        let add_u32 = library.import(Box::new(SafeAdd));
        let spilled_denominator_alloc = library.kmalloc(2);

        // The below code has been compiled from a Rust implementation of an LLVM function
        // called `divmoddi4` that can do u64 divmod with only access to u32 bit divmod and
        // some u64 arithmetic instructions or functions. The compiler used for this was the
        // `tasm-lang` compiler: https://github.com/TritonVM/tasm-lang
        // You could probably get a smaller cycle count if you hand-compiled the function.
        //
        // If you do attempt this, check out the following resources:
        // https://github.com/llvm/llvm-project/compiler-rt/lib/builtins/udivmoddi4.c
        // which is based on “The PowerPC Compiler Writer’s Guide”
        // (https://cr.yp.to/2005-590/powerpc-cwg.pdf) section 3.2.3.7:
        // “32-Bit Implementation of a 64-Bit Unsigned Divide”
        triton_asm!(
            // BEFORE: _ numerator_hi numerator_lo denominator_hi denominator_lo
            // AFTER:  _ quotient_hi quotient_lo remainder_hi remainder_lo
            {self.entrypoint()}:
                dup 1
                dup 1
                push {spilled_denominator_alloc.write_address()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
                pop 1
                push 32
                call {shift_right_u64}
                swap 1
                pop 1
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
                pop 1
                swap 3
                eq
                swap 2
                eq
                mul
                push 0
                eq
                assert error_id {Self::DIVISION_BY_ZERO_ERROR_ID}
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
                push {spilled_denominator_alloc.read_address()}
                read_mem {spilled_denominator_alloc.num_words()}
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::test_prelude::*;

    impl DivMod {
        fn set_up_initial_state(&self, numerator: u64, denominator: u64) -> FunctionInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &numerator);
            push_encodable(&mut stack, &denominator);

            FunctionInitialState {
                stack,
                ..Default::default()
            }
        }
    }

    impl Function for DivMod {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let denominator = pop_encodable::<u64>(stack);
            let numerator = pop_encodable::<u64>(stack);
            let quotient = numerator / denominator;
            let remainder = numerator % denominator;
            push_encodable(stack, &quotient);
            push_encodable(stack, &remainder);

            // Accomodate spilling. This could probably be avoided if the code was compiled
            // by hand instead.
            encode_to_memory(memory, STATIC_MEMORY_FIRST_ADDRESS - bfe!(1), &denominator);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let (numerator, denominator) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (u32::MAX.into(), 1 << 15),
                Some(BenchmarkCase::WorstCase) => (u64::MAX, (1 << 32) + 45454545),
                None => StdRng::from_seed(seed).random(),
            };

            self.set_up_initial_state(numerator, denominator)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            const NOISE: u64 = 0x6d26_150f_4669_d677;

            let u64s_of_different_magnitudes = (0..u64::BITS)
                .step_by(3) // test performance is atrocious otherwise
                .map(|i| 1 << i)
                .map(|x| x | (x - 1) & NOISE);

            let mut states = u64s_of_different_magnitudes
                .clone()
                .cartesian_product(u64s_of_different_magnitudes.clone())
                .map(|(n, d)| self.set_up_initial_state(n, d))
                .collect_vec();

            let additional_inputs = [
                (0, 1),
                (0, 2),
                (0, 3),
                (0, 100),
                (0, u32::MAX as u64),
                (0, 0xFFFF_FFFF_0000_0000),
                (0, 11428751156810088448),
                (1000, 100),
                // found in bug reports online
                (6098312677908545536, 6098805452391317504),
                (5373808693584330752, 11428751156810088448),
                (8268416007396130816, 6204028719464448000),
                // suggested by an LLM
                (u64::MAX, 1),
                (u64::MAX, 2),
                (u64::MAX, u64::MAX),
                (0x0000_0001_FFFF_FFFF, 0xFFFF_FFFF_0000_0000),
                (0xFFFF_FFFF_0000_0000, 0x0000_0000_FFFF_FFFF),
                (0xABCD_EF12_3456_789A, 0x1234_5678_9ABC_DEF0),
                // edge cases around powers of two
                (u64::MAX, (1 << 31) + 1),
                (u64::MAX, (1 << 31) + 454545454),
                (u64::MAX, (1 << 32) - 1),
                (u64::MAX, 1 << 32),
                (u64::MAX, (1 << 32) + 1),
                (u64::MAX, (1 << 32) + 2),
                (u64::MAX, (1 << 32) + 3),
                (u64::MAX, (1 << 32) + 454545454),
                (u64::MAX, (1 << 33) - 1),
                (u64::MAX, 1 << 33),
                (u64::MAX, (1 << 33) + 1),
                (u64::MAX, (1 << 33) + 454545454),
                (u64::MAX, (1 << 34) + 454545454),
                (u64::MAX, (1 << 35) + 454545454),
                (u64::MAX - 1, (1 << 32) - 2),
                (u64::MAX - 1, (1 << 32) - 1),
                (u64::MAX - 1, 1 << 32),
                (u64::MAX - 1, (1 << 32) + 1),
                (u64::MAX - 1, (1 << 32) + 2),
                (u64::MAX - 1, (1 << 32) + 3),
                (u64::MAX - 1, (1 << 33) - 1),
                (u64::MAX - 1, 1 << 33),
                (u64::MAX - 1, (1 << 33) + 1),
            ];

            states.extend(additional_inputs.map(|(n, d)| self.set_up_initial_state(n, d)));
            states
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(DivMod).test();
    }

    #[proptest]
    fn fail_vm_execution_on_divide_by_zero_u32_numerator(numerator: u64) {
        test_assertion_failure(
            &ShadowedFunction::new(DivMod),
            DivMod.set_up_initial_state(numerator, 0).into(),
            &[DivMod::DIVISION_BY_ZERO_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(DivMod).bench();
    }
}
