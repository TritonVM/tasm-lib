use triton_vm::prelude::*;

use crate::arithmetic::u32::is_u32::IsU32;
use crate::arithmetic::u32::shift_left::ShiftLeft as ShlU32;
use crate::arithmetic::u32::shift_right::ShiftRight as ShrU32;
use crate::prelude::*;

/// Right-shift for 128-bit integers AKA [right-shift for `i128`][shr].
///
/// # Behavior
///
/// ```text
/// BEFORE: _ arg3 arg2 arg1 arg0 shamt
/// AFTER:  _ res3 res2 res1 res0
/// ```
///
/// where `res == arg >> shamt` as `i128`s.
///
/// # Preconditions
///
///  - `arg` consists of 4 `u32`s
///  - `shamt` is in `[0:128)`
///
/// # Postconditions
///
///  - `res` consists of 4 `u32`s
///
/// # Panics
///
///  - If preconditions are not met.
///
/// [shr]: core::ops::Shr
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRight;

impl ShiftRight {
    pub const ARGUMENT_LIMB_3_NOT_U32_ERROR_ID: i128 = 323;
    pub const ARGUMENT_LIMB_2_NOT_U32_ERROR_ID: i128 = 322;
    pub const ARGUMENT_LIMB_1_NOT_U32_ERROR_ID: i128 = 321;
    pub const ARGUMENT_LIMB_0_NOT_U32_ERROR_ID: i128 = 320;
    pub const SHAMT_NOT_U32_ERROR_ID: i128 = 324;
}

impl BasicSnippet for ShiftRight {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::I128, "arg".to_string()),
            (DataType::U32, "shamt".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::I128, "res".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_i128_shift_right".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let shr_i128_by_32n = format!("{entrypoint}_by_32n");
        let clean_up_for_early_return = format!("{entrypoint}_early_return");
        let entrypoint = self.entrypoint();

        let is_u32 = library.import(Box::new(IsU32));
        let shr_u32 = library.import(Box::new(ShrU32));
        let shl_u32 = library.import(Box::new(ShlU32));

        triton_asm! {
            // BEFORE: _ arg3 arg2 arg1 arg0 shamt
            // AFTER: _ res3 res2 res1 res0
            {entrypoint}:

                /* assert preconditions */

                dup 4 dup 4 dup 4 dup 4
                // _ arg3 arg2 arg1 arg0 shamt arg3 arg2 arg1 arg0

                push 128 dup 5
                // _ arg3 arg2 arg1 arg0 shamt arg3 arg2 arg1 arg0 128 shamt

                lt
                // _ arg3 arg2 arg1 arg0 shamt arg3 arg2 arg1 arg0 (shamt < 128)

                assert error_id {Self::SHAMT_NOT_U32_ERROR_ID}
                // _ arg3 arg2 arg1 arg0 shamt arg3 arg2 arg1 arg0

                call {is_u32} assert error_id {Self::ARGUMENT_LIMB_0_NOT_U32_ERROR_ID}
                call {is_u32} assert error_id {Self::ARGUMENT_LIMB_1_NOT_U32_ERROR_ID}
                call {is_u32} assert error_id {Self::ARGUMENT_LIMB_2_NOT_U32_ERROR_ID}
                call {is_u32} assert error_id {Self::ARGUMENT_LIMB_3_NOT_U32_ERROR_ID}
                // _ arg3 arg2 arg1 arg0 shamt


                /* extract top bit */

                dup 4 push 31 call {shr_u32}
                hint msb = stack[0]
                // _ arg3 arg2 arg1 arg0 shamt msb


                /* shift right by multiple of 32 */

                call {shr_i128_by_32n}
                // _ arg3' arg2' arg1' arg0' (shamt % 32) msb
                // _ arg3' arg2' arg1' arg0' shamt' msb


                /* early return if possible */
                dup 1 push 0 eq dup 0
                // _ arg3' arg2' arg1' arg0' shamt' msb (shamt' == 0) (shamt' == 0)

                skiz call {clean_up_for_early_return}
                skiz return
                // _ arg3' arg2' arg1' arg0' shamt' msb


                /* shift right by the remainder modulo 32 */

                push 32 dup 2 push -1 mul add
                // _ arg3' arg2' arg1' arg0' shamt' msb (32-shamt')
                // _ arg3' arg2' arg1' arg0' shamt' msb compl'

                push {u32::MAX} dup 2 mul
                // _ arg3' arg2' arg1' arg0' shamt' msb compl' (u32::MAX * msb)

                dup 1 call {shl_u32}
                // _ arg3' arg2' arg1' arg0' shamt' msb compl' ((u32::MAX * msb) << compl')
                // _ arg3' arg2' arg1' arg0' shamt' msb compl' new_ms_limb

                pick 7 dup 0
                // _ arg2' arg1' arg0' shamt' msb compl' new_ms_limb arg3' arg3'

                dup 3 call {shl_u32}
                // _ arg2' arg1' arg0' shamt' msb compl' new_ms_limb arg3' (arg3' << compl')
                // _ arg2' arg1' arg0' shamt' msb compl' new_ms_limb arg3' arg3'_lo

                place 2
                // _ arg2' arg1' arg0' shamt' msb compl' arg3'_lo new_ms_limb arg3'

                dup 5 call {shr_u32}
                // _ arg2' arg1' arg0' shamt' msb compl' arg3'_lo new_ms_limb (arg3' >> shamt')
                // _ arg2' arg1' arg0' shamt' msb compl' arg3'_lo new_ms_limb arg3_hi

                add
                // _ arg2' arg1' arg0' shamt' msb compl' arg3'_lo arg3''

                swap 7 dup 0
                // _ arg3'' arg1' arg0' shamt' msb compl' arg3'_lo arg2' arg2'

                dup 3 call {shl_u32}
                // _ arg3'' arg1' arg0' shamt' msb compl' arg3'_lo arg2' (arg2' << compl')
                // _ arg3'' arg1' arg0' shamt' msb compl' arg3'_lo arg2' arg2'_lo

                place 2
                // _ arg3'' arg1' arg0' shamt' msb compl' arg2'_lo arg3'_lo arg2'

                dup 5 call {shr_u32}
                // _ arg3'' arg1' arg0' shamt' msb compl' arg2'_lo arg3'_lo (arg2' >> shamt')
                // _ arg3'' arg1' arg0' shamt' msb compl' arg2'_lo arg3'_lo arg2'_hi

                add
                // _ arg3'' arg1' arg0' shamt' msb compl' arg2'_lo (arg3'_lo + arg2'_hi)
                // _ arg3'' arg1' arg0' shamt' msb compl' arg2'_lo arg2''

                swap 6 dup 0
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg2'_lo arg1' arg1'

                dup 3 call {shl_u32}
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg2'_lo arg1' (arg1' << compl')
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg2'_lo arg1' arg1'_lo

                place 2
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg1'_lo arg2'_lo arg1'

                dup 5 call {shr_u32}
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg1'_lo arg2'_lo (arg1' >> shamt')
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg1'_lo arg2'_lo arg1'_hi

                add
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg1'_lo (arg2'_lo+ arg1'_hi)
                // _ arg3'' arg2'' arg0' shamt' msb compl' arg1'_lo arg1''

                swap 5
                // _ arg3'' arg2'' arg1'' shamt' msb compl' arg1'_lo arg0'

                pick 4
                // _ arg3'' arg2'' arg1'' msb compl' arg1'_lo arg0' shamt'

                call {shr_u32}
                // _ arg3'' arg2'' arg1'' msb compl' arg1'_lo (arg0' >> shamt')
                // _ arg3'' arg2'' arg1'' msb compl' arg1'_lo arg0'_hi

                add
                // _ arg3'' arg2'' arg1'' msb compl' (arg1'_lo + arg0'_hi)
                // _ arg3'' arg2'' arg1'' msb compl' argo0''

                place 2 pop 2
                // _ arg3'' arg2'' arg1'' argo0''

                return

            // BEFORE: _ arg3  arg2  arg1  arg0  shamt  msb
            // AFTER:  _ arg3' arg2' arg1' arg0' shamt' msb
            // where `arg >> shamt == arg' >> shamt'` and `shamt' < 32`
            {shr_i128_by_32n}:

                /* evaluate termination condition */

                push 32 dup 2 lt
                // _ arg3 arg2 arg1 arg0 shamt msb (shamt < 32)

                skiz return


                /* apply one limb-shift */

                push {u32::MAX} dup 1 mul
                // _ arg3 arg2 arg1 arg0 shamt msb (u32::MAX * msb)
                // _ arg3 arg2 arg1 arg0 shamt msb ms_limb

                place 6
                // _ ms_limb arg3 arg2 arg1 arg0 shamt msb

                pick 2 pop 1
                // _ ms_limb arg3 arg2 arg1 shamt msb

                pick 1 addi -32 place 1
                // _ ms_limb arg3 arg2 arg1 (shamt-32) msb

                recurse

            // BEFORE: _ arg3' arg2' arg1' arg0' shamt' msb b
            // AFTER:  _ arg3' arg2' arg1' arg0' b
            {clean_up_for_early_return}:
                place 2
                pop 2
                return

        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::tasm_final_state;
    use crate::test_prelude::*;

    impl ShiftRight {
        fn assert_expected_shift_behavior(&self, arg: i128, shamt: u32) {
            let initial_stack = self.set_up_test_stack((arg, shamt));

            let mut expected_stack = initial_stack.clone();
            self.rust_shadow(&mut expected_stack);

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_stack),
            );
        }
    }

    impl Closure for ShiftRight {
        type Args = (i128, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (arg, shift_amount) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(arg >> shift_amount));
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            let mut rng = StdRng::from_seed(seed);
            (rng.random(), rng.random_range(0..128))
        }
    }

    #[test]
    fn standard_test() {
        ShadowedClosure::new(ShiftRight).test()
    }

    #[proptest]
    fn proptest(#[strategy(arb())] arg: i128, #[strategy(0u32..128)] shamt: u32) {
        ShiftRight.assert_expected_shift_behavior(arg, shamt);
    }

    #[test]
    fn test_edge_cases() {
        // all i128s from all combinations of {-1, 0, 1} as their limbs
        let arguments = (0..4)
            .map(|_| [-1, 0, 1])
            .multi_cartesian_product()
            .map(|limbs| <[i128; 4]>::try_from(limbs).unwrap())
            .map(|[l0, l1, l2, l3]| l0 + (l1 << 32) + (l2 << 64) + (l3 << 96));

        let shift_amounts = [0, 1, 16, 31]
            .into_iter()
            .cartesian_product(0..4)
            .map(|(l, r)| l + 32 * r);

        arguments
            .cartesian_product(shift_amounts)
            .for_each(|(arg, shamt)| ShiftRight.assert_expected_shift_behavior(arg, shamt));
    }

    /// Shifting right by 127 must produce either 0xff..f, or 0x00..0, depending on
    /// the sign of the i128-argument.
    #[proptest(cases = 50)]
    fn shifting_right_by_127_is_zero_or_minus_1(arg: i128) {
        let mut final_state = tasm_final_state(
            &ShadowedClosure::new(ShiftRight),
            &ShiftRight.set_up_test_stack((arg, 127)),
            &[],
            NonDeterminism::default(),
            &None,
        );

        let final_stack = &mut final_state.op_stack.stack;
        let num_bits_in_result = pop_encodable::<i128>(final_stack).count_ones();

        if arg.is_positive() {
            prop_assert_eq!(0, num_bits_in_result);
        } else {
            prop_assert_eq!(i128::BITS, num_bits_in_result);
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftRight).bench()
    }
}
