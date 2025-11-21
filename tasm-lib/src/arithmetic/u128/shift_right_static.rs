use triton_vm::prelude::*;

use crate::prelude::*;

/// [Shift right][shr] for unsigned 128-bit integers, with the shift amount
/// specified at compile time.
///
/// The shift amount `N` must be in range `0..=32`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [v: u128]
/// AFTER:  _ [v >> SHIFT_AMOUNT: u128]
/// ```
///
/// ### Preconditions
///
/// - input argument `arg` is properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is properly [`BFieldCodec`] encoded
///
/// [shr]: core::ops::Shr
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftRightStatic<const N: u8>;

impl<const N: u8> BasicSnippet for ShiftRightStatic<N> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "shifted_value".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_arithmetic_u128_shift_right_static_{N}")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        assert!(N <= 32, "shift amount must be in range 0..=32");

        triton_asm!(
            // BEFORE: _ v_3 v_2 v_1 v_0
            // AFTER:  _ (v >> s)_3 (v >> s)_2 (v >> s)_1 (v >> s)_0
            {self.entrypoint()}:
                pick 3
                push {1_u64 << (32 - N)}
                mul
                place 3             // _ v3s v2 v1 v0
                push {1_u64 << (32 - N)}
                xb_mul              // _ v3s v2s v1s v0s

                /* see non-static version for stack annotations */
                pick 3
                split
                pick 4
                split
                pick 5
                split
                pick 6
                split

                pop 1
                add
                place 4
                add
                place 3
                add
                place 2

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arithmetic::u128::shift_left_static::ShiftLeftStatic;
    use crate::test_prelude::*;

    impl<const N: u8> Closure for ShiftRightStatic<N> {
        type Args = <ShiftLeftStatic<N> as Closure>::Args;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let v = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(v >> N));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            ShiftLeftStatic::<N>.pseudorandom_args(seed, bench_case)
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            ShiftLeftStatic::<N>.corner_case_args()
        }
    }

    #[test]
    fn rust_shadow() {
        macro_rules! test_shift_right_static {
            ($($i:expr),*$(,)?) => {
                $(ShadowedClosure::new(ShiftRightStatic::<$i>).test();)*
            };
        }

        test_shift_right_static!(0, 1, 2, 3, 4, 5, 6, 7);
        test_shift_right_static!(8, 9, 10, 11, 12, 13, 14, 15);
        test_shift_right_static!(16, 17, 18, 19, 20, 21, 22, 23);
        test_shift_right_static!(24, 25, 26, 27, 28, 29, 30, 31);
        test_shift_right_static!(32);
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        ShadowedClosure::new(ShiftRightStatic::<33>).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftRightStatic::<5>).bench();
    }
}
