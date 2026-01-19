use triton_vm::prelude::*;

use crate::prelude::*;

/// [Shift left][shl] for unsigned 128-bit integers, with the shift amount
/// specified at compile time.
///
/// The shift amount `N` must be in range `0..=32`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [v: u128]
/// AFTER:  _ [v << SHIFT_AMOUNT: u128]
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
/// [shl]: core::ops::Shl
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ShiftLeftStatic<const N: u8>;

impl<const N: u8> BasicSnippet for ShiftLeftStatic<N> {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "value".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "shifted_value".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_arithmetic_u128_shift_left_static_{N}")
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        assert!(N <= 32, "shift amount must be in range 0..=32");

        triton_asm!(
            // BEFORE: _ [value: u128]
            // AFTER:  _ [value << shift: u128]
            {self.entrypoint()}:
                push {1_u64 << N}
                xb_mul          // _ v3 (v2<<shift) (v1<<shift) (v0<<shift)
                pick 3          // _ (v2<<shift) (v1<<shift) (v0<<shift) v3
                push {1_u64 << N}
                mul             // _ (v2<<shift) (v1<<shift) (v0<<shift) (v3<<shift)
                                // _  v2s         v1s         v0s         v3s

                split           // _ v2s v1s v0s v3s_hi v3s_lo
                pick 4          // _ v1s v0s v3s_hi v3s_lo v2s
                split           // _ v1s v0s v3s_hi v3s_lo v2s_hi v2s_lo
                pick 5          // _ v0s v3s_hi v3s_lo v2s_hi v2s_lo v1s
                split           // _ v0s v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi v1s_lo
                pick 6          // _ v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi v1s_lo v0s
                split           // _ v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi v1s_lo v0s_hi v0'

                place 7         // _ v0' v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi v1s_lo v0s_hi
                add             // _ v0' v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi v1'
                place 6         // _ v1' v0' v3s_hi v3s_lo v2s_hi v2s_lo v1s_hi
                add             // _ v1' v0' v3s_hi v3s_lo v2s_hi v2'
                place 5         // _ v2' v1' v0' v3s_hi v3s_lo v2s_hi
                add             // _ v2' v1' v0' v3s_hi v3'
                place 4         // _ v3' v2' v1' v0' v3s_hi
                pop 1           // _ v3' v2' v1' v0'
                return
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use rand::rngs::StdRng;

    use super::*;
    use crate::test_prelude::*;

    impl<const N: u8> Closure for ShiftLeftStatic<N> {
        type Args = u128;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let v = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(v << N));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 0x1282,
                Some(BenchmarkCase::WorstCase) => 0x123456789abcdef,
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![0, 1, 8, u32::MAX.into(), u64::MAX.into(), u128::MAX]
        }
    }

    #[test]
    fn rust_shadow() {
        macro_rules! test_shift_left_static {
            ($($i:expr),*$(,)?) => {
                $(ShadowedClosure::new(ShiftLeftStatic::<$i>).test();)*
            };
        }

        test_shift_left_static!(0, 1, 2, 3, 4, 5, 6, 7);
        test_shift_left_static!(8, 9, 10, 11, 12, 13, 14, 15);
        test_shift_left_static!(16, 17, 18, 19, 20, 21, 22, 23);
        test_shift_left_static!(24, 25, 26, 27, 28, 29, 30, 31);
        test_shift_left_static!(32);
    }

    #[test]
    #[should_panic]
    fn shift_beyond_limit() {
        ShadowedClosure::new(ShiftLeftStatic::<33>).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(ShiftLeftStatic::<5>).bench();
    }
}
