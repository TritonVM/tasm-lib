use triton_vm::prelude::*;

use crate::arithmetic::u64::safe_mul::SafeMul;
use crate::prelude::*;

/// Multiply two `u64`s, resulting in a `u128`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [right: u64] [left: u64]
/// AFTER:  _ [left · right: u128]
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
///
/// ### Postconditions
///
/// - the output is the product of the input
/// - the output is properly [`BFieldCodec`] encoded
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct MulTwoU64sToU128;

impl BasicSnippet for MulTwoU64sToU128 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        SafeMul.parameters()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ r_hi r_lo l_hi l_lo
            // AFTER:  _ p_3 p_2 p_1 p_0
            {self.entrypoint()}:
                /*
                 *  p_0 is low limb, c_0 high limb of l_lo·r_lo
                 *  p_1 is low limb, c_1 high limb of (l_lo·r_hi)_lo + (l_hi·r_lo)_lo + c_0
                 *  p_2 is low limb, c_2 high limb of (l_lo·r_hi)_hi + (l_hi·r_lo)_hi + (l_hi·r_hi)_lo + c_1
                 *  p_3 is low limb (l_hi·r_hi)_hi + c_2
                 *
                 * There's no carry c_3 because max value of (l_hi·r_hi)_hi is 0xfffffffe.
                 */

                /* p_0 */
                dup 0
                dup 3
                mul
                split       // _ r_hi r_lo l_hi l_lo c_0 p_0

                /* p_1 */
                swap 2      // _ r_hi r_lo l_hi p_0 c_0 l_lo
                dup 5
                mul
                split       // _ r_hi r_lo l_hi p_0 c_0 (l_lo·r_hi)_hi (l_lo·r_hi)_lo
                pick 1
                swap 5      // _ r_hi (l_lo·r_hi)_hi l_hi p_0 c_0 (l_lo·r_hi)_lo r_lo
                dup 4
                mul
                split       // _ r_hi (l_lo·r_hi)_hi l_hi p_0 c_0 (l_lo·r_hi)_lo (r_lo·l_hi)_hi (r_lo·l_hi)_lo
                pick 1
                place 3     // _ r_hi (l_lo·r_hi)_hi l_hi p_0 (r_lo·l_hi)_hi c_0 (l_lo·r_hi)_lo (r_lo·l_hi)_lo
                add
                add
                split       // _ r_hi (l_lo·r_hi)_hi l_hi p_0 (r_lo·l_hi)_hi c_1 p_1

                /* p_2 */
                swap 4      // _ r_hi (l_lo·r_hi)_hi p_1 p_0 (r_lo·l_hi)_hi c_1 l_hi
                pick 6      // _ (l_lo·r_hi)_hi p_1 p_0 (r_lo·l_hi)_hi c_1 l_hi r_hi
                mul
                split       // _ (l_lo·r_hi)_hi p_1 p_0 (r_lo·l_hi)_hi c_1 (l_hi·r_hi)_hi (l_hi·r_hi)_lo
                pick 1
                swap 6      // _ (l_hi·r_hi)_hi p_1 p_0 (r_lo·l_hi)_hi c_1 (l_hi·r_hi)_lo (l_lo·r_hi)_hi
                add
                add
                add
                split       // _ (l_hi·r_hi)_hi p_1 p_0 c_2 p_2

                /* p_3 */
                swap 4      // _ p_2 p_1 p_0 c_2 (l_hi·r_hi)_hi
                add         // _ p_2 p_1 p_0 p_3
                place 3     // _ p_3 p_2 p_1 p_0

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arithmetic::u64::and::And;
    use crate::test_prelude::*;

    impl Closure for MulTwoU64sToU128 {
        type Args = (u64, u64);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (right, left) = pop_encodable::<Self::Args>(stack);
            let product = u128::from(left) * u128::from(right);
            push_encodable(stack, &product);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => (1 << 63, (1 << 45) - 1),
                Some(BenchmarkCase::WorstCase) => (1 << 63, (1 << 63) - 1),
                None => StdRng::from_seed(seed).random(),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            And.corner_case_args()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedClosure::new(MulTwoU64sToU128).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(MulTwoU64sToU128).bench();
    }
}
