use triton_vm::isa::triton_asm;
use triton_vm::prelude::LabelledInstruction;

use crate::arithmetic::u32::isu32::Isu32;
use crate::arithmetic::u32::shiftleft::Shiftleft;
use crate::arithmetic::u32::shiftright::Shiftright;
use crate::data_type::DataType;
use crate::library::Library;
use crate::prelude::BasicSnippet;

/// Sign-preserving right-shift for 128-bit integers AKA right-shift for `i128`.
///
/// # Behavior
///
/// BEFORE: `_ arg3 arg2 arg1 arg0 shamt`
///
/// AFTER: `_ res3 res2 res1 res0`
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
pub struct ShiftRight;

impl ShiftRight {
    pub const ARGUMENT_LIMB_3_NOT_U32_ERROR_ID: i128 = 323;
    pub const ARGUMENT_LIMB_2_NOT_U32_ERROR_ID: i128 = 322;
    pub const ARGUMENT_LIMB_1_NOT_U32_ERROR_ID: i128 = 321;
    pub const ARGUMENT_LIMB_0_NOT_U32_ERROR_ID: i128 = 320;
    pub const SHAMT_NOT_U32_ERROR_ID: i128 = 324;
}

impl BasicSnippet for ShiftRight {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U128, "arg".to_string()),
            (DataType::U32, "shamt".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U128, "res".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u128_shift_right_i128".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let shr_i128_by_32n = format!("{entrypoint}_by_32n");
        let clean_up_for_early_return = format!("{entrypoint}_early_return");
        let entrypoint = self.entrypoint();

        let is_u32 = library.import(Box::new(Isu32));
        let shr_u32 = library.import(Box::new(Shiftright));
        let shl_u32 = library.import(Box::new(Shiftleft));

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
                hint top = stack[0]
                // _ arg3 arg2 arg1 arg0 shamt top


                /* shift right by multiple of 32 */

                call {shr_i128_by_32n}
                // _ arg3' arg2' arg1' arg0' (shamt % 32) top
                // _ arg3' arg2' arg1' arg0' shamt' top


                /* early return if possible */
                dup 1 push 0 eq dup 0
                // _ arg3' arg2' arg1' arg0' shamt' top (shamt' == 0) (shamt' == 0)

                skiz call {clean_up_for_early_return}
                skiz return
                // _ arg3' arg2' arg1' arg0' shamt' top

                /* shift right by the remainder modulo 32 */

                push 32 dup 2 push {-1} mul add
                // _ arg3' arg2' arg1' arg0' shamt' top (32-shamt')
                // _ arg3' arg2' arg1' arg0' shamt' top compl'

                push {u32::MAX} dup 2 mul
                // _ arg3' arg2' arg1' arg0' shamt' top compl' (u32::MAX * top)

                dup 1 call {shl_u32}
                // _ arg3' arg2' arg1' arg0' shamt' top compl' ((u32::MAX * top) << compl')
                // _ arg3' arg2' arg1' arg0' shamt' top compl' top_new

                pick 7 dup 0
                // _ arg2' arg1' arg0' shamt' top compl' top_new arg3' arg3'

                dup 3 call {shl_u32}
                // _ arg2' arg1' arg0' shamt' top compl' top_new arg3' (arg3' << compl')
                // _ arg2' arg1' arg0' shamt' top compl' top_new arg3' arg3'_lo

                place 2
                // _ arg2' arg1' arg0' shamt' top compl' arg3'_lo top_new arg3'

                dup 5 call {shr_u32}
                // _ arg2' arg1' arg0' shamt' top compl' arg3'_lo top_new (arg3' >> shamt')
                // _ arg2' arg1' arg0' shamt' top compl' arg3'_lo top_new arg3_hi

                add
                // _ arg2' arg1' arg0' shamt' top compl' arg3'_lo arg3''

                swap 7 dup 0
                // _ arg3'' arg1' arg0' shamt' top compl' arg3'_lo arg2' arg2'

                dup 3 call {shl_u32}
                // _ arg3'' arg1' arg0' shamt' top compl' arg3'_lo arg2' (arg2' << compl')
                // _ arg3'' arg1' arg0' shamt' top compl' arg3'_lo arg2' arg2'_lo

                place 2
                // _ arg3'' arg1' arg0' shamt' top compl' arg2'_lo arg3'_lo arg2'

                dup 5 call {shr_u32}
                // _ arg3'' arg1' arg0' shamt' top compl' arg2'_lo arg3'_lo (arg2' >> shamt')
                // _ arg3'' arg1' arg0' shamt' top compl' arg2'_lo arg3'_lo arg2'_hi

                add
                // _ arg3'' arg1' arg0' shamt' top compl' arg2'_lo (arg3'_lo + arg2'_hi)
                // _ arg3'' arg1' arg0' shamt' top compl' arg2'_lo arg2''

                swap 6 dup 0
                // _ arg3'' arg2'' arg0' shamt' top compl' arg2'_lo arg1' arg1'

                dup 3 call {shl_u32}
                // _ arg3'' arg2'' arg0' shamt' top compl' arg2'_lo arg1' (arg1' << compl')
                // _ arg3'' arg2'' arg0' shamt' top compl' arg2'_lo arg1' arg1'_lo

                place 2
                // _ arg3'' arg2'' arg0' shamt' top compl' arg1'_lo arg2'_lo arg1'

                dup 5 call {shr_u32}
                // _ arg3'' arg2'' arg0' shamt' top compl' arg1'_lo arg2'_lo (arg1' >> shamt')
                // _ arg3'' arg2'' arg0' shamt' top compl' arg1'_lo arg2'_lo arg1'_hi

                add
                // _ arg3'' arg2'' arg0' shamt' top compl' arg1'_lo (arg2'_lo+ arg1'_hi)
                // _ arg3'' arg2'' arg0' shamt' top compl' arg1'_lo arg1''

                swap 5
                // _ arg3'' arg2'' arg1'' shamt' top compl' arg1'_lo arg0'

                pick 4
                // _ arg3'' arg2'' arg1'' top compl' arg1'_lo arg0' shamt'

                call {shr_u32}
                // _ arg3'' arg2'' arg1'' top compl' arg1'_lo (arg0' >> shamt')
                // _ arg3'' arg2'' arg1'' top compl' arg1'_lo arg0'_hi

                add
                // _ arg3'' arg2'' arg1'' top compl' (arg1'_lo + arg0'_hi)
                // _ arg3'' arg2'' arg1'' top compl' argo0''

                place 2 pop 2
                // _ arg3'' arg2'' arg1'' argo0''

                return

            // BEFORE: _ arg3  arg2  arg1  arg0  shamt  top
            // AFTER:  _ arg3' arg2' arg1' arg0' shamt' top
            // where `arg >> shamt == arg' >> shamt'` and `shamt' < 32`
            {shr_i128_by_32n}:

                /* evaluate termination condition */

                push 32 dup 2 lt
                // _ arg3 arg2 arg1 arg0 shamt top (shamt < 32)

                skiz return


                /* apply one limb-shift */

                push {u32::MAX} dup 1 mul
                // _ arg3 arg2 arg1 arg0 shamt top (u32::MAX * top)
                // _ arg3 arg2 arg1 arg0 shamt top top_limb

                place 6
                // _ top_limb arg3 arg2 arg1 arg0 shamt top

                pick 2 pop 1
                // _ top_limb arg3 arg2 arg1 shamt top

                pick 1 addi -32 place 1
                // _ top_limb arg3 arg2 arg1 (shamt-32) top

                recurse

            // BEFORE: _ arg3' arg2' arg1' arg0' shamt' top b
            // AFTER:  _ arg3' arg2' arg1' arg0' b
            {clean_up_for_early_return}:
                place 2
                pop 2
                return

        }
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use num::BigUint;
    use num::PrimInt;
    use proptest_arbitrary_interop::arb;
    use rand::rngs::StdRng;
    use rand::Rng;
    use rand::SeedableRng;
    use test_strategy::proptest;
    use triton_vm::prelude::bfe;
    use triton_vm::prelude::BFieldElement;
    use triton_vm::vm::NonDeterminism;

    use crate::prelude::BasicSnippet;
    use crate::push_encodable;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::traits::closure::Closure;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::ShiftRight;

    impl ShiftRight {
        pub(crate) fn prepare_stack(&self, arg: u128, shamt: u32) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();

            let mut arg_limbs = BigUint::from(arg)
                .to_u32_digits()
                .into_iter()
                .pad_using(4, |_| 0)
                .map(u64::from)
                .map(BFieldElement::new)
                .collect_vec();

            while let Some(element) = arg_limbs.pop() {
                stack.push(element);
            }

            stack.push(bfe!(shamt));

            stack
        }

        fn assert_expected_shift_behavior(&self, arg: u128, shamt: u32) {
            let init_stack = self.prepare_stack(arg, shamt);

            let expected = {
                let res = arg.signed_shr(shamt);
                let mut stack = self.init_stack_for_isolated_run();
                push_encodable(&mut stack, &res);
                stack
            };

            test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(Self),
                &init_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected),
            );
        }
    }

    impl Closure for ShiftRight {
        fn rust_shadow(&self, stack: &mut Vec<triton_vm::prelude::BFieldElement>) {
            let shamt: u32 = stack.pop().unwrap().value().try_into().unwrap();
            let arg_limbs = (0..4)
                .map(|_| stack.pop().unwrap().value().try_into().unwrap())
                .collect_vec();
            let arg = u128::try_from(BigUint::from_slice(&arg_limbs)).unwrap();

            let res = arg.signed_shr(shamt);

            let mut res_limbs = BigUint::from(res)
                .to_u32_digits()
                .into_iter()
                .pad_using(4, |_| 0)
                .map(u64::from)
                .map(BFieldElement::new)
                .collect_vec();
            while let Some(element) = res_limbs.pop() {
                stack.push(element);
            }
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
        ) -> Vec<triton_vm::prelude::BFieldElement> {
            let mut rng = StdRng::from_seed(seed);
            let arg: u128 = rng.gen();
            let shamt: u32 = rng.gen_range(0..128);
            self.prepare_stack(arg, shamt)
        }
    }

    #[test]
    fn standard_test() {
        ShadowedClosure::new(ShiftRight).test()
    }

    #[proptest]
    fn proptest(#[strategy(arb())] arg: u128, #[strategy(0u32..128)] shamt: u32) {
        ShiftRight.assert_expected_shift_behavior(arg, shamt);
    }

    #[test]
    fn test_edge_cases() {
        [0, 1, u32::MAX]
            .into_iter()
            .map(u128::from)
            .tuple_combinations()
            .map(|(l0, l1, l2, l3)| l0 + (l1 << 32) + (l2 << 64) + (l3 << 96))
            .cartesian_product(
                [0, 1, 16, 31]
                    .into_iter()
                    .cartesian_product(0..4)
                    .map(|(l, r)| l + 32 * r),
            )
            .for_each(|(arg, shamt)| {
                ShiftRight.assert_expected_shift_behavior(arg, shamt);
            });
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn shift_right_i128_benchmark() {
        ShadowedClosure::new(ShiftRight).bench()
    }
}
