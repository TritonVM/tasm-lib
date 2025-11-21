use triton_vm::prelude::*;

use crate::prelude::*;

/// A u32 `pow` that behaves like Rustc's `pow` method on `u32`, crashing in case of overflow.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SafePow;

impl BasicSnippet for SafePow {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "base".to_owned()),
            (DataType::U32, "exponent".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U32, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u32_safe_pow".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        // This algorithm is implemented below. `bpow2` has type
        // `u64` because it would otherwise erroneously overflow
        // in the last iteration of the loop when e.g. calculating
        // 2.pow(31).

        // fn safe_pow(base: u32, exponent: u64) -> Self {
        //     let mut bpow2: u64 = base as u64;
        //     let mut acc = 1u32;
        //     let mut i = exponent;

        //     while i != 0 {
        //         if i & 1 == 1 {
        //             acc *= bpow2;
        //         }

        //         bpow2 *= bpow2;
        //         i >>= 1;
        //     }

        //     acc
        // }

        let entrypoint = self.entrypoint();
        let while_acc_label = format!("{entrypoint}_while_acc");
        let mul_acc_with_bpow2_label = format!("{entrypoint}_mul_acc_with_bpow2");
        triton_asm!(
            {entrypoint}:
                // _ base exponent

                push 0
                swap 2
                swap 1
                // _ 0 base exponent

                push 1
                // _ [base_u64] exponent acc

                // rename: `exponent` -> `i`, `base_u64` -> `bpow2_u64`

                // _ [bpow2_u64] i acc
                call {while_acc_label}
                // _ [bpow2_u64] 0 acc

                swap 3
                pop 3
                return

            // INVARIANT: _ [bpow2_u64] i acc
            {while_acc_label}:
                // check condition
                dup 1 push 0 eq
                skiz
                    return
                // _ [bpow2_u64] i acc

                // Verify that `bpow2_u64` does not exceed `u32::MAX`
                dup 3 push 0 eq assert error_id 120

                // _ 0 bpow2 i acc
                dup 1
                push 1
                and
                // _ 0 bpow2 i acc (i & 1)
                skiz
                    call {mul_acc_with_bpow2_label}

                // _ 0 bpow2 i acc

                swap 2
                // _ 0 acc i bpow2

                dup 0 mul split
                // _ 0 acc i bpow2_next_hi bpow2_next_lo

                // GOAL: // _ [bpow_u64] i acc

                swap 3
                // _ 0 bpow2_next_lo i bpow2_next_hi acc

                swap 1
                // _ 0 bpow2_next_lo i acc bpow2_next_hi

                swap 4 pop 1
                // _ bpow2_next_hi bpow2_next_lo i acc

                // _ [bpow2_next_u64] i acc

                push 2
                // _ [bpow2_next_u64] i acc 2

                dup 2
                // _ [bpow2_next_u64] i acc 2 i

                div_mod
                // _ [bpow2_u64] i acc (i >> 2) (i % 2)

                pop 1 swap 2 pop 1
                // _ [bpow2_u64] (i >> 2) acc


                recurse

            {mul_acc_with_bpow2_label}:
                // _ 0 bpow2 i acc

                dup 2
                mul
                // _ 0 bpow2 i (acc * bpow2)

                split swap 1 push 0 eq assert error_id 121
                // _ 0 bpow2 i new_acc

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_prelude::*;

    impl Closure for SafePow {
        type Args = (u32, u32);

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (base, exponent) = pop_encodable::<Self::Args>(stack);
            push_encodable(stack, &(base.pow(exponent)));
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            let Some(bench_case) = bench_case else {
                let mut seeded_rng = StdRng::from_seed(seed);
                let base = seeded_rng.random_range(0..0x10);
                let exponent = seeded_rng.random_range(0..0x8);
                return (base, exponent);
            };

            match bench_case {
                BenchmarkCase::CommonCase => (10, 5),
                BenchmarkCase::WorstCase => (2, 31),
            }
        }

        fn corner_case_args(&self) -> Vec<Self::Args> {
            vec![(0, 0)]
        }
    }

    #[test]
    fn ruts_shadow() {
        ShadowedClosure::new(SafePow).test()
    }

    #[test]
    fn u32_pow_unit_test() {
        for (base, exp) in [
            (0, 0),
            (0, 1),
            (1, 0),
            (1, 1),
            (2, 30),
            (2, 31),
            (3, 20),
            (4, 15),
            (5, 13),
            (6, 12),
            (7, 11),
            (8, 10),
            (9, 10),
            (10, 9),
            (11, 9),
            (12, 8),
            (u32::MAX, 0),
            (u32::MAX, 1),
            (1, u32::MAX),
            (0, u32::MAX),
            (1, u32::MAX - 1),
            (0, u32::MAX - 1),
            (1, u32::MAX - 2),
            (0, u32::MAX - 2),
            (1, u32::MAX - 3),
            (0, u32::MAX - 3),
        ] {
            let initial_stack = SafePow.set_up_test_stack((base, exp));
            let mut expected_final_stack = initial_stack.clone();
            SafePow.rust_shadow(&mut expected_final_stack);

            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(SafePow),
                &initial_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_final_stack),
            );
        }
    }

    #[test]
    fn u32_pow_negative_test() {
        for (base, exp) in [
            (2, 32),
            (3, 21),
            (4, 16),
            (5, 14),
            (6, 13),
            (7, 12),
            (8, 11),
            (9, 11),
            (10, 10),
            (11, 10),
            (12, 10),
            (u32::MAX, 2),
            (u32::MAX, 3),
            (u32::MAX, 4),
            (u32::MAX, 5),
            (u32::MAX, 6),
            (u32::MAX, 7),
            (u32::MAX, 8),
            (u32::MAX, 9),
            (1 << 16, 2),
            (1 << 16, 3),
            (1 << 16, 4),
            (1 << 16, 5),
            (1 << 16, 6),
            (1 << 16, 7),
            (1 << 16, 8),
            (1 << 8, 4),
            (1 << 8, 8),
            (1 << 8, 16),
            (1 << 8, 32),
        ] {
            test_assertion_failure(
                &ShadowedClosure::new(SafePow),
                InitVmState::with_stack(SafePow.set_up_test_stack((base, exp))),
                &[120, 121],
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(SafePow).bench()
    }
}
