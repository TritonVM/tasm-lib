use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};

use crate::{
    closure::Closure,
    get_init_tvm_stack,
    snippet::{BasicSnippet, DataType},
    snippet_bencher::BenchmarkCase,
};

/// A u32 `pow` that behaves like Rustc's `pow` method on `u32`, crashing in case of overflow.
#[derive(Clone)]
pub struct SafePow;

impl BasicSnippet for SafePow {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::U32, "base".to_owned()),
            (DataType::U32, "exponent".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(DataType::U32, "result".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_u32_safe_pow_u32".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();

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

        let code = format!(
            "
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
                call {entrypoint}_while_acc
                // _ [bpow2_u64] 0 acc

                swap 3
                pop
                pop
                pop
                return

            // INVARIANT: _ [bpow2_u64] i acc
            {entrypoint}_while_acc:
                // check condition
                dup 1 push 0 eq
                skiz
                    return
                // _ [bpow2_u64] i acc

                // Verify that `bpow2_u64` does not exceed `u32::MAX`
                dup 3 push 0 eq assert

                // _ 0 bpow2 i acc
                dup 1
                push 1
                and
                // _ 0 bpow2 i acc (i & 1)
                skiz
                    call {entrypoint}_mul_acc_with_bpow2

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

                swap 4 pop
                // _ bpow2_next_hi bpow2_next_lo i acc

                // _ [bpow2_next_u64] i acc

                push 2
                // _ [bpow2_next_u64] i acc 2

                dup 2
                // _ [bpow2_next_u64] i acc 2 i

                div
                // _ [bpow2_u64] i acc (i >> 2) (i % 2)

                pop swap 2 pop
                // _ [bpow2_u64] (i >> 2) acc


                recurse

            {entrypoint}_mul_acc_with_bpow2:
                // _ 0 bpow2 i acc

                dup 2
                mul
                // _ 0 bpow2 i (acc * bpow2)

                split swap 1 push 0 eq assert
                // _ 0 bpow2 i new_acc

                return
                "
        );

        triton_asm!({ code })
    }
}

impl Closure for SafePow {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let exp: u32 = stack.pop().unwrap().try_into().unwrap();
        let base: u32 = stack.pop().unwrap().try_into().unwrap();
        stack.push(BFieldElement::new(base.pow(exp) as u64));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        let (base, exponent): (u32, u32) = match bench_case {
            Some(BenchmarkCase::CommonCase) => (10, 5),
            Some(BenchmarkCase::WorstCase) => (2, 31),
            None => {
                let mut seeded_rng = StdRng::from_seed(seed);
                let base: u32 = seeded_rng.gen_range(0..0x10);
                let exponent: u32 = seeded_rng.gen_range(0..0x8);
                (base, exponent)
            }
        };

        [
            get_init_tvm_stack(),
            vec![
                BFieldElement::new(base as u64),
                BFieldElement::new(exponent as u64),
            ],
        ]
        .concat()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;
    use triton_vm::NonDeterminism;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use super::*;
    use crate::closure::ShadowedClosure;
    use crate::linker::link_for_isolated_run;
    use crate::snippet::RustShadow;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::{execute_with_terminal_state, program_with_state_preparation, VmHasherState};

    #[test]
    fn u32_pow_pbt() {
        ShadowedClosure::new(SafePow).test()
    }

    #[test]
    fn u32_pow_unit_test() {
        let safe_pow = SafePow;
        let closure = ShadowedClosure::new(safe_pow);

        for (base, exp) in [
            (0u32, 0u32),
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
            (1, u32::MAX),
            (0, u32::MAX),
            (1, u32::MAX - 1),
            (0, u32::MAX - 1),
            (1, u32::MAX - 2),
            (0, u32::MAX - 2),
            (1, u32::MAX - 3),
            (0, u32::MAX - 3),
        ] {
            let init_stack = [
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new(base as u64),
                    BFieldElement::new(exp as u64),
                ],
            ]
            .concat();
            let expected_final_stack = [
                get_init_tvm_stack(),
                vec![BFieldElement::new(base.pow(exp) as u64)],
            ]
            .concat();
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &closure,
                &init_stack,
                &vec![],
                &NonDeterminism::new(vec![]),
                &HashMap::default(),
                &VmHasherState::new(Domain::VariableLength),
                1,
                Some(&expected_final_stack),
            );
        }
    }

    #[test]
    fn u32_pow_negative_test() {
        let safe_pow = SafePow;

        let code = link_for_isolated_run(Rc::new(RefCell::new(safe_pow)), 0);

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
            let init_stack = [
                get_init_tvm_stack(),
                vec![
                    BFieldElement::new(base as u64),
                    BFieldElement::new(exp as u64),
                ],
            ]
            .concat();
            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = init_stack.clone();
                ShadowedClosure::new(SafePow).rust_shadow_wrapper(
                    &vec![],
                    &NonDeterminism::new(vec![]),
                    &mut rust_stack,
                    &mut HashMap::default(),
                    &mut VmHasherState::new(Domain::VariableLength),
                )
            });

            // Run on Triton
            let program = program_with_state_preparation(
                &code,
                &init_stack,
                &mut NonDeterminism::new(vec![]),
                None,
            );
            let tvm_result =
                execute_with_terminal_state(&program, &vec![], &mut NonDeterminism::new(vec![]));

            assert!(rust_result.is_err() && tvm_result.is_err());
        }
    }

    #[test]
    fn u32_pow_bench() {
        ShadowedClosure::new(SafePow).bench()
    }
}
