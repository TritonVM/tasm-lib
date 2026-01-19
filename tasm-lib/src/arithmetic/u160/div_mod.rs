use triton_vm::prelude::*;

use crate::arithmetic;
use crate::prelude::*;

/// Return the quotient and remainder for division of u160 values. This
/// algorithm divines in the result from the secret-input stream. If the
/// divined-in inputs are not correct, this snippet crashes the VM. If the
/// divined input is not a `BFieldCodec` encoded u160, the VM also crashes.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DivMod;

impl DivMod {
    pub const DIVISION_BY_ZERO_ERROR_ID: i128 = 590;
    pub const REMAINDER_TOO_BIG: i128 = 591;
    pub const EULER_EQUATION_ERROR: i128 = 592;
}

impl BasicSnippet for DivMod {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U160, "numerator".to_owned()),
            (DataType::U160, "denominator".to_owned()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U160, "quotient".to_owned()),
            (DataType::U160, "remainder".to_owned()),
        ]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u160_div_mod".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let lt = library.import(Box::new(arithmetic::u160::lt::Lt));
        let safe_add = library.import(Box::new(arithmetic::u160::safe_add::SafeAdd));
        let safe_mul = library.import(Box::new(arithmetic::u160::safe_mul::SafeMul));
        let compare = DataType::U160.compare();

        let remainder_pointer = library.kmalloc(5);
        let quotient_pointer = library.kmalloc(5);

        let divine_u160 = triton_asm!(
            divine 5
        );
        let input_sanitation = triton_asm!(
            dup 4
            pop_count
            // _ [x] pop_count(x_4)

            dup 4
            pop_count
            // _ [x] p_cnt(x_4) p_cnt(x_3)

            dup 4
            pop_count
            // _ [x] p_cnt(x_4) p_cnt(x_3) p_cnt(x_2)

            dup 4
            pop_count
            // _ [x] p_cnt(x_4) p_cnt(x_3) p_cnt(x_2) p_cnt(x_1)

            dup 4
            pop_count
            // _ [x] p_cnt(x_4) p_cnt(x_3) p_cnt(x_2) p_cnt(x_1) p_cnt(x_0)

            pop 5
            // _ [x]
        );

        triton_asm!(
            // BEFORE: _ n_4 n_3 n_2 n_1 n_0 d_4 d_3 d_2 d_1 d_0
            // AFTER:  _ q_4 q_3 q_2 q_1 q_0 r_4 r_3 r_2 r_1 r_0
            {self.entrypoint()}:
                // _ [n] [d]


                /* Assert denominator ≠ 0 */
                {&triton_asm![dup 4; 5]}
                {&triton_asm![push 0; 5]}
                // _ [n] [d] [d] [0]

                {&compare}
                // _ [n] [d] (d == 0)

                push 0
                eq
                assert error_id {Self::DIVISION_BY_ZERO_ERROR_ID}
                // _ [n] [d]


                /* Read and input and assert well-formed u160s */
                {&divine_u160}
                {&input_sanitation}
                {&divine_u160}
                {&input_sanitation}
                // _ [n] [d] [q] [r]


                /* Verify r < d */
                {&triton_asm![dup 14; 5]}
                {&triton_asm![dup 9; 5]}
                // _ [n] [d] [q] [r] [d] [r]

                call {lt}
                // _ [n] [d] [q] [r] (r < d)

                assert error_id {Self::REMAINDER_TOO_BIG}
                // _ [n] [d] [q] [r]


                /* Verify q * d + r == n */
                push {remainder_pointer.write_address()}
                write_mem 5
                pop 1
                // _ [n] [d] [q]

                {&triton_asm![dup 4; 5]}
                // _ [n] [d] [q] [q]

                push {quotient_pointer.write_address()}
                write_mem 5
                pop 1
                // _ [n] [d] [q]

                call {safe_mul}
                // _ [n] [d * q]

                push {remainder_pointer.read_address()}
                read_mem 5
                pop 1
                // _ [n] [d * q] [r]

                call {safe_add}
                // _ [n] [d * q + r]

                {&compare}
                // _ (n == d * q + r)

                assert error_id {Self::EULER_EQUATION_ERROR}
                // _


                /* Put return value on stack */
                push {quotient_pointer.read_address()}
                read_mem 5
                pop 1
                // _ [q]

                push {remainder_pointer.read_address()}
                read_mem 5
                pop 1
                // _ [q] [r]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use num::BigUint;
    use num::Integer;
    use num_traits::Zero;
    use rand::rngs::StdRng;

    use super::*;
    use crate::arithmetic::u160::u128_to_u160;
    use crate::library::STATIC_MEMORY_FIRST_ADDRESS;
    use crate::test_helpers::negative_test;
    use crate::test_prelude::Algorithm;
    use crate::test_prelude::*;

    #[test]
    fn std_test() {
        ShadowedAlgorithm::new(DivMod).test()
    }

    #[test]
    fn too_big_remainder() {
        let numerator = 10;
        let denominator = 3;
        let stack = DivMod.stack(u128_to_u160(numerator), u128_to_u160(denominator));

        let too_big_remainder = 4;
        let too_small_quotient = 2;
        let nondeterminism = DivMod::nondeterminism(
            u128_to_u160(too_small_quotient).map(|x| bfe!(x)),
            u128_to_u160(too_big_remainder).map(|x| bfe!(x)),
        );

        test_assertion_failure(
            &ShadowedAlgorithm::new(DivMod),
            InitVmState {
                nondeterminism,
                stack,
                ..Default::default()
            },
            &[DivMod::REMAINDER_TOO_BIG],
        )
    }

    #[test]
    fn euler_equation_error() {
        let numerator = 1u128 << 99;
        let denominator = 1u128 << 45;
        let stack = DivMod.stack(u128_to_u160(numerator), u128_to_u160(denominator));

        let correct_remainder = 0;
        let bad_quotient = 1u128 << 43;
        let nondeterminism = DivMod::nondeterminism(
            u128_to_u160(bad_quotient).map(|x| bfe!(x)),
            u128_to_u160(correct_remainder).map(|x| bfe!(x)),
        );

        test_assertion_failure(
            &ShadowedAlgorithm::new(DivMod),
            InitVmState {
                nondeterminism,
                stack,
                ..Default::default()
            },
            &[DivMod::EULER_EQUATION_ERROR],
        )
    }

    #[test]
    fn division_by_zero() {
        let numerator = u128_to_u160(52);
        let denominator = u128_to_u160(0);
        let state = DivMod.prepare_state(numerator, denominator);
        test_assertion_failure(
            &ShadowedAlgorithm::new(DivMod),
            state.into(),
            &[DivMod::DIVISION_BY_ZERO_ERROR_ID],
        );
    }

    #[test]
    fn bad_encoding() {
        let not_u32 = bfe!(1u64 << 32);
        let valid_u32 = bfe!(14);
        let mut nondeterminism = DivMod::nondeterminism([valid_u32; 5], [valid_u32; 5]);
        let stack = DivMod.stack(u128_to_u160(4), u128_to_u160(4));

        for i in 0..10 {
            nondeterminism.individual_tokens[i] = not_u32;
            negative_test(
                &ShadowedAlgorithm::new(DivMod),
                InitVmState {
                    nondeterminism: nondeterminism.clone(),
                    stack: stack.clone(),
                    ..Default::default()
                },
                &[InstructionError::OpStackError(
                    OpStackError::FailedU32Conversion(not_u32),
                )],
            );

            nondeterminism.individual_tokens[i] = valid_u32;
        }
    }

    impl DivMod {
        fn nondeterminism(
            quotient: [BFieldElement; 5],
            remainder: [BFieldElement; 5],
        ) -> NonDeterminism {
            let individual_tokens = [quotient, remainder].concat();

            NonDeterminism {
                individual_tokens,
                digests: Default::default(),
                ram: Default::default(),
            }
        }

        fn stack(&self, numerator: [u32; 5], denominator: [u32; 5]) -> Vec<BFieldElement> {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &numerator);
            push_encodable(&mut stack, &denominator);

            stack
        }

        fn prepare_state(
            &self,
            numerator: [u32; 5],
            denominator: [u32; 5],
        ) -> AlgorithmInitialState {
            let stack = self.stack(numerator, denominator);

            let numerator: BigUint = BigUint::new(numerator.to_vec());
            let denominator: BigUint = BigUint::new(denominator.to_vec());

            let (quotient, remainder) = if denominator.is_zero() {
                // Special-casing to prevent crash in state-preparation.
                // Dividing by zero always crashes execution, so the values used
                // here are irrelevant.
                (0u32.into(), 0u32.into())
            } else {
                numerator.div_rem(&denominator)
            };

            let mut quotient = quotient.to_u32_digits();
            quotient.resize(5, 0);
            quotient.reverse();
            let quotient: [u32; 5] = quotient.try_into().unwrap();

            let mut remainder = remainder.to_u32_digits();
            remainder.resize(5, 0);
            remainder.reverse();
            let remainder: [u32; 5] = remainder.try_into().unwrap();

            let nondeterminism = Self::nondeterminism(
                quotient.encode().try_into().unwrap(),
                remainder.encode().try_into().unwrap(),
            );

            AlgorithmInitialState {
                stack,
                nondeterminism,
            }
        }

        fn edge_case_points() -> Vec<[u32; 5]> {
            [2, 1 << 32, 1 << 96, u128::MAX]
                .into_iter()
                .flat_map(|p| [p.checked_sub(1), Some(p), p.checked_add(1)])
                .flatten()
                .map(u128_to_u160)
                .chain([[u32::MAX; 5]])
                .collect()
        }
    }

    impl Algorithm for DivMod {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut std::collections::HashMap<BFieldElement, BFieldElement>,
            nondeterminism: &NonDeterminism,
        ) {
            let denominator: [u32; 5] = pop_encodable(stack);
            let denominator: BigUint = BigUint::new(denominator.to_vec());
            assert!(!denominator.is_zero());

            let numerator: [u32; 5] = pop_encodable(stack);
            let numerator: BigUint = BigUint::new(numerator.to_vec());

            let quotient = &nondeterminism.individual_tokens[0..5];
            let mut quotient: [u32; 5] =
                *TasmObject::decode_iter(&mut quotient.iter().cloned()).unwrap();
            quotient.reverse();
            let quotient: BigUint = BigUint::new(quotient.to_vec());

            let remainder = &nondeterminism.individual_tokens[5..10];
            let mut remainder: [u32; 5] =
                *TasmObject::decode_iter(&mut remainder.iter().cloned()).unwrap();
            remainder.reverse();
            let remainder: BigUint = BigUint::new(remainder.to_vec());

            assert!(remainder < denominator);
            assert!(numerator == quotient.clone() * denominator + remainder.clone());

            let mut quotient = quotient.to_u32_digits();
            quotient.resize(5, 0);
            let quotient: [u32; 5] = quotient.try_into().unwrap();

            let mut remainder = remainder.to_u32_digits();
            remainder.resize(5, 0);
            let remainder: [u32; 5] = remainder.try_into().unwrap();

            // Imitate use of static memory
            encode_to_memory(memory, STATIC_MEMORY_FIRST_ADDRESS - bfe!(4), &remainder);
            encode_to_memory(memory, STATIC_MEMORY_FIRST_ADDRESS - bfe!(9), &quotient);

            push_encodable(stack, &quotient);
            push_encodable(stack, &remainder);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _bench_case: Option<crate::test_prelude::BenchmarkCase>,
        ) -> AlgorithmInitialState {
            let mut rng = StdRng::from_seed(seed);
            let numerator: [u32; 5] = rng.random();
            let mut denominator: [u32; 5] = rng.random();

            // Limit size of denominator
            let denominator_div = rng.next_u32();
            denominator[4] /= denominator_div;

            self.prepare_state(numerator, denominator)
        }

        fn corner_case_initial_states(&self) -> Vec<AlgorithmInitialState> {
            let edge_case_points = Self::edge_case_points();

            edge_case_points
                .iter()
                .cartesian_product(&edge_case_points)
                .map(|(l, r)| self.prepare_state(*l, *r))
                .collect()
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedAlgorithm::new(DivMod).bench()
    }
}
