use std::collections::HashMap;

use triton_vm::prelude::*;
use twenty_first::math::traits::PrimitiveRootOfUnity as PRU;

use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Fetch the primitive root of unity of the given order.
///
/// ### Pre-conditions
///
/// - the order is [encoded](BFieldCodec) correctly
/// - the order is a power of two
/// - the order is not 0
/// - the order is less than or equal to 2^32
///
/// ### Post-conditions
///
/// - the root is a primitive root of the given order for the field with
///   [`BFieldElement::P`] elements
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PrimitiveRootOfUnity;

impl BasicSnippet for PrimitiveRootOfUnity {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "order".to_owned())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bfe, "root_of_unity".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_bfe_primitive_root_of_unity".to_string()
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        let root_of_pow = |pow: u64| BFieldElement::primitive_root_of_unity(1 << pow).unwrap();

        triton_asm!(
            {self.entrypoint()}:
            // _ order_hi order_lo

            /* Assert correct encoding of the input. `order_hi` is checked later. */

            dup 0
            split
            pop 1
            push 0
            eq
            assert error_id 142

            /* check if order is 2^32, i.e., (order_hi, order_lo) == (1, 0) */

            dup 1
            push 1
            eq
            // _ order_hi order_lo (order_hi == 1)

            dup 1
            push 0
            eq
            mul
            // _ order_hi order_lo (order_hi == 1 && order_lo == 0)

            skiz
                push {root_of_pow(32)}
            // _ order_hi order_lo [root]

            /* At this point, `st1` *must* be zero:
             * if order == 2^32:      _ 1 0        root
             * any other legal order: _ 0 order_lo
             */

            dup 1
            push 0
            eq
            assert error_id 140

            /* Now we only have to check `order_lo`. We can ignore `order_hi` as we've
             * verified that it's 0 in case the order was not $1^{32}$.
             * Furthermore, the primitive root of order 2^32 is not itself a legal order
             * of some other primitive root.
             */

            dup 0 push 1             eq skiz push {root_of_pow(0)}
            dup 0 push {1_u32 << 1}  eq skiz push {root_of_pow(1)}
            dup 0 push {1_u32 << 2}  eq skiz push {root_of_pow(2)}
            dup 0 push {1_u32 << 3}  eq skiz push {root_of_pow(3)}
            dup 0 push {1_u32 << 4}  eq skiz push {root_of_pow(4)}
            dup 0 push {1_u32 << 5}  eq skiz push {root_of_pow(5)}
            dup 0 push {1_u32 << 6}  eq skiz push {root_of_pow(6)}
            dup 0 push {1_u32 << 7}  eq skiz push {root_of_pow(7)}
            dup 0 push {1_u32 << 8}  eq skiz push {root_of_pow(8)}
            dup 0 push {1_u32 << 9}  eq skiz push {root_of_pow(9)}
            dup 0 push {1_u32 << 10} eq skiz push {root_of_pow(10)}
            dup 0 push {1_u32 << 11} eq skiz push {root_of_pow(11)}
            dup 0 push {1_u32 << 12} eq skiz push {root_of_pow(12)}
            dup 0 push {1_u32 << 13} eq skiz push {root_of_pow(13)}
            dup 0 push {1_u32 << 14} eq skiz push {root_of_pow(14)}
            dup 0 push {1_u32 << 15} eq skiz push {root_of_pow(15)}
            dup 0 push {1_u32 << 16} eq skiz push {root_of_pow(16)}
            dup 0 push {1_u32 << 17} eq skiz push {root_of_pow(17)}
            dup 0 push {1_u32 << 18} eq skiz push {root_of_pow(18)}
            dup 0 push {1_u32 << 19} eq skiz push {root_of_pow(19)}
            dup 0 push {1_u32 << 20} eq skiz push {root_of_pow(20)}
            dup 0 push {1_u32 << 21} eq skiz push {root_of_pow(21)}
            dup 0 push {1_u32 << 22} eq skiz push {root_of_pow(22)}
            dup 0 push {1_u32 << 23} eq skiz push {root_of_pow(23)}
            dup 0 push {1_u32 << 24} eq skiz push {root_of_pow(24)}
            dup 0 push {1_u32 << 25} eq skiz push {root_of_pow(25)}
            dup 0 push {1_u32 << 26} eq skiz push {root_of_pow(26)}
            dup 0 push {1_u32 << 27} eq skiz push {root_of_pow(27)}
            dup 0 push {1_u32 << 28} eq skiz push {root_of_pow(28)}
            dup 0 push {1_u32 << 29} eq skiz push {root_of_pow(29)}
            dup 0 push {1_u32 << 30} eq skiz push {root_of_pow(30)}
            dup 0 push {1_u32 << 31} eq skiz push {root_of_pow(31)}

            /* Since all roots happen to be either 1 or larger than `u32::MAX`, we can
             * test if the top element is a root or not. If this assumption
             * were to change, VM execution would crash here, and tests would
             * catch that.
             */

            // stack if result found:     _ order_hi order_lo root
            // stack if result not found: _ order_hi order_lo

            dup 0
            push 1
            eq
            // Result found:     _ order_hi order_lo root (root == 1)
            // Result not found: _ order_hi order_lo (order_lo == 1)
            //      If order_lo is 1, a primitive root exists, i.e., a result was found. This
            //      contradicts this case's assumption. Therefore, order_lo cannot be 1 here,
            //      and the stack is:
            //                   _ order_hi order_lo 0

            dup 1
            split
            // Result found:     _ order_hi order_lo root (root == 1) root_hi root_lo
            // Result not found: _ order_hi order_lo 0 0 order_lo

            pop 1
            // Result found:     _ order_hi order_lo root (root == 1) root_hi
            // Result not found: _ order_hi order_lo 0 0

            push 0
            eq
            push 0
            eq
            // Result found:     _ order_hi order_lo root (root == 1) (root_hi != 0)
            // Result not found: _ order_hi order_lo 0 (0 != 0)
            //                                         ~~~~~~~~
            //                                           == 0

            add
            push 0
            eq
            push 0
            eq
            // Result found:     _ order_hi order_lo root ((root == 1) || (root_hi != 0))
            // Result not found: _ order_hi order_lo (0 || 0)
            //                                       ~~~~~~~~
            //                                         == 0

            assert error_id 141
            // Result found:     _ order_hi order_lo root
            // Result not found: VM crashed

            place 2
            pop 2

            return
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x7e7f78606c82b7d1.into());

        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use num_traits::Zero;

    use super::*;
    use crate::empty_stack;
    use crate::test_prelude::*;

    impl Closure for PrimitiveRootOfUnity {
        type Args = u64;

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let order = pop_encodable::<Self::Args>(stack);
            assert!(!order.is_zero(), "No root of order 0 exists");

            let root_of_unity = BFieldElement::primitive_root_of_unity(order).unwrap();
            stack.push(root_of_unity);
        }

        fn pseudorandom_args(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> Self::Args {
            match bench_case {
                Some(BenchmarkCase::CommonCase) => 1_u64 << 10,
                Some(BenchmarkCase::WorstCase) => 1 << 32,
                None => 1 << StdRng::from_seed(seed).random_range(1..=32),
            }
        }
    }

    #[test]
    fn primitive_root_of_order_2_pow_32_is_not_a_legal_order() {
        let root = BFieldElement::primitive_root_of_unity(1 << 32).unwrap();

        // this assumption is made in the snippet
        assert!(BFieldElement::primitive_root_of_unity(root.value()).is_none());
    }

    #[test]
    fn all_primitive_roots_are_either_1_or_larger_than_u32_max() {
        for pow in 1..=32 {
            let root = BFieldElement::primitive_root_of_unity(1 << pow)
                .unwrap()
                .value();

            // this assumption is made in the snippet
            assert!(root == 1 || root > u64::from(u32::MAX));
        }
    }

    #[test]
    fn primitive_root_of_unity_pbt() {
        ShadowedClosure::new(PrimitiveRootOfUnity).test()
    }

    #[test]
    fn primitive_root_of_unity_unit_test() {
        for log2_order in 1..=32 {
            let order = 1u64 << log2_order;
            let mut init_stack = empty_stack();
            for elem in order.encode().iter().rev() {
                init_stack.push(*elem);
            }

            let expected = BFieldElement::primitive_root_of_unity(order).unwrap();
            let expected_final_stack = [empty_stack(), vec![expected]].concat();
            let _vm_output_state = test_rust_equivalence_given_complete_state(
                &ShadowedClosure::new(PrimitiveRootOfUnity),
                &init_stack,
                &[],
                &NonDeterminism::default(),
                &None,
                Some(&expected_final_stack),
            );
        }
    }

    #[test]
    fn primitive_root_negative_test() {
        let small_non_powers_of_two = (0_u64..100).filter(|x| !x.is_power_of_two());
        let larger_non_powers_of_two = (1_u64..50).map(|x| (1 << 32) - x);
        let too_large_powers_of_two = (33..64).map(|x| 1_u64 << x);

        for order in small_non_powers_of_two
            .chain(larger_non_powers_of_two)
            .chain(too_large_powers_of_two)
        {
            dbg!(order);
            let mut init_stack = empty_stack();
            init_stack.extend(order.encode().iter().rev());

            test_assertion_failure(
                &ShadowedClosure::new(PrimitiveRootOfUnity),
                InitVmState::with_stack(init_stack),
                &[140, 141],
            );
        }
    }

    #[proptest]
    fn triton_vm_crashes_if_order_lo_is_not_u32(
        #[strategy(1_u8..=32)] log_2_order: u8,
        #[strategy(0..=u32::MAX)]
        #[map(u64::from)]
        noise: u64,
    ) {
        let [mut order_lo, order_hi] = (1_u64 << log_2_order).encode()[..] else {
            unreachable!()
        };
        order_lo += bfe!(noise << 32);
        prop_assume!((order_lo.value() >> 32) == noise); // no finite-field wrap-around shenanigans

        test_assertion_failure(
            &ShadowedClosure::new(PrimitiveRootOfUnity),
            InitVmState::with_stack([empty_stack(), vec![order_hi, order_lo]].concat()),
            &[142],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(PrimitiveRootOfUnity).bench()
    }
}
