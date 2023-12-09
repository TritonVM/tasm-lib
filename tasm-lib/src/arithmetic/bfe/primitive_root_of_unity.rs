use num_traits::Zero;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::shared_math::traits::PrimitiveRootOfUnity as PRU;

use crate::{
    closure::Closure,
    empty_stack,
    snippet::{BasicSnippet, DataType},
    snippet_bencher::BenchmarkCase,
};

pub struct PrimitiveRootOfUnity;

impl BasicSnippet for PrimitiveRootOfUnity {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::U64, "order".to_owned())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::BFE, "root_of_unity".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_bfe_primitive_root_of_unity".to_string()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();

        triton_asm!(
            {entrypoint}:
            // _ order_hi order_lo

            // First check if order i $1^{32}$.

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
                push 1753635133440165772
            // _ order_hi order_lo [root]

            // at this point `st1` *must* be zero.

            dup 1
            push 0
            eq
            assert

            // Now we only have to check `order_lo`. We can ignore `order_hi` as we've
            // verified that it's 0 in case the order was not $1^{32}$.

            // check if order is 1
            dup 0
            push 1
            eq
            // _  order_hi order_lo (order_lo == 1)
            skiz
                push 1
            // _  order_hi order_lo {root}

            // check if order is 2
            dup 0
            push 2
            eq
            // _  order_hi order_lo (order_lo == 2)
            skiz
                push 18446744069414584320
            // _  order_hi order_lo {root}

            // 4
            dup 0
            push 4
            eq
            // _  order_hi order_lo (order_lo == 4)
            skiz
                push 281474976710656
            // _  order_hi order_lo {root}

            // 8
            dup 0
            push 8
            eq
            skiz
                push 18446744069397807105

            // 16
            dup 0
            push 16
            eq
            skiz
                push 17293822564807737345

            // 32
            dup 0
            push 32
            eq
            skiz
                push 70368744161280

            // 1 << 6
            dup 0
            push 64
            eq
            skiz
                push 549755813888

            // 1 << 7
            dup 0
            push 128
            eq
            skiz
                push 17870292113338400769

            // 1 << 8
            dup 0
            push 256
            eq
            skiz
                push 13797081185216407910


            // 1 << 9
            dup 0
            push 512
            eq
            skiz
                push 1803076106186727246

            // 1 << 10
            dup 0
            push 1024
            eq
            skiz
                push 11353340290879379826

            // 1 << 11
            dup 0
            push 2048
            eq
            skiz
                push 455906449640507599

            // 1 << 12
            dup 0
            push 4096
            eq
            skiz
                push 17492915097719143606


            // 1 << 13
            dup 0
            push 8192
            eq
            skiz
                push 1532612707718625687

            // 1 << 14
            dup 0
            push 16384
            eq
            skiz
                push 16207902636198568418

            // 1 << 15
            dup 0
            push 32768
            eq
            skiz
                push 17776499369601055404

            // 1 << 16
            dup 0
            push 65536
            eq
            skiz
                push 6115771955107415310


            // 1 << 17
            dup 0
            push 131072
            eq
            skiz
                push 12380578893860276750

            // 1 << 18
            dup 0
            push 262144
            eq
            skiz
                push 9306717745644682924

            // 1 << 19
            dup 0
            push 524288
            eq
            skiz
                push 18146160046829613826

            // 1 << 20
            dup 0
            push 1048576
            eq
            skiz
                push 3511170319078647661


            // 1 << 21
            dup 0
            push 2097152
            eq
            skiz
                push 17654865857378133588

            // 1 << 22
            dup 0
            push 4194304
            eq
            skiz
                push 5416168637041100469


            // 1 << 23
            dup 0
            push 8388608
            eq
            skiz
                push 16905767614792059275


            // 1 << 24
            dup 0
            push 16777216
            eq
            skiz
                push 9713644485405565297


            // 1 << 25
            dup 0
            push 33554432
            eq
            skiz
                push 5456943929260765144

            // 1 << 26
            dup 0
            push 67108864
            eq
            skiz
                push 17096174751763063430

            // 1 << 27
            dup 0
            push 134217728
            eq
            skiz
                push 1213594585890690845

            // 1 << 28
            dup 0
            push 268435456
            eq
            skiz
                push 6414415596519834757

            // 1 << 29
            dup 0
            push 536870912
            eq
            skiz
                push 16116352524544190054

            // 1 << 30
            dup 0
            push 1073741824
            eq
            skiz
                push 9123114210336311365

            // 1 << 31
            dup 0
            push 2147483648
            eq
            skiz
                push 4614640910117430873

            // Since all roots happen to be larger than `u32::MAX`, or `1` we can
            // test if the top element is a root or not. If this assumption
            // were to change, VM execution would crash here, and tests would
            // catch that.

            // stack if result found:     _ // _  order_hi order_lo root
            // stack if result not found: _ order_hi order_lo

            dup 0
            push 1
            eq
            // stack if result found:     _ order_hi order_lo root (root == 1)
            // stack if result not found: _ order_hi order_lo (order_lo == 1)

            dup 1
            split
            // Result found:     _ order_hi order_lo root (root == 1) root_hi root_lo
            // Result not found: _ order_hi order_lo (order_lo == 1) 0 order_lo

            pop
            // Result found:     _ order_hi order_lo root (root == 1) root_hi
            // Result not found: _  order_hi order_lo (order_lo == 1) 0

            push 0
            eq
            push 0
            eq
            // Result found:     _  order_hi order_lo root (root == 1) (root_hi != 0)
            // Result not found: _  order_hi order_lo (order_lo == 1) (0 != 0)

            add
            push 0
            eq
            push 0
            eq
            // Result found:     _  order_hi order_lo root ((root == 1) || (root_hi != 0))
            // Result not found: _  order_hi order_lo ((order_lo == 1) || (0 != 0))

            assert
            // Result found:     _  order_hi order_lo root
            // Result not found: VM crashed

            swap 2
            pop
            pop

            return

        )
    }
}

impl Closure for PrimitiveRootOfUnity {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let order_lo: u32 = stack.pop().unwrap().try_into().unwrap();
        let order_hi: u32 = stack.pop().unwrap().try_into().unwrap();
        let order: u64 = order_lo as u64 + ((order_hi as u64) << 32);
        assert!(!order.is_zero(), "No root of order 0 exists");

        let root_of_unity = BFieldElement::primitive_root_of_unity(order).unwrap();

        stack.push(root_of_unity);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        let order = match bench_case {
            Some(BenchmarkCase::CommonCase) => 1024,
            Some(BenchmarkCase::WorstCase) => 1u64 << 32,
            None => {
                let mut rng = StdRng::from_seed(seed);
                let log = rng.gen_range(1..=32);
                1u64 << log
            }
        };

        [
            empty_stack(),
            vec![
                BFieldElement::new(order >> 32),
                BFieldElement::new(order & u32::MAX as u64),
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
    use twenty_first::shared_math::bfield_codec::BFieldCodec;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use super::*;
    use crate::closure::ShadowedClosure;
    use crate::linker::link_for_isolated_run;
    use crate::snippet::RustShadow;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::{execute_with_terminal_state, prepend_state_preparation, VmHasherState};

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
                &NonDeterminism::new(vec![]),
                &HashMap::default(),
                &VmHasherState::new(Domain::VariableLength),
                1,
                Some(&expected_final_stack),
            );
        }
    }

    #[test]
    fn primitive_root_negative_test() {
        let safe_pow = PrimitiveRootOfUnity;

        let code = link_for_isolated_run(Rc::new(RefCell::new(safe_pow)), 0);

        for order in [
            0u64,
            3,
            5,
            6,
            7,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            17,
            18,
            (1 << 32) - 2,
            (1 << 32) - 1,
            (1 << 32) + 1,
            (1 << 32) + 2,
            (1 << 33),
            (1 << 34),
            (1 << 63),
        ] {
            let mut init_stack = empty_stack();
            for elem in order.encode().iter().rev() {
                init_stack.push(*elem);
            }

            // run rust shadow
            let rust_result = std::panic::catch_unwind(|| {
                let mut rust_stack = init_stack.clone();
                ShadowedClosure::new(PrimitiveRootOfUnity).rust_shadow_wrapper(
                    &[],
                    &NonDeterminism::new(vec![]),
                    &mut rust_stack,
                    &mut HashMap::default(),
                    &mut VmHasherState::new(Domain::VariableLength),
                )
            });

            // Run on Triton
            let program = prepend_state_preparation(&code, &init_stack);
            let tvm_result =
                execute_with_terminal_state(&program, &[], &mut NonDeterminism::new(vec![]), None);

            assert!(
                rust_result.is_err() && tvm_result.is_err(),
                "Test case: primitive root of order {order} must fail since it does not exist"
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{closure::ShadowedClosure, snippet::RustShadow};

    #[test]
    fn bfe_primitive_root_of_unity_bench() {
        ShadowedClosure::new(PrimitiveRootOfUnity).bench()
    }
}
