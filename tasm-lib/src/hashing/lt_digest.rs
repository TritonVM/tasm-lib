use tip5::Digest;
use triton_vm::prelude::*;

use crate::data_type::DataType;
use crate::library::Library;
use crate::traits::basic_snippet::BasicSnippet;

/// Compare two digests
pub struct LtDigest;

impl BasicSnippet for LtDigest {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::VoidPointer, "*digest_lhs".to_owned()),
            (DataType::VoidPointer, "*digest_rhs".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "digest_lhs > digest_rhs".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_lt_digest".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();

        let loop_label = format!("{entrypoint}_short_circuiting_loop");

        // result ∈ {0, 1} : {0: !(lhs > rhs), 1: lhs > rhs}
        let compare_bfes_loop = triton_asm!(
            // Invariant: _ result (*lhs - 1) *lhs[i] *rhs[i] g0 g1
            {loop_label}:
                hint result: bool = stack[5]
                hint lhs_end_condition: BFieldElement = stack[4]
                hint lhs_i_ptr = stack[3]
                hint rhs_i_ptr = stack[2]

                /* Check if we are done */
                dup 4
                dup 4
                eq
                skiz return

                pop 2
                // _ result (*lhs - 1) *lhs[i] *rhs[i]

                read_mem 1
                // _ result (*lhs - 1) *lhs[i] rhs[i] *rhs[i-1]

                swap 2
                read_mem 1
                // _ result (*lhs - 1) *rhs[i-1] rhs[i] lhs[i] *lhs[i-1]

                swap 3
                swap 2
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] lhs[i] rhs[i]

                dup 1 dup 1
                eq
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] lhs[i] rhs[i] (lhs[i] == rhs[i])

                skiz recurse
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] lhs[i] rhs[i]

                split
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] lhs[i] rhs[i]_hi rhs[i]_lo

                swap 2
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] rhs[i]_lo rhs[i]_hi lhs[i]

                split
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] rhs[i]_lo rhs[i]_hi lhs[i]_hi lhs[i]_lo

                /* Calculate rhs[i]_hi < lhs[i]_hi || rhs[i]_hi == rhs[i]_hi && rhs[i]_lo < lhs[i]_lo*/

                dup 1 dup 3 lt
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] rhs[i]_lo rhs[i]_hi lhs[i]_hi lhs[i]_lo (lhs[i]_hi > rhs[i]_hi)

                swap 4
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] (lhs[i]_hi > rhs[i]_hi) rhs[i]_hi lhs[i]_hi lhs[i]_lo rhs[i]_lo

                lt
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] (lhs[i]_hi > rhs[i]_hi) rhs[i]_hi lhs[i]_hi (lhs[i]_lo > rhs[i]_lo)

                swap 2 eq
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] (lhs[i]_hi > rhs[i]_hi) (lhs[i]_lo > rhs[i]_lo) (lhs[i]_hi == rhs[i]_hi)

                mul
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] (lhs[i]_hi > rhs[i]_hi) ((lhs[i]_lo > rhs[i]_lo) && (lhs[i]_hi == rhs[i]_hi))

                add
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] (lhs[i]_hi > rhs[i]_hi) || ((lhs[i]_lo > rhs[i]_lo) && (lhs[i]_hi == rhs[i]_hi))
                // _ result (*lhs - 1) *lhs[i-1] *rhs[i-1] result'

                swap 4
                push 0
                // _ result' (*lhs - 1) *lhs[i-1] *rhs[i-1] g0 g1

                return
        );

        triton_asm!(
            // BEFORE: _ *lhs *rhs
            // AFTER: _ (lhs > rhs)
            {entrypoint}:
                // _ *lhs *rhs

                // goal:  _ result (*lhs - 1) *lhs[i] *rhs[i] g0 g1

                push 0
                swap 2
                // _ 0 *rhs *lhs

                push {Digest::LEN - 1} add
                // _ 0 *rhs *lhs[4]

                dup 0 push {-(Digest::LEN as isize)} add
                // _ 0 *rhs *lhs[4] (*lhs - 1)

                swap 2
                // _ 0 (*lhs - 1) *lhs[4] *rhs

                push {Digest::LEN - 1} add
                // _ 0 (*lhs - 1) *lhs[4] *rhs[4]

                push 0
                push 0
                // _ 0 (*lhs - 1) *lhs[4] *rhs[4] 0 0
                // _ result (*lhs - 1) *lhs[4] *rhs[4] g0 g1

                call {loop_label}
                // _ result (*lhs - 1) *lhs[i] *rhs[i] g0 g1

                pop 5
                // _ result

                return

            {&compare_bfes_loop}
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use rand::thread_rng;
    use rand::{rngs::StdRng, Rng, SeedableRng};

    use super::*;
    use crate::memory::encode_to_memory;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::traits::function::Function;
    use crate::traits::function::FunctionInitialState;
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    #[test]
    fn lt_digest_pbt() {
        ShadowedFunction::new(LtDigest).test()
    }

    impl LtDigest {
        fn prepare_state(
            &self,
            lhs_ptr: BFieldElement,
            rhs_ptr: BFieldElement,
            lhs: Digest,
            rhs: Digest,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, lhs_ptr, &lhs);
            encode_to_memory(&mut memory, rhs_ptr, &rhs);

            let stack = [self.init_stack_for_isolated_run(), vec![lhs_ptr, rhs_ptr]].concat();

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for LtDigest {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let load_digest = |ptr: BFieldElement| {
                Digest::new([
                    memory[&ptr],
                    memory[&(ptr + bfe!(1))],
                    memory[&(ptr + bfe!(2))],
                    memory[&(ptr + bfe!(3))],
                    memory[&(ptr + bfe!(4))],
                ])
            };
            let rhs_ptr = stack.pop().unwrap();
            let lhs_ptr = stack.pop().unwrap();
            let rhs = load_digest(rhs_ptr);
            let lhs = load_digest(lhs_ptr);

            stack.push(bfe!((rhs < lhs) as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng: StdRng = SeedableRng::from_seed(seed);

            let a_digest = Digest::new([bfe!(1), bfe!(2), bfe!(3), bfe!(4), bfe!(5)]);
            let another_digest = Digest::new([bfe!(6), bfe!(7), bfe!(8), bfe!(9), bfe!(10)]);

            let (lhs, rhs) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (a_digest, another_digest),
                Some(BenchmarkCase::WorstCase) => (a_digest, a_digest),
                None => (rng.gen(), rng.gen()),
            };
            let rhs_ptr: BFieldElement = rng.gen();
            let lhs_ptr: BFieldElement = rng.gen();

            self.prepare_state(lhs_ptr, rhs_ptr, lhs, rhs)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let lhs_ptr = bfe!(0);
            let rhs_ptr = bfe!(5);

            let a_digest = Digest::new([bfe!(1), bfe!(2), bfe!(3), bfe!(4), bfe!(5)]);
            let another_digest = Digest::new([bfe!(6), bfe!(7), bfe!(8), bfe!(9), bfe!(10)]);
            let digests_on_lowest_addresses =
                self.prepare_state(lhs_ptr, rhs_ptr, a_digest, another_digest);
            let same_digest_values = self.prepare_state(lhs_ptr, rhs_ptr, a_digest, a_digest);

            let mut adjacent_digest_pairs = vec![];
            let mut rng = thread_rng();
            for i in 0..Digest::LEN {
                let a: Digest = rng.gen();
                let mut b: Digest = a;

                // `lhs == rhs`
                adjacent_digest_pairs.push(self.prepare_state(lhs_ptr, rhs_ptr, a, a));
                adjacent_digest_pairs.push(self.prepare_state(lhs_ptr, rhs_ptr, b, b));

                // `b` "bigger" than `a` by $2^i$ at index i
                // (b might be smaller than a due to wrap-around)
                for j in 0..64 {
                    b = a;
                    b.0[i] += bfe!(1u64 << j);
                    adjacent_digest_pairs.push(self.prepare_state(lhs_ptr, rhs_ptr, a, b));
                    adjacent_digest_pairs.push(self.prepare_state(lhs_ptr, rhs_ptr, b, a));
                }
            }

            [
                vec![digests_on_lowest_addresses, same_digest_values],
                adjacent_digest_pairs,
            ]
            .concat()
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn lt_digest_bench() {
        ShadowedFunction::new(LtDigest).bench()
    }
}
