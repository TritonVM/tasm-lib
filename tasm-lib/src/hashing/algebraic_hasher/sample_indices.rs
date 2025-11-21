use triton_vm::prelude::*;

use crate::list::length::Length;
use crate::list::new::New;
use crate::list::push::Push;
use crate::prelude::*;

/// Sample n pseudorandom integers between 0 and k. It does this by squeezing the sponge. It is the
/// caller's responsibility to ensure that the sponge is initialized to the right state.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SampleIndices;

impl BasicSnippet for SampleIndices {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "number".to_string()),
            (DataType::U32, "upper_bound".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::U32)),
            "*indices".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_hashing_algebraic_hasher_sample_indices".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let then_reduce_and_save = format!("{entrypoint}_then_reduce_and_save");
        let else_drop_tip = format!("{entrypoint}_else_drop_tip");

        let new_list = library.import(Box::new(New));
        let length = library.import(Box::new(Length));
        let push_element = library.import(Box::new(Push::new(DataType::U32)));

        let if_can_sample = triton_asm! (
            // BEFORE: _ prn number upper_bound *indices
            // AFTER:  _ prn number upper_bound *indices ~can_use can_use
            dup 0 call {length}         // _ prn number upper_bound *indices length
            dup 3 eq                    // _ prn number upper_bound *indices length==number
            push 0 eq                   // _ prn number upper_bound *indices length!=number
            dup 4 push -1 eq            // _ prn number upper_bound *indices length!=number prn==max
            push 0 eq                   // _ prn number upper_bound *indices length!=number prn!=max
            mul                         // _ prn number upper_bound *indices length!=number&&prn!=max
            dup 0                       // _ prn number upper_bound *indices length!=number&&prn!=max length!=number&&prn!=max
            push 0 eq                   // _ prn number upper_bound *indices length!=number&&prn!=max ~(length!=number&&prn!=max)
            swap 1                      // _ prn number upper_bound *indices ~(length!=number&&prn!=max) length!=number&&prn!=max
        );

        triton_asm! (
            // BEFORE: _ number upper_bound
            // AFTER:  _ *indices
            {entrypoint}:
                call {new_list}         // _ number upper_bound *indices

                // prepare and call main while lop
                swap 1                  // _ number *indices upper_bound
                push -1 add             // _ number *indices upper_bound-1
                swap 1                  // _ number upper_bound-1 *indices
                call {main_loop}        // _ number upper_bound-1 *indices

                // clean up and return
                swap 2 pop 2
                return

            // INVARIANT: _ number upper_bound-1 *indices
            {main_loop}:
                // evaluate termination condition
                dup 0 call {length}     // _ number upper_bound-1 *indices length
                dup 3 eq                // _ number upper_bound-1 *indices length==number
                skiz return             // _ number upper_bound-1 *indices

                // we need to squeeze so squeeze
                sponge_squeeze          // _ number upper_bound-1 *indices [prn]

                // reject or reduce-and-store
                dup 12 dup 12 dup 12    // _ number upper_bound-1 *indices [prn] number upper_bound-1 *indices

                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                {&if_can_sample} skiz call {then_reduce_and_save} skiz call {else_drop_tip}
                                        // _ number upper_bound-1 *indices number upper_bound-1 *indices

                // return to invariant and repeat
                pop 3                   // _ number upper_bound-1 *indices
                recurse

            // BEFORE: _ prn number upper_bound-1 *indices 0
            // AFTER:  _ number upper_bound-1 *indices 0
            {then_reduce_and_save}:
                pop 1                   // _ prn number upper_bound-1 *indices
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                split                   // _ number *indices upper_bound-1 hi lo
                dup 2 and               // _ number *indices upper_bound-1 hi index
                swap 1 pop 1            // _ number *indices upper_bound-1 index

                swap 1 swap 2 swap 1    // _ number upper_bound-1 *indices index
                dup 1 swap 1            // _ number upper_bound-1 *indices *indices index
                call {push_element}

                push 0
                return

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER:  _ number upper_bound-1 *indices
            {else_drop_tip}:
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                pop 1 swap 1            // _ number upper_bound-1 *indices
                return

        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::empty_stack;
    use crate::rust_shadowing_helper_functions;
    use crate::test_prelude::*;

    impl Procedure for SampleIndices {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
            _: &NonDeterminism,
            _: &[BFieldElement],
            sponge: &mut Option<Tip5>,
        ) -> Vec<BFieldElement> {
            let sponge = sponge.as_mut().expect("sponge must be initialized");

            // collect upper bound and number from stack
            let upper_bound = stack.pop().unwrap().value() as u32;
            let number = stack.pop().unwrap().value() as usize;

            println!("sampling {number} indices between 0 and {upper_bound}");
            println!("sponge before: {}", sponge.state.iter().join(","));

            let indices = sponge.sample_indices(upper_bound, number);

            // allocate memory for list
            let list_pointer =
                rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(memory);
            rust_shadowing_helper_functions::list::list_new(list_pointer, memory);

            // store all indices
            for index in indices.iter() {
                rust_shadowing_helper_functions::list::list_push(
                    list_pointer,
                    vec![BFieldElement::new(*index as u64)],
                    memory,
                );
            }
            println!("sponge after: {}", sponge.state.iter().join(","));

            stack.push(list_pointer);

            vec![]
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> ProcedureInitialState {
            let mut rng = StdRng::from_seed(seed);
            let number = if let Some(case) = bench_case {
                match case {
                    // For FRI num_collinearity checks is 80 for expansion factor 4
                    BenchmarkCase::CommonCase => 40,

                    // For FRI num_collinearity checks is 40 for expansion factor 8
                    BenchmarkCase::WorstCase => 80,
                }
            } else {
                rng.random_range(0..20)
            };
            let upper_bound = if let Some(case) = bench_case {
                match case {
                    BenchmarkCase::CommonCase => 1 << 12,
                    BenchmarkCase::WorstCase => 1 << 23,
                }
            } else {
                1 << rng.random_range(0..20)
            };

            let mut stack = empty_stack();
            stack.push(BFieldElement::new(number as u64));
            stack.push(BFieldElement::new(upper_bound as u64));

            let public_input: Vec<BFieldElement> = vec![];
            let state = Tip5 {
                state: rng.random(),
            };

            ProcedureInitialState {
                stack,
                nondeterminism: NonDeterminism::default(),
                public_input,
                sponge: Some(state),
            }
        }
    }

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleIndices).test();
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedProcedure::new(SampleIndices).bench();
    }
}
