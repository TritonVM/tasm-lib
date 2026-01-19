use std::collections::HashMap;

use triton_vm::prelude::*;

use crate::list::new::New;
use crate::prelude::*;
use crate::traits::basic_snippet::Reviewer;
use crate::traits::basic_snippet::SignOffFingerprint;

/// Generates a list containing all integers between the minimum (inclusive
/// lower bound) and the supremum (exclusive upper bound).
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [minimum: u32] [supremum: u32]
/// AFTER:  _ *list
/// ```
///
/// ### Preconditions
///
/// - all input arguments are properly [`BFieldCodec`] encoded
/// - the `minimum` is less than or equal to the `supremum`
///
/// ### Postconditions
///
/// - `*list` is a pointer to a [`BFieldCodec`] encoded list of properly encoded
///   `u32`s
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Range;

impl Range {
    pub const INVALID_ERROR_ID: i128 = 550;
}

impl BasicSnippet for Range {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["minimum", "supremum"]
            .map(|s| (DataType::U32, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        let list_type = DataType::List(Box::new(DataType::U32));
        vec![(list_type, "*list".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_range".into()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let new_list = library.import(Box::new(New));

        let entrypoint = self.entrypoint();
        let inner_loop = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ minimum supremum
            // AFTER:  _ *list
            {entrypoint}:
                dup 0 addi 1 dup 2      // _ minimum supremum (supremum + 1) minimum
                lt                      // _ minimum supremum (minimum <= supremum)
                assert error_id {Self::INVALID_ERROR_ID}

                // calculate length
                dup 0 dup 2             // _ minimum supremum supremum minimum
                push -1 mul add         // _ minimum supremum (supremum - minimum)
                                        // _ minimum supremum length

                // create list object
                call {new_list}         // _ minimum supremum length *list
                dup 0 place 4           // _ *list minimum supremum length *list
                write_mem 1             // _ *list minimum supremum *list[0]
                call {inner_loop}       // _ *list supremum supremum *list[n]
                pop 3                   // _ *list
                return

            // BEFORE:    _ minimum     supremum *list[0]
            // INVARIANT: _ (minimum+i) supremum *list[i]
            // AFTER:     _ supremum    supremum *list[n]
            {inner_loop}:
                dup 2 dup 2 eq          // _ (minimum+i) supremum *list[i] (minimum+i == supremum)
                skiz return             // _ (minimum+i) supremum *list[i]

                dup 2 place 1           // _ (minimum+i) supremum (minimum+i) *list[i]
                write_mem 1             // _ (minimum+i) supremum *list[i+1]

                pick 2 addi 1 place 2   // _ (minimum+i+1) supremum *list[i+1]
                recurse
        )
    }

    fn sign_offs(&self) -> HashMap<Reviewer, SignOffFingerprint> {
        let mut sign_offs = HashMap::new();
        sign_offs.insert(Reviewer("ferdinand"), 0x39a164b082c968d.into());
        sign_offs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator;
    use crate::test_prelude::*;

    impl Range {
        fn set_up_initial_state(&self, minimum: u32, supremum: u32) -> FunctionInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            stack.push(bfe!(minimum));
            stack.push(bfe!(supremum));

            FunctionInitialState {
                stack,
                ..Default::default()
            }
        }
    }

    impl Function for Range {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let supremum = pop_encodable::<u32>(stack);
            let minimum = pop_encodable::<u32>(stack);
            assert!(minimum <= supremum);

            let list_pointer = dynamic_allocator(memory);
            let list = (minimum..supremum).collect_vec();
            encode_to_memory(memory, list_pointer, &list);

            stack.push(list_pointer);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let (minimum, supremum) = match bench_case {
                Some(BenchmarkCase::CommonCase) => (0, 45),
                Some(BenchmarkCase::WorstCase) => (0, 250),
                None => {
                    let mut rng = StdRng::from_seed(seed);
                    let supremum = rng.random_range(0..=400);
                    let minimum = rng.random_range(0..=supremum);
                    (minimum, supremum)
                }
            };

            self.set_up_initial_state(minimum, supremum)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            [(0, 0), (0, 1), (0, 10), (5, 15), (15, 15)]
                .map(|(min, sup)| self.set_up_initial_state(min, sup))
                .to_vec()
        }
    }

    #[test]
    fn rust_shadow() {
        ShadowedFunction::new(Range).test();
    }

    #[proptest]
    fn invalid_range_crashes_vm(
        #[strategy(1_u32..)] minimum: u32,
        #[strategy(..#minimum)] supremum: u32,
    ) {
        test_assertion_failure(
            &ShadowedFunction::new(Range),
            Range.set_up_initial_state(minimum, supremum).into(),
            &[Range::INVALID_ERROR_ID],
        );
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(Range).bench();
    }
}
