use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::{
    shared_math::{b_field_element::BFieldElement, tip5::STATE_SIZE},
    util_types::algebraic_hasher::AlgebraicHasher,
};

use crate::{
    data_type::DataType, empty_stack, list::ListType, procedure::Procedure,
    rust_shadowing_helper_functions, snippet::BasicSnippet, VmHasher, VmHasherState,
};

/// Sample n pseudorandom integers between 0 and k. It does this by squeezing the sponge. It is the
/// caller's responsibility to ensure that the sponge is initialized to the right state.
#[derive(Clone, Debug)]
pub struct SampleIndices {
    pub list_type: ListType,
}

impl BasicSnippet for SampleIndices {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::U32, "number".to_string()),
            (DataType::U32, "upper_bound".to_string()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::U32)),
            "*indices".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "sample_indices".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_main_loop");
        let then_reduce_and_save = format!("{entrypoint}_then_reduce_and_save");
        let else_drop_tip = format!("{entrypoint}_else_drop_tip");

        let new_list = library.import(self.list_type.new_list(DataType::U32));
        let length = library.import(self.list_type.length(DataType::U32));
        let push_element = library.import(self.list_type.push(DataType::U32));

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
                // allocate a large enough list
                dup 1                   // _ number upper_bound length
                call {new_list}         // _ number upper_bound *indices

                // prepare and call main while lop
                swap 1                  // _ number *indices upper_bound
                push -1 add             // _ number *indices upper_bound-1
                swap 1                  // _ number upper_bound-1 *indices
                call {main_loop}        // _ number upper_bound-1 *indices

                // clean up and return
                swap 2 pop pop
                return

            // INVARIANT: _ number upper_bound-1 *indices
            {main_loop}:
                // evaluate termination condition
                dup 0 call {length}     // _ number upper_bound-1 *indices length
                dup 3 eq                // _ number upper_bound-1 *indices length==number
                skiz return             // _ number upper_bound-1 *indices

                // we need to squeeze so squeeze
                push 0 push 0 push 0 push 0 push 0
                push 0 push 0 push 0 push 0 push 0
                sponge_squeeze          // _ number upper_bound-1 *indices [prn]

                // reject or reduce-and-store
                dup 12 dup 12 dup 12    // _ number upper_bound-1 *indices [prn] number upper_bound-1 *indices

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}


                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}

                {&if_can_sample}
                skiz call {then_reduce_and_save}
                skiz call {else_drop_tip}         // _ number upper_bound-1 *indices number upper_bound-1 *indices

                // return to invariant and repeat
                pop pop pop                       // _ number upper_bound-1 *indices
                recurse

            // BEFORE: _ prn number upper_bound-1 *indices 0
            // AFTER:  _ number upper_bound-1 *indices 0
            {then_reduce_and_save}:
                pop                     // _ prn number upper_bound-1 *indices
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                split                   // _ number *indices upper_bound-1 hi lo
                dup 2 and               // _ number *indices upper_bound-1 hi index
                swap 1 pop              // _ number *indices upper_bound-1 index

                swap 1 swap 2 swap 1    // _ number upper_bound-1 *indices index
                dup 1 swap 1            // _ number upper_bound-1 *indices *indices index
                call {push_element}

                push 0
                return

            // BEFORE: _ prn number upper_bound-1 *indices
            // AFTER:  _ number upper_bound-1 *indices
            {else_drop_tip}:
                swap 2 swap 3           // _ number *indices upper_bound-1 prn
                pop swap 1              // _ number upper_bound-1 *indices
                return

        )
    }
}

impl Procedure for SampleIndices {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
        _nondeterminism: &NonDeterminism<BFieldElement>,
        _public_input: &[BFieldElement],
        sponge_state: &mut Option<VmHasherState>,
    ) -> Vec<BFieldElement> {
        // collect upper bound and number from stack
        let upper_bound = stack.pop().unwrap().value() as u32;
        let number = stack.pop().unwrap().value() as usize;

        println!("sampling {number} indices between 0 and {upper_bound}");
        println!(
            "sponge state before: {}",
            sponge_state.state.iter().map(|b| b.value()).join(","),
        );

        // sample indices
        let indices = VmHasher::sample_indices(sponge_state, upper_bound, number);

        // allocate memory for (unsafe) list
        let list_pointer =
            rust_shadowing_helper_functions::dyn_malloc::dynamic_allocator(1 + number, memory);
        rust_shadowing_helper_functions::unsafe_list::unsafe_list_new(list_pointer, memory);

        // store all indices
        for index in indices.iter() {
            rust_shadowing_helper_functions::unsafe_list::unsafe_list_push(
                list_pointer,
                vec![BFieldElement::new(*index as u64)],
                memory,
                1,
            );
        }
        println!(
            "sponge state after: {}",
            sponge_state.state.iter().map(|b| b.value()).join(",")
        );

        // populate the stack with the list pointer
        stack.push(list_pointer);

        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<BFieldElement>,
        HashMap<BFieldElement, BFieldElement>,
        NonDeterminism<BFieldElement>,
        Vec<BFieldElement>,
        VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let number = if let Some(case) = bench_case {
            match case {
                crate::snippet_bencher::BenchmarkCase::CommonCase => 45,
                crate::snippet_bencher::BenchmarkCase::WorstCase => 160,
            }
        } else {
            rng.gen_range(0..20)
        };
        let upper_bound = if let Some(case) = bench_case {
            match case {
                crate::snippet_bencher::BenchmarkCase::CommonCase => 1 << 12,
                crate::snippet_bencher::BenchmarkCase::WorstCase => 1 << 23,
            }
        } else {
            1 << rng.gen_range(0..20)
        };

        let mut stack = empty_stack();
        stack.push(BFieldElement::new(number as u64));
        stack.push(BFieldElement::new(upper_bound as u64));

        let memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();

        let nondeterminism = NonDeterminism::new(vec![]);
        let public_input: Vec<BFieldElement> = vec![];
        let state = VmHasherState {
            state: rng.gen::<[BFieldElement; STATE_SIZE]>(),
        };

        (stack, memory, nondeterminism, public_input, state)
    }
}

#[cfg(test)]
mod test {
    use crate::{list::ListType, procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleIndices;

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleIndices {
            list_type: ListType::Unsafe,
        })
        .test();
    }
}

#[cfg(test)]
mod bench {
    use crate::{list::ListType, procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleIndices;

    #[test]
    fn bench() {
        ShadowedProcedure::new(SampleIndices {
            list_type: ListType::Unsafe,
        })
        .bench();
    }
}
