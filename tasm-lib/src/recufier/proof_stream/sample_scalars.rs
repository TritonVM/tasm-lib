use std::collections::HashMap;

use itertools::Itertools;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement, NonDeterminism};
use twenty_first::{
    shared_math::{
        tip5::{RATE, STATE_SIZE},
        x_field_element::XFieldElement,
    },
    util_types::algebraic_hasher::SpongeHasher,
};

use crate::{
    empty_stack,
    hashing::squeeze_repeatedly::SqueezeRepeatedly,
    list::unsafeimplu32::{new::UnsafeNew, set_length::UnsafeSetLength},
    memory::dyn_malloc::DYN_MALLOC_ADDRESS,
    procedure::Procedure,
    snippet::{BasicSnippet, DataType},
    structure::tasm_object::load_to_memory,
    VmHasher, VmHasherState,
};

/// Squeeze the sponge to sample a given number of `XFieldElement`s.
pub struct SampleScalars;

impl BasicSnippet for SampleScalars {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(DataType::U32, "num_scalars".to_string())]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(
            DataType::List(Box::new(DataType::XFE)),
            "*scalars".to_string(),
        )]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_proof_stream_sample_scalars".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let new_list_of_xfes = library.import(Box::new(UnsafeNew(DataType::XFE)));
        let set_length = library.import(Box::new(UnsafeSetLength(DataType::XFE)));
        let safety_offset = 1;
        let squeeze_repeatedly = library.import(Box::new(SqueezeRepeatedly));
        let rate = RATE;
        triton_asm! {
            // BEFORE: _ num_scalars
            // AFTER: _ *scalars
            {entrypoint}:

                // create list of enough elements
                dup 0 // _ num_scalars num_scalars
                call {new_list_of_xfes}
                // _ num_scalars *scalars

                // set length
                dup 1 // _ num_scalars *scalars num_scalars
                call {set_length} // _ num_scalars *scalars

                // calculate number of squeezes
                dup 1           // _ num_scalars *scalars num_scalars
                push 3 mul      // _ num_scalars *scalars num_bfes
                push 9 add      // _ num_scalars *scalars (num_bfes+9)
                push {rate} swap 1  // _ num_scalars *scalars rate (num_bfes+9)
                div_mod pop     // _ num_scalars *scalars floor((num_bfes+9)/rate)
                                // _ num_scalars *scalars num_squeezes

                // prepare stack for call to squeeze_repeatedly
                dup 1
                push {safety_offset}
                add
                swap 1          // _ num_scalars *scalars *scalars+so num_squeezes

                // squeeze
                call {squeeze_repeatedly}
                                // _ num_scalars *scalars *scalars' 0

                // clean up stack
                pop pop
                swap 1
                pop  // _ *scalars
                return

        }
    }
}

impl Procedure for SampleScalars {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        _nondeterminism: &triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        _public_input: &[triton_vm::BFieldElement],
        sponge_state: &mut crate::VmHasherState,
    ) -> Vec<triton_vm::BFieldElement> {
        let num_scalars = stack.pop().unwrap().value() as usize;
        let num_squeezes = (num_scalars * 3 + 9) / RATE;
        let pseudorandomness = (0..num_squeezes)
            .flat_map(|_| VmHasher::squeeze(sponge_state).to_vec())
            .collect_vec();
        let scalars = pseudorandomness
            .chunks(3)
            .take(num_scalars)
            .map(|ch| XFieldElement::new(ch.try_into().unwrap()))
            .collect_vec();
        let scalars_pointer = load_to_memory(memory, scalars);

        // store all pseudorandomness (not just sampled scalars) to memory
        let safety_offset = BFieldElement::new(1);
        for (i, pr) in pseudorandomness.iter().enumerate() {
            memory.insert(
                BFieldElement::new(i as u64) + scalars_pointer + safety_offset,
                *pr,
            );
        }

        // the list of scalars was allocated properly; reflect that fact
        memory.insert(
            BFieldElement::new(DYN_MALLOC_ADDRESS as u64),
            BFieldElement::new(1) + safety_offset + BFieldElement::new(num_scalars as u64 * 3),
        );
        memory.insert(scalars_pointer, BFieldElement::new(num_scalars as u64));

        stack.push(scalars_pointer);
        vec![]
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
        triton_vm::NonDeterminism<triton_vm::BFieldElement>,
        Vec<triton_vm::BFieldElement>,
        crate::VmHasherState,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let num_scalars = rng.gen_range(0..40);
        let mut stack = empty_stack();
        stack.push(BFieldElement::new(num_scalars as u64));
        let sponge_state: VmHasherState = twenty_first::shared_math::tip5::Tip5State {
            state: rng.gen::<[BFieldElement; STATE_SIZE]>(),
        };
        let memory: HashMap<BFieldElement, BFieldElement> = HashMap::new();
        let nondeterminism = NonDeterminism::new(vec![]);
        let stdin = vec![];
        (stack, memory, nondeterminism, stdin, sponge_state)
    }
}

#[cfg(test)]
mod test {
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleScalars;

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleScalars).test();
    }
}

#[cfg(test)]
mod bench {
    use crate::{procedure::ShadowedProcedure, snippet::RustShadow};

    use super::SampleScalars;

    #[test]
    fn test() {
        ShadowedProcedure::new(SampleScalars).bench();
    }
}
