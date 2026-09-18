use tasm_lib::data_type::DataType;
use tasm_lib::library::StaticAllocation;
use tasm_lib::prelude::BasicSnippet;
use tasm_lib::prelude::Library;
use tasm_lib::triton_vm::twenty_first::tip5::Digest;
use triton_vm::isa::instruction::LabelledInstruction;
use triton_vm::isa::triton_asm;
use triton_vm::prelude::BFieldElement;
use triton_vm::twenty_first::bfe;

/// Convert a [`Digest`] into the base-2^32 limbs of the number it
/// represents, `Σ digest[i] · p^i`.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [digest: Digest]
/// AFTER:  _ [limbs: [u32; 10]]
/// ```
///
/// Limb 0, the least significant, is on top.
///
/// Horner's scheme over the elements, most significant first:
/// `x ← x·p + digest[i]`, where `x·p = (x·(2^32 − 1))·2^32 + x`, so that
/// no subtraction is needed.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct DigestToU320;

impl DigestToU320 {
    pub const NUM_LIMBS: usize = 2 * Digest::LEN;
}

impl BasicSnippet for DigestToU320 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Digest, "digest".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        let limbs = DataType::Tuple(vec![DataType::U32; Self::NUM_LIMBS]);
        vec![(limbs, "limbs".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_digest_to_u320".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        // The u32 halves of the digest's elements, and the limbs being built.
        let parts = library.kmalloc(Self::NUM_LIMBS as u32);
        let limbs = library.kmalloc(Self::NUM_LIMBS as u32);

        let cell = |alloc: &StaticAllocation, k: usize| alloc.write_address() + bfe!(k as u64);
        let read = |alloc: &StaticAllocation, k: usize| triton_asm!(push {cell(alloc, k)} read_mem 1 pop 1);
        let write = |alloc: &StaticAllocation, k: usize| triton_asm!(push {cell(alloc, k)} write_mem 1 pop 1);

        let zero_limbs = triton_asm!(
            push 0 push 0 push 0 push 0 push 0
            push {limbs.write_address()} write_mem 5
            push 0 push 0 push 0 push 0 push 0
            pick 5 write_mem 5 pop 1
        );

        // BEFORE: _ [digest]
        // AFTER:  _
        let store_parts = (0..Digest::LEN)
            .flat_map(|i| triton_asm!(split push {cell(&parts, 2 * i)} write_mem 2 pop 1))
            .collect::<Vec<_>>();

        // One Horner step. Per limb the stack carries the multiplication's
        // carry, the previous limb of `x·(2^32 − 1)`, and the addition's
        // carry.
        let two_pow_32_minus_one = u64::from(u32::MAX);
        let horner_step = |i: usize| {
            let limb_step = |k: usize| {
                let add_part = if k < 2 {
                    triton_asm!({&read(&parts, 2 * i + k)} add)
                } else {
                    triton_asm!()
                };
                triton_asm!(
                    // _ mul_carry prev_limb add_carry
                    {&read(&limbs, k)}
                    // _ mul_carry prev_limb add_carry x

                    dup 0 push {two_pow_32_minus_one} mul dup 4 add split
                    // _ mul_carry prev_limb add_carry x mul_carry' limb

                    pick 5 pop 1
                    pick 1 place 4
                    // _ mul_carry' prev_limb add_carry x limb

                    pick 3 pick 2 add pick 2 add
                    // _ mul_carry' limb (prev_limb + x + add_carry)

                    {&add_part}
                    split
                    // _ mul_carry' limb add_carry' x'

                    {&write(&limbs, k)}
                    // _ mul_carry' limb add_carry'
                )
            };
            let limb_steps = (0..Self::NUM_LIMBS).flat_map(limb_step).collect::<Vec<_>>();
            triton_asm!(
                push 0 push 0 push 0
                {&limb_steps}
                pop 3
            )
        };
        let horner = (0..Digest::LEN - 1)
            .rev()
            .flat_map(horner_step)
            .collect::<Vec<_>>();

        let push_limbs = triton_asm!(
            push {limbs.read_address()} read_mem 5 read_mem 5 pop 1
        );

        triton_asm!(
            // BEFORE: _ [digest]
            // AFTER:  _ [limbs]
            {self.entrypoint()}:
                {&zero_limbs}
                {&store_parts}
                // _

                // The most significant element seeds Horner's scheme.
                {&read(&parts, Self::NUM_LIMBS - 2)}
                {&write(&limbs, 0)}
                {&read(&parts, Self::NUM_LIMBS - 1)}
                {&write(&limbs, 1)}
                {&horner}

                {&push_limbs}
                // _ [limbs]

                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use num_bigint::BigUint;
    use rand::Rng;
    use rand::SeedableRng;
    use rand::prelude::StdRng;
    use tasm_lib::library::STATIC_MEMORY_FIRST_ADDRESS;
    use tasm_lib::pop_encodable;
    use tasm_lib::push_encodable;
    use tasm_lib::snippet_bencher::BenchmarkCase;
    use tasm_lib::traits::function::Function;
    use tasm_lib::traits::function::FunctionInitialState;
    use tasm_lib::traits::function::ShadowedFunction;
    use tasm_lib::traits::rust_shadow::RustShadow;
    use tasm_lib::traits::rust_shadow::RustShadowError;
    use triton_vm::twenty_first::math::b_field_element::BFieldElement;

    use super::*;

    /// Where the snippet's two static allocations land in an isolated run.
    fn parts_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(9)
    }

    fn limbs_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(19)
    }

    impl Function for DigestToU320 {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) -> Result<(), RustShadowError> {
            let digest: Digest = pop_encodable(stack)?;

            for (i, element) in digest.values().into_iter().enumerate() {
                let (lo, hi) = (element.value() & u64::from(u32::MAX), element.value() >> 32);
                memory.insert(parts_address() + bfe!(2 * i), bfe!(lo));
                memory.insert(parts_address() + bfe!(2 * i + 1), bfe!(hi));
            }

            let mut limbs = BigUint::from(digest).to_u32_digits();
            limbs.resize(Self::NUM_LIMBS, 0);
            for (k, limb) in limbs.iter().enumerate() {
                memory.insert(limbs_address() + bfe!(k), bfe!(*limb));
            }

            let limbs: [u32; Self::NUM_LIMBS] = limbs.try_into().unwrap();
            push_encodable(stack, &limbs);

            Ok(())
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let digest: Digest = StdRng::from_seed(seed).random();
            self.initial_state(digest)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let max = Digest::new([BFieldElement::new(BFieldElement::MAX); Digest::LEN]);
            [Digest::default(), max]
                .map(|digest| self.initial_state(digest))
                .to_vec()
        }
    }

    impl DigestToU320 {
        fn initial_state(&self, digest: Digest) -> FunctionInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &digest);
            FunctionInitialState {
                stack,
                memory: HashMap::default(),
            }
        }
    }

    #[test]
    fn rust_and_tasm_agree() {
        ShadowedFunction::new(DigestToU320).test()
    }
}
