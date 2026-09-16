use tasm_lib::data_type::DataType;
use tasm_lib::library::StaticAllocation;
use tasm_lib::prelude::BasicSnippet;
use tasm_lib::prelude::Library;
use tasm_lib::triton_vm::prelude::*;

/// Compare two u480s, each a little-endian array of u32 limbs with limb 0
/// on top of the stack.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: [u32; 15]] [lhs: [u32; 15]]
/// AFTER:  _ (lhs < rhs)
/// ```
///
/// ### Crashes
///
/// - if any limb is not a u32
///
/// The operands are moved to static memory, since the deeper one is out of
/// the stack's reach, and compared limb by limb from the most significant.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Lt;

impl Lt {
    pub const NUM_LIMBS: usize = 15;
}

impl BasicSnippet for Lt {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let limbs = DataType::Tuple(vec![DataType::U32; Self::NUM_LIMBS]);
        ["rhs", "lhs"]
            .map(|name| (limbs.clone(), name.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "lhs_lt_rhs".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u480_lt".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let lhs = library.kmalloc(Self::NUM_LIMBS as u32);
        let rhs = library.kmalloc(Self::NUM_LIMBS as u32);

        let cell = |alloc: &StaticAllocation, k: usize| alloc.write_address() + bfe!(k as u64);
        let read = |alloc: &StaticAllocation, k: usize| triton_asm!(push {cell(alloc, k)} read_mem 1 pop 1);
        // BEFORE: _ [limbs]
        // AFTER:  _
        let store_limbs = |alloc: &StaticAllocation| {
            triton_asm!(
                push {alloc.write_address()}
                write_mem 5 write_mem 5 write_mem 5
                pop 1
            )
        };

        // While no limb has decided the comparison, a smaller limb decides
        // it in favor, a larger one against.
        let compare_limb = |k: usize| {
            triton_asm!(
                // _ result undecided
                {&read(&rhs, k)} {&read(&lhs, k)}
                // _ result undecided r l

                dup 1 dup 1 lt
                place 2 eq
                // _ result undecided (l < r) (l == r)

                dup 2 dup 2 mul pick 4 add place 3
                // _ result' undecided (l < r) (l == r)

                pick 2 mul
                pick 1 pop 1
                // _ result' undecided'
            )
        };
        let compare_limbs = (0..Self::NUM_LIMBS)
            .rev()
            .flat_map(compare_limb)
            .collect::<Vec<_>>();

        triton_asm!(
            // BEFORE: _ [rhs] [lhs]
            // AFTER:  _ (lhs < rhs)
            {self.entrypoint()}:
                {&store_limbs(&lhs)}
                {&store_limbs(&rhs)}
                // _

                push 0 push 1
                {&compare_limbs}
                // _ result undecided

                pop 1
                // _ (lhs < rhs)

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

    use super::*;

    /// Where the snippet's static allocations land in an isolated run.
    fn lhs_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(14)
    }
    fn rhs_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(29)
    }

    impl Function for Lt {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) -> Result<(), RustShadowError> {
            let lhs: [u32; Self::NUM_LIMBS] = pop_encodable(stack)?;
            let rhs: [u32; Self::NUM_LIMBS] = pop_encodable(stack)?;

            for k in 0..Self::NUM_LIMBS {
                memory.insert(lhs_address() + bfe!(k), bfe!(lhs[k]));
                memory.insert(rhs_address() + bfe!(k), bfe!(rhs[k]));
            }

            let lhs = BigUint::new(lhs.to_vec());
            let rhs = BigUint::new(rhs.to_vec());
            push_encodable(stack, &(lhs < rhs));

            Ok(())
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let lhs: [u32; Self::NUM_LIMBS] = rng.random();
            let rhs = match rng.random_range(0..4) {
                0 => lhs,
                1 => {
                    let mut rhs = lhs;
                    rhs[rng.random_range(0..Self::NUM_LIMBS)] = rng.random();
                    rhs
                }
                _ => rng.random(),
            };
            self.initial_state(lhs, rhs)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let zero = [0; Self::NUM_LIMBS];
            let max = [u32::MAX; Self::NUM_LIMBS];
            let mut one = zero;
            one[0] = 1;
            let mut two = zero;
            two[0] = 2;
            let mut three = zero;
            three[0] = 3;
            let mut u32_max = zero;
            u32_max[0] = u32::MAX;
            let mut twopow32 = zero;
            twopow32[1] = 1;
            let mut top = zero;
            top[Self::NUM_LIMBS - 1] = 1;

            let bindings = [zero, one, two, three, max, u32_max, twopow32, top];

            bindings
                .iter()
                .flat_map(|&lhs| bindings.iter().map(move |&rhs| (lhs, rhs)))
                .map(|(lhs, rhs)| self.initial_state(lhs, rhs))
                .collect()
        }
    }

    impl Lt {
        fn initial_state(
            &self,
            lhs: [u32; Self::NUM_LIMBS],
            rhs: [u32; Self::NUM_LIMBS],
        ) -> FunctionInitialState {
            let mut stack = self.init_stack_for_isolated_run();
            push_encodable(&mut stack, &rhs);
            push_encodable(&mut stack, &lhs);
            FunctionInitialState {
                stack,
                memory: HashMap::default(),
            }
        }
    }

    #[test]
    fn rust_and_tasm_agree() {
        ShadowedFunction::new(Lt).test()
    }
}
