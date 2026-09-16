use tasm_lib::data_type::DataType;
use tasm_lib::library::StaticAllocation;
use tasm_lib::prelude::BasicSnippet;
use tasm_lib::prelude::Library;
use tasm_lib::triton_vm::prelude::*;

/// Multiply a u320 by a u160, giving a u480. All three are little-endian
/// arrays of u32 limbs with limb 0 on top of the stack.
///
/// ### Behavior
///
/// ```text
/// BEFORE: _ [rhs: [u32; 5]] [lhs: [u32; 10]]
/// AFTER:  _ [product: [u32; 15]]
/// ```
///
/// ### Preconditions
///
/// - all limbs are u32s
///
/// Schoolbook multiplication in static memory: the low and high halves of
/// the partial products accumulate separately, then one pass resolves the
/// carries. The code is unrolled.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct U320MulU160;

impl U320MulU160 {
    pub const NUM_LHS_LIMBS: usize = 10;
    pub const NUM_RHS_LIMBS: usize = 5;
    pub const NUM_PRODUCT_LIMBS: usize = Self::NUM_LHS_LIMBS + Self::NUM_RHS_LIMBS;

    fn limbs(length: usize) -> DataType {
        DataType::Tuple(vec![DataType::U32; length])
    }
}

impl BasicSnippet for U320MulU160 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (Self::limbs(Self::NUM_RHS_LIMBS), "rhs".to_string()),
            (Self::limbs(Self::NUM_LHS_LIMBS), "lhs".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(Self::limbs(Self::NUM_PRODUCT_LIMBS), "product".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_arithmetic_u320_mul_u160".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let lhs = library.kmalloc(Self::NUM_LHS_LIMBS as u32);
        let rhs = library.kmalloc(Self::NUM_RHS_LIMBS as u32);
        let product_lo = library.kmalloc(Self::NUM_PRODUCT_LIMBS as u32);
        let product_hi = library.kmalloc(Self::NUM_PRODUCT_LIMBS as u32);
        let product = library.kmalloc(Self::NUM_PRODUCT_LIMBS as u32);

        let cell = |alloc: &StaticAllocation, k: usize| alloc.write_address() + bfe!(k as u64);
        let read = |alloc: &StaticAllocation, k: usize| triton_asm!(push {cell(alloc, k)} read_mem 1 pop 1);
        let write = |alloc: &StaticAllocation, k: usize| triton_asm!(push {cell(alloc, k)} write_mem 1 pop 1);
        let zero_all = |alloc: &StaticAllocation| {
            let zero_five_words =
                triton_asm!(push 0 push 0 push 0 push 0 push 0 pick 5 write_mem 5);
            triton_asm!(
                push {alloc.write_address()}
                {&zero_five_words} {&zero_five_words} {&zero_five_words}
                pop 1
            )
        };

        // BEFORE: _ [limbs]
        // AFTER:  _
        let store_limbs = |alloc: &StaticAllocation| {
            let write_five_words = triton_asm!(write_mem 5);
            let writes = (0..alloc.num_words() as usize / 5)
                .flat_map(|_| write_five_words.clone())
                .collect::<Vec<_>>();
            triton_asm!(push {alloc.write_address()} {&writes} pop 1)
        };

        let partial_products = (0..Self::NUM_LHS_LIMBS)
            .flat_map(|i| {
                (0..Self::NUM_RHS_LIMBS).flat_map(move |j| {
                    triton_asm!(
                        {&read(&lhs, i)} {&read(&rhs, j)} mul split
                        // _ hi lo
                        {&read(&product_lo, i + j)} add {&write(&product_lo, i + j)}
                        {&read(&product_hi, i + j + 1)} add {&write(&product_hi, i + j + 1)}
                    )
                })
            })
            .collect::<Vec<_>>();

        // BEFORE: _ carry
        // AFTER:  _ carry
        let resolve_carries = (0..Self::NUM_PRODUCT_LIMBS)
            .flat_map(|k| {
                triton_asm!(
                    {&read(&product_lo, k)} add {&read(&product_hi, k)} add split
                    // _ carry' limb
                    {&write(&product, k)}
                )
            })
            .collect::<Vec<_>>();

        let push_product = triton_asm!(
            push {product.read_address()} read_mem 5 read_mem 5 read_mem 5 pop 1
        );

        triton_asm!(
            // BEFORE: _ [rhs] [lhs]
            // AFTER:  _ [product]
            {self.entrypoint()}:
                {&store_limbs(&lhs)}
                {&store_limbs(&rhs)}
                {&zero_all(&product_lo)}
                {&zero_all(&product_hi)}
                {&partial_products}

                push 0
                {&resolve_carries}
                // _ 0

                pop 1
                {&push_product}
                // _ [product]

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
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(9)
    }
    fn rhs_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(14)
    }
    fn product_lo_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(29)
    }
    fn product_hi_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(44)
    }
    fn product_address() -> BFieldElement {
        STATIC_MEMORY_FIRST_ADDRESS - bfe!(59)
    }

    impl Function for U320MulU160 {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) -> Result<(), RustShadowError> {
            let lhs: [u32; Self::NUM_LHS_LIMBS] = pop_encodable(stack)?;
            let rhs: [u32; Self::NUM_RHS_LIMBS] = pop_encodable(stack)?;

            let mut product_lo = [0u64; Self::NUM_PRODUCT_LIMBS];
            let mut product_hi = [0u64; Self::NUM_PRODUCT_LIMBS];
            for (i, &l) in lhs.iter().enumerate() {
                for (j, &r) in rhs.iter().enumerate() {
                    let partial_product = u64::from(l) * u64::from(r);
                    product_lo[i + j] += partial_product & u64::from(u32::MAX);
                    product_hi[i + j + 1] += partial_product >> 32;
                }
            }

            let mut product =
                (BigUint::new(lhs.to_vec()) * BigUint::new(rhs.to_vec())).to_u32_digits();
            product.resize(Self::NUM_PRODUCT_LIMBS, 0);

            for (k, limb) in lhs.iter().enumerate() {
                memory.insert(lhs_address() + bfe!(k), bfe!(*limb));
            }
            for (k, limb) in rhs.iter().enumerate() {
                memory.insert(rhs_address() + bfe!(k), bfe!(*limb));
            }
            for k in 0..Self::NUM_PRODUCT_LIMBS {
                memory.insert(product_lo_address() + bfe!(k), bfe!(product_lo[k]));
                memory.insert(product_hi_address() + bfe!(k), bfe!(product_hi[k]));
                memory.insert(product_address() + bfe!(k), bfe!(product[k]));
            }

            let product: [u32; Self::NUM_PRODUCT_LIMBS] = product.try_into().unwrap();
            push_encodable(stack, &product);

            Ok(())
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            self.initial_state(rng.random(), rng.random())
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let zeros = ([0; Self::NUM_LHS_LIMBS], [0; Self::NUM_RHS_LIMBS]);
            let maxes = (
                [u32::MAX; Self::NUM_LHS_LIMBS],
                [u32::MAX; Self::NUM_RHS_LIMBS],
            );
            let one = ([1, 0, 0, 0, 0, 0, 0, 0, 0, 0], [1, 0, 0, 0, 0]);
            [zeros, maxes, one]
                .map(|(lhs, rhs)| self.initial_state(lhs, rhs))
                .to_vec()
        }
    }

    impl U320MulU160 {
        fn initial_state(
            &self,
            lhs: [u32; Self::NUM_LHS_LIMBS],
            rhs: [u32; Self::NUM_RHS_LIMBS],
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
        ShadowedFunction::new(U320MulU160).test()
    }
}
