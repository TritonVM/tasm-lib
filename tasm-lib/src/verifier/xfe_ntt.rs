use std::collections::HashMap;

use rand::prelude::*;
use triton_vm::prelude::*;
use twenty_first::math::ntt::ntt;
use twenty_first::math::traits::PrimitiveRootOfUnity;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::memory::encode_to_memory;
use crate::structure::tasm_object::TasmObject;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::function::Function;
use crate::traits::function::FunctionInitialState;
use crate::Library;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct XfeNtt;

impl BasicSnippet for XfeNtt {
    fn entrypoint(&self) -> String {
        "tasmlib_verifier_xfe_ntt".to_owned()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Tuple(vec![]), "result".to_owned())]
    }

    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::Xfe)), "x".to_owned()),
            (DataType::Bfe, "omega".to_owned()),
        ]
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let tasm_arithmetic_u32_leadingzeros =
            library.import(Box::new(crate::arithmetic::u32::leadingzeros::Leadingzeros));
        #[allow(non_snake_case)]
        let tasm_list_length___xfe =
            library.import(Box::new(crate::list::length::Length::new(DataType::Xfe)));
        const THREE_INV: BFieldElement = BFieldElement::new(12297829379609722881);

        let while_loop_with_bitreverse = format!("{entrypoint}_while_with_bitreverse");
        let outer_loop = format!("{entrypoint}_while_outer");
        let middle_loop = format!("{entrypoint}_while_middle");
        let inner_loop = format!("{entrypoint}_while_inner");
        let bitreverse_function = format!("{entrypoint}_bitreverse_function");
        let bitreverse_loop = format!("{entrypoint}_bitreverse_while");
        let k_lt_r_then_branch = format!("{entrypoint}_k_lt_r_then_branch");
        // _binop_Lt__LboolR_bool_74_while_loop

        triton_asm!(

        {entrypoint}:
        // _ *x omega

            dup 1
            // _ *x omega *x

            call {tasm_list_length___xfe}
            // _ *x omega size

            push 32
            dup 1
            call {tasm_arithmetic_u32_leadingzeros}
            push -1
            mul
            add
            push -1
            add
            push 0
            // _ *x omega size log_2_size k

            call {while_loop_with_bitreverse}
            // _ *x omega size log_2_size k

            pop 1
            // _ *x omega size log_2_size

            push 1
            // _ *x omega size log_2_size m

            push 0
            // _ *x omega size log_2_size m outer_count

            call {outer_loop}
            pop 5
            pop 1

            return

        // Subroutines:

        // Invariant: n l r i
        {bitreverse_loop}:
            dup 0
            dup 3
            eq
            skiz
            return
            // _ n l r i

            swap 1
            push 2
            mul
            // _ n l i (r * 2)

            dup 3
            // _ n l i (r * 2) n

            push 1
            and
            // _ n l i (r * 2) (n & 1)
                dup 1
                dup 1
                // _ n l i (r * 2) (n & 1) (r * 2) (n & 1)
                xor
                // _ n l i (r * 2) (n & 1) ((r * 2) ^ (n & 1))

                swap 2
                // _ n l i ((r * 2) ^ (n & 1)) (n & 1) (r * 2)

                and
                // _ n l i ((r * 2) ^ (n & 1)) ((n & 1) && (r * 2))

                add
                // _ n l i (((r * 2) ^ (n & 1)) + ((n & 1) && (r * 2)))
                // _ n l i r'

            swap 1
            // _ n l r' i

            push 2
            dup 4
            // _ n l r' i n

            div_mod
            pop 1
            // _ n l r' i (n / 2)

            swap 4
            pop 1
            // _ (n / 2) l r' i

            push 1
            add
            // _ (n / 2) l r' i'

            recurse

        {bitreverse_function}:
            // _ *x omega size log_2_size k n l

            push 0
            push 0
            call {bitreverse_loop}
            pop 1
            swap 2
            pop 2
            return

            // _ *x omega size log_2_size k rk
        {k_lt_r_then_branch}:

            dup 5
            // _ *x omega size log_2_size k rk *x

            dup 0
            // _ *x omega size log_2_size k rk *x *x

            swap 2
            // _ *x omega size log_2_size k *x *x rk

            push 3
            mul
            push 3
            add
            add
            // _ *x omega size log_2_size k *x *(x[rk] + 2)

            read_mem 3
            // _ *x omega size log_2_size k *x [x[rk]] *(x[rk] - 1)

            push 1
            add
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk]

            dup 5
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk] k

            push 3
            mul
            push 3
            add
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk] k_offset

            dup 5
            add
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk] *(x[k] + 2)

            read_mem 3
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk] [x[k]] *(x[k] - 1)

            push 1
            add
            // _ *x omega size log_2_size k *x [x[rk]] *x[rk] [x[k]] *x[k]

            swap 4
            // _ *x omega size log_2_size k *x [x[rk]] *x[k] [x[k]] *x[rk]

            write_mem 3
            pop 1
            // _ *x omega size log_2_size k *x [x[rk]] *x[k]

            write_mem 3
            // _ *x omega size log_2_size k *x *(x[k] +3)

            pop 1
            // _ *x omega size log_2_size k *x

        return

        // 1st loop, where `bitreverse` is called
        {while_loop_with_bitreverse}:
        // _ *x omega size log_2_size k

            dup 0
            dup 3
            eq
            skiz
            return
            // _ *x omega size log_2_size k

            dup 0
            dup 2
            // _ *x omega size log_2_size k k log_2_size
            call {bitreverse_function}
            // _ *x omega size log_2_size k rk

            dup 0
            dup 2
            // _ *x omega size log_2_size k rk rk k

            lt
            // _ *x omega size log_2_size k rk (k < rk)

            skiz
            call {k_lt_r_then_branch}
            // _ *x omega size log_2_size k (rk|*x)

            pop 1
            // _ *x omega size log_2_size k

            push 1
            add
            // _ *x omega size log_2_size (k+1)

            recurse

        // Last while-loop, *inner*, `j != m` <-- The busy-loop!
        {inner_loop}:
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k+j]

            dup 1
            dup 1
            eq
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k + j] (j == m)
            skiz
            return
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k + j]
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx

            dup 0
            push 2
            add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx *x[k + j]_last_word

            read_mem 3
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [x[k + j]] *x[k + j - 1]_last_word

            dup 10
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [x[k + j]] *x[k + j - 1]_last_word (3*m)

            push 3
            add
            add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [x[k + j]] *x[k + j + m]_last_word

            read_mem 3
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [x[k+j]] [x[k+j+m]] *x[k+j+m-1]_last_word

            pop 1
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [x[k+j]] [x[k+j+m]]
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u]      [v]

            dup 8
            xb_mul
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u] (v * w)
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u] [v']

            dup 5
            dup 5
            dup 5
            dup 5
            dup 5
            dup 5
            xx_add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u] [v'] [u + v']

            dup 9
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u] [v'] [u + v'] *x[k + j]

            write_mem 3
            pop 1
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u] [v']

            push -1
            xb_mul
            xx_add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u - v']

            dup 3
            dup 10
            add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx [u - v'] *x[k + j + m]

            write_mem 3
            pop 1
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *xx

            push 3 add
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k + j + 1]

            swap 2
            dup 4
            mul
            swap 2
            // _ *x omega size log_2_size (3*m) outer_count w_m k (w * w_m) *x[k+m] *x[k + j + 1]

            recurse

        // Last while-loop middle, k < size
        {middle_loop}:
            // _ *x omega size log_2_size m outer_count w_m k

            dup 5
            dup 1
            lt
            push 0
            eq
            skiz
            return
            // _ *x omega size log_2_size m outer_count w_m k

            push 1
            // _ *x omega size log_2_size m outer_count w_m k w

            dup 8
            // _ *x omega size log_2_size m outer_count w_m k w *x

            dup 2
            dup 6
            add
            // _ *x omega size log_2_size m outer_count w_m k w *x (k + m)

            push 3
            mul
            add
            push 1
            add
            // _ *x omega size log_2_size m outer_count w_m k w *x[k+m]

            dup 9
            dup 3
            push 3
            mul
            add
            push 1
            add
            // _ *x omega size log_2_size m outer_count w_m k w *x[k+m] *x[k+j]

            // `m` -> `3 * m` for fewer clock cycles in busy-loop
            swap 6
            push 3
            mul
            swap 6
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k+j]

            call {inner_loop}
            // _ *x omega size log_2_size (3*m) outer_count w_m k w *x[k+m] *x[k+j]

            // Undo `3*` transformation
            // `3 * m` -> `m`
            swap 6
            push {THREE_INV}
            mul
            swap 6

            pop 3
            // _ *x omega size log_2_size m outer_count w_m k

            dup 3
            // _ *x omega size log_2_size m outer_count w_m k m

            push 2
            mul
            // _ *x omega size log_2_size m outer_count w_m k (m * 2)

            add
            // _ *x omega size log_2_size m outer_count w_m (k + (m * 2))

            recurse

        // Last while-loop outer
        {outer_loop}:
            // _ *x omega size log_2_size m outer_count

            dup 0
            dup 3
            eq
            skiz
            return
            // _ *x omega size log_2_size m outer_count

            dup 4
            // _ *x omega size log_2_size m outer_count omega

            dup 4
            // _ *x omega size log_2_size m outer_count omega size

            push 2
            // _ *x omega size log_2_size m outer_count omega size 2

            dup 4
            mul
            // _ *x omega size log_2_size m outer_count omega size (2 * m)

            swap 1
            div_mod
            pop 1
            // _ *x omega size log_2_size m outer_count omega (size / (2 * m))

            swap 1
            pow
            // _ *x omega size log_2_size m outer_count (omega ** (size / (2 * m)))
            // _ *x omega size log_2_size m outer_count w_m

            push 0
            // _ *x omega size log_2_size m outer_count w_m k

            call {middle_loop}
            // _ *x omega size log_2_size m outer_count w_m k

            swap 3
            // _ *x omega size log_2_size k outer_count w_m m

            push 2
            mul
            // _ *x omega size log_2_size k outer_count w_m (m * 2)

            swap 3
            // _ *x omega size log_2_size (m * 2) outer_count w_m k

            pop 2
            // _ *x omega size log_2_size (m * 2) outer_count

            push 1 add

            recurse
            )
    }
}

impl Function for XfeNtt {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let omega = stack.pop().unwrap();
        let input_pointer = stack.pop().unwrap();

        let mut vector = *Vec::<XFieldElement>::decode_from_memory(memory, input_pointer).unwrap();
        let n = vector.len();

        ntt(&mut vector, omega, n.ilog2());

        encode_to_memory(memory, input_pointer, vector);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> FunctionInitialState {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let n = match bench_case {
            Some(crate::snippet_bencher::BenchmarkCase::CommonCase) => 256,
            Some(crate::snippet_bencher::BenchmarkCase::WorstCase) => 512,
            None => 1 << rng.gen_range(1..=9),
        };
        let vector = (0..n).map(|_| rng.gen()).collect::<Vec<XFieldElement>>();

        let mut stack = empty_stack();
        let mut memory = HashMap::new();

        let vector_pointer = BFieldElement::new(100);
        encode_to_memory(&mut memory, vector_pointer, vector);
        stack.push(vector_pointer);
        stack.push(BFieldElement::primitive_root_of_unity(n as u64).unwrap());

        FunctionInitialState { stack, memory }
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::prelude::*;

    use crate::structure::tasm_object::TasmObject;
    use crate::test_helpers::rust_final_state;
    use crate::test_helpers::tasm_final_state;
    use crate::test_helpers::verify_stack_equivalence;
    use crate::test_helpers::verify_stack_growth;
    use crate::traits::function::Function;
    use crate::traits::function::ShadowedFunction;

    use super::*;

    #[test]
    fn test() {
        let function = ShadowedFunction::new(XfeNtt);
        let num_states = 5;
        let mut rng = thread_rng();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            let FunctionInitialState { stack, memory } =
                XfeNtt.pseudorandom_initial_state(seed, None);
            let vector_address = stack[stack.len() - 2];

            let stdin = vec![];

            let init_stack = stack.to_vec();
            let nondeterminism = NonDeterminism::default().with_ram(memory);

            let rust = rust_final_state(&function, &stack, &stdin, &nondeterminism, &None);

            // run tvm
            let tasm = tasm_final_state(&function, &stack, &stdin, nondeterminism, &None);

            assert_eq!(
                rust.public_output, tasm.public_output,
                "Rust shadowing and VM std out must agree"
            );

            let len = 16;
            verify_stack_equivalence(
                "Rust-shadow",
                &rust.stack[0..len - 1],
                "TASM execution",
                &tasm.op_stack.stack[0..len - 1],
            );
            verify_stack_growth(&function, &init_stack, &tasm.op_stack.stack);

            // read out the output vectors and test agreement
            let rust_result =
                *Vec::<XFieldElement>::decode_from_memory(&rust.ram, vector_address).unwrap();
            let tasm_result =
                *Vec::<XFieldElement>::decode_from_memory(&tasm.ram, vector_address).unwrap();
            assert_eq!(
                rust_result,
                tasm_result,
                "\nrust: {}\ntasm: {}",
                rust_result.iter().join(" | "),
                tasm_result.iter().join(" | ")
            );

            println!(
                "tasm stack: {}",
                tasm.op_stack.stack.iter().skip(16).join(",")
            );
            println!("rust stack: {}", rust.stack.iter().skip(16).join(","));
        }
    }
}

#[cfg(test)]
mod benches {
    use crate::traits::function::ShadowedFunction;
    use crate::traits::rust_shadow::RustShadow;

    use super::*;

    #[test]
    fn xfe_ntt_benchmark() {
        ShadowedFunction::new(XfeNtt).bench();
    }
}
