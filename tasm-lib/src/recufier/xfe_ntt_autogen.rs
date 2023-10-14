use crate::function::Function;
use crate::empty_stack;
use crate::snippet::BasicSnippet;
use crate::snippet::DataType;
use crate::structure::tasm_object::encode_to_memory;
use crate::structure::tasm_object::TasmObject;
use crate::Library;
use std::collections::HashMap;
use triton_vm::triton_asm;

use num_traits::One;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::BFieldElement;
use twenty_first::shared_math::{
    ntt::ntt, traits::PrimitiveRootOfUnity, x_field_element::XFieldElement,
};

pub struct XfeNtt;

// This BasicSnippet implementation was autogenerate by the `tasm-lang` compiler
// on 2023-09-29 00:39:28.371476538 +02:00
impl BasicSnippet for XfeNtt {
    fn entrypoint(&self) -> String {
        "xfe_ntt".to_owned()
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Tuple(vec![]), "result".to_owned())]
    }

    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::XFE)), "x".to_owned()),
            (DataType::BFE, "omega".to_owned()),
        ]
    }

    fn code(&self, library: &mut Library) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let tasm_arithmetic_u32_leadingzeros =
            library.import(Box::new(crate::arithmetic::u32::leadingzeros::Leadingzeros));
        let tasm_arithmetic_u32_or = library.import(Box::new(crate::arithmetic::u32::or::Or));
        let tasm_arithmetic_u32_safeadd =
            library.import(Box::new(crate::arithmetic::u32::safeadd::Safeadd));
        let tasm_arithmetic_u32_safemul =
            library.import(Box::new(crate::arithmetic::u32::safemul::Safemul));
        let tasm_arithmetic_u32_safesub =
            library.import(Box::new(crate::arithmetic::u32::safesub::Safesub));
        let tasm_arithmetic_u32_shiftleft =
            library.import(Box::new(crate::arithmetic::u32::shiftleft::Shiftleft));
        let tasm_arithmetic_u32_shiftright =
            library.import(Box::new(crate::arithmetic::u32::shiftright::Shiftright));
        #[allow(non_snake_case)]
        let tasm_list_unsafeimplu32_length___xfe = library.import(Box::new(
            crate::list::unsafeimplu32::length::Length(DataType::XFE),
        ));
        #[allow(non_snake_case)]
        let _1__Lu32R_u32_59 = library.kmalloc(1);
        #[allow(non_snake_case)]
        let _fn_arg_reference_to_LVec_RXField_LR_0 = library.kmalloc(1);
        triton_asm!(
                {entrypoint}:

                push {_fn_arg_reference_to_LVec_RXField_LR_0}
        dup 2
        write_mem
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        call {tasm_list_unsafeimplu32_length___xfe}
        push 32
        dup 1
        call {tasm_arithmetic_u32_leadingzeros}
        swap 1
        call {tasm_arithmetic_u32_safesub}
        push 1
        swap 1
        call {tasm_arithmetic_u32_safesub}
        push 0
        call _binop_Neq__LboolR_bool_34_while_loop
        pop
        push 1
        push {_1__Lu32R_u32_59}
        dup 1
        write_mem
        pop
        push 0
        call _binop_Neq__LboolR_bool_63_while_loop
        pop
        pop
        pop
        pop
        pop
        pop

                return

                // Subroutines:
                _binop_Lt__LboolR_bool_8_while_loop:
        dup 0
        dup 3
        swap 1
        lt
        push 0
        eq
        skiz
        return
        dup 1
        push 1
        call {tasm_arithmetic_u32_shiftleft}
        dup 4
        push 1
        and
        call {tasm_arithmetic_u32_or}
        swap 2
        pop
        dup 3
        push 1
        call {tasm_arithmetic_u32_shiftright}
        swap 4
        pop
        dup 0
        push 1
        call {tasm_arithmetic_u32_safeadd}
        swap 1
        pop
        recurse
        bitreverse:
        push 0
        push 0
        call _binop_Lt__LboolR_bool_8_while_loop
        pop
        swap 2
        pop
        pop
        return
        _binop_Lt__LboolR_bool_40_then:
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 1
        push 3
        mul
        push 1
        add
        add
        push 2
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 5
        push 3
        mul
        push 1
        add
        add
        push 2
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 7
        push 3
        mul
        push 1
        add
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        dup 2
        dup 2
        dup 2
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 8
        push 3
        mul
        push 1
        add
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        pop
        pop
        pop
        push 0
        return
        _binop_Lt__LboolR_bool_40_else:
        return
        _binop_Neq__LboolR_bool_34_while_loop:
        dup 0
        dup 3
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 0
        dup 2
        call bitreverse
        dup 1
        dup 1
        swap 1
        lt
        push 1
        swap 1
        skiz
        call _binop_Lt__LboolR_bool_40_then
        skiz
        call _binop_Lt__LboolR_bool_40_else
        dup 1
        push 1
        call {tasm_arithmetic_u32_safeadd}
        swap 2
        pop
        pop
        recurse
        _binop_Neq__LboolR_bool_79_while_loop:
        dup 0
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 3
        dup 2
        call {tasm_arithmetic_u32_safeadd}
        push 3
        mul
        push 1
        add
        add
        push 2
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 6
        dup 5
        call {tasm_arithmetic_u32_safeadd}
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        call {tasm_arithmetic_u32_safeadd}
        push 3
        mul
        push 1
        add
        add
        push 2
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        push -1
        add
        read_mem
        swap 1
        pop
        dup 2
        dup 2
        dup 2
        dup 10
        xbmul
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        dup 5
        dup 5
        dup 5
        dup 5
        dup 5
        dup 5
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 12
        dup 11
        call {tasm_arithmetic_u32_safeadd}
        push 3
        mul
        push 1
        add
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        dup 5
        dup 5
        dup 5
        dup 5
        dup 5
        dup 5
        push -1
        xbmul
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        push {_fn_arg_reference_to_LVec_RXField_LR_0}
        read_mem
        swap 1
        pop
        dup 12
        dup 11
        call {tasm_arithmetic_u32_safeadd}
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        call {tasm_arithmetic_u32_safeadd}
        push 3
        mul
        push 1
        add
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        push 1
        add
        swap 1
        write_mem
        pop
        dup 7
        dup 10
        mul
        swap 8
        pop
        dup 6
        push 1
        call {tasm_arithmetic_u32_safeadd}
        swap 7
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        recurse
        _binop_Lt__LboolR_bool_74_while_loop:
        dup 0
        dup 6
        swap 1
        lt
        push 0
        eq
        skiz
        return
        push 1
        push 0
        call _binop_Neq__LboolR_bool_79_while_loop
        dup 2
        push 2
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        call {tasm_arithmetic_u32_safemul}
        call {tasm_arithmetic_u32_safeadd}
        swap 3
        pop
        pop
        pop
        recurse
        _binop_Neq__LboolR_bool_63_while_loop:
        dup 0
        dup 3
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 4
        dup 4
        push 2
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        call {tasm_arithmetic_u32_safemul}
        swap 1
        div
        pop
        swap 1
        pow
        push 0
        call _binop_Lt__LboolR_bool_74_while_loop
        push {_1__Lu32R_u32_59}
        read_mem
        swap 1
        pop
        push 2
        call {tasm_arithmetic_u32_safemul}
        push {_1__Lu32R_u32_59}
        swap 1
        write_mem
        pop
        dup 2
        push 1
        call {tasm_arithmetic_u32_safeadd}
        swap 3
        pop
        pop
        pop
        recurse

                // Methods, entrypoints:


                // Method subroutines
            )
    }
}

impl Function for XfeNtt {
    fn rust_shadow(
        &self,
        stack: &mut Vec<triton_vm::BFieldElement>,
        memory: &mut std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
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
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let n = match bench_case {
            Some(crate::snippet_bencher::BenchmarkCase::CommonCase) => 32,
            Some(crate::snippet_bencher::BenchmarkCase::WorstCase) => 128,
            None => 32,
        };
        let mut vector = (0..n).map(|_| rng.gen()).collect::<Vec<XFieldElement>>();
        vector[0] = XFieldElement::one();
        vector[1] = XFieldElement::one();

        let mut stack = empty_stack();
        let mut memory = HashMap::new();

        // Autogenerated TASM code spills to memory. So cannot use the memory
        // with the lowest addresses.
        let vector_pointer = BFieldElement::new(100);
        encode_to_memory(&mut memory, vector_pointer, vector);
        stack.push(vector_pointer);
        stack.push(BFieldElement::primitive_root_of_unity(n as u64).unwrap());

        (stack, memory)
    }
}

#[cfg(test)]
mod test {
    use itertools::Itertools;
    use rand::{thread_rng, Rng};
    use triton_vm::NonDeterminism;
    use twenty_first::{
        shared_math::x_field_element::XFieldElement, util_types::algebraic_hasher::Domain,
    };

    use crate::{
        function::{Function, ShadowedFunction},
        structure::tasm_object::TasmObject,
        test_helpers::{
            rust_final_state, tasm_final_state, verify_stack_equivalence, verify_stack_growth,
        },
        VmHasherState,
    };

    use super::XfeNtt;

    #[test]
    fn test() {
        let function = ShadowedFunction::new(XfeNtt);
        let num_states = 5;
        let mut rng = thread_rng();

        for _ in 0..num_states {
            let seed: [u8; 32] = rng.gen();
            let (stack, memory) = XfeNtt.pseudorandom_initial_state(seed, None);
            let vector_address = stack[stack.len() - 2];

            let stdin = vec![];

            let init_stack = stack.to_vec();
            let nondeterminism = NonDeterminism::new(vec![]);
            let sponge_state = VmHasherState::new(Domain::VariableLength);
            let words_statically_allocated = 1;

            let rust = rust_final_state(
                &function,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                &sponge_state,
                words_statically_allocated,
            );

            // run tvm
            let tasm = tasm_final_state(
                &function,
                &stack,
                &stdin,
                &nondeterminism,
                &memory,
                &sponge_state,
                words_statically_allocated,
            );

            assert_eq!(
                rust.output, tasm.output,
                "Rust shadowing and VM std out must agree"
            );

            let len = rust.final_stack.len();
            verify_stack_equivalence(&rust.final_stack[0..len - 1], &tasm.final_stack[0..len - 1]);
            verify_stack_growth(&function, &init_stack, &tasm.final_stack);

            // read out the output vectors and test agreement
            let rust_result =
                *Vec::<XFieldElement>::decode_from_memory(&rust.final_ram, vector_address).unwrap();
            let tasm_result =
                *Vec::<XFieldElement>::decode_from_memory(&tasm.final_ram, vector_address).unwrap();
            assert_eq!(
                rust_result,
                tasm_result,
                "\nrust: {}\ntasm: {}",
                rust_result.iter().join(" | "),
                tasm_result.iter().join(" | ")
            );
        }
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    #[test]
    fn xfe_ntt_benchmark() {
        ShadowedFunction::new(XfeNtt).bench();
    }
}
