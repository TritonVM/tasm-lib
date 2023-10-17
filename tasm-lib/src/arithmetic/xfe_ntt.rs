use std::collections::HashMap;

use itertools::Itertools;
use num_traits::One;
use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::{triton_asm, BFieldElement};
use twenty_first::shared_math::{
    ntt::ntt, traits::PrimitiveRootOfUnity, x_field_element::XFieldElement,
};

use crate::{
    arithmetic::u32::safeadd::Safeadd,
    empty_stack,
    function::Function,
    list::unsafeimplu32::{length::Length as UnsafeLength, new::UnsafeNew, push::UnsafePush},
    snippet::{BasicSnippet, DataType},
    structure::tasm_object::{load_to_memory, TasmObject},
};

pub struct XfeNtt;

impl BasicSnippet for XfeNtt {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::List(Box::new(DataType::XFE)), "vector".to_owned()),
            (DataType::BFE, "omega".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(DataType::List(Box::new(DataType::XFE)), "vector".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_arithmetic_xfe_intt".to_string()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        let entrypoint = self.entrypoint();
        let base_case = format!("{entrypoint}_base_case");
        let tasm_list_unsafeimplu32_length_long_xfe =
            library.import(Box::new(UnsafeLength(DataType::XFE)));
        let tasm_list_unsafeimplu32_new_xfe = library.import(Box::new(UnsafeNew(DataType::XFE)));
        let tasm_list_unsafeimplu32_push_xfe = library.import(Box::new(UnsafePush(DataType::XFE)));
        let tasm_arithmetic_u32_safe_add_u32 = library.import(Box::new(Safeadd));
        let tasm_list_unsafeimplu32_push_bfe = library.import(Box::new(UnsafePush(DataType::BFE)));
        let tasm_list_unsafeimplu32_new_bfe = library.import(Box::new(UnsafeNew(DataType::BFE)));
        triton_asm! {
            {entrypoint}:
        dup 1
        call {tasm_list_unsafeimplu32_length_long_xfe}
        dup 0
        push 2
        eq
        push 1
        swap 1
        skiz
        call _fn_call__LVec_RXField_LR_Vec_RXField_L_32_then
        skiz
        call _res__LVec_RXField_LR_Vec_RXField_L_160_else
        swap 3
        pop
        pop
        pop
        return

        {base_case}:
        push 2
        call {tasm_list_unsafeimplu32_new_xfe}
        dup 0
        dup 3
        push 0
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
        dup 6
        push 1
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
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        call {tasm_list_unsafeimplu32_push_xfe}
        dup 0
        dup 3
        push 0
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
        dup 6
        push 1
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
        dup 8
        xbmul
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        call {tasm_list_unsafeimplu32_push_xfe}
        swap 2
        pop
        pop
        return
        _binop_Eq__LboolR_bool_49_then:
        pop
        dup 2
        dup 6
        dup 2
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
        call {tasm_list_unsafeimplu32_push_xfe}
        push 0
        return
        _binop_Eq__LboolR_bool_49_else:
        dup 1
        dup 6
        dup 2
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
        call {tasm_list_unsafeimplu32_push_xfe}
        return
        _binop_Lt__LboolR_bool_44_while_loop:
        dup 0
        dup 4
        swap 1
        lt
        push 0
        eq
        skiz
        return
        dup 0
        push 2
        swap 1
        div_mod
        swap 1
        pop
        push 0
        eq
        push 1
        swap 1
        skiz
        call _binop_Eq__LboolR_bool_49_then
        skiz
        call _binop_Eq__LboolR_bool_49_else
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        _binop_Lt__LboolR_bool_76_while_loop:
        dup 1
        dup 9
        swap 1
        lt
        push 0
        eq
        skiz
        return
        dup 2
        dup 1
        call {tasm_list_unsafeimplu32_push_bfe}
        dup 0
        dup 10
        mul
        swap 1
        pop
        dup 1
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 2
        pop
        recurse
        _binop_Neq__LboolR_bool_98_while_loop:
        dup 0
        dup 10
        push 2
        swap 1
        div_mod
        pop
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 2
        dup 4
        dup 2
        push 1
        mul
        push 1
        add
        add
        read_mem
        swap 1
        pop
        call {tasm_list_unsafeimplu32_push_bfe}
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        _binop_Neq__LboolR_bool_108_while_loop:
        dup 0
        dup 10
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 1
        dup 4
        dup 2
        push 1
        mul
        push 1
        add
        add
        read_mem
        swap 1
        pop
        call {tasm_list_unsafeimplu32_push_bfe}
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        _binop_Neq__LboolR_bool_123_while_loop:
        dup 0
        dup 11
        push 2
        swap 1
        div_mod
        pop
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 1
        dup 7
        dup 2
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
        dup 9
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
        dup 10
        dup 8
        push 1
        mul
        push 1
        add
        add
        read_mem
        swap 1
        pop
        xbmul
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        call {tasm_list_unsafeimplu32_push_xfe}
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        _binop_Neq__LboolR_bool_144_while_loop:
        dup 0
        dup 11
        push 2
        swap 1
        div_mod
        pop
        eq
        push 0
        eq
        push 0
        eq
        skiz
        return
        dup 1
        dup 7
        dup 2
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
        dup 9
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
        dup 9
        dup 8
        push 1
        mul
        push 1
        add
        add
        read_mem
        swap 1
        pop
        xbmul
        xxadd
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        call {tasm_list_unsafeimplu32_push_xfe}
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        _fn_call__LVec_RXField_LR_Vec_RXField_L_32_then:
        pop
        dup 2
        dup 2
        call {base_case}
        push 0
        return
        _res__LVec_RXField_LR_Vec_RXField_L_160_else:
        dup 0
        push 2
        swap 1
        div_mod
        pop
        call {tasm_list_unsafeimplu32_new_xfe}
        dup 1
        push 2
        swap 1
        div_mod
        pop
        call {tasm_list_unsafeimplu32_new_xfe}
        push 0
        call _binop_Lt__LboolR_bool_44_while_loop
        pop
        dup 3
        dup 4
        mul
        dup 2
        dup 1
        call {entrypoint}
        dup 2
        dup 2
        call {entrypoint}
        dup 5
        call {tasm_list_unsafeimplu32_new_bfe}
        push 0
        push 1
        call _binop_Lt__LboolR_bool_76_while_loop
        pop
        pop
        dup 6
        push 2
        swap 1
        div_mod
        pop
        call {tasm_list_unsafeimplu32_new_bfe}
        dup 7
        push 2
        swap 1
        div_mod
        pop
        call {tasm_list_unsafeimplu32_new_bfe}
        push 0
        call _binop_Neq__LboolR_bool_98_while_loop
        call _binop_Neq__LboolR_bool_108_while_loop
        pop
        dup 8
        call {tasm_list_unsafeimplu32_new_xfe}
        push 0
        call _binop_Neq__LboolR_bool_123_while_loop
        push 0
        swap 1
        pop
        call _binop_Neq__LboolR_bool_144_while_loop
        pop
        dup 0
        swap 9
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        return

        _binop_Lt__LboolR_bool_179_while_loop:
        dup 0
        dup 7
        swap 1
        lt
        push 0
        eq
        skiz
        return
        dup 1
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
        dup 8
        dup 8
        dup 8
        xinvert
        xxmul
        swap 3
        pop
        swap 3
        pop
        swap 3
        pop
        dup 4
        dup 4
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
        dup 0
        push 1
        call {tasm_arithmetic_u32_safe_add_u32}
        swap 1
        pop
        recurse
        xfe_intt:
        dup 1
        call {tasm_list_unsafeimplu32_length_long_xfe}
        dup 0
        push 0
        swap 1
        call bfe_new_from_u64
        push 0
        push 0
        swap 2
        push 1
        dup 5
        invert
        mul
        dup 6
        dup 1
        call {entrypoint}
        push 0
        call _binop_Lt__LboolR_bool_179_while_loop
        pop
        swap 7
        pop
        pop
        pop
        pop
        pop
        pop
        pop
        return
        bfe_new_from_u64:
        swap 1
        push 00000000004294967296
        mul
        add
        return
        }
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

        println!("observed length of vector: {n}");

        ntt(&mut vector, omega, n.ilog2());

        println!("outp: {}", vector.iter().join(" | "));

        let output_pointer = load_to_memory(memory, vector);
        stack.push(output_pointer);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (
        Vec<triton_vm::BFieldElement>,
        std::collections::HashMap<triton_vm::BFieldElement, triton_vm::BFieldElement>,
    ) {
        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let n = 32;
        let mut vector = (0..n).map(|_| rng.gen()).collect::<Vec<XFieldElement>>();
        vector[0] = XFieldElement::one();
        vector[1] = XFieldElement::one();

        let mut stack = empty_stack();
        let mut memory = HashMap::new();
        let vector_pointer = load_to_memory(&mut memory, vector);
        println!("{}", memory.get(&vector_pointer).unwrap());
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
        snippet::BasicSnippet,
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
            println!("testing {} with seed: {:x?}", XfeNtt.entrypoint(), seed);
            let (stack, memory) = XfeNtt.pseudorandom_initial_state(seed, None);

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
            // verify_memory_equivalence(&rust.final_ram, &tasm.final_ram);
            verify_stack_growth(&function, &init_stack, &tasm.final_stack);

            // read out the output vectors and test agreement
            let rust_output = *Vec::<XFieldElement>::decode_from_memory(
                &rust.final_ram,
                *rust.final_stack.last().unwrap(),
            )
            .unwrap();
            let tasm_output = *Vec::<XFieldElement>::decode_from_memory(
                &tasm.final_ram,
                *tasm.final_stack.last().unwrap(),
            )
            .unwrap();
            assert_eq!(
                rust_output,
                tasm_output,
                "\nrust: {}\ntasm: {}",
                rust_output.iter().join(" | "),
                tasm_output.iter().join(" | ")
            );
        }
    }
}
