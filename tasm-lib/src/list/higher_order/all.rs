use itertools::Itertools;
use num::Zero;
use rand::rngs::StdRng;
use rand::{RngCore, SeedableRng};
use std::collections::HashMap;
use triton_vm::instruction::LabelledInstruction;
use triton_vm::parser::tokenize;
use triton_vm::{triton_asm, NonDeterminism};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::util_types::algebraic_hasher::AlgebraicHasher;

use crate::function::Function;
use crate::list::safe_u32::get::SafeGet;
use crate::list::safe_u32::length::SafeLength;
use crate::list::unsafe_u32::get::UnsafeGet;
use crate::list::unsafe_u32::length::UnsafeLength;
use crate::list::ListType;
use crate::rust_shadowing_helper_functions::safe_list::safe_insert_random_list;
use crate::rust_shadowing_helper_functions::unsafe_list::untyped_unsafe_insert_random_list;
use crate::snippet::BasicSnippet;
use crate::snippet_bencher::BenchmarkCase;
use crate::{arithmetic, get_init_tvm_stack, rust_shadowing_helper_functions, VmHasher};
use crate::{
    library::Library,
    snippet::{DataType, DeprecatedSnippet},
    ExecutionState,
};

use super::inner_function::InnerFunction;

/// Runs a predicate over all elements of a list and returns true
/// only if all elements satisfy the predicate.
pub struct All {
    pub list_type: ListType,
    pub f: InnerFunction,
}

impl All {
    fn generate_input_state(
        &self,
        list_pointer: BFieldElement,
        list_length: usize,
        random: bool,
    ) -> ExecutionState {
        let capacity = list_length;
        let mut stack = get_init_tvm_stack();
        stack.push(list_pointer);

        let mut memory = HashMap::default();
        let input_type = self.f.domain();
        memory.insert(
            BFieldElement::zero(),
            match self.list_type {
                ListType::Safe => {
                    list_pointer
                        + BFieldElement::new((2 + list_length * input_type.get_size()) as u64)
                }
                ListType::Unsafe => {
                    list_pointer
                        + BFieldElement::new((1 + list_length * input_type.get_size()) as u64)
                }
            },
        );

        if random {
            match self.list_type {
                ListType::Safe => safe_insert_random_list(
                    &input_type,
                    list_pointer,
                    capacity as u32,
                    list_length,
                    &mut memory,
                ),
                ListType::Unsafe => untyped_unsafe_insert_random_list(
                    list_pointer,
                    list_length,
                    &mut memory,
                    input_type.get_size(),
                ),
            };
        } else {
            match self.list_type {
                ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_insert(
                    list_pointer,
                    capacity as u32,
                    (0..list_length as u64)
                        .map(BFieldElement::new)
                        .collect_vec(),
                    &mut memory,
                ),
                ListType::Unsafe => {
                    rust_shadowing_helper_functions::unsafe_list::unsafe_list_insert(
                        list_pointer,
                        (0..list_length as u64)
                            .map(BFieldElement::new)
                            .collect_vec(),
                        &mut memory,
                    )
                }
            };
        }

        ExecutionState {
            stack,
            std_in: vec![],
            nondeterminism: NonDeterminism::new(vec![]),
            memory,
            words_allocated: 0,
        }
    }
}

impl BasicSnippet for All {
    fn inputs(&self) -> Vec<(DataType, String)> {
        let input_type = match &self.f {
            InnerFunction::BasicSnippet(basic_snippet) => {
                DataType::List(Box::new(basic_snippet.inputs()[0].0.clone()))
            }
            _ => DataType::VoidPointer,
        };
        vec![(input_type, "*input_list".to_string())]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "all_true".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!(
            "tasm_list_higher_order_{}_u32_all_{}",
            self.list_type,
            self.f.entrypoint()
        )
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert_eq!(output_type, DataType::Bool);
        let get_length = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeLength(input_type.clone()))),
            ListType::Unsafe => library.import(Box::new(UnsafeLength(input_type.clone()))),
        };
        let list_get = match self.list_type {
            ListType::Safe => library.import(Box::new(SafeGet(input_type))),
            ListType::Unsafe => library.import(Box::new(UnsafeGet(input_type))),
        };

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::DeprecatedSnippet(sn) => {
                let fn_body = sn.function_code(library);
                let (_, instructions) = tokenize(&fn_body).unwrap();
                let labelled_instructions =
                    triton_vm::parser::to_labelled_instructions(&instructions);
                library.explicit_import(&sn.entrypoint_name(), &labelled_instructions)
            }
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(bs) => {
                let labelled_instructions = bs.code(library);
                library.explicit_import(&bs.entrypoint(), &labelled_instructions)
            }
        };

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::DeprecatedSnippet(_) => String::default(),
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(_) => Default::default(),
        };
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");

        triton_asm!(
            // BEFORE: _ input_list
            // AFTER: _ result
            {entrypoint}:
                push 1 // _ input_list res
                swap 1 // _ res input_list
                dup 0 // _  res input_list input_list
                call {get_length} // _ res input_list len

                call {main_loop} // _ res input_list 0

                pop // _ res input_list
                pop // _ res
                return

            // INVARIANT: _ res input_list index
            {main_loop}:
                // test return condition
                dup 0 push 0 eq // _ res input_list index index==0

                skiz return
                // _ res input_list index

                // decrement index
                push -1 add

                // body

                // read
                dup 1 dup 1 // _ res input_list index input_list index
                call {list_get} // _ res input_list index [input_elements]

                // compute predicate
                call {inner_function_name} // _ res input_list index b

                // accumulate
                dup 3 // _ res input_list index b res
                mul //    _ res input_list index (b && res)
                swap 3 // _ (b && res) input_list index res
                pop

                recurse

            {maybe_inner_function_body_raw}
        )
    }
}

impl Function for All {
    fn rust_shadow(
        &self,
        stack: &mut Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let input_type = self.f.domain();

        let list_pointer = stack.pop().unwrap();

        // get list length
        let len = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get_length(
                list_pointer,
                memory,
            ),
            ListType::Unsafe => {
                rust_shadowing_helper_functions::unsafe_list::unsafe_list_get_length(
                    list_pointer,
                    memory,
                )
            }
        };

        let get_element = match self.list_type {
            ListType::Safe => rust_shadowing_helper_functions::safe_list::safe_list_get,
            ListType::Unsafe => rust_shadowing_helper_functions::unsafe_list::unsafe_list_get,
        };

        // forall elements, read + map + maybe copy
        let mut satisfied = true;
        for i in 0..len {
            // read
            let mut input_item = get_element(list_pointer, i, memory, input_type.get_size());

            // put on stack
            while let Some(element) = input_item.pop() {
                stack.push(element);
            }

            self.f.apply(stack, memory);

            let single_result = stack.pop().unwrap().value() != 0;
            satisfied = satisfied && single_result;
        }

        // set result
        stack.push(BFieldElement::new(satisfied as u64));
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> (Vec<BFieldElement>, HashMap<BFieldElement, BFieldElement>) {
        match bench_case {
            Some(BenchmarkCase::CommonCase) => {
                let list_pointer = BFieldElement::new(5);
                let list_length = 10;
                let execution_state = self.generate_input_state(list_pointer, list_length, false);
                (execution_state.stack, execution_state.memory)
            }
            Some(BenchmarkCase::WorstCase) => {
                let list_pointer = BFieldElement::new(5);
                let list_length = 100;
                let execution_state = self.generate_input_state(list_pointer, list_length, false);
                (execution_state.stack, execution_state.memory)
            }
            None => {
                let mut rng: StdRng = SeedableRng::from_seed(seed);
                let list_pointer = BFieldElement::new(rng.next_u64() % (1 << 20));
                let list_length = 1 << (rng.next_u32() as usize % 4);
                let execution_state = self.generate_input_state(list_pointer, list_length, true);
                (execution_state.stack, execution_state.memory)
            }
        }
    }
}

// Only used for tests. Please don't export this.
#[derive(Debug, Clone)]
struct TestHashXFieldElementLsb;

impl DeprecatedSnippet for TestHashXFieldElementLsb {
    fn entrypoint_name(&self) -> String {
        "test_hash_xfield_element_lsb".to_string()
    }

    fn input_field_names(&self) -> Vec<String> {
        vec![
            "elem2".to_string(),
            "elem1".to_string(),
            "elem0".to_string(),
        ]
    }

    fn input_types(&self) -> Vec<DataType> {
        vec![DataType::XFE]
    }

    fn output_types(&self) -> Vec<DataType> {
        vec![DataType::Bool]
    }

    fn output_field_names(&self) -> Vec<String> {
        vec!["bool".to_string()]
    }

    fn stack_diff(&self) -> isize {
        -2
    }

    fn function_code(&self, library: &mut Library) -> String {
        let entrypoint = self.entrypoint_name();
        let unused_import = library.import(Box::new(arithmetic::u32::safe_add::SafeAdd));
        format!(
            "
        // BEFORE: _ x2 x1 x0
        // AFTER: _ b
        {entrypoint}:
            // Useless additions, to ensure that dependencies are accepted inside the `all` generated code
                push 0
                push 0
                call {unused_import}
                pop

            push 0
            push 0
            push 0
            push 1 // _ x2 x1 x0 0 0 0 1
            push 0 swap 7 // _ 0 x1 x0 0 0 0 1 x2
            push 0 swap 7 // _ 0 0 x0 0 0 0 1 x2 x1
            push 0 swap 7 // _ 0 0 0 0 0 0 1 x2 x1 x0

            absorb_init
            squeeze // _ d9 d8 d7 d6 d5 d4 d3 d2 d1 d0
            swap 5 pop // _ d9 d8 d7 d6 d0 d4 d3 d2 d1
            swap 5 pop // _ d9 d8 d7 d1 d0 d4 d3 d2
            swap 5 pop
            swap 5 pop
            swap 5 pop

            // _ d4 d3 d2 d1 d0

            split // _ d4 d3 d2 d1 hi lo
            push 2 // _ d4 d3 d2 d1 hi lo 2
            swap 1
            div // _ d4 d3 d2 d1 hi q r
            swap 6
            pop pop pop pop pop pop
            return
        "
        )
    }

    fn crash_conditions(&self) -> Vec<String> {
        vec![]
    }

    fn gen_input_states(&self) -> Vec<ExecutionState> {
        // Function does not output random values, since that would make the benchmark output
        // non-deterministic.
        vec![ExecutionState::with_stack(
            [
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(4888),
                    BFieldElement::new(1u64 << 63),
                    BFieldElement::new((1u64 << 51) + 1000),
                ],
            ]
            .concat(),
        )]
    }

    fn common_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(4888),
                    BFieldElement::new(1u64 << 63),
                    BFieldElement::new((1u64 << 51) + 1000),
                ],
            ]
            .concat(),
        )
    }

    fn worst_case_input_state(&self) -> ExecutionState {
        ExecutionState::with_stack(
            [
                vec![BFieldElement::zero(); 16],
                vec![
                    BFieldElement::new(488800000),
                    BFieldElement::new(1u64 << 62),
                    BFieldElement::new((1u64 << 41) + 1001),
                ],
            ]
            .concat(),
        )
    }

    fn rust_shadowing(
        &self,
        stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
        _memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        let mut xfield_element = vec![];
        for _ in 0..3 {
            xfield_element.push(stack.pop().unwrap());
        }
        let digest = VmHasher::hash_varlen(&xfield_element).values().to_vec();
        let b = digest[0].value() % 2;
        stack.push(BFieldElement::new(b));
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use triton_vm::triton_asm;
    use twenty_first::util_types::algebraic_hasher::Domain;

    use crate::{
        function::ShadowedFunction, list::higher_order::inner_function::RawCode,
        snippet::RustShadow, test_helpers::test_rust_equivalence_given_complete_state,
        VmHasherState,
    };

    use super::*;

    #[test]
    fn unsafe_list_prop_test() {
        let snippet = All {
            list_type: ListType::Unsafe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        };
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn with_safe_list_prop_test() {
        let snippet = All {
            list_type: ListType::Safe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        };
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn safe_list_all_lt_test() {
        const TWO_POW_31: u64 = 1u64 << 31;
        let rawcode = RawCode::new(
            triton_asm!(
                less_than_2_pow_31:
                    push 2147483648 // == 2^31
                    swap 1
                    lt
                    return
            ),
            DataType::BFE,
            DataType::Bool,
        );
        let snippet = All {
            list_type: ListType::Safe,
            f: InnerFunction::RawCode(rawcode),
        };
        let mut memory = HashMap::new();

        // Should return true
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            BFieldElement::new(42),
            42,
            (0..30).map(BFieldElement::new).collect_vec(),
            &mut memory,
        );
        let input_stack = [get_init_tvm_stack(), vec![BFieldElement::new(42)]].concat();
        let expected_end_stack_true =
            vec![get_init_tvm_stack(), vec![BFieldElement::one()]].concat();
        let shadowed_snippet = ShadowedFunction::new(snippet);
        test_rust_equivalence_given_complete_state(
            &shadowed_snippet,
            &input_stack,
            &[],
            &NonDeterminism::new(vec![]),
            &memory,
            &VmHasherState::new(Domain::VariableLength),
            1,
            Some(&expected_end_stack_true),
        );

        // Should return false
        rust_shadowing_helper_functions::safe_list::safe_list_insert(
            BFieldElement::new(42),
            42,
            (0..30)
                .map(|x| BFieldElement::new(x + TWO_POW_31 - 20))
                .collect_vec(),
            &mut memory,
        );
        let expected_end_stack_false =
            vec![get_init_tvm_stack(), vec![BFieldElement::zero()]].concat();
        test_rust_equivalence_given_complete_state(
            &shadowed_snippet,
            &input_stack,
            &[],
            &NonDeterminism::new(vec![]),
            &memory,
            &VmHasherState::new(Domain::VariableLength),
            1,
            Some(&expected_end_stack_false),
        );
    }

    #[test]
    fn test_with_raw_function_lsb_on_bfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                lsb_bfe:
                split    // _ hi lo
                push 2   // _ hi lo 2
                swap 1   // _ hi 2 lo
                div      // _ hi q r
                swap 2   // _ r q hi
                pop      // _ r q
                pop      // _ r
                return
            ),
            DataType::BFE,
            DataType::Bool,
        );
        let snippet = All {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(rawcode),
        };
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_lsb_on_xfe() {
        let rawcode = RawCode::new(
            triton_asm!(
                lsb_xfe:
                split    // _ x2 x1 hi lo
                push 2   // _ x2 x1 hi lo 2
                swap 1   // _ x2 x1 hi 2 lo
                div      // _ x2 x1 hi q r
                swap 4   // _ r x1 q hi x2
                pop      // _ r x1 q hi
                pop      // _ r x1 q
                pop      // _ r q
                pop      // _ r
                return
            ),
            DataType::XFE,
            DataType::Bool,
        );
        let snippet = All {
            list_type: ListType::Unsafe,
            f: InnerFunction::RawCode(rawcode),
        };
        ShadowedFunction::new(snippet).test();
    }
}

#[cfg(test)]
mod benches {
    use super::*;
    use crate::{function::ShadowedFunction, snippet::RustShadow};

    #[test]
    fn unsafe_list_all_benchmark() {
        ShadowedFunction::new(All {
            list_type: ListType::Unsafe,
            f: InnerFunction::DeprecatedSnippet(Box::new(TestHashXFieldElementLsb)),
        })
        .bench();
    }
}
