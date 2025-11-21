use itertools::Itertools;
use triton_vm::prelude::*;

use super::inner_function::InnerFunction;
use crate::list::get::Get;
use crate::list::length::Length;
use crate::prelude::*;

/// Runs a predicate over all elements of a list and returns true only if all elements satisfy the
/// predicate.
pub struct All {
    pub f: InnerFunction,
}

impl All {
    pub fn new(f: InnerFunction) -> Self {
        Self { f }
    }
}

impl BasicSnippet for All {
    fn parameters(&self) -> Vec<(DataType, String)> {
        let element_type = self.f.domain();
        let list_type = DataType::List(Box::new(element_type));
        vec![(list_type, "*input_list".to_string())]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Bool, "all_true".to_string())]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_list_higher_order_u32_all_{}", self.f.entrypoint())
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let input_type = self.f.domain();
        let output_type = self.f.range();
        assert_eq!(output_type, DataType::Bool);

        let get_length = library.import(Box::new(Length));
        let list_get = library.import(Box::new(Get::new(input_type)));

        let inner_function_name = match &self.f {
            InnerFunction::RawCode(rc) => rc.entrypoint(),
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(bs) => {
                let labelled_instructions = bs.annotated_code(library);
                library.explicit_import(&bs.entrypoint(), &labelled_instructions)
            }
        };

        // If function was supplied as raw instructions, we need to append the inner function to the function
        // body. Otherwise, `library` handles the imports.
        let maybe_inner_function_body_raw = match &self.f {
            InnerFunction::RawCode(rc) => rc.function.iter().map(|x| x.to_string()).join("\n"),
            InnerFunction::NoFunctionBody(_) => todo!(),
            InnerFunction::BasicSnippet(_) => Default::default(),
        };
        let entrypoint = self.entrypoint();
        let main_loop = format!("{entrypoint}_loop");

        let result_type_hint = format!("hint all_{}: Boolean = stack[0]", self.f.entrypoint());

        triton_asm!(
            // BEFORE: _ input_list
            // AFTER:  _ result
            {entrypoint}:
                hint input_list = stack[0]
                push 1  // _ input_list res
                {result_type_hint}
                swap 1  // _ res input_list
                dup 0   // _ res input_list input_list
                call {get_length}
                hint list_item: Index = stack[0]
                        // _ res input_list len

                call {main_loop}
                        // _ res input_list 0

                pop 2   // _ res
                return

            // INVARIANT: _ res input_list index
            {main_loop}:
                // test return condition
                dup 0 push 0 eq
                        // _ res input_list index index==0

                skiz return
                        // _ res input_list index

                // decrement index
                push -1 add

                // body

                // read
                dup 1 dup 1
                        // _ res input_list index input_list index
                call {list_get}
                        // _ res input_list index [input_elements]

                // compute predicate
                call {inner_function_name}
                        // _ res input_list index b

                // accumulate
                dup 3   // _ res input_list index b res
                mul     // _ res input_list index (b && res)
                swap 3  // _ (b && res) input_list index res
                pop 1   // _ (b && res) input_list index

                recurse

            {maybe_inner_function_body_raw}
        )
    }
}

#[cfg(test)]
mod tests {
    use num::One;
    use num::Zero;

    use super::*;
    use crate::arithmetic;
    use crate::empty_stack;
    use crate::list::LIST_METADATA_SIZE;
    use crate::list::higher_order::inner_function::RawCode;
    use crate::rust_shadowing_helper_functions;
    use crate::rust_shadowing_helper_functions::list::list_get;
    use crate::rust_shadowing_helper_functions::list::untyped_insert_random_list;
    use crate::test_helpers::test_rust_equivalence_given_complete_state;
    use crate::test_prelude::*;

    impl All {
        fn generate_input_state(
            &self,
            list_pointer: BFieldElement,
            list_length: usize,
            random: bool,
        ) -> InitVmState {
            let mut stack = empty_stack();
            stack.push(list_pointer);

            let mut memory = HashMap::default();
            let input_type = self.f.domain();
            let list_bookkeeping_offset = LIST_METADATA_SIZE;
            let element_index_in_list =
                list_bookkeeping_offset + list_length * input_type.stack_size();
            let element_index = list_pointer + BFieldElement::new(element_index_in_list as u64);
            memory.insert(BFieldElement::zero(), element_index);

            if random {
                untyped_insert_random_list(
                    list_pointer,
                    list_length,
                    &mut memory,
                    input_type.stack_size(),
                );
            } else {
                rust_shadowing_helper_functions::list::list_insert(
                    list_pointer,
                    (0..list_length as u64)
                        .map(BFieldElement::new)
                        .collect_vec(),
                    &mut memory,
                );
            }

            InitVmState::with_stack_and_memory(stack, memory)
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

            // forall elements, read + map + maybe copy
            let list_length =
                rust_shadowing_helper_functions::list::list_get_length(list_pointer, memory);
            let mut satisfied = true;
            for i in 0..list_length {
                let input_item = list_get(list_pointer, i, memory, input_type.stack_size());
                for bfe in input_item.into_iter().rev() {
                    stack.push(bfe);
                }

                self.f.apply(stack, memory);

                let single_result = stack.pop().unwrap().value() != 0;
                satisfied = satisfied && single_result;
            }

            stack.push(BFieldElement::new(satisfied as u64));
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let (stack, memory) = match bench_case {
                Some(BenchmarkCase::CommonCase) => {
                    let list_pointer = BFieldElement::new(5);
                    let list_length = 10;
                    let execution_state =
                        self.generate_input_state(list_pointer, list_length, false);
                    (execution_state.stack, execution_state.nondeterminism.ram)
                }
                Some(BenchmarkCase::WorstCase) => {
                    let list_pointer = BFieldElement::new(5);
                    let list_length = 100;
                    let execution_state =
                        self.generate_input_state(list_pointer, list_length, false);
                    (execution_state.stack, execution_state.nondeterminism.ram)
                }
                None => {
                    let mut rng = StdRng::from_seed(seed);
                    let list_pointer = BFieldElement::new(rng.next_u64() % (1 << 20));
                    let list_length = 1 << (rng.next_u32() as usize % 4);
                    let execution_state =
                        self.generate_input_state(list_pointer, list_length, true);
                    (execution_state.stack, execution_state.nondeterminism.ram)
                }
            };

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn rust_shadow() {
        let inner_function = InnerFunction::BasicSnippet(Box::new(TestHashXFieldElementLsb));
        ShadowedFunction::new(All::new(inner_function)).test();
    }

    #[test]
    fn all_lt_test() {
        const TWO_POW_31: u64 = 1u64 << 31;
        let rawcode = RawCode::new(
            triton_asm!(
                less_than_2_pow_31:
                    push 2147483648 // == 2^31
                    swap 1
                    lt
                    return
            ),
            DataType::Bfe,
            DataType::Bool,
        );
        let snippet = All::new(InnerFunction::RawCode(rawcode));
        let mut memory = HashMap::new();

        // Should return true
        rust_shadowing_helper_functions::list::list_insert(
            BFieldElement::new(42),
            (0..30).map(BFieldElement::new).collect_vec(),
            &mut memory,
        );
        let input_stack = [empty_stack(), vec![BFieldElement::new(42)]].concat();
        let expected_end_stack_true = [empty_stack(), vec![BFieldElement::one()]].concat();
        let shadowed_snippet = ShadowedFunction::new(snippet);
        let mut nondeterminism = NonDeterminism::default().with_ram(memory);
        test_rust_equivalence_given_complete_state(
            &shadowed_snippet,
            &input_stack,
            &[],
            &nondeterminism,
            &None,
            Some(&expected_end_stack_true),
        );

        // Should return false
        rust_shadowing_helper_functions::list::list_insert(
            BFieldElement::new(42),
            (0..30)
                .map(|x| BFieldElement::new(x + TWO_POW_31 - 20))
                .collect_vec(),
            &mut nondeterminism.ram,
        );
        let expected_end_stack_false = [empty_stack(), vec![BFieldElement::zero()]].concat();
        test_rust_equivalence_given_complete_state(
            &shadowed_snippet,
            &input_stack,
            &[],
            &nondeterminism,
            &None,
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
                div_mod  // _ hi q r
                swap 2   // _ r q hi
                pop 2    // _ r
                return
            ),
            DataType::Bfe,
            DataType::Bool,
        );
        let snippet = All::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    #[test]
    fn test_with_raw_function_eq_42() {
        let raw_code = RawCode::new(
            triton_asm!(
                eq_42:
                push 42
                eq
                return
            ),
            DataType::U32,
            DataType::Bool,
        );
        let snippet = All::new(InnerFunction::RawCode(raw_code));
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
                div_mod  // _ x2 x1 hi q r
                swap 4   // _ r x1 q hi x2
                pop 4    // _ r x1 q hi
                return
            ),
            DataType::Xfe,
            DataType::Bool,
        );
        let snippet = All::new(InnerFunction::RawCode(rawcode));
        ShadowedFunction::new(snippet).test();
    }

    /// Only used for tests. Please don't export this.
    #[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
    pub(super) struct TestHashXFieldElementLsb;

    impl BasicSnippet for TestHashXFieldElementLsb {
        fn parameters(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Xfe, "element".to_string())]
        }

        fn return_values(&self) -> Vec<(DataType, String)> {
            vec![(DataType::Bool, "bool".to_string())]
        }

        fn entrypoint(&self) -> String {
            "test_hash_xfield_element_lsb".to_string()
        }

        fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
            let entrypoint = self.entrypoint();
            let unused_import = library.import(Box::new(arithmetic::u32::safe_add::SafeAdd));
            triton_asm!(
            // BEFORE: _ [x: XFieldElement]
            // AFTER:  _ [b: bool]
            {entrypoint}:
                /* Useless additions: ensure that dependencies are accepted inside
                 * the generated code of `all`
                 */
                push 0
                push 0
                call {unused_import}
                pop 1

                push 0
                push 0
                push 0
                push 0
                push 0
                push 0
                push 1  // _ x2 x1 x0 0 0 0 0 0 0 1
                pick 9
                pick 9
                pick 9  // _ 0 0 0 0 0 0 1 x2 x1 x0

                sponge_init
                sponge_absorb
                sponge_squeeze
                        // _ [d; 10]

                split
                push 2
                place 1
                div_mod // _ [d'; 9] d0_hi (d0_lo // 2) (d0_lo % 2)

                place 11
                pop 5
                pop 5
                pop 1   // _ (d0_lo % 2)

                return
            )
        }
    }
}

#[cfg(test)]
mod benches {
    use super::tests::TestHashXFieldElementLsb;
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        let inner_function = InnerFunction::BasicSnippet(Box::new(TestHashXFieldElementLsb));
        ShadowedFunction::new(All::new(inner_function)).bench();
    }
}
