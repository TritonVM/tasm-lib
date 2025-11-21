use itertools::Itertools;
use triton_vm::prelude::*;

use crate::list::length::Length;
use crate::prelude::*;

/// HornerEvaluationDynamicLength takes a list of XFieldElements, representing
/// the coefficients of a polynomial, and evaluates it in a given indeterminate,
/// which is also an XFieldElement.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct HornerEvaluationDynamicLength;

impl BasicSnippet for HornerEvaluationDynamicLength {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![
            (
                DataType::List(Box::new(DataType::Xfe)),
                "*coefficients".to_string(),
            ),
            (DataType::Xfe, "indeterminate".to_string()),
        ]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "value".to_string())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_list_horner_evaluation_dynamic_length".to_string()
    }

    fn code(&self, library: &mut Library) -> Vec<LabelledInstruction> {
        let horner_iteration = triton_asm! {
            // BEFORE: _ *ptr [x] [acc]
            // AFTER:  _ *ptr-3 [x] [acc*x+ct]
            dup 5 dup 5 dup 5    // _ *ptr [x] [acc] [x]
            xx_mul               // _ *ptr [x] [x*acc]
            dup 6                // _ *ptr [x] [x*acc] *ptr
            read_mem 3           // _ *ptr [x] [x*acc] [ct] *ptr-3
            swap 10 pop 1        // _ *ptr-3 [x] [x*acc] [ct]
            xx_add               // _ *ptr-3 [x] [x*acc+ct]

        };
        let batch_size = 16;
        let three_times_batch_size_plus_two = batch_size * 3 + 2;
        let many_horner_iterations = (0..batch_size)
            .flat_map(|_| horner_iteration.clone())
            .collect_vec();

        let entrypoint = self.entrypoint();
        let loop_batches = format!("{entrypoint}_loop_batches");
        let loop_remainder = format!("{entrypoint}_loop_remainder");
        let length_of_list = library.import(Box::new(Length));

        triton_asm! {
            // BEFORE: *coefficients [x]
            // AFTER: [poly(x)]
            {entrypoint}:
                dup 3                           // _ *coefficients [x] *coefficients
                call {length_of_list}           // _ *coefficients [x] num_coefficients
                push 3 mul                      // _ *coefficients [x] size
                dup 4 add                       // _ *coefficients [x] *last_coefficient+2
                dup 3 dup 3 dup 3               // _ *coefficients [x] *last_coefficient+2 [x]
                push 0 push 0 push 0            // _ *coefficients [x] *last_coefficient+2 [x] [0]
                call {loop_batches}
                call {loop_remainder}           // _ *coefficients [x] *coefficients-1 [x] [poly(x)]
                                                // _ *coefficients x2 x1 x0 *coefficients-1 x2 x1 x0 v2 v1 v0
                swap 8 pop 1                    // _ *coefficients x2 v0 x0 *coefficients-1 x2 x1 x0 v2 v1
                swap 8 pop 1                    // _ *coefficients v1 v0 x0 *coefficients-1 x2 x1 x0 v2
                swap 8 pop 5 pop 1              // _ v2 v1 v0
                return

            // INVARIANT: *start [x] *ptr [x] [acc]
            {loop_batches}:
                dup 6       // *start [x] *ptr [x] [acc] *ptr
                dup 11      // *start [x] *ptr [x] [acc] *ptr *start
                push {three_times_batch_size_plus_two} add
                            // *start [x] *ptr [x] [acc] *ptr *start+3batch+2
                push -1 mul add
                            // *start [x] *ptr [x] [acc] *ptr-2-*start-3batch
                split pop 1 // *start [x] *ptr [x] [acc] hi
                skiz return
                {&many_horner_iterations}
                recurse

            // INVARIANT: *start [x] *ptr [x] [acc]
            {loop_remainder}:
                dup 6       // *start [x] *ptr [x] [acc] *ptr
                dup 11      // *start [x] *ptr [x] [acc] *ptr *start
                eq          // *start [x] *ptr [x] [acc] *ptr==*start
                push 1 eq
                skiz return
                {&horner_iteration}
                recurse
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;
    use twenty_first::math::polynomial::Polynomial;

    use super::*;
    use crate::test_prelude::*;

    impl HornerEvaluationDynamicLength {
        fn prepare_state(
            &self,
            coefficients: Vec<XFieldElement>,
            coefficients_pointer: BFieldElement,
            indeterminate: XFieldElement,
        ) -> FunctionInitialState {
            let mut memory = HashMap::default();
            let mut stack = self.init_stack_for_isolated_run();
            encode_to_memory(&mut memory, coefficients_pointer, &coefficients);

            stack.push(coefficients_pointer);
            stack.push(indeterminate.coefficients[2]);
            stack.push(indeterminate.coefficients[1]);
            stack.push(indeterminate.coefficients[0]);

            FunctionInitialState { stack, memory }
        }
    }

    impl Function for HornerEvaluationDynamicLength {
        fn rust_shadow(
            &self,
            stack: &mut Vec<BFieldElement>,
            memory: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            let x0 = stack.pop().unwrap();
            let x1 = stack.pop().unwrap();
            let x2 = stack.pop().unwrap();
            let x = XFieldElement::new([x0, x1, x2]);

            let address = stack.pop().unwrap();
            let coefficients = *Vec::<XFieldElement>::decode_from_memory(memory, address).unwrap();
            let polynomial = Polynomial::new(coefficients);

            let value = polynomial.evaluate_in_same_field(x);
            stack.push(value.coefficients[2]);
            stack.push(value.coefficients[1]);
            stack.push(value.coefficients[0]);
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            bench_case: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            let mut rng = StdRng::from_seed(seed);
            let num_coefficients = match bench_case {
                Some(BenchmarkCase::CommonCase) => 256,
                Some(BenchmarkCase::WorstCase) => 512,
                None => rng.random_range(0..1000),
            };
            let coefficients = (0..num_coefficients)
                .map(|_| rng.random::<XFieldElement>())
                .collect_vec();

            let coefficients_pointer = bfe!(rng.random_range(0..(1u64 << 35)));

            let indeterminate = rng.random::<XFieldElement>();

            self.prepare_state(coefficients, coefficients_pointer, indeterminate)
        }

        fn corner_case_initial_states(&self) -> Vec<FunctionInitialState> {
            let an_indeterminate = xfe!([1u64 << 45, 47, 49]);
            let one_coefficient = self.prepare_state(xfe_vec![19991], bfe!(333), an_indeterminate);
            let two_coefficients =
                self.prepare_state(xfe_vec![19991, 299999992], bfe!(333), an_indeterminate);
            let three_coefficients = self.prepare_state(
                xfe_vec![19991, 299999992, 399999993],
                bfe!(333),
                an_indeterminate,
            );

            vec![one_coefficient, two_coefficients, three_coefficients]
        }
    }

    #[test]
    fn test() {
        for _ in 0..10 {
            ShadowedFunction::new(HornerEvaluationDynamicLength).test();
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedFunction::new(HornerEvaluationDynamicLength).bench();
    }
}
