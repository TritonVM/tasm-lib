use ndarray::Array1;
use triton_vm::challenges::Challenges;
use triton_vm::constraints::dynamic_air_constraint_evaluation_tasm;
use triton_vm::constraints::static_air_constraint_evaluation_tasm;
use triton_vm::memory_layout::DynamicTasmConstraintEvaluationMemoryLayout;
use triton_vm::memory_layout::IntegralMemoryLayout;
use triton_vm::memory_layout::StaticTasmConstraintEvaluationMemoryLayout;
use triton_vm::prelude::*;
use triton_vm::table::auxiliary_table::Evaluable;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::master_table::MasterMainTable;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::ArrayType;
use crate::memory::dyn_malloc::DYN_MALLOC_ADDRESS;
use crate::prelude::*;
use crate::verifier::challenges::shared::conventional_challenges_pointer;

#[derive(Debug, Clone, Copy)]
pub enum MemoryLayout {
    Dynamic(DynamicTasmConstraintEvaluationMemoryLayout),
    Static(StaticTasmConstraintEvaluationMemoryLayout),
}

impl MemoryLayout {
    pub fn conventional_static() -> Self {
        const CURRENT_MAIN_ROW_PTR: u64 = 30u64;
        const MAIN_ROW_SIZE: u64 = (MasterMainTable::NUM_COLUMNS * EXTENSION_DEGREE) as u64;
        const AUX_ROW_SIZE: u64 = (MasterAuxTable::NUM_COLUMNS * EXTENSION_DEGREE) as u64;
        const METADATA_SIZE_PER_PROOF_ITEM_ELEMENT: u64 = 2; // 1 for discriminant, 1 for elem size
        let mem_layout = StaticTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new(((1u64 << 32) - 3) * (1u64 << 32)),
            curr_main_row_ptr: BFieldElement::new(CURRENT_MAIN_ROW_PTR),
            curr_aux_row_ptr: BFieldElement::new(
                CURRENT_MAIN_ROW_PTR + MAIN_ROW_SIZE + METADATA_SIZE_PER_PROOF_ITEM_ELEMENT,
            ),
            next_main_row_ptr: BFieldElement::new(
                CURRENT_MAIN_ROW_PTR
                    + MAIN_ROW_SIZE
                    + AUX_ROW_SIZE
                    + 2 * METADATA_SIZE_PER_PROOF_ITEM_ELEMENT,
            ),
            next_aux_row_ptr: BFieldElement::new(
                CURRENT_MAIN_ROW_PTR
                    + 2 * MAIN_ROW_SIZE
                    + AUX_ROW_SIZE
                    + 3 * METADATA_SIZE_PER_PROOF_ITEM_ELEMENT,
            ),
            challenges_ptr: conventional_challenges_pointer(),
        };
        assert!(mem_layout.is_integral());

        Self::Static(mem_layout)
    }

    /// Generate a memory layout that allows you to store the proof anywhere in
    /// memory.
    pub fn conventional_dynamic() -> Self {
        let mem_layout = DynamicTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new(((1u64 << 32) - 3) * (1u64 << 32)),
            challenges_ptr: conventional_challenges_pointer(),
        };
        assert!(mem_layout.is_integral());

        Self::Dynamic(mem_layout)
    }

    pub fn challenges_pointer(&self) -> BFieldElement {
        match self {
            MemoryLayout::Dynamic(dl) => dl.challenges_ptr,
            MemoryLayout::Static(sl) => sl.challenges_ptr,
        }
    }

    /// Check that the memory layout matches the convention of this standard-library
    pub fn is_integral(&self) -> bool {
        // A: cannot collide with `kmalloc`
        // B: cannot collide with `dyn_malloc` (must only use static memory (address.value() >= 2^{63}))
        // C: cannot collide with state of `dyn_malloc`
        // D: cannot have overlapping regions

        // Notice that it's allowed to point to ND memory, $[0, 2^{32})$.
        let memory_regions = match self {
            MemoryLayout::Dynamic(dl) => dl.memory_regions(),
            MemoryLayout::Static(sl) => sl.memory_regions(),
        };

        let kmalloc_region = Library::kmalloc_memory_region();
        let kmalloc_disjoint = memory_regions
            .iter()
            .all(|mr| mr.disjoint_from(&kmalloc_region));

        let dyn_malloc_region = DynMalloc::memory_region();
        let dyn_malloc_disjoint = memory_regions
            .iter()
            .all(|mr| mr.disjoint_from(&dyn_malloc_region));

        let dyn_malloc_state_disjoint = memory_regions
            .iter()
            .all(|mr| !mr.contains_address(DYN_MALLOC_ADDRESS));

        let internally_consistent = match self {
            MemoryLayout::Dynamic(dl) => dl.is_integral(),
            MemoryLayout::Static(sl) => sl.is_integral(),
        };

        kmalloc_disjoint
            && dyn_malloc_disjoint
            && dyn_malloc_state_disjoint
            && internally_consistent
    }

    pub fn label_friendly_name(&self) -> &str {
        match self {
            MemoryLayout::Dynamic(_) => "dynamic",
            MemoryLayout::Static(_) => "static",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AirConstraintEvaluation {
    pub memory_layout: MemoryLayout,
}

impl AirConstraintEvaluation {
    pub fn new_static(static_layout: StaticTasmConstraintEvaluationMemoryLayout) -> Self {
        Self {
            memory_layout: MemoryLayout::Static(static_layout),
        }
    }

    pub fn new_dynamic(static_layout: DynamicTasmConstraintEvaluationMemoryLayout) -> Self {
        Self {
            memory_layout: MemoryLayout::Dynamic(static_layout),
        }
    }

    pub fn with_conventional_static_memory_layout() -> Self {
        Self {
            memory_layout: MemoryLayout::conventional_static(),
        }
    }

    pub fn with_conventional_dynamic_memory_layout() -> Self {
        Self {
            memory_layout: MemoryLayout::conventional_dynamic(),
        }
    }

    pub fn output_type() -> DataType {
        DataType::Array(Box::new(ArrayType {
            element_type: DataType::Xfe,
            length: MasterAuxTable::NUM_CONSTRAINTS,
        }))
    }

    /// Return the concatenated AIR-constraint evaluation
    pub fn host_machine_air_constraint_evaluation(
        input_values: AirConstraintSnippetInputs,
    ) -> Vec<XFieldElement> {
        let current_main_row = Array1::from(input_values.current_main_row);
        let current_aux_row = Array1::from(input_values.current_aux_row);
        let next_main_row = Array1::from(input_values.next_main_row);
        let next_aux_row = Array1::from(input_values.next_aux_row);
        let evaluated_initial_constraints = MasterAuxTable::evaluate_initial_constraints(
            current_main_row.view(),
            current_aux_row.view(),
            &input_values.challenges,
        );
        let evaluated_consistency_constraints = MasterAuxTable::evaluate_consistency_constraints(
            current_main_row.view(),
            current_aux_row.view(),
            &input_values.challenges,
        );
        let evaluated_transition_constraints = MasterAuxTable::evaluate_transition_constraints(
            current_main_row.view(),
            current_aux_row.view(),
            next_main_row.view(),
            next_aux_row.view(),
            &input_values.challenges,
        );
        let evaluated_terminal_constraints = MasterAuxTable::evaluate_terminal_constraints(
            current_main_row.view(),
            current_aux_row.view(),
            &input_values.challenges,
        );

        [
            evaluated_initial_constraints,
            evaluated_consistency_constraints,
            evaluated_transition_constraints,
            evaluated_terminal_constraints,
        ]
        .concat()
    }
}

impl BasicSnippet for AirConstraintEvaluation {
    fn parameters(&self) -> Vec<(DataType, String)> {
        match self.memory_layout {
            MemoryLayout::Dynamic(_) => vec![
                (DataType::VoidPointer, "*curr_main_row".to_string()),
                (DataType::VoidPointer, "*curr_extrow".to_string()),
                (DataType::VoidPointer, "*next_main_row".to_string()),
                (DataType::VoidPointer, "*next_aux_row".to_string()),
            ],
            MemoryLayout::Static(_) => vec![],
        }
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(Self::output_type(), "evaluated_constraints".to_owned())]
    }

    fn entrypoint(&self) -> String {
        assert!(
            self.memory_layout.is_integral(),
            "Memory layout for input values for constraint evaluation must be integral"
        );

        // Consider parameterizing this entrypoint name if you need more than one instance.
        "tasmlib_verifier_master_table_air_constraint_evaluation".to_owned()
    }

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        assert!(
            self.memory_layout.is_integral(),
            "Memory layout for input values for constraint evaluation must be integral"
        );

        let snippet_body = match self.memory_layout {
            MemoryLayout::Dynamic(dynamic_layout) => {
                dynamic_air_constraint_evaluation_tasm(dynamic_layout)
            }
            MemoryLayout::Static(static_layout) => {
                static_air_constraint_evaluation_tasm(static_layout)
            }
        };

        let entrypoint = self.entrypoint();
        let mut code = triton_asm!(
            {entrypoint}:
        );
        code.extend(snippet_body);
        code.extend(triton_asm!(return));

        code
    }
}

/// Please notice that putting the proof into ND memory will *not* result in
/// memory that's compatible with this layout. So this layout will fail to
/// yield a functional STARK verifier program.
#[cfg(test)]
pub fn an_integral_but_profane_static_memory_layout() -> StaticTasmConstraintEvaluationMemoryLayout
{
    let mem_layout = StaticTasmConstraintEvaluationMemoryLayout {
        free_mem_page_ptr: BFieldElement::new((u32::MAX as u64 - 200) * (1u64 << 32)),
        curr_main_row_ptr: BFieldElement::new(1u64),
        curr_aux_row_ptr: BFieldElement::new(1u64 << 20),
        next_main_row_ptr: BFieldElement::new(1u64 << 21),
        next_aux_row_ptr: BFieldElement::new(1u64 << 22),
        challenges_ptr: BFieldElement::new(1u64 << 23),
    };
    assert!(mem_layout.is_integral());

    mem_layout
}

#[cfg(test)]
pub fn an_integral_but_profane_dynamic_memory_layout() -> DynamicTasmConstraintEvaluationMemoryLayout
{
    let mem_layout = DynamicTasmConstraintEvaluationMemoryLayout {
        free_mem_page_ptr: BFieldElement::new((u32::MAX as u64 - 100) * (1u64 << 32)),
        challenges_ptr: BFieldElement::new(1u64 << 30),
    };
    assert!(mem_layout.is_integral());

    mem_layout
}

#[derive(Debug, Clone)]
pub struct AirConstraintSnippetInputs {
    pub current_main_row: Vec<XFieldElement>,
    pub current_aux_row: Vec<XFieldElement>,
    pub next_main_row: Vec<XFieldElement>,
    pub next_aux_row: Vec<XFieldElement>,
    pub challenges: Challenges,
}

#[cfg(test)]
mod tests {
    use arbitrary::Arbitrary;
    use arbitrary::Unstructured;
    use num_traits::ConstZero;
    use rand::distr::StandardUniform;
    use triton_vm::proof_stream::ProofStream;
    use triton_vm::table::master_table::MasterMainTable;
    use twenty_first::math::x_field_element::EXTENSION_DEGREE;

    use super::*;
    use crate::execute_test;
    use crate::library::STATIC_MEMORY_LAST_ADDRESS;
    use crate::rust_shadowing_helper_functions::array::array_get;
    use crate::rust_shadowing_helper_functions::array::insert_as_array;
    use crate::structure::tasm_object::decode_from_memory_with_size;
    use crate::test_prelude::*;

    #[test]
    fn conventional_air_constraint_memory_layouts_are_integral() {
        assert!(MemoryLayout::conventional_static().is_integral());
        assert!(MemoryLayout::conventional_dynamic().is_integral());
    }

    #[test]
    fn disallow_using_kmalloc_region() {
        let mem_layout = MemoryLayout::Dynamic(DynamicTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: STATIC_MEMORY_LAST_ADDRESS,
            challenges_ptr: BFieldElement::new(1u64 << 30),
        });
        assert!(!mem_layout.is_integral());
    }

    #[test]
    fn disallow_using_dynmalloc_region() {
        let mem_layout = MemoryLayout::Dynamic(DynamicTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new(42 * (1u64 << 32)),
            challenges_ptr: BFieldElement::new(1u64 << 30),
        });
        assert!(!mem_layout.is_integral());
    }

    #[test]
    fn disallow_using_dynmalloc_state() {
        let mem_layout = MemoryLayout::Dynamic(DynamicTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new(42 * (1u64 << 32)),
            challenges_ptr: BFieldElement::new(BFieldElement::MAX),
        });
        assert!(!mem_layout.is_integral());
    }

    #[test]
    fn disallow_overlapping_regions() {
        let mut dyn_memory_layout = DynamicTasmConstraintEvaluationMemoryLayout {
            free_mem_page_ptr: BFieldElement::new((u32::MAX as u64 - 100) * (1u64 << 32)),
            challenges_ptr: conventional_challenges_pointer(),
        };
        assert!(MemoryLayout::Dynamic(dyn_memory_layout).is_integral());

        dyn_memory_layout.challenges_ptr = dyn_memory_layout.free_mem_page_ptr;
        assert!(!MemoryLayout::Dynamic(dyn_memory_layout).is_integral());
    }

    #[test]
    fn conventional_memory_layout_agrees_with_tvm_proof_stored_at_address_zero() {
        let program = triton_program!(halt);
        let claim = Claim::about_program(&program);

        let proof =
            triton_vm::prove(Stark::default(), &claim, program, NonDeterminism::default()).unwrap();

        const PROOF_ADDRESS: BFieldElement = BFieldElement::ZERO;
        let mut memory = HashMap::<BFieldElement, BFieldElement>::new();
        let proof_stream = ProofStream::try_from(&proof).unwrap();
        encode_to_memory(&mut memory, PROOF_ADDRESS, &proof);

        let assumed_memory_layout = MemoryLayout::conventional_static();
        let MemoryLayout::Static(assumed_memory_layout) = assumed_memory_layout else {
            panic!()
        };
        const MAIN_ROW_SIZE: usize = MasterMainTable::NUM_COLUMNS * EXTENSION_DEGREE;
        const AUX_ROW_SIZE: usize = MasterAuxTable::NUM_COLUMNS * EXTENSION_DEGREE;

        let assumed_curr_main_row: [XFieldElement; MasterMainTable::NUM_COLUMNS] =
            *decode_from_memory_with_size(
                &memory,
                assumed_memory_layout.curr_main_row_ptr,
                MAIN_ROW_SIZE,
            )
            .unwrap();
        let actual_curr_main_row_from_proof = proof_stream.items[4]
            .clone()
            .try_into_out_of_domain_main_row()
            .unwrap();
        assert_eq!(*actual_curr_main_row_from_proof, assumed_curr_main_row);

        let assumed_curr_aux_row: [XFieldElement; MasterAuxTable::NUM_COLUMNS] =
            *decode_from_memory_with_size(
                &memory,
                assumed_memory_layout.curr_aux_row_ptr,
                AUX_ROW_SIZE,
            )
            .unwrap();
        let actual_curr_aux_row_from_proof = proof_stream.items[5]
            .clone()
            .try_into_out_of_domain_aux_row()
            .unwrap();
        assert_eq!(*actual_curr_aux_row_from_proof, assumed_curr_aux_row);

        let assumed_next_main_row: [XFieldElement; MasterMainTable::NUM_COLUMNS] =
            *decode_from_memory_with_size(
                &memory,
                assumed_memory_layout.next_main_row_ptr,
                MAIN_ROW_SIZE,
            )
            .unwrap();
        let actual_next_main_row_from_proof = proof_stream.items[6]
            .clone()
            .try_into_out_of_domain_main_row()
            .unwrap();
        assert_eq!(*actual_next_main_row_from_proof, assumed_next_main_row);

        let assumed_next_aux_row: [XFieldElement; MasterAuxTable::NUM_COLUMNS] =
            *decode_from_memory_with_size(
                &memory,
                assumed_memory_layout.next_aux_row_ptr,
                AUX_ROW_SIZE,
            )
            .unwrap();
        let actual_next_aux_row_from_proof = proof_stream.items[7]
            .clone()
            .try_into_out_of_domain_aux_row()
            .unwrap();
        assert_eq!(*actual_next_aux_row_from_proof, assumed_next_aux_row);
    }

    impl Function for AirConstraintEvaluation {
        fn rust_shadow(
            &self,
            _: &mut Vec<BFieldElement>,
            _: &mut HashMap<BFieldElement, BFieldElement>,
        ) {
            // Never called as we do a more manual test.
            // The more manual test is done bc we don't want to
            // have to simulate all the intermediate calculations
            // that are stored to memory.
            unimplemented!()
        }

        fn pseudorandom_initial_state(
            &self,
            seed: [u8; 32],
            _: Option<BenchmarkCase>,
        ) -> FunctionInitialState {
            // Used for benchmarking
            let mut rng = StdRng::from_seed(seed);
            let input_values = Self::random_input_values(&mut rng);
            let (memory, stack) = self.prepare_tvm_memory_and_stack(input_values);

            FunctionInitialState { stack, memory }
        }
    }

    #[test]
    fn constraint_evaluation_test() {
        let static_snippet = AirConstraintEvaluation {
            memory_layout: MemoryLayout::Static(an_integral_but_profane_static_memory_layout()),
        };

        let dynamic_snippet = AirConstraintEvaluation {
            memory_layout: MemoryLayout::Dynamic(an_integral_but_profane_dynamic_memory_layout()),
        };

        let mut seed: [u8; 32] = [0u8; 32];
        rand::rng().fill_bytes(&mut seed);
        static_snippet.test_equivalence_with_host_machine_evaluation(seed);

        rand::rng().fill_bytes(&mut seed);
        dynamic_snippet.test_equivalence_with_host_machine_evaluation(seed);
    }

    impl AirConstraintEvaluation {
        fn test_equivalence_with_host_machine_evaluation(&self, seed: [u8; 32]) {
            let mut rng = StdRng::from_seed(seed);
            let input_values = Self::random_input_values(&mut rng);

            let (tasm_result, _) = self.tasm_result(input_values.clone());
            let host_machine_result = Self::host_machine_air_constraint_evaluation(input_values);

            assert_eq!(tasm_result.len(), host_machine_result.len());
            assert_eq!(
                tasm_result.iter().copied().sum::<XFieldElement>(),
                host_machine_result.iter().copied().sum::<XFieldElement>()
            );
            assert_eq!(tasm_result, host_machine_result);
        }

        pub(crate) fn random_input_values(rng: &mut StdRng) -> AirConstraintSnippetInputs {
            let current_main_row: Vec<XFieldElement> = rng
                .sample_iter(StandardUniform)
                .take(MasterMainTable::NUM_COLUMNS)
                .collect();
            let current_aux_row: Vec<XFieldElement> = rng
                .sample_iter(StandardUniform)
                .take(MasterAuxTable::NUM_COLUMNS)
                .collect();
            let next_main_row: Vec<XFieldElement> = rng
                .sample_iter(StandardUniform)
                .take(MasterMainTable::NUM_COLUMNS)
                .collect();
            let next_aux_row: Vec<XFieldElement> = rng
                .sample_iter(StandardUniform)
                .take(MasterAuxTable::NUM_COLUMNS)
                .collect();

            let mut ch_seed = [0u8; 12000];
            rng.fill_bytes(&mut ch_seed);
            let mut unstructured = Unstructured::new(&ch_seed);
            let challenges: Challenges = Challenges::arbitrary(&mut unstructured).unwrap();

            AirConstraintSnippetInputs {
                current_main_row,
                current_aux_row,
                next_main_row,
                next_aux_row,
                challenges,
            }
        }

        pub(crate) fn prepare_tvm_memory_and_stack(
            &self,
            input_values: AirConstraintSnippetInputs,
        ) -> (HashMap<BFieldElement, BFieldElement>, Vec<BFieldElement>) {
            match self.memory_layout {
                MemoryLayout::Static(static_layout) => {
                    let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
                    insert_as_array(
                        static_layout.curr_main_row_ptr,
                        &mut memory,
                        input_values.current_main_row,
                    );
                    insert_as_array(
                        static_layout.curr_aux_row_ptr,
                        &mut memory,
                        input_values.current_aux_row,
                    );
                    insert_as_array(
                        static_layout.next_main_row_ptr,
                        &mut memory,
                        input_values.next_main_row,
                    );
                    insert_as_array(
                        static_layout.next_aux_row_ptr,
                        &mut memory,
                        input_values.next_aux_row,
                    );
                    insert_as_array(
                        static_layout.challenges_ptr,
                        &mut memory,
                        input_values.challenges.challenges.to_vec(),
                    );

                    (memory, self.init_stack_for_isolated_run())
                }
                MemoryLayout::Dynamic(dynamic_layout) => {
                    let mut memory: HashMap<BFieldElement, BFieldElement> = HashMap::default();
                    let curr_main_row_ptr = dynamic_layout.challenges_ptr + bfe!(10000);
                    let curr_aux_row_ptr = curr_main_row_ptr
                        + bfe!((input_values.current_main_row.len() * EXTENSION_DEGREE + 1) as u64);
                    let next_main_row_ptr = curr_aux_row_ptr
                        + bfe!((input_values.current_aux_row.len() * EXTENSION_DEGREE + 2) as u64);
                    let next_aux_row_ptr = next_main_row_ptr
                        + bfe!((input_values.next_main_row.len() * EXTENSION_DEGREE + 3) as u64);

                    insert_as_array(
                        curr_main_row_ptr,
                        &mut memory,
                        input_values.current_main_row,
                    );
                    insert_as_array(curr_aux_row_ptr, &mut memory, input_values.current_aux_row);
                    insert_as_array(next_main_row_ptr, &mut memory, input_values.next_main_row);
                    insert_as_array(next_aux_row_ptr, &mut memory, input_values.next_aux_row);
                    insert_as_array(
                        dynamic_layout.challenges_ptr,
                        &mut memory,
                        input_values.challenges.challenges.to_vec(),
                    );

                    let mut stack = self.init_stack_for_isolated_run();
                    stack.push(curr_main_row_ptr);
                    stack.push(curr_aux_row_ptr);
                    stack.push(next_main_row_ptr);
                    stack.push(next_aux_row_ptr);

                    (memory, stack)
                }
            }
        }

        /// Return the pointed-to array and its address.
        /// Note that the result lives as an array in TVM memory but is represented as a list here
        /// since its length is not known at `tasm-lib`'s compile time.
        pub(crate) fn read_result_from_memory(
            mut final_state: VMState,
        ) -> (Vec<XFieldElement>, BFieldElement) {
            let result_pointer = final_state.op_stack.stack.pop().unwrap();
            let mut tasm_result: Vec<XFieldElement> = vec![];
            for i in 0..MasterAuxTable::NUM_CONSTRAINTS {
                tasm_result.push(XFieldElement::new(
                    array_get(result_pointer, i, &final_state.ram, EXTENSION_DEGREE)
                        .try_into()
                        .unwrap(),
                ));
            }

            (tasm_result, result_pointer)
        }

        /// Return evaluated constraints and their location in memory
        pub(crate) fn tasm_result(
            &self,
            input_values: AirConstraintSnippetInputs,
        ) -> (Vec<XFieldElement>, BFieldElement) {
            let (init_memory, stack) = self.prepare_tvm_memory_and_stack(input_values);

            let final_state = execute_test(
                &self.link_for_isolated_run(),
                &mut stack.clone(),
                self.stack_diff(),
                vec![],
                NonDeterminism::default().with_ram(init_memory),
                None,
            );

            Self::read_result_from_memory(final_state)
        }
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn bench_air_constraint_evaluation() {
        ShadowedFunction::new(AirConstraintEvaluation {
            memory_layout: MemoryLayout::Static(an_integral_but_profane_static_memory_layout()),
        })
        .bench();
    }
}
