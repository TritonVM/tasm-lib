use crate::{
    arithmetic::{
        u32::{
            is_odd::U32IsOdd, is_u32::IsU32, safe_add::SafeAdd, safe_mul::SafeMul,
            safe_sub::SafeSub,
        },
        u64::{
            add_u64::AddU64,
            and_u64::AndU64,
            decr_u64::DecrU64,
            div2_u64::Div2U64,
            eq_u64::EqU64,
            incr_u64::IncrU64,
            log_2_floor_u64::Log2FloorU64,
            lt_u64::{LtStandardU64, LtU64},
            pow2_u64::Pow2U64,
            sub_u64::SubU64,
        },
    },
    dyn_malloc::DynMalloc,
    hashing::{
        eq_digest::EqDigest, load_auth_path_from_secret_in::LoadAuthPathFromSecretIn,
        load_auth_path_from_std_in::LoadAuthPathFromStdIn, swap_digest::SwapDigest,
    },
    library::{DummyTestSnippetA, DummyTestSnippetB, DummyTestSnippetC},
    list::unsafe_u32::{
        get::Get,
        length::{LengthLong, LengthShort},
        new::New,
        pop::Pop,
        push::Push,
        set::Set,
        set_length::SetLength,
    },
    mmr::{
        calculate_new_peaks_from_append::CalculateNewPeaksFromAppend,
        calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices,
        data_index_to_node_index::DataIndexToNodeIndex,
        get_height_from_data_index::GetHeightFromDataIndex,
        leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex, left_child::MmrLeftChild,
        leftmost_ancestor::MmrLeftMostAncestor,
        load_from_secret_in_then_verify::MmrLoadFromSecretInThenVerify,
        non_leaf_nodes_left::MmrNonLeafNodesLeftUsingAnd, right_child::MmrRightChild,
        right_child_and_height::MmrRightChildAndHeight,
        right_lineage_count_and_own_height::MmrRightLineageCountAndHeight,
        right_lineage_length::MmrRightLineageLength, verify_from_memory::MmrVerifyFromMemory,
        verify_from_secret_in::MmrVerifyLeafMembershipFromSecretIn,
    },
    other_snippets::bfe_add::BfeAdd,
    pseudo::{lsb::Lsb, neg::Neg, sub::Sub},
    recufier::merkle_tree_ap_verify_from_secret_input::MtApVerifyFromSecretInput,
    snippet::{DataType, Snippet},
};

pub fn name_to_snippet(fn_name: &str) -> Box<dyn Snippet> {
    match fn_name {
        // dummy test values used for tests of `Library`
        "tasm_a_dummy_test_value" => Box::new(DummyTestSnippetA),
        "tasm_b_dummy_test_value" => Box::new(DummyTestSnippetB),
        "tasm_c_dummy_test_value" => Box::new(DummyTestSnippetC),

        // u32
        "tasm_arithmetic_u32_is_odd" => Box::new(U32IsOdd),
        "tasm_arithmetic_u32_is_u32" => Box::new(IsU32),
        "tasm_arithmetic_u32_safe_add_u32" => Box::new(SafeAdd),
        "tasm_arithmetic_u32_safe_sub_u32" => Box::new(SafeSub),
        "tasm_arithmetic_u32_u32_safe_mul" => Box::new(SafeMul),

        // u64
        "tasm_arithmetic_u64_add" => Box::new(AddU64),
        "tasm_arithmetic_u64_and" => Box::new(AndU64),
        "tasm_arithmetic_u64_decr" => Box::new(DecrU64),
        "tasm_arithmetic_u64_div2" => Box::new(Div2U64),
        "tasm_arithmetic_u64_eq" => Box::new(EqU64),
        "tasm_arithmetic_u64_incr" => Box::new(IncrU64),
        "tasm_arithmetic_u64_log_2_floor" => Box::new(Log2FloorU64),
        "tasm_arithmetic_u64_lt" => Box::new(LtU64),
        "tasm_arithmetic_u64_lt_standard" => Box::new(LtStandardU64),
        "tasm_arithmetic_u64_pow2" => Box::new(Pow2U64),
        "tasm_arithmetic_u64_sub" => Box::new(SubU64),

        // Hashing
        "tasm_hashing_eq_digest" => Box::new(EqDigest),
        "tasm_hashing_load_auth_path_from_secret_in" => Box::new(LoadAuthPathFromSecretIn),
        "tasm_hashing_load_auth_path_from_std_in" => Box::new(LoadAuthPathFromStdIn),
        "tasm_hashing_swap_digest" => Box::new(SwapDigest),

        // unsafe lists
        "tasm_list_unsafe_u32_get_element_bool" => Box::new(Get(DataType::Bool)),
        "tasm_list_unsafe_u32_get_element_u32" => Box::new(Get(DataType::U32)),
        "tasm_list_unsafe_u32_get_element_u64" => Box::new(Get(DataType::U64)),
        "tasm_list_unsafe_u32_get_element_bfe" => Box::new(Get(DataType::BFE)),
        "tasm_list_unsafe_u32_get_element_xfe" => Box::new(Get(DataType::XFE)),
        "tasm_list_unsafe_u32_get_element_digest" => Box::new(Get(DataType::Digest)),

        "tasm_list_unsafe_u32_pop_bool" => Box::new(Pop(DataType::Bool)),
        "tasm_list_unsafe_u32_pop_u32" => Box::new(Pop(DataType::U32)),
        "tasm_list_unsafe_u32_pop_u64" => Box::new(Pop(DataType::U64)),
        "tasm_list_unsafe_u32_pop_bfe" => Box::new(Pop(DataType::BFE)),
        "tasm_list_unsafe_u32_pop_xfe" => Box::new(Pop(DataType::XFE)),
        "tasm_list_unsafe_u32_pop_digest" => Box::new(Pop(DataType::Digest)),

        "tasm_list_unsafe_u32_push_bool" => Box::new(Push(DataType::Bool)),
        "tasm_list_unsafe_u32_push_u32" => Box::new(Push(DataType::U32)),
        "tasm_list_unsafe_u32_push_u64" => Box::new(Push(DataType::U64)),
        "tasm_list_unsafe_u32_push_bfe" => Box::new(Push(DataType::BFE)),
        "tasm_list_unsafe_u32_push_xfe" => Box::new(Push(DataType::XFE)),
        "tasm_list_unsafe_u32_push_digest" => Box::new(Push(DataType::Digest)),

        "tasm_list_unsafe_u32_set_element_bool" => Box::new(Set(DataType::Bool)),
        "tasm_list_unsafe_u32_set_element_u32" => Box::new(Set(DataType::U32)),
        "tasm_list_unsafe_u32_set_element_u64" => Box::new(Set(DataType::U64)),
        "tasm_list_unsafe_u32_set_element_bfe" => Box::new(Set(DataType::BFE)),
        "tasm_list_unsafe_u32_set_element_xfe" => Box::new(Set(DataType::XFE)),
        "tasm_list_unsafe_u32_set_element_digest" => Box::new(Set(DataType::Digest)),

        "tasm_list_unsafe_u32_new_bool" => Box::new(New(DataType::Bool)),
        "tasm_list_unsafe_u32_new_u32" => Box::new(New(DataType::U32)),
        "tasm_list_unsafe_u32_new_u64" => Box::new(New(DataType::U64)),
        "tasm_list_unsafe_u32_new_bfe" => Box::new(New(DataType::BFE)),
        "tasm_list_unsafe_u32_new_xfe" => Box::new(New(DataType::XFE)),
        "tasm_list_unsafe_u32_new_digest" => Box::new(New(DataType::Digest)),

        "tasm_list_unsafe_u32_length_long_bool" => Box::new(LengthLong(DataType::Bool)),
        "tasm_list_unsafe_u32_length_long_u32" => Box::new(LengthLong(DataType::U32)),
        "tasm_list_unsafe_u32_length_long_u64" => Box::new(LengthLong(DataType::U64)),
        "tasm_list_unsafe_u32_length_long_bfe" => Box::new(LengthLong(DataType::BFE)),
        "tasm_list_unsafe_u32_length_long_xfe" => Box::new(LengthLong(DataType::XFE)),
        "tasm_list_unsafe_u32_length_long_digest" => Box::new(LengthLong(DataType::Digest)),

        "tasm_list_unsafe_u32_length_short_bool" => Box::new(LengthShort(DataType::Bool)),
        "tasm_list_unsafe_u32_length_short_u32" => Box::new(LengthShort(DataType::U32)),
        "tasm_list_unsafe_u32_length_short_u64" => Box::new(LengthShort(DataType::U64)),
        "tasm_list_unsafe_u32_length_short_bfe" => Box::new(LengthShort(DataType::BFE)),
        "tasm_list_unsafe_u32_length_short_xfe" => Box::new(LengthShort(DataType::XFE)),
        "tasm_list_unsafe_u32_length_short_digest" => Box::new(LengthShort(DataType::Digest)),

        "tasm_list_unsafe_u32_set_length_bool" => Box::new(SetLength(DataType::Bool)),
        "tasm_list_unsafe_u32_set_length_u32" => Box::new(SetLength(DataType::U32)),
        "tasm_list_unsafe_u32_set_length_u64" => Box::new(SetLength(DataType::U64)),
        "tasm_list_unsafe_u32_set_length_bfe" => Box::new(SetLength(DataType::BFE)),
        "tasm_list_unsafe_u32_set_length_xfe" => Box::new(SetLength(DataType::XFE)),
        "tasm_list_unsafe_u32_set_length_digest" => Box::new(SetLength(DataType::Digest)),
        // "tasm_list_unsafe_u32_get_element" => match element_type {
        //     Some(et) => {
        //         assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
        //         Box::new(Get(et))},
        //     None => panic!("Must have element type for list"),
        // }

        // "tasm_list_unsafe_u32_pop" => match element_type {
        //     Some(et) => {
        //         assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
        //         Box::new(Pop(et))
        //     },
        //     None => panic!("Must have element type for list"),
        // }

        // "tasm_list_unsafe_u32_push" => match element_type {
        //     Some(et) => {
        //         assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
        //         Box::new(Push(et))
        //     },
        //     None => panic!("Must have element type for list"),
        // }

        // "tasm_list_unsafe_u32_set_element" => match element_type {
        //     Some(et) => {
        //         assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
        //         Box::new(Set(et))
        //     },
        //     None => panic!("Must have element type for list"),
        // }

        // "tasm_list_unsafe_u32_new" => match element_type {
        //     Some(et) => {
        //         assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
        //         Box::new(New(et))
        //     },
        //     None => panic!("Must have element type for list"),
        // }
        // "tasm_list_unsafe_u32_length_long" => Box::new(LengthLong(element_type.unwrap())),
        // "tasm_list_unsafe_u32_length_short" => Box::new(LengthShort(element_type.unwrap())),
        // "tasm_list_unsafe_u32_set_length" => Box::new(SetLength(element_type.unwrap())),

        // MMR
        "tasm_mmr_calculate_new_peaks_from_append" => Box::new(CalculateNewPeaksFromAppend),
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices)
        }
        "tasm_mmr_data_index_to_node_index" => Box::new(DataIndexToNodeIndex),
        "tasm_mmr_get_height_from_leaf_index" => Box::new(GetHeightFromDataIndex),
        "tasm_mmr_leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "tasm_mmr_left_child" => Box::new(MmrLeftChild),
        "tasm_mmr_leftmost_ancestor" => Box::new(MmrLeftMostAncestor),
        "tasm_mmr_verify_load_from_secret_in" => Box::new(MmrLoadFromSecretInThenVerify),
        "tasm_mmr_non_leaf_nodes_left" => Box::new(MmrNonLeafNodesLeftUsingAnd),
        "tasm_mmr_right_child_and_height" => Box::new(MmrRightChildAndHeight),
        "tasm_mmr_right_child" => Box::new(MmrRightChild),
        "tasm_mmr_right_lineage_count_and_own_height" => Box::new(MmrRightLineageCountAndHeight),
        "tasm_mmr_right_lineage_length" => Box::new(MmrRightLineageLength),
        "tasm_mmr_verify_from_memory" => Box::new(MmrVerifyFromMemory),
        "tasm_mmr_verify_from_secret_in" => Box::new(MmrVerifyLeafMembershipFromSecretIn),

        // other
        "tasm_other_bfe_add" => Box::new(BfeAdd),

        // pseudo
        "tasm_pseudo_lsb" => Box::new(Lsb),
        "tasm_pseudo_neg" => Box::new(Neg),
        "tasm_pseudo_sub" => Box::new(Sub),

        // recufy
        "tasm_recufier_mt_ap_verify" => Box::new(MtApVerifyFromSecretInput),

        // dyn_malloc
        "dyn_malloc" => Box::new(DynMalloc),

        _ => panic!("Could not find \"{fn_name}\" in the function `name_to_snippet`. Did you include it there?"),
    }
}
