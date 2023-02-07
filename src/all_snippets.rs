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

pub fn name_to_snippet(fn_name: &str, element_type: Option<DataType>) -> Box<dyn Snippet> {
    match fn_name {
        // dummy test values used for tests of `Library`
        "a_dummy_test_value" => Box::new(DummyTestSnippetA),
        "b_dummy_test_value" => Box::new(DummyTestSnippetB),
        "c_dummy_test_value" => Box::new(DummyTestSnippetC),

        // u32
        "is_odd" => Box::new(U32IsOdd),
        "is_u32" => Box::new(IsU32),
        "safe_add_u32" => Box::new(SafeAdd),
        "safe_sub_u32" => Box::new(SafeSub),
        "u32_safe_mul" => Box::new(SafeMul),

        // u64
        "add_u64" => Box::new(AddU64),
        "and_u64" => Box::new(AndU64),
        "decr_u64" => Box::new(DecrU64),
        "div2_u64" => Box::new(Div2U64),
        "eq_u64" => Box::new(EqU64),
        "incr_u64" => Box::new(IncrU64),
        "log_2_floor_u64" => Box::new(Log2FloorU64),
        "lt_u64" => Box::new(LtU64),
        "lt_standard_u64" => Box::new(LtStandardU64),
        "pow2_u64" => Box::new(Pow2U64),
        "sub_u64" => Box::new(SubU64),

        // Hashing
        "eq_digest" => Box::new(EqDigest),
        "load_auth_path_from_secret_in" => Box::new(LoadAuthPathFromSecretIn),
        "load_auth_path_from_std_in" => Box::new(LoadAuthPathFromStdIn),
        "swap_digest" => Box::new(SwapDigest),

        // unsafe lists
        // TODO: Special-case on name, cf. #18
        "list_get_element" => match element_type {
            Some(et) => {
                assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
                Box::new(Get(et))},
            None => panic!("Must have element type for list"),
        }

        "pop_u32" => match element_type {
            Some(et) => {
                assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
                Box::new(Pop(et))
            },
            None => panic!("Must have element type for list"),
        }

        "push_u32" => match element_type {
            Some(et) => {
                assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
                Box::new(Push(et))
            },
            None => panic!("Must have element type for list"),
        }

        "list_set_element" => match element_type {
            Some(et) => {
                assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
                Box::new(Set(et))
            },
            None => panic!("Must have element type for list"),
        }

        "tasm_lib_list_unsafe_u32_new" => match element_type {
            Some(et) => {
                assert!(!matches!(et, DataType::List(_)), "Nested lists not allowed");
                Box::new(New(et))
            },
            None => panic!("Must have element type for list"),
        }
        "list_u32_length_long" => Box::new(LengthLong(element_type.unwrap())),
        "list_u32_length_short" => Box::new(LengthShort(element_type.unwrap())),
        "list_u32_set_length" => Box::new(SetLength(element_type.unwrap())),

        // MMR
        "calculate_new_peaks_from_append" => Box::new(CalculateNewPeaksFromAppend),
        "calculate_new_peaks_from_leaf_mutation" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices)
        }
        "data_index_to_node_index" => Box::new(DataIndexToNodeIndex),
        "get_height_from_leaf_index" => Box::new(GetHeightFromDataIndex),
        "leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "mmr_left_child" => Box::new(MmrLeftChild),
        "mmr_leftmost_ancestor" => Box::new(MmrLeftMostAncestor),
        "verify_load_from_secret_in" => Box::new(MmrLoadFromSecretInThenVerify),
        "non_leaf_nodes_left" => Box::new(MmrNonLeafNodesLeftUsingAnd),
        "right_child_and_height" => Box::new(MmrRightChildAndHeight),
        "mmr_right_child" => Box::new(MmrRightChild),
        "right_lineage_count_and_own_height" => Box::new(MmrRightLineageCountAndHeight),
        "right_lineage_length" => Box::new(MmrRightLineageLength),
        "verify_from_memory" => Box::new(MmrVerifyFromMemory),
        "mmr_verify_from_secret_in" => Box::new(MmrVerifyLeafMembershipFromSecretIn),

        // other
        "bfe_add" => Box::new(BfeAdd),

        // pseudo
        "lsb" => Box::new(Lsb),
        "neg" => Box::new(Neg),
        "sub" => Box::new(Sub),

        // recufy
        "mt_ap_verify" => Box::new(MtApVerifyFromSecretInput),

        _ => panic!("Could not find \"{fn_name}\" in the function `name_to_snippet`. Did you include it there?"),
    }
}
