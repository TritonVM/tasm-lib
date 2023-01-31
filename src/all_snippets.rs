use crate::{
    arithmetic::{
        u32::{is_odd::U32IsOdd, is_u32::IsU32, safe_add::SafeAdd, safe_sub::SafeSub},
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
    list::u32::{
        get::Get,
        length::{LengthLong, LengthShort},
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
    snippet::DataType,
};

pub fn name_to_snippet(fn_name: &str, element_type: Option<DataType>) -> AllSnippetsE {
    match fn_name {
        // u32
        "is_odd" => AllSnippetsE::U32IsOdd(U32IsOdd),
        "is_u32" => AllSnippetsE::IsU32(IsU32),
        "safe_add_u32" => AllSnippetsE::SafeAdd(SafeAdd),
        "safe_sub_u32" => AllSnippetsE::SafeSub(SafeSub),

        // u64
        "add_u64" => AllSnippetsE::AddU64(AddU64),
        "and_u64" => AllSnippetsE::AndU64(AndU64),
        "decr_u64" => AllSnippetsE::DecrU64(DecrU64),
        "div2_u64" => AllSnippetsE::Div2U64(Div2U64),
        "eq_u64" => AllSnippetsE::EqU64(EqU64),
        "incr_u64" => AllSnippetsE::IncrU64(IncrU64),
        "log_2_floor_u64" => AllSnippetsE::Log2FloorU64(Log2FloorU64),
        "lt_u64" => AllSnippetsE::LtU64(LtU64),
        "lt_standard_u64" => AllSnippetsE::LtStandardU64(LtStandardU64),
        "pow2_u64" => AllSnippetsE::Pow2U64(Pow2U64),
        "sub_u64" => AllSnippetsE::SubU64(SubU64),

        // Hashing
        "eq_digest" => AllSnippetsE::EqDigest(EqDigest),
        "load_auth_path_from_secret_in" => {
            AllSnippetsE::LoadAuthPathFromSecretIn(LoadAuthPathFromSecretIn)
        }
        "load_auth_path_from_std_in" => AllSnippetsE::LoadAuthPathFromStdIn(LoadAuthPathFromStdIn),
        "swap_digest" => AllSnippetsE::SwapDigest(SwapDigest),

        "list_get_element_1" => AllSnippetsE::Get1(Get::<1>(element_type.unwrap())),
        "list_get_element_2" => AllSnippetsE::Get2(Get::<2>(element_type.unwrap())),
        "list_get_element_3" => AllSnippetsE::Get3(Get::<3>(element_type.unwrap())),
        "list_get_element_5" => AllSnippetsE::Get5(Get::<5>(element_type.unwrap())),

        "pop_u32_1" => AllSnippetsE::Pop1(Pop::<1>(element_type.unwrap())),
        "pop_u32_2" => AllSnippetsE::Pop2(Pop::<2>(element_type.unwrap())),
        "pop_u32_3" => AllSnippetsE::Pop3(Pop::<3>(element_type.unwrap())),
        "pop_u32_5" => AllSnippetsE::Pop5(Pop::<5>(element_type.unwrap())),

        "push_u32_1" => AllSnippetsE::Push1(Push::<1>(element_type.unwrap())),
        "push_u32_2" => AllSnippetsE::Push2(Push::<2>(element_type.unwrap())),
        "push_u32_3" => AllSnippetsE::Push3(Push::<3>(element_type.unwrap())),
        "push_u32_4" => AllSnippetsE::Push5(Push::<5>(element_type.unwrap())),

        "list_set_element_1" => AllSnippetsE::Set1(Set::<1>(element_type.unwrap())),
        "list_set_element_2" => AllSnippetsE::Set2(Set::<2>(element_type.unwrap())),
        "list_set_element_3" => AllSnippetsE::Set3(Set::<3>(element_type.unwrap())),
        "list_set_element_5" => AllSnippetsE::Set5(Set::<5>(element_type.unwrap())),

        "list_u32_length_long" => AllSnippetsE::LengthLong(LengthLong(element_type.unwrap())),
        "list_u32_length_short" => AllSnippetsE::LengthShort(LengthShort(element_type.unwrap())),
        "list_u32_set_length" => AllSnippetsE::SetLength(SetLength(element_type.unwrap())),

        // MMR
        "calculate_new_peaks_from_append" => {
            AllSnippetsE::CalculateNewPeaksFromAppend(CalculateNewPeaksFromAppend)
        }
        "calculate_new_peaks_from_leaf_mutation" => {
            AllSnippetsE::MmrCalculateNewPeaksFromLeafMutationMtIndices(
                MmrCalculateNewPeaksFromLeafMutationMtIndices,
            )
        }
        "data_index_to_node_index" => AllSnippetsE::DataIndexToNodeIndex(DataIndexToNodeIndex),
        "get_height_from_leaf_index" => {
            AllSnippetsE::GetHeightFromDataIndex(GetHeightFromDataIndex)
        }
        "leaf_index_to_mt_index_and_peak_index" => {
            AllSnippetsE::MmrLeafIndexToMtIndexAndPeakIndex(MmrLeafIndexToMtIndexAndPeakIndex)
        }
        "mmr_left_child" => AllSnippetsE::MmrLeftChild(MmrLeftChild),
        "mmr_leftmost_ancestor" => AllSnippetsE::MmrLeftMostAncestor(MmrLeftMostAncestor),
        "verify_load_from_secret_in" => {
            AllSnippetsE::MmrLoadFromSecretInThenVerify(MmrLoadFromSecretInThenVerify)
        }
        "non_leaf_nodes_left" => {
            AllSnippetsE::MmrNonLeafNodesLeftUsingAnd(MmrNonLeafNodesLeftUsingAnd)
        }
        "right_child_and_height" => AllSnippetsE::MmrRightChildAndHeight(MmrRightChildAndHeight),
        "mmr_right_child" => AllSnippetsE::MmrRightChild(MmrRightChild),
        "right_lineage_count_and_own_height" => {
            AllSnippetsE::MmrRightLineageCountAndHeight(MmrRightLineageCountAndHeight)
        }
        "right_lineage_length" => AllSnippetsE::MmrRightLineageLength(MmrRightLineageLength),
        "verify_from_memory" => AllSnippetsE::MmrVerifyFromMemory(MmrVerifyFromMemory),
        "mmr_verify_from_secret_in" => {
            AllSnippetsE::MmrVerifyLeafMembershipFromSecretIn(MmrVerifyLeafMembershipFromSecretIn)
        }

        // other
        "bfe_add" => AllSnippetsE::BfeAdd(BfeAdd),

        // pseudo
        "lsb" => AllSnippetsE::Lsb(Lsb),
        "neg" => AllSnippetsE::Neg(Neg),
        "sub" => AllSnippetsE::Sub(Sub),

        // recufy
        "mt_ap_verify" => AllSnippetsE::MtApVerifyFromSecretInput(MtApVerifyFromSecretInput),

        _ => panic!("Not found"),
    }
}

pub enum AllSnippetsE {
    U32IsOdd(U32IsOdd),
    IsU32(IsU32),
    SafeAdd(SafeAdd),
    SafeSub(SafeSub),
    AddU64(AddU64),
    AndU64(AndU64),
    DecrU64(DecrU64),
    Div2U64(Div2U64),
    EqU64(EqU64),
    IncrU64(IncrU64),
    Log2FloorU64(Log2FloorU64),
    LtU64(LtU64),
    LtStandardU64(LtStandardU64),
    Pow2U64(Pow2U64),
    SubU64(SubU64),
    EqDigest(EqDigest),
    LoadAuthPathFromSecretIn(LoadAuthPathFromSecretIn),
    LoadAuthPathFromStdIn(LoadAuthPathFromStdIn),
    SwapDigest(SwapDigest),

    // For some list snippets we have to indicate element length in words
    Get1(Get<1>),
    Get2(Get<2>),
    Get3(Get<3>),
    Get5(Get<5>),

    Pop1(Pop<1>),
    Pop2(Pop<2>),
    Pop3(Pop<3>),
    Pop5(Pop<5>),

    Push1(Push<1>),
    Push2(Push<2>),
    Push3(Push<3>),
    Push5(Push<5>),

    Set1(Set<1>),
    Set2(Set<2>),
    Set3(Set<3>),
    Set5(Set<5>),

    LengthLong(LengthLong),
    LengthShort(LengthShort),
    SetLength(SetLength),

    /// MMR
    CalculateNewPeaksFromAppend(CalculateNewPeaksFromAppend),
    MmrCalculateNewPeaksFromLeafMutationMtIndices(MmrCalculateNewPeaksFromLeafMutationMtIndices),
    DataIndexToNodeIndex(DataIndexToNodeIndex),
    GetHeightFromDataIndex(GetHeightFromDataIndex),
    MmrLeafIndexToMtIndexAndPeakIndex(MmrLeafIndexToMtIndexAndPeakIndex),
    MmrLeftChild(MmrLeftChild),
    MmrLeftMostAncestor(MmrLeftMostAncestor),
    MmrLoadFromSecretInThenVerify(MmrLoadFromSecretInThenVerify),
    MmrNonLeafNodesLeftUsingAnd(MmrNonLeafNodesLeftUsingAnd),
    MmrRightChild(MmrRightChild),
    MmrRightChildAndHeight(MmrRightChildAndHeight),
    MmrRightLineageCountAndHeight(MmrRightLineageCountAndHeight),
    MmrRightLineageLength(MmrRightLineageLength),
    MmrVerifyFromMemory(MmrVerifyFromMemory),
    MmrVerifyLeafMembershipFromSecretIn(MmrVerifyLeafMembershipFromSecretIn),

    // Other snippets
    BfeAdd(BfeAdd),

    // Pseudo
    Lsb(Lsb),
    Neg(Neg),
    Sub(Sub),

    // Recufier
    MtApVerifyFromSecretInput(MtApVerifyFromSecretInput),
}
