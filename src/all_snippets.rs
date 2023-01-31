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
};

pub enum AllSnippets {
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
