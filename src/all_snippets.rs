use std::marker::PhantomData;

use crate::{
    arithmetic::{
        u32::{
            is_odd::U32IsOdd, is_u32::IsU32, safe_add::SafeAdd, safe_mul::SafeMul,
            safe_sub::SafeSub, shift_left::ShiftLeftU32, shift_right::ShiftRightU32,
        },
        u64::{
            add_u64::AddU64,
            and_u64::AndU64,
            decr_u64::DecrU64,
            div2_u64::Div2U64,
            double_pow2_u64::DoublePow2U64,
            eq_u64::EqU64,
            incr_u64::IncrU64,
            index_of_last_nonzero_bit::IndexOfLastNonZeroBitU64,
            log_2_floor_u64::Log2FloorU64,
            lt_u64::{LtStandardU64, LtU64},
            popcount_u64::PopCountU64,
            pow2_u64::Pow2U64,
            safe_mul_u64::SafeMulU64,
            shift_left_u64::ShiftLeftU64,
            shift_right_u64::ShiftRightU64,
            sub_u64::SubU64,
            wrapping_mul_u64::WrappingMulU64,
            xor_u64::XorU64,
        },
    },
    dyn_malloc::DynMalloc,
    hashing::{
        eq_digest::EqDigest,
        load_auth_path_from_secret_in_safe_list::LoadAuthPathFromSecretInSafeList,
        load_auth_path_from_secret_in_unsafe_list::LoadAuthPathFromSecretInUnsafeList,
        load_auth_path_from_std_in_safe_list::LoadAuthPathFromStdInSafeList,
        load_auth_path_from_std_in_unsafe_list::LoadAuthPathFromStdInUnsafeList,
        swap_digest::SwapDigest,
    },
    io::{read_secret::ReadSecret, read_stdin::ReadStdIn},
    library::{DummyTestSnippetA, DummyTestSnippetB, DummyTestSnippetC},
    list::{
        safe_u32::{
            get::SafeGet, length::SafeLength, new::SafeNew, pop::SafePop, push::SafePush,
            set::SafeSet, set_length::SafeSetLength,
        },
        unsafe_u32::{
            get::UnsafeGet,
            length::{UnsafeLengthLong, UnsafeLengthShort},
            new::UnsafeNew,
            pop::UnsafePop,
            push::UnsafePush,
            set::UnsafeSet,
            set_length::UnsafeSetLength,
        },
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
    VmHasher,
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
        "tasm_arithmetic_u32_shift_right_u32" => Box::new(ShiftRightU32),
        "tasm_arithmetic_u32_shift_left_u32" => Box::new(ShiftLeftU32),

        // u64
        "tasm_arithmetic_u64_add" => Box::new(AddU64),
        "tasm_arithmetic_u64_and" => Box::new(AndU64),
        "tasm_arithmetic_u64_xor" => Box::new(XorU64),
        "tasm_arithmetic_u64_decr" => Box::new(DecrU64),
        "tasm_arithmetic_u64_div2" => Box::new(Div2U64),
        "tasm_arithmetic_u64_eq" => Box::new(EqU64),
        "tasm_arithmetic_u64_incr" => Box::new(IncrU64),
        "tasm_arithmetic_u64_log_2_floor" => Box::new(Log2FloorU64),
        "tasm_arithmetic_u64_lt" => Box::new(LtU64),
        "tasm_arithmetic_u64_lt_standard" => Box::new(LtStandardU64),
        "tasm_arithmetic_u64_pow2" => Box::new(Pow2U64),
        "tasm_arithmetic_u64_sub" => Box::new(SubU64),
        "tasm_arithmetic_u64_index_of_last_nonzero_bit" => Box::new(IndexOfLastNonZeroBitU64),
        "tasm_arithmetic_pow2_double" => Box::new(DoublePow2U64),
        "tasm_arithmetic_u64_wrapping_mul" => Box::new(WrappingMulU64),
        "tasm_arithmetic_u64_safe_mul" => Box::new(SafeMulU64),
        "tasm_arithmetic_u64_popcount" => Box::new(PopCountU64),
        "tasm_arithmetic_u64_shift_right" => Box::new(ShiftRightU64),
        "tasm_arithmetic_u64_shift_left" => Box::new(ShiftLeftU64),

        // Hashing
        "tasm_hashing_eq_digest" => Box::new(EqDigest),
        "tasm_hashing_load_auth_path_from_secret_in_unsafe_list" => Box::new(LoadAuthPathFromSecretInUnsafeList),
        "tasm_hashing_load_auth_path_from_std_in_unsafe_list" => Box::new(LoadAuthPathFromStdInUnsafeList),
        "tasm_hashing_load_auth_path_from_secret_in_safe_list" => Box::new(LoadAuthPathFromSecretInSafeList),
        "tasm_hashing_load_auth_path_from_std_in_safe_list" => Box::new(LoadAuthPathFromStdInSafeList),
        "tasm_hashing_swap_digest" => Box::new(SwapDigest),

        // io
        "tasm_io_read_secret_bool" => Box::new(ReadSecret(DataType::Bool)),
        "tasm_io_read_secret_u32" => Box::new(ReadSecret(DataType::U32)),
        "tasm_io_read_secret_u64" => Box::new(ReadSecret(DataType::U64)),
        "tasm_io_read_secret_bfe" => Box::new(ReadSecret(DataType::BFE)),
        "tasm_io_read_secret_xfe" => Box::new(ReadSecret(DataType::XFE)),
        "tasm_io_read_secret_digest" => Box::new(ReadSecret(DataType::Digest)),

        "tasm_io_read_stdin_bool" => Box::new(ReadStdIn(DataType::Bool)),
        "tasm_io_read_stdin_u32" => Box::new(ReadStdIn(DataType::U32)),
        "tasm_io_read_stdin_u64" => Box::new(ReadStdIn(DataType::U64)),
        "tasm_io_read_stdin_bfe" => Box::new(ReadStdIn(DataType::BFE)),
        "tasm_io_read_stdin_xfe" => Box::new(ReadStdIn(DataType::XFE)),
        "tasm_io_read_stdin_digest" => Box::new(ReadStdIn(DataType::Digest)),

        // safe lists
        "tasm_list_safe_u32_get_element_bool" => Box::new(SafeGet(DataType::Bool)),
        "tasm_list_safe_u32_get_element_u32" => Box::new(SafeGet(DataType::U32)),
        "tasm_list_safe_u32_get_element_u64" => Box::new(SafeGet(DataType::U64)),
        "tasm_list_safe_u32_get_element_bfe" => Box::new(SafeGet(DataType::BFE)),
        "tasm_list_safe_u32_get_element_xfe" => Box::new(SafeGet(DataType::XFE)),
        "tasm_list_safe_u32_get_element_digest" => Box::new(SafeGet(DataType::Digest)),

        "tasm_list_safe_u32_pop_bool" => Box::new(SafePop(DataType::Bool)),
        "tasm_list_safe_u32_pop_u32" => Box::new(SafePop(DataType::U32)),
        "tasm_list_safe_u32_pop_u64" => Box::new(SafePop(DataType::U64)),
        "tasm_list_safe_u32_pop_bfe" => Box::new(SafePop(DataType::BFE)),
        "tasm_list_safe_u32_pop_xfe" => Box::new(SafePop(DataType::XFE)),
        "tasm_list_safe_u32_pop_digest" => Box::new(SafePop(DataType::Digest)),

        "tasm_list_safe_u32_push_bool" => Box::new(SafePush(DataType::Bool)),
        "tasm_list_safe_u32_push_u32" => Box::new(SafePush(DataType::U32)),
        "tasm_list_safe_u32_push_u64" => Box::new(SafePush(DataType::U64)),
        "tasm_list_safe_u32_push_bfe" => Box::new(SafePush(DataType::BFE)),
        "tasm_list_safe_u32_push_xfe" => Box::new(SafePush(DataType::XFE)),
        "tasm_list_safe_u32_push_digest" => Box::new(SafePush(DataType::Digest)),

        "tasm_list_safe_u32_set_element_bool" => Box::new(SafeSet(DataType::Bool)),
        "tasm_list_safe_u32_set_element_u32" => Box::new(SafeSet(DataType::U32)),
        "tasm_list_safe_u32_set_element_u64" => Box::new(SafeSet(DataType::U64)),
        "tasm_list_safe_u32_set_element_bfe" => Box::new(SafeSet(DataType::BFE)),
        "tasm_list_safe_u32_set_element_xfe" => Box::new(SafeSet(DataType::XFE)),
        "tasm_list_safe_u32_set_element_digest" => Box::new(SafeSet(DataType::Digest)),

        "tasm_list_safe_u32_new_bool" => Box::new(SafeNew(DataType::Bool)),
        "tasm_list_safe_u32_new_u32" => Box::new(SafeNew(DataType::U32)),
        "tasm_list_safe_u32_new_u64" => Box::new(SafeNew(DataType::U64)),
        "tasm_list_safe_u32_new_bfe" => Box::new(SafeNew(DataType::BFE)),
        "tasm_list_safe_u32_new_xfe" => Box::new(SafeNew(DataType::XFE)),
        "tasm_list_safe_u32_new_digest" => Box::new(SafeNew(DataType::Digest)),

        "tasm_list_safe_u32_length_bool" => Box::new(SafeLength(DataType::Bool)),
        "tasm_list_safe_u32_length_u32" => Box::new(SafeLength(DataType::U32)),
        "tasm_list_safe_u32_length_u64" => Box::new(SafeLength(DataType::U64)),
        "tasm_list_safe_u32_length_bfe" => Box::new(SafeLength(DataType::BFE)),
        "tasm_list_safe_u32_length_xfe" => Box::new(SafeLength(DataType::XFE)),
        "tasm_list_safe_u32_length_digest" => Box::new(SafeLength(DataType::Digest)),

        "tasm_list_safe_u32_set_length_bool" => Box::new(SafeSetLength(DataType::Bool)),
        "tasm_list_safe_u32_set_length_u32" => Box::new(SafeSetLength(DataType::U32)),
        "tasm_list_safe_u32_set_length_u64" => Box::new(SafeSetLength(DataType::U64)),
        "tasm_list_safe_u32_set_length_bfe" => Box::new(SafeSetLength(DataType::BFE)),
        "tasm_list_safe_u32_set_length_xfe" => Box::new(SafeSetLength(DataType::XFE)),
        "tasm_list_safe_u32_set_length_digest" => Box::new(SafeSetLength(DataType::Digest)),

        // unsafe lists
        "tasm_list_unsafe_u32_get_element_bool" => Box::new(UnsafeGet(DataType::Bool)),
        "tasm_list_unsafe_u32_get_element_u32" => Box::new(UnsafeGet(DataType::U32)),
        "tasm_list_unsafe_u32_get_element_u64" => Box::new(UnsafeGet(DataType::U64)),
        "tasm_list_unsafe_u32_get_element_bfe" => Box::new(UnsafeGet(DataType::BFE)),
        "tasm_list_unsafe_u32_get_element_xfe" => Box::new(UnsafeGet(DataType::XFE)),
        "tasm_list_unsafe_u32_get_element_digest" => Box::new(UnsafeGet(DataType::Digest)),

        "tasm_list_unsafe_u32_pop_bool" => Box::new(UnsafePop(DataType::Bool)),
        "tasm_list_unsafe_u32_pop_u32" => Box::new(UnsafePop(DataType::U32)),
        "tasm_list_unsafe_u32_pop_u64" => Box::new(UnsafePop(DataType::U64)),
        "tasm_list_unsafe_u32_pop_bfe" => Box::new(UnsafePop(DataType::BFE)),
        "tasm_list_unsafe_u32_pop_xfe" => Box::new(UnsafePop(DataType::XFE)),
        "tasm_list_unsafe_u32_pop_digest" => Box::new(UnsafePop(DataType::Digest)),

        "tasm_list_unsafe_u32_push_bool" => Box::new(UnsafePush(DataType::Bool)),
        "tasm_list_unsafe_u32_push_u32" => Box::new(UnsafePush(DataType::U32)),
        "tasm_list_unsafe_u32_push_u64" => Box::new(UnsafePush(DataType::U64)),
        "tasm_list_unsafe_u32_push_bfe" => Box::new(UnsafePush(DataType::BFE)),
        "tasm_list_unsafe_u32_push_xfe" => Box::new(UnsafePush(DataType::XFE)),
        "tasm_list_unsafe_u32_push_digest" => Box::new(UnsafePush(DataType::Digest)),

        "tasm_list_unsafe_u32_set_element_bool" => Box::new(UnsafeSet(DataType::Bool)),
        "tasm_list_unsafe_u32_set_element_u32" => Box::new(UnsafeSet(DataType::U32)),
        "tasm_list_unsafe_u32_set_element_u64" => Box::new(UnsafeSet(DataType::U64)),
        "tasm_list_unsafe_u32_set_element_bfe" => Box::new(UnsafeSet(DataType::BFE)),
        "tasm_list_unsafe_u32_set_element_xfe" => Box::new(UnsafeSet(DataType::XFE)),
        "tasm_list_unsafe_u32_set_element_digest" => Box::new(UnsafeSet(DataType::Digest)),

        "tasm_list_unsafe_u32_new_bool" => Box::new(UnsafeNew(DataType::Bool)),
        "tasm_list_unsafe_u32_new_u32" => Box::new(UnsafeNew(DataType::U32)),
        "tasm_list_unsafe_u32_new_u64" => Box::new(UnsafeNew(DataType::U64)),
        "tasm_list_unsafe_u32_new_bfe" => Box::new(UnsafeNew(DataType::BFE)),
        "tasm_list_unsafe_u32_new_xfe" => Box::new(UnsafeNew(DataType::XFE)),
        "tasm_list_unsafe_u32_new_digest" => Box::new(UnsafeNew(DataType::Digest)),

        "tasm_list_unsafe_u32_length_long_bool" => Box::new(UnsafeLengthLong(DataType::Bool)),
        "tasm_list_unsafe_u32_length_long_u32" => Box::new(UnsafeLengthLong(DataType::U32)),
        "tasm_list_unsafe_u32_length_long_u64" => Box::new(UnsafeLengthLong(DataType::U64)),
        "tasm_list_unsafe_u32_length_long_bfe" => Box::new(UnsafeLengthLong(DataType::BFE)),
        "tasm_list_unsafe_u32_length_long_xfe" => Box::new(UnsafeLengthLong(DataType::XFE)),
        "tasm_list_unsafe_u32_length_long_digest" => Box::new(UnsafeLengthLong(DataType::Digest)),

        "tasm_list_unsafe_u32_length_short_bool" => Box::new(UnsafeLengthShort(DataType::Bool)),
        "tasm_list_unsafe_u32_length_short_u32" => Box::new(UnsafeLengthShort(DataType::U32)),
        "tasm_list_unsafe_u32_length_short_u64" => Box::new(UnsafeLengthShort(DataType::U64)),
        "tasm_list_unsafe_u32_length_short_bfe" => Box::new(UnsafeLengthShort(DataType::BFE)),
        "tasm_list_unsafe_u32_length_short_xfe" => Box::new(UnsafeLengthShort(DataType::XFE)),
        "tasm_list_unsafe_u32_length_short_digest" => Box::new(UnsafeLengthShort(DataType::Digest)),

        "tasm_list_unsafe_u32_set_length_bool" => Box::new(UnsafeSetLength(DataType::Bool)),
        "tasm_list_unsafe_u32_set_length_u32" => Box::new(UnsafeSetLength(DataType::U32)),
        "tasm_list_unsafe_u32_set_length_u64" => Box::new(UnsafeSetLength(DataType::U64)),
        "tasm_list_unsafe_u32_set_length_bfe" => Box::new(UnsafeSetLength(DataType::BFE)),
        "tasm_list_unsafe_u32_set_length_xfe" => Box::new(UnsafeSetLength(DataType::XFE)),
        "tasm_list_unsafe_u32_set_length_digest" => Box::new(UnsafeSetLength(DataType::Digest)),

        // MMR
        "tasm_mmr_calculate_new_peaks_from_append" => Box::new(CalculateNewPeaksFromAppend(PhantomData::<VmHasher>)),
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices(PhantomData::<VmHasher>))
        }
        "tasm_mmr_data_index_to_node_index" => Box::new(DataIndexToNodeIndex),
        "tasm_mmr_get_height_from_leaf_index" => Box::new(GetHeightFromDataIndex),
        "tasm_mmr_leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "tasm_mmr_left_child" => Box::new(MmrLeftChild),
        "tasm_mmr_leftmost_ancestor" => Box::new(MmrLeftMostAncestor),
        "tasm_mmr_verify_load_from_secret_in" => Box::new(MmrLoadFromSecretInThenVerify(PhantomData::<VmHasher>)),
        "tasm_mmr_non_leaf_nodes_left" => Box::new(MmrNonLeafNodesLeftUsingAnd),
        "tasm_mmr_right_child_and_height" => Box::new(MmrRightChildAndHeight),
        "tasm_mmr_right_child" => Box::new(MmrRightChild),
        "tasm_mmr_right_lineage_count_and_own_height" => Box::new(MmrRightLineageCountAndHeight),
        "tasm_mmr_right_lineage_length" => Box::new(MmrRightLineageLength),
        "tasm_mmr_verify_from_memory" => Box::new(MmrVerifyFromMemory(PhantomData::<VmHasher>)),
        "tasm_mmr_verify_from_secret_in" => Box::new(MmrVerifyLeafMembershipFromSecretIn(PhantomData::<VmHasher>)),

        // other
        "tasm_other_bfe_add" => Box::new(BfeAdd),

        // pseudo
        "tasm_pseudo_lsb" => Box::new(Lsb),
        "tasm_pseudo_neg" => Box::new(Neg),
        "tasm_pseudo_sub" => Box::new(Sub),

        // recufy
        "tasm_recufier_mt_ap_verify" => Box::new(MtApVerifyFromSecretInput(PhantomData::<VmHasher>)),

        // dyn_malloc
        "dyn_malloc" => Box::new(DynMalloc),

        _ => panic!("Could not find \"{fn_name}\" in the function `name_to_snippet`. Did you include it there?"),
    }
}
