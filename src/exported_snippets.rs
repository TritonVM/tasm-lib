use std::marker::PhantomData;

use crate::{
    arithmetic::{
        u128::{
            add_u128::AddU128, shift_left_static_u128::ShiftLeftStaticU128,
            shift_left_u128::ShiftLeftU128, shift_right_static_u128::ShiftRightStaticU128,
            shift_right_u128::ShiftRightU128, sub_u128::SubU128,
        },
        u32::{
            is_odd::U32IsOdd, is_u32::IsU32, leading_zeros_u32::LeadingZerosU32, or::OrU32,
            safe_add::SafeAdd, safe_mul::SafeMul, safe_sub::SafeSub, shift_left::ShiftLeftU32,
            shift_right::ShiftRightU32,
        },
        u64::{
            add_u64::AddU64,
            and_u64::AndU64,
            decr_u64::DecrU64,
            div2_u64::Div2U64,
            div_mod_u64::DivModU64,
            double_pow2_u64::DoublePow2U64,
            eq_u64::EqU64,
            incr_u64::IncrU64,
            index_of_last_nonzero_bit::IndexOfLastNonZeroBitU64,
            leading_zeros_u64::LeadingZerosU64,
            log_2_floor_u64::Log2FloorU64,
            lt_u64::{LtStandardU64, LtU64},
            or_u64::OrU64,
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
    hashing::{
        eq_digest::EqDigest, hash_varlen::HashVarlen,
        load_auth_path_from_secret_in_safe_list::LoadAuthPathFromSecretInSafeList,
        load_auth_path_from_secret_in_unsafe_list::LoadAuthPathFromSecretInUnsafeList,
        load_auth_path_from_std_in_safe_list::LoadAuthPathFromStdInSafeList,
        load_auth_path_from_std_in_unsafe_list::LoadAuthPathFromStdInUnsafeList,
        reverse_digest::ReverseDigest, sample_indices::SampleIndices, swap_digest::SwapDigest,
    },
    io::{load_from_input::LoadFromInput, read_secret::ReadSecret, read_stdin::ReadStdIn},
    list::{
        contiguous_list,
        range::Range,
        safe_u32::{
            get::SafeGet, length::SafeLength, new::SafeNew, pop::SafePop, push::SafePush,
            set::SafeSet, set_length::SafeSetLength,
        },
        unsafe_u32::{
            get::UnsafeGet, length::UnsafeLength, new::UnsafeNew, pop::UnsafePop, push::UnsafePush,
            set::UnsafeSet, set_length::UnsafeSetLength,
        },
        ListType,
    },
    memory::{dyn_malloc::DynMalloc, memcpy::MemCpy, push_ram_to_stack::PushRamToStack},
    mmr::{
        bag_peaks::BagPeaks, calculate_new_peaks_from_append::CalculateNewPeaksFromAppend,
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
    neptune::{
        mutator_set::{commit::Commit, get_swbf_indices::GetSwbfIndices},
        transaction::transaction_kernel_mast_hash::TransactionKernelMastHash,
    },
    other_snippets::bfe_add::BfeAdd,
    pseudo::{lsb::Lsb, neg::Neg, sub::Sub},
    recufier::merkle_tree_ap_verify_from_secret_input::MtApVerifyFromSecretInput,
    snippet::{DataType, InputSource, Snippet},
    structure::{get_field::GetField, get_field_with_size::GetFieldWithSize},
    VmHasher,
};

pub fn name_to_snippet(fn_name: &str) -> Box<dyn Snippet> {
    match fn_name {
        // u32
        "tasm_arithmetic_u32_is_odd" => Box::new(U32IsOdd),
        "tasm_arithmetic_u32_is_u32" => Box::new(IsU32),
        "tasm_arithmetic_u32_safe_add_u32" => Box::new(SafeAdd),
        "tasm_arithmetic_u32_safe_sub_u32" => Box::new(SafeSub),
        "tasm_arithmetic_u32_u32_safe_mul" => Box::new(SafeMul),
        "tasm_arithmetic_u32_shift_right_u32" => Box::new(ShiftRightU32),
        "tasm_arithmetic_u32_shift_left_u32" => Box::new(ShiftLeftU32),
        "tasm_arithmetic_u32_or_u32" => Box::new(OrU32),
        "tasm_arithmetic_u32_leading_zeros_u32" => Box::new(LeadingZerosU32),

        // u64
        "tasm_arithmetic_u64_add" => Box::new(AddU64),
        "tasm_arithmetic_u64_and" => Box::new(AndU64),
        "tasm_arithmetic_u64_xor" => Box::new(XorU64),
        "tasm_arithmetic_u64_or_u64" => Box::new(OrU64),
        "tasm_arithmetic_u64_decr" => Box::new(DecrU64),
        "tasm_arithmetic_u64_div2" => Box::new(Div2U64),
        "tasm_arithmetic_u64_div_mod" => Box::new(DivModU64),
        "tasm_arithmetic_u64_eq" => Box::new(EqU64),
        "tasm_arithmetic_u64_incr" => Box::new(IncrU64),
        "tasm_arithmetic_u64_log_2_floor" => Box::new(Log2FloorU64),
        "tasm_arithmetic_u64_lt" => Box::new(LtU64),
        "tasm_arithmetic_u64_lt_standard" => Box::new(LtStandardU64),
        "tasm_arithmetic_u64_pow2" => Box::new(Pow2U64),
        "tasm_arithmetic_u64_sub" => Box::new(SubU64),
        "tasm_arithmetic_u64_leading_zeros" => Box::new(LeadingZerosU64),
        "tasm_arithmetic_u64_index_of_last_nonzero_bit" => Box::new(IndexOfLastNonZeroBitU64),
        "tasm_arithmetic_u64_pow2_double" => Box::new(DoublePow2U64),
        "tasm_arithmetic_u64_wrapping_mul" => Box::new(WrappingMulU64),
        "tasm_arithmetic_u64_safe_mul" => Box::new(SafeMulU64),
        "tasm_arithmetic_u64_popcount" => Box::new(PopCountU64),
        "tasm_arithmetic_u64_shift_right" => Box::new(ShiftRightU64),
        "tasm_arithmetic_u64_shift_left" => Box::new(ShiftLeftU64),

        // u128
        "tasm_arithmetic_u128_add" => Box::new(AddU128),
        "tasm_arithmetic_u128_shift_left" => Box::new(ShiftLeftU128),
        "tasm_arithmetic_u128_shift_right" => Box::new(ShiftRightU128),
        "tasm_arithmetic_u128_sub" => Box::new(SubU128),

        "tasm_arithmetic_u128_shift_left_static_1" => Box::new(ShiftLeftStaticU128::<1>),
        "tasm_arithmetic_u128_shift_left_static_2" => Box::new(ShiftLeftStaticU128::<2>),
        "tasm_arithmetic_u128_shift_left_static_3" => Box::new(ShiftLeftStaticU128::<3>),
        "tasm_arithmetic_u128_shift_left_static_4" => Box::new(ShiftLeftStaticU128::<4>),
        "tasm_arithmetic_u128_shift_left_static_5" => Box::new(ShiftLeftStaticU128::<5>),
        "tasm_arithmetic_u128_shift_left_static_6" => Box::new(ShiftLeftStaticU128::<6>),
        "tasm_arithmetic_u128_shift_left_static_7" => Box::new(ShiftLeftStaticU128::<7>),
        "tasm_arithmetic_u128_shift_left_static_8" => Box::new(ShiftLeftStaticU128::<8>),
        "tasm_arithmetic_u128_shift_left_static_9" => Box::new(ShiftLeftStaticU128::<9>),
        "tasm_arithmetic_u128_shift_left_static_10" => Box::new(ShiftLeftStaticU128::<10>),
        "tasm_arithmetic_u128_shift_left_static_11" => Box::new(ShiftLeftStaticU128::<11>),
        "tasm_arithmetic_u128_shift_left_static_12" => Box::new(ShiftLeftStaticU128::<12>),
        "tasm_arithmetic_u128_shift_left_static_13" => Box::new(ShiftLeftStaticU128::<13>),
        "tasm_arithmetic_u128_shift_left_static_14" => Box::new(ShiftLeftStaticU128::<14>),
        "tasm_arithmetic_u128_shift_left_static_15" => Box::new(ShiftLeftStaticU128::<15>),
        "tasm_arithmetic_u128_shift_left_static_16" => Box::new(ShiftLeftStaticU128::<16>),
        "tasm_arithmetic_u128_shift_left_static_17" => Box::new(ShiftLeftStaticU128::<17>),
        "tasm_arithmetic_u128_shift_left_static_18" => Box::new(ShiftLeftStaticU128::<18>),
        "tasm_arithmetic_u128_shift_left_static_19" => Box::new(ShiftLeftStaticU128::<19>),
        "tasm_arithmetic_u128_shift_left_static_20" => Box::new(ShiftLeftStaticU128::<20>),
        "tasm_arithmetic_u128_shift_left_static_21" => Box::new(ShiftLeftStaticU128::<21>),
        "tasm_arithmetic_u128_shift_left_static_22" => Box::new(ShiftLeftStaticU128::<22>),
        "tasm_arithmetic_u128_shift_left_static_23" => Box::new(ShiftLeftStaticU128::<23>),
        "tasm_arithmetic_u128_shift_left_static_24" => Box::new(ShiftLeftStaticU128::<24>),
        "tasm_arithmetic_u128_shift_left_static_25" => Box::new(ShiftLeftStaticU128::<25>),
        "tasm_arithmetic_u128_shift_left_static_26" => Box::new(ShiftLeftStaticU128::<26>),
        "tasm_arithmetic_u128_shift_left_static_27" => Box::new(ShiftLeftStaticU128::<27>),
        "tasm_arithmetic_u128_shift_left_static_28" => Box::new(ShiftLeftStaticU128::<28>),
        "tasm_arithmetic_u128_shift_left_static_29" => Box::new(ShiftLeftStaticU128::<29>),
        "tasm_arithmetic_u128_shift_left_static_30" => Box::new(ShiftLeftStaticU128::<30>),
        "tasm_arithmetic_u128_shift_left_static_31" => Box::new(ShiftLeftStaticU128::<31>),
        "tasm_arithmetic_u128_shift_left_static_32" => Box::new(ShiftLeftStaticU128::<32>),

        "tasm_arithmetic_u128_shift_right_static_1" => Box::new(ShiftRightStaticU128::<1>),
        "tasm_arithmetic_u128_shift_right_static_2" => Box::new(ShiftRightStaticU128::<2>),
        "tasm_arithmetic_u128_shift_right_static_3" => Box::new(ShiftRightStaticU128::<3>),
        "tasm_arithmetic_u128_shift_right_static_4" => Box::new(ShiftRightStaticU128::<4>),
        "tasm_arithmetic_u128_shift_right_static_5" => Box::new(ShiftRightStaticU128::<5>),
        "tasm_arithmetic_u128_shift_right_static_6" => Box::new(ShiftRightStaticU128::<6>),
        "tasm_arithmetic_u128_shift_right_static_7" => Box::new(ShiftRightStaticU128::<7>),
        "tasm_arithmetic_u128_shift_right_static_8" => Box::new(ShiftRightStaticU128::<8>),
        "tasm_arithmetic_u128_shift_right_static_9" => Box::new(ShiftRightStaticU128::<9>),
        "tasm_arithmetic_u128_shift_right_static_10" => Box::new(ShiftRightStaticU128::<10>),
        "tasm_arithmetic_u128_shift_right_static_11" => Box::new(ShiftRightStaticU128::<11>),
        "tasm_arithmetic_u128_shift_right_static_12" => Box::new(ShiftRightStaticU128::<12>),
        "tasm_arithmetic_u128_shift_right_static_13" => Box::new(ShiftRightStaticU128::<13>),
        "tasm_arithmetic_u128_shift_right_static_14" => Box::new(ShiftRightStaticU128::<14>),
        "tasm_arithmetic_u128_shift_right_static_15" => Box::new(ShiftRightStaticU128::<15>),
        "tasm_arithmetic_u128_shift_right_static_16" => Box::new(ShiftRightStaticU128::<16>),
        "tasm_arithmetic_u128_shift_right_static_17" => Box::new(ShiftRightStaticU128::<17>),
        "tasm_arithmetic_u128_shift_right_static_18" => Box::new(ShiftRightStaticU128::<18>),
        "tasm_arithmetic_u128_shift_right_static_19" => Box::new(ShiftRightStaticU128::<19>),
        "tasm_arithmetic_u128_shift_right_static_20" => Box::new(ShiftRightStaticU128::<20>),
        "tasm_arithmetic_u128_shift_right_static_21" => Box::new(ShiftRightStaticU128::<21>),
        "tasm_arithmetic_u128_shift_right_static_22" => Box::new(ShiftRightStaticU128::<22>),
        "tasm_arithmetic_u128_shift_right_static_23" => Box::new(ShiftRightStaticU128::<23>),
        "tasm_arithmetic_u128_shift_right_static_24" => Box::new(ShiftRightStaticU128::<24>),
        "tasm_arithmetic_u128_shift_right_static_25" => Box::new(ShiftRightStaticU128::<25>),
        "tasm_arithmetic_u128_shift_right_static_26" => Box::new(ShiftRightStaticU128::<26>),
        "tasm_arithmetic_u128_shift_right_static_27" => Box::new(ShiftRightStaticU128::<27>),
        "tasm_arithmetic_u128_shift_right_static_28" => Box::new(ShiftRightStaticU128::<28>),
        "tasm_arithmetic_u128_shift_right_static_29" => Box::new(ShiftRightStaticU128::<29>),
        "tasm_arithmetic_u128_shift_right_static_30" => Box::new(ShiftRightStaticU128::<30>),
        "tasm_arithmetic_u128_shift_right_static_31" => Box::new(ShiftRightStaticU128::<31>),
        "tasm_arithmetic_u128_shift_right_static_32" => Box::new(ShiftRightStaticU128::<32>),

        // Hashing
        "tasm_hashing_eq_digest" => Box::new(EqDigest),
        "tasm_hashing_load_auth_path_from_secret_in_unsafe_list" => Box::new(LoadAuthPathFromSecretInUnsafeList),
        "tasm_hashing_load_auth_path_from_std_in_unsafe_list" => Box::new(LoadAuthPathFromStdInUnsafeList),
        "tasm_hashing_load_auth_path_from_secret_in_safe_list" => Box::new(LoadAuthPathFromSecretInSafeList),
        "tasm_hashing_load_auth_path_from_std_in_safe_list" => Box::new(LoadAuthPathFromStdInSafeList),
        "tasm_hashing_swap_digest" => Box::new(SwapDigest),
        "tasm_hashing_hash_varlen" => Box::new(HashVarlen),
        "tasm_hashing_sample_indices_to_safe_list" => Box::new(SampleIndices{list_type: ListType::Safe}),
        "tasm_hashing_sample_indices_to_unsafe_list" => Box::new(SampleIndices{list_type: ListType::Unsafe}),
        "tasm_hashing_reverse_digest" => Box::new(ReverseDigest),

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

        "tasm_io_load_from_input_stdin" => Box::new(LoadFromInput(InputSource::StdIn)),
        "tasm_io_load_from_input_secin" => Box::new(LoadFromInput(InputSource::SecretIn)),

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

        "tasm_list_safe_u32_multiset_equality" => Box::new(crate::list::multiset_equality::MultisetEquality(ListType::Safe)),

        "tasm_list_safe_range" => Box::new(Range{list_type: ListType::Safe}),

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

        "tasm_list_unsafe_u32_length_long_bool" => Box::new(UnsafeLength(DataType::Bool)),
        "tasm_list_unsafe_u32_length_long_u32" => Box::new(UnsafeLength(DataType::U32)),
        "tasm_list_unsafe_u32_length_long_u64" => Box::new(UnsafeLength(DataType::U64)),
        "tasm_list_unsafe_u32_length_long_bfe" => Box::new(UnsafeLength(DataType::BFE)),
        "tasm_list_unsafe_u32_length_long_xfe" => Box::new(UnsafeLength(DataType::XFE)),
        "tasm_list_unsafe_u32_length_long_digest" => Box::new(UnsafeLength(DataType::Digest)),

        "tasm_list_unsafe_u32_set_length_bool" => Box::new(UnsafeSetLength(DataType::Bool)),
        "tasm_list_unsafe_u32_set_length_u32" => Box::new(UnsafeSetLength(DataType::U32)),
        "tasm_list_unsafe_u32_set_length_u64" => Box::new(UnsafeSetLength(DataType::U64)),
        "tasm_list_unsafe_u32_set_length_bfe" => Box::new(UnsafeSetLength(DataType::BFE)),
        "tasm_list_unsafe_u32_set_length_xfe" => Box::new(UnsafeSetLength(DataType::XFE)),
        "tasm_list_unsafe_u32_set_length_digest" => Box::new(UnsafeSetLength(DataType::Digest)),

        "tasm_list_unsafe_u32_multiset_equality" => Box::new(crate::list::multiset_equality::MultisetEquality(ListType::Unsafe)),
        "tasm_list_unsafe_range" => Box::new(Range{list_type: ListType::Unsafe}),

        // Contiguous lists
        "tasm_list_contiguous_list_get_length" => Box::new(contiguous_list::get_length::GetLength),

        // MMR
        "tasm_mmr_calculate_new_peaks_from_append_unsafe" => Box::new(CalculateNewPeaksFromAppend { list_type: ListType::Unsafe }),
        "tasm_mmr_calculate_new_peaks_from_append_safe" => Box::new(CalculateNewPeaksFromAppend { list_type: ListType::Safe }),
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation_unsafe" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices{ list_type: ListType::Unsafe} )
        }
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation_safe" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices{ list_type: ListType::Safe} )
        }
        "tasm_mmr_data_index_to_node_index" => Box::new(DataIndexToNodeIndex),
        "tasm_mmr_get_height_from_leaf_index" => Box::new(GetHeightFromDataIndex),
        "tasm_mmr_leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "tasm_mmr_left_child" => Box::new(MmrLeftChild),
        "tasm_mmr_leftmost_ancestor" => Box::new(MmrLeftMostAncestor),
        "tasm_mmr_verify_load_from_secret_in_unsafe" => Box::new(MmrLoadFromSecretInThenVerify { list_type: ListType::Unsafe }),
        "tasm_mmr_verify_load_from_secret_in_safe" => Box::new(MmrLoadFromSecretInThenVerify { list_type: ListType::Safe }),
        "tasm_mmr_non_leaf_nodes_left" => Box::new(MmrNonLeafNodesLeftUsingAnd),
        "tasm_mmr_right_child_and_height" => Box::new(MmrRightChildAndHeight),
        "tasm_mmr_right_child" => Box::new(MmrRightChild),
        "tasm_mmr_right_lineage_count_and_own_height" => Box::new(MmrRightLineageCountAndHeight),
        "tasm_mmr_right_lineage_length" => Box::new(MmrRightLineageLength),
        "tasm_mmr_verify_from_memory_unsafe" => Box::new(MmrVerifyFromMemory { list_type: ListType::Unsafe} ),
        "tasm_mmr_verify_from_memory_safe" => Box::new(MmrVerifyFromMemory { list_type: ListType::Safe} ),
        "tasm_mmr_verify_from_secret_in_unsafe" => Box::new(MmrVerifyLeafMembershipFromSecretIn { list_type: ListType::Unsafe }),
        "tasm_mmr_verify_from_secret_in_safe" => Box::new(MmrVerifyLeafMembershipFromSecretIn { list_type: ListType::Safe }),
        "tasm_mmr_bag_peaks" => Box::new(BagPeaks),

        // other
        "tasm_other_bfe_add" => Box::new(BfeAdd),

        // pseudo
        "tasm_pseudo_lsb" => Box::new(Lsb),
        "tasm_pseudo_neg" => Box::new(Neg),
        "tasm_pseudo_sub" => Box::new(Sub),

        // recufy
        "tasm_recufier_mt_ap_verify" => Box::new(MtApVerifyFromSecretInput(PhantomData::<VmHasher>)),

        // memory
        "tasm_memory_dyn_malloc" => Box::new(DynMalloc),
        "tasm_memory_memcpy" => Box::new(MemCpy),

        "tasm_memory_push_ram_to_stack_digest" => Box::new(PushRamToStack {
            output_type: DataType::Digest,
        }),
        "tasm_memory_push_ram_to_stack_bool" => Box::new(PushRamToStack {
            output_type: DataType::Bool,
        }),
        "tasm_memory_push_ram_to_stack_u32" => Box::new(PushRamToStack {
            output_type: DataType::U32,
        }),
        "tasm_memory_push_ram_to_stack_u64" => Box::new(PushRamToStack {
            output_type: DataType::U64,
        }),
        "tasm_memory_push_ram_to_stack_u128" => Box::new(PushRamToStack {
            output_type: DataType::U128,
        }),
        "tasm_memory_push_ram_to_stack_void_pointer" => Box::new(PushRamToStack {
            output_type: DataType::VoidPointer,
        }),
        "tasm_memory_push_ram_to_stack_bfe" => Box::new(PushRamToStack {
            output_type: DataType::BFE,
        }),
        "tasm_memory_push_ram_to_stack_xfe" => Box::new(PushRamToStack {
            output_type: DataType::XFE,
        }),

        // structure
        "tasm_structure_get_field" => Box::new(GetField),
        "tasm_structure_get_field_with_size" => Box::new(GetFieldWithSize),

        // mutator sets
        "tasm_neptune_mutator_set_commit" => Box::new(Commit),
        "tasm_neptune_mutator_get_swbf_indices_1048576_45" => Box::new(GetSwbfIndices{ window_size: 1048576, num_trials: 45 }),

        // transaction
        "tasm_neptune_transaction_transaction_kernel_mast_hash" => Box::new(TransactionKernelMastHash),

        _ => panic!("Could not find \"{fn_name}\" in the function `exported_snippets`. Did you include it there?"),
    }
}
