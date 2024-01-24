use crate::arithmetic::u128::add_u128::AddU128;
use crate::arithmetic::u128::safe_mul_u128::SafeMulU128;
use crate::arithmetic::u128::shift_left_static_u128::ShiftLeftStaticU128;
use crate::arithmetic::u128::shift_left_u128::ShiftLeftU128;
use crate::arithmetic::u128::shift_right_static_u128::ShiftRightStaticU128;
use crate::arithmetic::u128::shift_right_u128::ShiftRightU128;
use crate::arithmetic::u128::sub_u128::SubU128;
use crate::arithmetic::u32::isodd::Isodd;
use crate::arithmetic::u32::isu32::Isu32;
use crate::arithmetic::u32::leadingzeros::Leadingzeros;
use crate::arithmetic::u32::or::Or;
use crate::arithmetic::u32::overflowingadd::Overflowingadd;
use crate::arithmetic::u32::safeadd::Safeadd;
use crate::arithmetic::u32::safemul::Safemul;
use crate::arithmetic::u32::safepow::Safepow;
use crate::arithmetic::u32::safesub::Safesub;
use crate::arithmetic::u32::shiftleft::Shiftleft;
use crate::arithmetic::u32::shiftright::Shiftright;
use crate::arithmetic::u64::add_u64::AddU64;
use crate::arithmetic::u64::and_u64::AndU64;
use crate::arithmetic::u64::decr_u64::DecrU64;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::div_mod_u64::DivModU64;
use crate::arithmetic::u64::double_pow2_u64::DoublePow2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::arithmetic::u64::incr_u64::IncrU64;
use crate::arithmetic::u64::index_of_last_nonzero_bit::IndexOfLastNonZeroBitU64;
use crate::arithmetic::u64::leading_zeros_u64::LeadingZerosU64;
use crate::arithmetic::u64::log_2_floor_u64::Log2FloorU64;
use crate::arithmetic::u64::lt_u64::LtStandardU64;
use crate::arithmetic::u64::lt_u64::LtU64;
use crate::arithmetic::u64::mul_two_u64s_to_u128_u64::MulTwoU64sToU128;
use crate::arithmetic::u64::or_u64::OrU64;
use crate::arithmetic::u64::overflowing_sub_u64::OverflowingSub;
use crate::arithmetic::u64::popcount_u64::PopCountU64;
use crate::arithmetic::u64::pow2_u64::Pow2U64;
use crate::arithmetic::u64::safe_mul_u64::SafeMulU64;
use crate::arithmetic::u64::shift_left_u64::ShiftLeftU64;
use crate::arithmetic::u64::shift_right_u64::ShiftRightU64;
use crate::arithmetic::u64::sub_u64::SubU64;
use crate::arithmetic::u64::wrapping_mul_u64::WrappingMulU64;
use crate::arithmetic::u64::wrapping_sub_u64::WrappingSub;
use crate::arithmetic::u64::xor_u64::XorU64;
use crate::data_type::DataType;
use crate::hashing::eq_digest::EqDigest;
use crate::hashing::hash_varlen::HashVarlen;
use crate::hashing::reverse_digest::ReverseDigest;
use crate::hashing::sample_indices::SampleIndices;
use crate::hashing::swap_digest::SwapDigest;
use crate::io::read_input::ReadInput;
use crate::io::write_to_stdout::WriteToStdout;
use crate::io::InputSource;
use crate::list::contiguous_list;
use crate::list::range::Range;
use crate::list::safeimplu32::get::SafeGet;
use crate::list::safeimplu32::length::Length as SafeLength;
use crate::list::safeimplu32::new::SafeNew;
use crate::list::safeimplu32::pop::SafePop;
use crate::list::safeimplu32::push::SafePush;
use crate::list::safeimplu32::set::SafeSet;
use crate::list::safeimplu32::set_length::SafeSetLength;
use crate::list::unsafeimplu32::get::UnsafeGet;
use crate::list::unsafeimplu32::length::Length as UnsafeLength;
use crate::list::unsafeimplu32::new::UnsafeNew;
use crate::list::unsafeimplu32::pop::UnsafePop;
use crate::list::unsafeimplu32::push::UnsafePush;
use crate::list::unsafeimplu32::set::UnsafeSet;
use crate::list::unsafeimplu32::set_length::UnsafeSetLength;
use crate::list::ListType;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::memcpy::MemCpy;
use crate::mmr::bag_peaks::BagPeaks;
use crate::mmr::calculate_new_peaks_from_append::CalculateNewPeaksFromAppend;
use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
use crate::mmr::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::mmr::verify_from_memory::MmrVerifyFromMemory;
use crate::mmr::verify_from_secret_in::MmrVerifyLeafMembershipFromSecretIn;
use crate::neptune::mutator_set::commit::Commit;
use crate::neptune::mutator_set::get_swbf_indices::GetSwbfIndices;
use crate::other_snippets::bfe_add::BfeAdd;
use crate::recufier::merkle_verify::MerkleVerify;
use crate::recufier::proof_stream::dequeue::Dequeue;
use crate::traits::basic_snippet::BasicSnippet;

pub fn name_to_snippet(fn_name: &str) -> Box<dyn BasicSnippet> {
    match fn_name {
        // u32
        "tasm_arithmetic_u32_isodd" => Box::new(Isodd),
        "tasm_arithmetic_u32_isu32" => Box::new(Isu32),
        "tasm_arithmetic_u32_safeadd" => Box::new(Safeadd),
        "tasm_arithmetic_u32_safesub" => Box::new(Safesub),
        "tasm_arithmetic_u32_safemul" => Box::new(Safemul),
        "tasm_arithmetic_u32_shiftright" => Box::new(Shiftright),
        "tasm_arithmetic_u32_shiftleft" => Box::new(Shiftleft),
        "tasm_arithmetic_u32_or" => Box::new(Or),
        "tasm_arithmetic_u32_leadingzeros" => Box::new(Leadingzeros),
        "tasm_arithmetic_u32_safepow" => Box::new(Safepow),
        "tasm_arithmetic_u32_overflowingadd" => Box::new(Overflowingadd),

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
        "tasm_arithmetic_u64_mul_two_u64s_to_u128_u64" => Box::new(MulTwoU64sToU128),
        "tasm_arithmetic_u64_wrapping_sub" => Box::new(WrappingSub),
        "tasm_arithmetic_u64_overflowing_sub" => Box::new(OverflowingSub),

        // u128
        "tasm_arithmetic_u128_add" => Box::new(AddU128),
        "tasm_arithmetic_u128_shift_left" => Box::new(ShiftLeftU128),
        "tasm_arithmetic_u128_shift_right" => Box::new(ShiftRightU128),
        "tasm_arithmetic_u128_sub" => Box::new(SubU128),
        "tasm_arithmetic_u128_safe_mul" => Box::new(SafeMulU128),

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
        "tasm_hashing_swap_digest" => Box::new(SwapDigest),
        "tasm_hashing_hash_varlen" => Box::new(HashVarlen),
        "tasm_hashing_sample_indices_to_safeimplu32_list" => Box::new(SampleIndices{list_type: ListType::Safe}),
        "tasm_hashing_sample_indices_to_unsafeimplu32_list" => Box::new(SampleIndices{list_type: ListType::Unsafe}),
        "tasm_hashing_reverse_digest" => Box::new(ReverseDigest),

        // io
        "tasm_io_read_secin___bool" => Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___u32" => Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___u64" => Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___u128" => Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___bfe" => Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___xfe" => Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::SecretIn,
        }),
        "tasm_io_read_secin___digest" => Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::SecretIn,
        }),

        "tasm_io_read_stdin___bool" => Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___u32" => Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___u64" => Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___u128" => Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___bfe" => Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___xfe" => Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::StdIn,
        }),
        "tasm_io_read_stdin___digest" => Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::StdIn,
        }),

        "tasm_io_write_to_stdout___bool" => Box::new(WriteToStdout{ data_type: DataType::Bool}),
        "tasm_io_write_to_stdout___u32" => Box::new(WriteToStdout{ data_type: DataType::U32}),
        "tasm_io_write_to_stdout___u64" => Box::new(WriteToStdout{ data_type: DataType::U64}),
        "tasm_io_write_to_stdout___u128" => Box::new(WriteToStdout{ data_type: DataType::U128}),
        "tasm_io_write_to_stdout___bfe" => Box::new(WriteToStdout{ data_type: DataType::Bfe }),
        "tasm_io_write_to_stdout___xfe" => Box::new(WriteToStdout{ data_type: DataType::Xfe }),
        "tasm_io_write_to_stdout___digest" => Box::new(WriteToStdout{ data_type: DataType::Digest}),

        // safe lists
        "tasm_list_safeimplu32_get_element___bool" => Box::new(SafeGet { data_type: DataType::Bool }),
        "tasm_list_safeimplu32_get_element___u32" => Box::new(SafeGet { data_type: DataType::U32 }),
        "tasm_list_safeimplu32_get_element___u64" => Box::new(SafeGet { data_type: DataType::U64 }),
        "tasm_list_safeimplu32_get_element___bfe" => Box::new(SafeGet { data_type: DataType::Bfe }),
        "tasm_list_safeimplu32_get_element___xfe" => Box::new(SafeGet { data_type: DataType::Xfe }),
        "tasm_list_safeimplu32_get_element___digest" => Box::new(SafeGet { data_type: DataType::Digest }),

        "tasm_list_safeimplu32_pop___bool" => Box::new(SafePop { data_type: DataType::Bool } ),
        "tasm_list_safeimplu32_pop___u32" => Box::new(SafePop { data_type: DataType::U32 } ),
        "tasm_list_safeimplu32_pop___u64" => Box::new(SafePop { data_type: DataType::U64 } ),
        "tasm_list_safeimplu32_pop___bfe" => Box::new(SafePop { data_type: DataType::Bfe } ),
        "tasm_list_safeimplu32_pop___xfe" => Box::new(SafePop { data_type: DataType::Xfe } ),
        "tasm_list_safeimplu32_pop___digest" => Box::new(SafePop { data_type: DataType::Digest } ),

        "tasm_list_safeimplu32_push___bool" => Box::new(SafePush { data_type: DataType::Bool }),
        "tasm_list_safeimplu32_push___u32" => Box::new(SafePush { data_type: DataType::U32 }),
        "tasm_list_safeimplu32_push___u64" => Box::new(SafePush { data_type: DataType::U64 }),
        "tasm_list_safeimplu32_push___bfe" => Box::new(SafePush { data_type: DataType::Bfe }),
        "tasm_list_safeimplu32_push___xfe" => Box::new(SafePush { data_type: DataType::Xfe }),
        "tasm_list_safeimplu32_push___digest" => Box::new(SafePush { data_type: DataType::Digest }),

        "tasm_list_safeimplu32_set_element___bool" => Box::new(SafeSet{ data_type: DataType::Bool}),
        "tasm_list_safeimplu32_set_element___u32" => Box::new(SafeSet{ data_type: DataType::U32}),
        "tasm_list_safeimplu32_set_element___u64" => Box::new(SafeSet{ data_type: DataType::U64}),
        "tasm_list_safeimplu32_set_element___bfe" => Box::new(SafeSet{ data_type: DataType::Bfe}),
        "tasm_list_safeimplu32_set_element___xfe" => Box::new(SafeSet{ data_type: DataType::Xfe}),
        "tasm_list_safeimplu32_set_element___digest" => Box::new(SafeSet{ data_type: DataType::Digest}),

        "tasm_list_safeimplu32_new___bool" => Box::new(SafeNew { data_type: DataType::Bool }),
        "tasm_list_safeimplu32_new___u32" => Box::new(SafeNew { data_type: DataType::U32 }),
        "tasm_list_safeimplu32_new___u64" => Box::new(SafeNew { data_type: DataType::U64 }),
        "tasm_list_safeimplu32_new___bfe" => Box::new(SafeNew { data_type: DataType::Bfe }),
        "tasm_list_safeimplu32_new___xfe" => Box::new(SafeNew { data_type: DataType::Xfe }),
        "tasm_list_safeimplu32_new___digest" => Box::new(SafeNew { data_type: DataType::Digest }),

        "tasm_list_safeimplu32_length___bool" => Box::new(SafeLength{ data_type: DataType::Bool }),
        "tasm_list_safeimplu32_length___u32" => Box::new(SafeLength{ data_type: DataType::U32 }),
        "tasm_list_safeimplu32_length___u64" => Box::new(SafeLength{ data_type: DataType::U64 }),
        "tasm_list_safeimplu32_length___bfe" => Box::new(SafeLength{ data_type: DataType::Bfe }),
        "tasm_list_safeimplu32_length___xfe" => Box::new(SafeLength{ data_type: DataType::Xfe }),
        "tasm_list_safeimplu32_length___digest" => Box::new(SafeLength{ data_type: DataType::Digest }),

        "tasm_list_safeimplu32_set_length___bool" => Box::new(SafeSetLength { data_type: DataType::Bool }),
        "tasm_list_safeimplu32_set_length___u32" => Box::new(SafeSetLength { data_type: DataType::U32 }),
        "tasm_list_safeimplu32_set_length___u64" => Box::new(SafeSetLength { data_type: DataType::U64 }),
        "tasm_list_safeimplu32_set_length___bfe" => Box::new(SafeSetLength { data_type: DataType::Bfe }),
        "tasm_list_safeimplu32_set_length___xfe" => Box::new(SafeSetLength { data_type: DataType::Xfe }),
        "tasm_list_safeimplu32_set_length___digest" => Box::new(SafeSetLength { data_type: DataType::Digest }),

        "tasm_list_safeimplu32_multiset_equality" => Box::new(crate::list::multiset_equality::MultisetEquality(ListType::Safe)),

        "tasm_list_safeimplu32_range" => Box::new(Range{list_type: ListType::Safe}),

        // unsafe lists
        "tasm_list_unsafeimplu32_get_element___bool" => Box::new(UnsafeGet{data_type: DataType::Bool}),
        "tasm_list_unsafeimplu32_get_element___u32" => Box::new(UnsafeGet{data_type: DataType::U32}),
        "tasm_list_unsafeimplu32_get_element___u64" => Box::new(UnsafeGet{data_type: DataType::U64}),
        "tasm_list_unsafeimplu32_get_element___bfe" => Box::new(UnsafeGet{data_type: DataType::Bfe }),
        "tasm_list_unsafeimplu32_get_element___xfe" => Box::new(UnsafeGet{data_type: DataType::Xfe }),
        "tasm_list_unsafeimplu32_get_element___digest" => Box::new(UnsafeGet{data_type: DataType::Digest}),

        "tasm_list_unsafeimplu32_pop___bool" => Box::new(UnsafePop{ data_type: DataType::Bool }),
        "tasm_list_unsafeimplu32_pop___u32" => Box::new(UnsafePop{ data_type: DataType::U32 }),
        "tasm_list_unsafeimplu32_pop___u64" => Box::new(UnsafePop{ data_type: DataType::U64 }),
        "tasm_list_unsafeimplu32_pop___bfe" => Box::new(UnsafePop{ data_type: DataType::Bfe }),
        "tasm_list_unsafeimplu32_pop___xfe" => Box::new(UnsafePop{ data_type: DataType::Xfe }),
        "tasm_list_unsafeimplu32_pop___digest" => Box::new(UnsafePop{ data_type: DataType::Digest }),

        "tasm_list_unsafeimplu32_push___bool" => Box::new(UnsafePush { data_type: DataType::Bool }),
        "tasm_list_unsafeimplu32_push___u32" => Box::new(UnsafePush { data_type: DataType::U32 }),
        "tasm_list_unsafeimplu32_push___u64" => Box::new(UnsafePush { data_type: DataType::U64 }),
        "tasm_list_unsafeimplu32_push___bfe" => Box::new(UnsafePush { data_type: DataType::Bfe }),
        "tasm_list_unsafeimplu32_push___xfe" => Box::new(UnsafePush { data_type: DataType::Xfe }),
        "tasm_list_unsafeimplu32_push___digest" => Box::new(UnsafePush { data_type: DataType::Digest }),

        "tasm_list_unsafeimplu32_set_element___bool" => Box::new(UnsafeSet {
            data_type: DataType::Bool
        }),
        "tasm_list_unsafeimplu32_set_element___u32" => Box::new(UnsafeSet {
            data_type: DataType::U32
        }),
        "tasm_list_unsafeimplu32_set_element___u64" => Box::new(UnsafeSet {
            data_type: DataType::U64
        }),
        "tasm_list_unsafeimplu32_set_element___bfe" => Box::new(UnsafeSet {
            data_type: DataType::Bfe
        }),
        "tasm_list_unsafeimplu32_set_element___xfe" => Box::new(UnsafeSet {
            data_type: DataType::Xfe
        }),
        "tasm_list_unsafeimplu32_set_element___digest" => Box::new(UnsafeSet {
            data_type: DataType::Digest
        }),

        "tasm_list_unsafeimplu32_new___bool" => Box::new(UnsafeNew { data_type: DataType::Bool }),
        "tasm_list_unsafeimplu32_new___u32" => Box::new(UnsafeNew { data_type: DataType::U32 }),
        "tasm_list_unsafeimplu32_new___u64" => Box::new(UnsafeNew { data_type: DataType::U64 }),
        "tasm_list_unsafeimplu32_new___bfe" => Box::new(UnsafeNew { data_type: DataType::Bfe }),
        "tasm_list_unsafeimplu32_new___xfe" => Box::new(UnsafeNew { data_type: DataType::Xfe }),
        "tasm_list_unsafeimplu32_new___digest" => Box::new(UnsafeNew { data_type: DataType::Digest }),

        "tasm_list_unsafeimplu32_length___bool" => Box::new(UnsafeLength{ data_type: DataType::Bool }),
        "tasm_list_unsafeimplu32_length___u32" => Box::new(UnsafeLength{ data_type: DataType::U32 }),
        "tasm_list_unsafeimplu32_length___u64" => Box::new(UnsafeLength{ data_type: DataType::U64 }),
        "tasm_list_unsafeimplu32_length___bfe" => Box::new(UnsafeLength{ data_type: DataType::Bfe }),
        "tasm_list_unsafeimplu32_length___xfe" => Box::new(UnsafeLength{ data_type: DataType::Xfe }),
        "tasm_list_unsafeimplu32_length___digest" => Box::new(UnsafeLength{ data_type: DataType::Digest }),

        "tasm_list_unsafeimplu32_set_length___bool" => Box::new(UnsafeSetLength {
data_type: DataType::Bool
}),
        "tasm_list_unsafeimplu32_set_length___u32" => Box::new(UnsafeSetLength {
data_type: DataType::U32
}),
        "tasm_list_unsafeimplu32_set_length___u64" => Box::new(UnsafeSetLength {
data_type: DataType::U64
}),
        "tasm_list_unsafeimplu32_set_length___bfe" => Box::new(UnsafeSetLength {
data_type: DataType::Bfe
}),
        "tasm_list_unsafeimplu32_set_length___xfe" => Box::new(UnsafeSetLength {
data_type: DataType::Xfe
}),
        "tasm_list_unsafeimplu32_set_length___digest" => Box::new(UnsafeSetLength {
data_type: DataType::Digest
}),

        "tasm_list_unsafeimplu32_multiset_equality" => Box::new(crate::list::multiset_equality::MultisetEquality(ListType::Unsafe)),
        "tasm_list_unsafeimplu32_range" => Box::new(Range{list_type: ListType::Unsafe}),

        // Contiguous lists
        "tasm_list_contiguous_list_get_length" => Box::new(contiguous_list::get_length::GetLength),
        "tasm_list_contiguous_list_get_pointer_list_unsafeimplu32" => Box::new(contiguous_list::get_pointer_list::GetPointerList{output_list_type:ListType::Unsafe}),
        "tasm_list_contiguous_list_get_pointer_list_safeimplu32" => Box::new(contiguous_list::get_pointer_list::GetPointerList{output_list_type:ListType::Safe}),

        // MMR
        "tasm_mmr_calculate_new_peaks_from_append_unsafeimplu32" => Box::new(CalculateNewPeaksFromAppend { list_type: ListType::Unsafe }),
        "tasm_mmr_calculate_new_peaks_from_append_safeimplu32" => Box::new(CalculateNewPeaksFromAppend { list_type: ListType::Safe }),
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation_unsafeimplu32" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices{ list_type: ListType::Unsafe} )
        }
        "tasm_mmr_calculate_new_peaks_from_leaf_mutation_safeimplu32" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices{ list_type: ListType::Safe} )
        }
        "tasm_mmr_leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "tasm_mmr_verify_from_secret_in_unsafeimplu32" => Box::new(MmrVerifyLeafMembershipFromSecretIn { list_type: ListType::Unsafe }),
        "tasm_mmr_verify_from_secret_in_safeimplu32" => Box::new(MmrVerifyLeafMembershipFromSecretIn { list_type: ListType::Safe }),
        "tasm_mmr_bag_peaks" => Box::new(BagPeaks),
        "tasm_mmr_verify_from_memory_unsafeimplu32" => Box::new(MmrVerifyFromMemory { list_type: ListType::Unsafe} ),
        "tasm_mmr_verify_from_memory_safeimplu32" => Box::new(MmrVerifyFromMemory { list_type: ListType::Safe} ),

        // other
        "tasm_other_bfe_add" => Box::new(BfeAdd),

        // recufy
        "tasm_recufier_mt_ap_verify" => Box::new(MerkleVerify),
        "tasm_recufier_proof_stream_dequeue" => Box::new(Dequeue),

        // memory
        "tasm_memory_dyn_malloc" => Box::new(DynMalloc),
        "tasm_memory_memcpy" => Box::new(MemCpy),

        // structure

        // mutator sets
        "tasm_neptune_mutator_set_commit" => Box::new(Commit),
        "tasm_neptune_mutator_get_swbf_indices_1048576_45" => Box::new(GetSwbfIndices{ window_size: 1048576, num_trials: 45 }),

        _ => panic!("Could not find \"{fn_name}\" in the function `exported_snippets`. Did you include it there?"),
    }
}
