use const_format::formatcp;
use triton_vm::air::challenge_id::ChallengeId;
use triton_vm::challenges::Challenges;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::table::master_table::MasterAuxTable;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;

use crate::arithmetic::u128;
use crate::arithmetic::u32;
use crate::arithmetic::u64;
use crate::array::horner_evaluation::HornerEvaluation;
use crate::array::inner_product_of_three_rows_with_weights::InnerProductOfThreeRowsWithWeights;
use crate::array::inner_product_of_three_rows_with_weights::MainElementType;
use crate::array::inner_product_of_xfes::InnerProductOfXfes;
use crate::hashing;
use crate::hashing::algebraic_hasher;
use crate::hashing::sponge_hasher;
use crate::io::read_input::ReadInput;
use crate::io::write_to_stdout::WriteToStdout;
use crate::io::InputSource;
use crate::list;
use crate::list::contiguous_list;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::memcpy::MemCpy;
use crate::mmr::bag_peaks::BagPeaks;
use crate::mmr::calculate_new_peaks_from_append::CalculateNewPeaksFromAppend;
use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
use crate::mmr::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::mmr::verify_from_memory::MmrVerifyFromMemory;
use crate::mmr::verify_from_secret_in_secret_leaf_index::MmrVerifyFromSecretInSecretLeafIndex;
use crate::neptune::mutator_set::commit::Commit;
use crate::neptune::mutator_set::get_swbf_indices::GetSwbfIndices;
use crate::other_snippets::bfe_add::BfeAdd;
use crate::prelude::*;
use crate::verifier::challenges;
use crate::verifier::challenges::new_empty_input_and_output::NewEmptyInputAndOutput;
use crate::verifier::challenges::new_generic_dyn_claim::NewGenericDynClaim;
use crate::verifier::claim::instantiate_fiat_shamir_with_claim::InstantiateFiatShamirWithClaim;
use crate::verifier::fri;
use crate::verifier::master_table::air_constraint_evaluation::AirConstraintEvaluation;
use crate::verifier::master_table::divide_out_zerofiers::DivideOutZerofiers;
use crate::verifier::master_table::verify_table_rows::ColumnType;
use crate::verifier::master_table::verify_table_rows::VerifyTableRows;
use crate::verifier::own_program_digest::OwnProgramDigest;
use crate::verifier::read_and_verify_own_program_digest_from_std_in::ReadAndVerifyOwnProgramDigestFromStdIn;
use crate::verifier::vm_proof_iter::dequeue_next_as::DequeueNextAs;

const NUM_CONSTRAINTS_TVM: usize = MasterAuxTable::NUM_CONSTRAINTS;
const WEIGHTS_QUOTIENTS_INNER_PRODUCT_ENTRYPOINT: &str = formatcp!(
    "tasmlib_array_inner_product_of_{}_xfes",
    NUM_CONSTRAINTS_TVM
);
const HORNER_EVALUATION_FOR_SUM_OF_EVALUATED_OUT_OF_DOMAIN_QUOTIENT_SEGMENTS_ENTRYPOINT: &str = formatcp!(
    "tasmlib_array_horner_evaluation_with_{}_coefficients",
    NUM_QUOTIENT_SEGMENTS
);
const CHALLENGES_NEW_FROM_DYN_CLAIM: &str = formatcp!(
    "tasmlib_verifier_challenges_new_generic_dyn_claim_{}_{}",
    Challenges::SAMPLE_COUNT,
    Challenges::COUNT - Challenges::SAMPLE_COUNT
);

pub fn name_to_snippet(fn_name: &str) -> Box<dyn BasicSnippet> {
    match fn_name {
        // XFieldElement
        "tasmlib_arithmetic_xfe_square" => Box::new(crate::arithmetic::xfe::square::Square),
        "tasmlib_arithmetic_xfe_cube" => Box::new(crate::arithmetic::xfe::cube::Cube),
        "tasmlib_arithmetic_xfe_to_the_fourth" => Box::new(crate::arithmetic::xfe::to_the_fourth::ToTheFourth),

        // u32
        "tasmlib_arithmetic_u32_isodd" => Box::new(u32::isodd::Isodd),
        "tasmlib_arithmetic_u32_isu32" => Box::new(u32::isu32::Isu32),
        "tasmlib_arithmetic_u32_safeadd" => Box::new(u32::safeadd::Safeadd),
        "tasmlib_arithmetic_u32_safesub" => Box::new(u32::safesub::Safesub),
        "tasmlib_arithmetic_u32_safemul" => Box::new(u32::safemul::Safemul),
        "tasmlib_arithmetic_u32_shiftright" => Box::new(u32::shiftright::Shiftright),
        "tasmlib_arithmetic_u32_shiftleft" => Box::new(u32::shiftleft::Shiftleft),
        "tasmlib_arithmetic_u32_or" => Box::new(u32::or::Or),
        "tasmlib_arithmetic_u32_leading_zeros" => Box::new(u32::leading_zeros::LeadingZeros),
        "tasmlib_arithmetic_u32_safepow" => Box::new(u32::safepow::Safepow),
        "tasmlib_arithmetic_u32_overflowingadd" => Box::new(u32::overflowingadd::Overflowingadd),
        "tasmlib_arithmetic_u32_trailing_zeros" => Box::new(u32::trailing_zeros::TrailingZeros),

        // u64
        "tasmlib_arithmetic_u64_add" => Box::new(u64::add::Add),
        "tasmlib_arithmetic_u64_and" => Box::new(u64::and::And),
        "tasmlib_arithmetic_u64_decr" => Box::new(u64::decr::Decr),
        "tasmlib_arithmetic_u64_div2" => Box::new(u64::div2::Div2),
        "tasmlib_arithmetic_u64_div_mod" => Box::new(u64::div_mod::DivMod),
        "tasmlib_arithmetic_u64_pow2_double" => Box::new(u64::double_pow2::DoublePow2),
        "tasmlib_arithmetic_u64_eq" => Box::new(u64::eq::Eq),
        "tasmlib_arithmetic_u64_incr" => Box::new(u64::incr::Incr),
        "tasmlib_arithmetic_u64_leading_zeros" => Box::new(u64::leading_zeros::LeadingZeros),
        "tasmlib_arithmetic_u64_trailing_zeros" => Box::new(u64::trailing_zeros::TrailingZeros),
        "tasmlib_arithmetic_u64_log_2_floor" => Box::new(u64::log_2_floor::Log2Floor),
        "tasmlib_arithmetic_u64_lt" => Box::new(u64::lt::Lt),
        "tasmlib_arithmetic_u64_lt_preserve_args" => Box::new(u64::lt_preserve_args::LtPreserveArgs),
        "tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64" => Box::new(u64::mul_two_u64s_to_u128::MulTwoU64sToU128),
        "tasmlib_arithmetic_u64_or_u64" => Box::new(u64::or::Or),
        "tasmlib_arithmetic_u64_overflowing_sub" => Box::new(u64::overflowing_sub::OverflowingSub),
        "tasmlib_arithmetic_u64_popcount" => Box::new(u64::popcount::PopCount),
        "tasmlib_arithmetic_u64_pow2" => Box::new(u64::pow2::Pow2),
        "tasmlib_arithmetic_u64_safe_mul" => Box::new(u64::safe_mul::SafeMul),
        "tasmlib_arithmetic_u64_shift_left" => Box::new(u64::shift_left::ShiftLeft),
        "tasmlib_arithmetic_u64_shift_right" => Box::new(u64::shift_right::ShiftRight),
        "tasmlib_arithmetic_u64_sub" => Box::new(u64::sub::Sub),
        "tasmlib_arithmetic_u64_wrapping_mul" => Box::new(u64::wrapping_mul::WrappingMul),
        "tasmlib_arithmetic_u64_wrapping_sub" => Box::new(u64::wrapping_sub::WrappingSub),
        "tasmlib_arithmetic_u64_xor" => Box::new(u64::xor::Xor),

        // u128
        "tasmlib_arithmetic_u128_lt" => Box::new(u128::lt::Lt),
        "tasmlib_arithmetic_u128_safe_add" => Box::new(u128::safe_add::SafeAdd),
        "tasmlib_arithmetic_u128_safe_mul" => Box::new(u128::safe_mul::SafeMul),
        "tasmlib_arithmetic_u128_shift_left" => Box::new(u128::shift_left::ShiftLeft),
        "tasmlib_arithmetic_u128_shift_right" => Box::new(u128::shift_right::ShiftRight),
        "tasmlib_arithmetic_u128_sub" => Box::new(u128::sub::Sub),

        "tasmlib_arithmetic_u128_shift_left_static_1" => Box::new(u128::shift_left_static::ShiftLeftStatic::<1>),
        "tasmlib_arithmetic_u128_shift_left_static_2" => Box::new(u128::shift_left_static::ShiftLeftStatic::<2>),
        "tasmlib_arithmetic_u128_shift_left_static_3" => Box::new(u128::shift_left_static::ShiftLeftStatic::<3>),
        "tasmlib_arithmetic_u128_shift_left_static_4" => Box::new(u128::shift_left_static::ShiftLeftStatic::<4>),
        "tasmlib_arithmetic_u128_shift_left_static_5" => Box::new(u128::shift_left_static::ShiftLeftStatic::<5>),
        "tasmlib_arithmetic_u128_shift_left_static_6" => Box::new(u128::shift_left_static::ShiftLeftStatic::<6>),
        "tasmlib_arithmetic_u128_shift_left_static_7" => Box::new(u128::shift_left_static::ShiftLeftStatic::<7>),
        "tasmlib_arithmetic_u128_shift_left_static_8" => Box::new(u128::shift_left_static::ShiftLeftStatic::<8>),
        "tasmlib_arithmetic_u128_shift_left_static_9" => Box::new(u128::shift_left_static::ShiftLeftStatic::<9>),
        "tasmlib_arithmetic_u128_shift_left_static_10" => Box::new(u128::shift_left_static::ShiftLeftStatic::<10>),
        "tasmlib_arithmetic_u128_shift_left_static_11" => Box::new(u128::shift_left_static::ShiftLeftStatic::<11>),
        "tasmlib_arithmetic_u128_shift_left_static_12" => Box::new(u128::shift_left_static::ShiftLeftStatic::<12>),
        "tasmlib_arithmetic_u128_shift_left_static_13" => Box::new(u128::shift_left_static::ShiftLeftStatic::<13>),
        "tasmlib_arithmetic_u128_shift_left_static_14" => Box::new(u128::shift_left_static::ShiftLeftStatic::<14>),
        "tasmlib_arithmetic_u128_shift_left_static_15" => Box::new(u128::shift_left_static::ShiftLeftStatic::<15>),
        "tasmlib_arithmetic_u128_shift_left_static_16" => Box::new(u128::shift_left_static::ShiftLeftStatic::<16>),
        "tasmlib_arithmetic_u128_shift_left_static_17" => Box::new(u128::shift_left_static::ShiftLeftStatic::<17>),
        "tasmlib_arithmetic_u128_shift_left_static_18" => Box::new(u128::shift_left_static::ShiftLeftStatic::<18>),
        "tasmlib_arithmetic_u128_shift_left_static_19" => Box::new(u128::shift_left_static::ShiftLeftStatic::<19>),
        "tasmlib_arithmetic_u128_shift_left_static_20" => Box::new(u128::shift_left_static::ShiftLeftStatic::<20>),
        "tasmlib_arithmetic_u128_shift_left_static_21" => Box::new(u128::shift_left_static::ShiftLeftStatic::<21>),
        "tasmlib_arithmetic_u128_shift_left_static_22" => Box::new(u128::shift_left_static::ShiftLeftStatic::<22>),
        "tasmlib_arithmetic_u128_shift_left_static_23" => Box::new(u128::shift_left_static::ShiftLeftStatic::<23>),
        "tasmlib_arithmetic_u128_shift_left_static_24" => Box::new(u128::shift_left_static::ShiftLeftStatic::<24>),
        "tasmlib_arithmetic_u128_shift_left_static_25" => Box::new(u128::shift_left_static::ShiftLeftStatic::<25>),
        "tasmlib_arithmetic_u128_shift_left_static_26" => Box::new(u128::shift_left_static::ShiftLeftStatic::<26>),
        "tasmlib_arithmetic_u128_shift_left_static_27" => Box::new(u128::shift_left_static::ShiftLeftStatic::<27>),
        "tasmlib_arithmetic_u128_shift_left_static_28" => Box::new(u128::shift_left_static::ShiftLeftStatic::<28>),
        "tasmlib_arithmetic_u128_shift_left_static_29" => Box::new(u128::shift_left_static::ShiftLeftStatic::<29>),
        "tasmlib_arithmetic_u128_shift_left_static_30" => Box::new(u128::shift_left_static::ShiftLeftStatic::<30>),
        "tasmlib_arithmetic_u128_shift_left_static_31" => Box::new(u128::shift_left_static::ShiftLeftStatic::<31>),
        "tasmlib_arithmetic_u128_shift_left_static_32" => Box::new(u128::shift_left_static::ShiftLeftStatic::<32>),

        "tasmlib_arithmetic_u128_shift_right_static_1" => Box::new(u128::shift_right_static::ShiftRightStatic::<1>),
        "tasmlib_arithmetic_u128_shift_right_static_2" => Box::new(u128::shift_right_static::ShiftRightStatic::<2>),
        "tasmlib_arithmetic_u128_shift_right_static_3" => Box::new(u128::shift_right_static::ShiftRightStatic::<3>),
        "tasmlib_arithmetic_u128_shift_right_static_4" => Box::new(u128::shift_right_static::ShiftRightStatic::<4>),
        "tasmlib_arithmetic_u128_shift_right_static_5" => Box::new(u128::shift_right_static::ShiftRightStatic::<5>),
        "tasmlib_arithmetic_u128_shift_right_static_6" => Box::new(u128::shift_right_static::ShiftRightStatic::<6>),
        "tasmlib_arithmetic_u128_shift_right_static_7" => Box::new(u128::shift_right_static::ShiftRightStatic::<7>),
        "tasmlib_arithmetic_u128_shift_right_static_8" => Box::new(u128::shift_right_static::ShiftRightStatic::<8>),
        "tasmlib_arithmetic_u128_shift_right_static_9" => Box::new(u128::shift_right_static::ShiftRightStatic::<9>),
        "tasmlib_arithmetic_u128_shift_right_static_10" => Box::new(u128::shift_right_static::ShiftRightStatic::<10>),
        "tasmlib_arithmetic_u128_shift_right_static_11" => Box::new(u128::shift_right_static::ShiftRightStatic::<11>),
        "tasmlib_arithmetic_u128_shift_right_static_12" => Box::new(u128::shift_right_static::ShiftRightStatic::<12>),
        "tasmlib_arithmetic_u128_shift_right_static_13" => Box::new(u128::shift_right_static::ShiftRightStatic::<13>),
        "tasmlib_arithmetic_u128_shift_right_static_14" => Box::new(u128::shift_right_static::ShiftRightStatic::<14>),
        "tasmlib_arithmetic_u128_shift_right_static_15" => Box::new(u128::shift_right_static::ShiftRightStatic::<15>),
        "tasmlib_arithmetic_u128_shift_right_static_16" => Box::new(u128::shift_right_static::ShiftRightStatic::<16>),
        "tasmlib_arithmetic_u128_shift_right_static_17" => Box::new(u128::shift_right_static::ShiftRightStatic::<17>),
        "tasmlib_arithmetic_u128_shift_right_static_18" => Box::new(u128::shift_right_static::ShiftRightStatic::<18>),
        "tasmlib_arithmetic_u128_shift_right_static_19" => Box::new(u128::shift_right_static::ShiftRightStatic::<19>),
        "tasmlib_arithmetic_u128_shift_right_static_20" => Box::new(u128::shift_right_static::ShiftRightStatic::<20>),
        "tasmlib_arithmetic_u128_shift_right_static_21" => Box::new(u128::shift_right_static::ShiftRightStatic::<21>),
        "tasmlib_arithmetic_u128_shift_right_static_22" => Box::new(u128::shift_right_static::ShiftRightStatic::<22>),
        "tasmlib_arithmetic_u128_shift_right_static_23" => Box::new(u128::shift_right_static::ShiftRightStatic::<23>),
        "tasmlib_arithmetic_u128_shift_right_static_24" => Box::new(u128::shift_right_static::ShiftRightStatic::<24>),
        "tasmlib_arithmetic_u128_shift_right_static_25" => Box::new(u128::shift_right_static::ShiftRightStatic::<25>),
        "tasmlib_arithmetic_u128_shift_right_static_26" => Box::new(u128::shift_right_static::ShiftRightStatic::<26>),
        "tasmlib_arithmetic_u128_shift_right_static_27" => Box::new(u128::shift_right_static::ShiftRightStatic::<27>),
        "tasmlib_arithmetic_u128_shift_right_static_28" => Box::new(u128::shift_right_static::ShiftRightStatic::<28>),
        "tasmlib_arithmetic_u128_shift_right_static_29" => Box::new(u128::shift_right_static::ShiftRightStatic::<29>),
        "tasmlib_arithmetic_u128_shift_right_static_30" => Box::new(u128::shift_right_static::ShiftRightStatic::<30>),
        "tasmlib_arithmetic_u128_shift_right_static_31" => Box::new(u128::shift_right_static::ShiftRightStatic::<31>),
        "tasmlib_arithmetic_u128_shift_right_static_32" => Box::new(u128::shift_right_static::ShiftRightStatic::<32>),

        // Hashing
        "tasmlib_hashing_eq_digest" => Box::new(hashing::eq_digest::EqDigest),
        "tasmlib_hashing_merkle_verify" => Box::new(hashing::merkle_verify::MerkleVerify),
        "tasmlib_hashing_reverse_digest" => Box::new(hashing::reverse_digest::ReverseDigest),
        "tasmlib_hashing_swap_digest" => Box::new(hashing::swap_digest::SwapDigest),

        // Hashing -> algebraic hasher trait
        "tasmlib_hashing_algebraic_hasher_hash_varlen" => Box::new(algebraic_hasher::hash_varlen::HashVarlen),
        "tasmlib_hashing_algebraic_hasher_sample_indices" => Box::new(algebraic_hasher::sample_indices::SampleIndices),
        "tasmlib_hashing_algebraic_hasher_sample_scalars" => Box::new(algebraic_hasher::sample_scalars::SampleScalars),

        // Hashing -> Sponge hasher trait
        "tasmlib_hashing_sponge_hasher_init" => Box::new(sponge_hasher::init::Init),
        "tasmlib_hashing_sponge_hasher_absorb" => Box::new(sponge_hasher::absorb::Absorb),
        "tasmlib_hashing_sponge_hasher_squeeze" => Box::new(sponge_hasher::squeeze::Squeeze),
        "tasmlib_hashing_sponge_hasher_pad_and_absorb_all" => Box::new(sponge_hasher::pad_and_absorb_all::PadAndAbsorbAll),

        // io
        "tasmlib_io_read_secin___bool" => Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___u32" => Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___u64" => Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___u128" => Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___bfe" => Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___xfe" => Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::SecretIn,
        }),
        "tasmlib_io_read_secin___digest" => Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::SecretIn,
        }),

        "tasmlib_io_read_stdin___bool" => Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___u32" => Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___u64" => Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___u128" => Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___bfe" => Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___xfe" => Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::StdIn,
        }),
        "tasmlib_io_read_stdin___digest" => Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::StdIn,
        }),

        "tasmlib_io_write_to_stdout___bool" => Box::new(WriteToStdout { data_type: DataType::Bool }),
        "tasmlib_io_write_to_stdout___u32" => Box::new(WriteToStdout { data_type: DataType::U32 }),
        "tasmlib_io_write_to_stdout___u64" => Box::new(WriteToStdout { data_type: DataType::U64 }),
        "tasmlib_io_write_to_stdout___u128" => Box::new(WriteToStdout { data_type: DataType::U128 }),
        "tasmlib_io_write_to_stdout___bfe" => Box::new(WriteToStdout { data_type: DataType::Bfe }),
        "tasmlib_io_write_to_stdout___xfe" => Box::new(WriteToStdout { data_type: DataType::Xfe }),
        "tasmlib_io_write_to_stdout___digest" => Box::new(WriteToStdout { data_type: DataType::Digest }),

        // lists
        "tasmlib_list_get_element___bool" => Box::new(list::get::Get::new(DataType::Bool)),
        "tasmlib_list_get_element___u32" => Box::new(list::get::Get::new(DataType::U32)),
        "tasmlib_list_get_element___u64" => Box::new(list::get::Get::new(DataType::U64)),
        "tasmlib_list_get_element___bfe" => Box::new(list::get::Get::new(DataType::Bfe)),
        "tasmlib_list_get_element___xfe" => Box::new(list::get::Get::new(DataType::Xfe)),
        "tasmlib_list_get_element___digest" => Box::new(list::get::Get::new(DataType::Digest)),

        "tasmlib_list_pop___bool" => Box::new(list::pop::Pop::new(DataType::Bool)),
        "tasmlib_list_pop___u32" => Box::new(list::pop::Pop::new(DataType::U32)),
        "tasmlib_list_pop___u64" => Box::new(list::pop::Pop::new(DataType::U64)),
        "tasmlib_list_pop___bfe" => Box::new(list::pop::Pop::new(DataType::Bfe)),
        "tasmlib_list_pop___xfe" => Box::new(list::pop::Pop::new(DataType::Xfe)),
        "tasmlib_list_pop___digest" => Box::new(list::pop::Pop::new(DataType::Digest)),

        "tasmlib_list_push___bool" => Box::new(list::push::Push::new(DataType::Bool)),
        "tasmlib_list_push___u32" => Box::new(list::push::Push::new(DataType::U32)),
        "tasmlib_list_push___u64" => Box::new(list::push::Push::new(DataType::U64)),
        "tasmlib_list_push___bfe" => Box::new(list::push::Push::new(DataType::Bfe)),
        "tasmlib_list_push___xfe" => Box::new(list::push::Push::new(DataType::Xfe)),
        "tasmlib_list_push___digest" => Box::new(list::push::Push::new(DataType::Digest)),

        "tasmlib_list_set_element___bool" => Box::new(list::set::Set::new(DataType::Bool)),
        "tasmlib_list_set_element___u32" => Box::new(list::set::Set::new(DataType::U32)),
        "tasmlib_list_set_element___u64" => Box::new(list::set::Set::new(DataType::U64)),
        "tasmlib_list_set_element___bfe" => Box::new(list::set::Set::new(DataType::Bfe)),
        "tasmlib_list_set_element___xfe" => Box::new(list::set::Set::new(DataType::Xfe)),
        "tasmlib_list_set_element___digest" => Box::new(list::set::Set::new(DataType::Digest)),

        "tasmlib_list_new___bool" => Box::new(list::new::New::new(DataType::Bool)),
        "tasmlib_list_new___u32" => Box::new(list::new::New::new(DataType::U32)),
        "tasmlib_list_new___u64" => Box::new(list::new::New::new(DataType::U64)),
        "tasmlib_list_new___bfe" => Box::new(list::new::New::new(DataType::Bfe)),
        "tasmlib_list_new___xfe" => Box::new(list::new::New::new(DataType::Xfe)),
        "tasmlib_list_new___digest" => Box::new(list::new::New::new(DataType::Digest)),

        "tasmlib_list_length___bool" => Box::new(list::length::Length::new(DataType::Bool)),
        "tasmlib_list_length___u32" => Box::new(list::length::Length::new(DataType::U32)),
        "tasmlib_list_length___u64" => Box::new(list::length::Length::new(DataType::U64)),
        "tasmlib_list_length___bfe" => Box::new(list::length::Length::new(DataType::Bfe)),
        "tasmlib_list_length___xfe" => Box::new(list::length::Length::new(DataType::Xfe)),
        "tasmlib_list_length___digest" => Box::new(list::length::Length::new(DataType::Digest)),

        "tasmlib_list_set_length___bool" => Box::new(list::set_length::SetLength::new(DataType::Bool)),
        "tasmlib_list_set_length___u32" => Box::new(list::set_length::SetLength::new(DataType::U32)),
        "tasmlib_list_set_length___u64" => Box::new(list::set_length::SetLength::new(DataType::U64)),
        "tasmlib_list_set_length___bfe" => Box::new(list::set_length::SetLength::new(DataType::Bfe)),
        "tasmlib_list_set_length___xfe" => Box::new(list::set_length::SetLength::new(DataType::Xfe)),
        "tasmlib_list_set_length___digest" => Box::new(list::set_length::SetLength::new(DataType::Digest)),

        "tasmlib_list_multiset_equality_digests" => Box::new(list::multiset_equality_digests::MultisetEqualityDigests),
        "tasmlib_list_range" => Box::new(list::range::Range),

        // Contiguous lists
        "tasmlib_list_contiguous_list_get_length" => Box::new(contiguous_list::get_length::GetLength),
        "tasmlib_list_contiguous_list_get_pointer_list" => Box::new(contiguous_list::get_pointer_list::GetPointerList),

        // MMR
        "tasmlib_mmr_calculate_new_peaks_from_append" => Box::new(CalculateNewPeaksFromAppend),
        "tasmlib_mmr_calculate_new_peaks_from_leaf_mutation" => {
            Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices)
        }
        "tasmlib_mmr_leaf_index_to_mt_index_and_peak_index" => Box::new(MmrLeafIndexToMtIndexAndPeakIndex),
        "tasmlib_mmr_verify_from_secret_in_secret_leaf_index" => Box::new(MmrVerifyFromSecretInSecretLeafIndex),
        "tasmlib_mmr_bag_peaks" => Box::new(BagPeaks),
        "tasmlib_mmr_verify_from_memory" => Box::new(MmrVerifyFromMemory),

        // other
        "tasmlib_other_bfe_add" => Box::new(BfeAdd),

        // recufy
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_merkleroot" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::MerkleRoot })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainmainrow" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::OutOfDomainMainRow })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainauxrow" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::OutOfDomainAuxRow })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainquotientsegments" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::OutOfDomainQuotientSegments })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_authenticationstructure" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::AuthenticationStructure })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_mastermaintablerows" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::MasterMainTableRows })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_masterauxtablerows" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::MasterAuxTableRows })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_log2paddedheight" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::Log2PaddedHeight })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_quotientsegmentselements" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::QuotientSegmentsElements })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_fricodeword" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::FriCodeword })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_fripolynomial" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::FriPolynomial })
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_friresponse" => {
            Box::new(DequeueNextAs { proof_item: ProofItemVariant::FriResponse })
        }
        "tasmlib_verifier_read_and_verify_own_program_digest_from_std_in" => {
            Box::new(ReadAndVerifyOwnProgramDigestFromStdIn)
        }
        "tasmlib_verifier_challenges_new_empty_input_and_output_59_4" => {
            let num_challenges_to_sample = Challenges::SAMPLE_COUNT;
            let num_challenges_to_compute = Challenges::COUNT - num_challenges_to_sample;
            assert_eq!(59, num_challenges_to_sample);
            assert_eq!(4, num_challenges_to_compute);
            let challenge_snippet
                = NewEmptyInputAndOutput::new(num_challenges_to_sample, num_challenges_to_compute, challenges::shared::conventional_challenges_pointer());
            Box::new(challenge_snippet)
        }
        "tasmlib_verifier_master_table_air_constraint_evaluation" => {
            Box::new(AirConstraintEvaluation::with_conventional_dynamic_memory_layout())
        }
        "tasmlib_verifier_master_table_divide_out_zerofiers" => {
            Box::new(DivideOutZerofiers)
        }
        "tasmlib_verifier_master_table_verify_Main_table_rows"=> {
            Box::new(VerifyTableRows::new(ColumnType::Main))
        }
        "tasmlib_verifier_master_table_verify_Aux_table_rows"=> {
            Box::new(VerifyTableRows::new(ColumnType::Aux))
        }
        "tasmlib_verifier_master_table_verify_Quotient_table_rows"=> {
            Box::new(VerifyTableRows::new(ColumnType::Quotient))
        }

        "tasmlib_verifier_fri_number_of_rounds" => { Box::new(fri::number_of_rounds::NumberOfRounds{}) }

        "tasmlib_array_inner_product_of_4_xfes" => {
            Box::new(InnerProductOfXfes::new(4))
        }
        WEIGHTS_QUOTIENTS_INNER_PRODUCT_ENTRYPOINT => {
            Box::new(InnerProductOfXfes::new(NUM_CONSTRAINTS_TVM))
        }
        HORNER_EVALUATION_FOR_SUM_OF_EVALUATED_OUT_OF_DOMAIN_QUOTIENT_SEGMENTS_ENTRYPOINT => {
            Box::new(HornerEvaluation::new(NUM_QUOTIENT_SEGMENTS))
        }
        "tasmlib_verifier_own_program_digest" => {
            Box::new(OwnProgramDigest)
        }
        "tasmlib_array_inner_product_of_three_rows_with_weights_Bfe_mainrowelem" => {
            Box::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe))
        }
        "tasmlib_array_inner_product_of_three_rows_with_weights_Xfe_mainrowelem" => {
            Box::new(InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe))
        }
        "tasmlib_verifier_claim_instantiate_fiat_shamir_with_claim" => {
            Box::new(InstantiateFiatShamirWithClaim)
        }
        CHALLENGES_NEW_FROM_DYN_CLAIM => Box::new(NewGenericDynClaim::new(Challenges::SAMPLE_COUNT, ChallengeId::NUM_DERIVED_CHALLENGES, challenges::shared::conventional_challenges_pointer())),

        // memory
        "tasmlib_memory_dyn_malloc" => Box::new(DynMalloc),
        "tasmlib_memory_memcpy" => Box::new(MemCpy),

        // FRI
        #[cfg(not(test))]
        "tasmlib_verifier_fri_verify" => Box::new(crate::verifier::fri::verify::FriSnippet {}),

        // structure

        // mutator sets
        "tasmlib_neptune_mutator_set_commit" => Box::new(Commit),
        "tasmlib_neptune_mutator_get_swbf_indices_1048576_45" => Box::new(GetSwbfIndices { window_size: 1048576, num_trials: 45 }),

        _ => panic!("Could not find \"{fn_name}\" in the function `exported_snippets`. Did you include it there?"),
    }
}
