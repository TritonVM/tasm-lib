use const_format::formatcp;
use triton_vm::air::challenge_id::ChallengeId;
use triton_vm::challenges::Challenges;
use triton_vm::prelude::Stark;
use triton_vm::proof_item::ProofItemVariant;
use triton_vm::table::NUM_QUOTIENT_SEGMENTS;
use triton_vm::table::master_table::MasterAuxTable;

use crate::arithmetic::i128;
use crate::arithmetic::u32;
use crate::arithmetic::u64;
use crate::arithmetic::u128;
use crate::arithmetic::u128::shift_left_static::ShiftLeftStatic as SShlU128;
use crate::arithmetic::u128::shift_right_static::ShiftRightStatic as SShrU128;
use crate::array::horner_evaluation::HornerEvaluation;
use crate::array::inner_product_of_three_rows_with_weights::InnerProductOfThreeRowsWithWeights;
use crate::array::inner_product_of_three_rows_with_weights::MainElementType;
use crate::array::inner_product_of_xfes::InnerProductOfXfes;
use crate::hashing;
use crate::hashing::algebraic_hasher;
use crate::hashing::sponge_hasher;
use crate::io::InputSource;
use crate::io::read_input::ReadInput;
use crate::io::write_to_stdout::WriteToStdout;
use crate::list;
use crate::memory::dyn_malloc::DynMalloc;
use crate::memory::memcpy::MemCpy;
use crate::mmr::bag_peaks::BagPeaks;
use crate::mmr::calculate_new_peaks_from_append::CalculateNewPeaksFromAppend;
use crate::mmr::calculate_new_peaks_from_leaf_mutation::MmrCalculateNewPeaksFromLeafMutationMtIndices;
use crate::mmr::leaf_index_to_mt_index_and_peak_index::MmrLeafIndexToMtIndexAndPeakIndex;
use crate::mmr::verify_from_memory::MmrVerifyFromMemory;
use crate::mmr::verify_from_secret_in_leaf_index_on_stack::MmrVerifyFromSecretInLeafIndexOnStack;
use crate::mmr::verify_from_secret_in_secret_leaf_index::MmrVerifyFromSecretInSecretLeafIndex;
use crate::mmr::verify_mmr_successor::VerifyMmrSuccessor;
use crate::neptune::mutator_set::commit::Commit;
use crate::neptune::mutator_set::get_swbf_indices::GetSwbfIndices;
use crate::prelude::*;
use crate::verifier;
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

pub fn name_to_snippet(fn_name: &str) -> Option<Box<dyn BasicSnippet>> {
    match fn_name {
        // BFieldElement
        "tasmlib_arithmetic_bfe_primitive_root_of_unity" => Some(Box::new(
            crate::arithmetic::bfe::primitive_root_of_unity::PrimitiveRootOfUnity,
        )),

        // XFieldElement
        "tasmlib_arithmetic_xfe_to_the_power_of_power_of_2" => Some(Box::new(
            crate::arithmetic::xfe::to_the_power_of_power_of_2::ToThePowerOfPowerOf2,
        )),

        // u32
        "tasmlib_arithmetic_u32_is_odd" => Some(Box::new(u32::is_odd::IsOdd)),
        "tasmlib_arithmetic_u32_is_u32" => Some(Box::new(u32::is_u32::IsU32)),
        "tasmlib_arithmetic_u32_leading_zeros" => Some(Box::new(u32::leading_zeros::LeadingZeros)),
        "tasmlib_arithmetic_u32_next_power_of_two" => {
            Some(Box::new(u32::next_power_of_two::NextPowerOfTwo))
        }
        "tasmlib_arithmetic_u32_or" => Some(Box::new(u32::or::Or)),
        "tasmlib_arithmetic_u32_overflowing_add" => {
            Some(Box::new(u32::overflowing_add::OverflowingAdd))
        }
        "tasmlib_arithmetic_u32_safe_add" => Some(Box::new(u32::safe_add::SafeAdd)),
        "tasmlib_arithmetic_u32_safe_mul" => Some(Box::new(u32::safe_mul::SafeMul)),
        "tasmlib_arithmetic_u32_safe_pow" => Some(Box::new(u32::safe_pow::SafePow)),
        "tasmlib_arithmetic_u32_safe_sub" => Some(Box::new(u32::safe_sub::SafeSub)),
        "tasmlib_arithmetic_u32_shift_left" => Some(Box::new(u32::shift_left::ShiftLeft)),
        "tasmlib_arithmetic_u32_shift_right" => Some(Box::new(u32::shift_right::ShiftRight)),
        "tasmlib_arithmetic_u32_trailing_zeros" => {
            Some(Box::new(u32::trailing_zeros::TrailingZeros))
        }

        // u64
        "tasmlib_arithmetic_u64_add" => Some(Box::new(u64::add::Add)),
        "tasmlib_arithmetic_u64_and" => Some(Box::new(u64::and::And)),
        "tasmlib_arithmetic_u64_decr" => Some(Box::new(u64::decr::Decr)),
        "tasmlib_arithmetic_u64_div2" => Some(Box::new(u64::div2::Div2)),
        "tasmlib_arithmetic_u64_div_mod" => Some(Box::new(u64::div_mod::DivMod)),
        "tasmlib_arithmetic_u64_incr" => Some(Box::new(u64::incr::Incr)),
        "tasmlib_arithmetic_u64_leading_zeros" => Some(Box::new(u64::leading_zeros::LeadingZeros)),
        "tasmlib_arithmetic_u64_trailing_zeros" => {
            Some(Box::new(u64::trailing_zeros::TrailingZeros))
        }
        "tasmlib_arithmetic_u64_log_2_floor" => Some(Box::new(u64::log_2_floor::Log2Floor)),
        "tasmlib_arithmetic_u64_lt" => Some(Box::new(u64::lt::Lt)),
        "tasmlib_arithmetic_u64_lt_preserve_args" => {
            Some(Box::new(u64::lt_preserve_args::LtPreserveArgs))
        }
        "tasmlib_arithmetic_u64_mul_two_u64s_to_u128_u64" => {
            Some(Box::new(u64::mul_two_u64s_to_u128::MulTwoU64sToU128))
        }
        "tasmlib_arithmetic_u64_or" => Some(Box::new(u64::or::Or)),
        "tasmlib_arithmetic_u64_overflowing_sub" => {
            Some(Box::new(u64::overflowing_sub::OverflowingSub))
        }
        "tasmlib_arithmetic_u64_popcount" => Some(Box::new(u64::popcount::PopCount)),
        "tasmlib_arithmetic_u64_pow2" => Some(Box::new(u64::pow2::Pow2)),
        "tasmlib_arithmetic_u64_safe_mul" => Some(Box::new(u64::safe_mul::SafeMul)),
        "tasmlib_arithmetic_u64_shift_left" => Some(Box::new(u64::shift_left::ShiftLeft)),
        "tasmlib_arithmetic_u64_shift_right" => Some(Box::new(u64::shift_right::ShiftRight)),
        "tasmlib_arithmetic_u64_sub" => Some(Box::new(u64::sub::Sub)),
        "tasmlib_arithmetic_u64_wrapping_mul" => Some(Box::new(u64::wrapping_mul::WrappingMul)),
        "tasmlib_arithmetic_u64_wrapping_sub" => Some(Box::new(u64::wrapping_sub::WrappingSub)),
        "tasmlib_arithmetic_u64_xor" => Some(Box::new(u64::xor::Xor)),

        // u128
        "tasmlib_arithmetic_u128_lt" => Some(Box::new(u128::lt::Lt)),
        "tasmlib_arithmetic_u128_overflowing_add" => {
            Some(Box::new(u128::overflowing_add::OverflowingAdd))
        }
        "tasmlib_arithmetic_u128_safe_add" => Some(Box::new(u128::safe_add::SafeAdd)),
        "tasmlib_arithmetic_u128_safe_mul" => Some(Box::new(u128::safe_mul::SafeMul)),
        "tasmlib_arithmetic_u128_shift_left" => Some(Box::new(u128::shift_left::ShiftLeft)),
        "tasmlib_arithmetic_u128_shift_right" => Some(Box::new(u128::shift_right::ShiftRight)),
        "tasmlib_arithmetic_u128_sub" => Some(Box::new(u128::sub::Sub)),

        "tasmlib_arithmetic_u128_shift_left_static_1" => Some(Box::new(SShlU128::<1>)),
        "tasmlib_arithmetic_u128_shift_left_static_2" => Some(Box::new(SShlU128::<2>)),
        "tasmlib_arithmetic_u128_shift_left_static_3" => Some(Box::new(SShlU128::<3>)),
        "tasmlib_arithmetic_u128_shift_left_static_4" => Some(Box::new(SShlU128::<4>)),
        "tasmlib_arithmetic_u128_shift_left_static_5" => Some(Box::new(SShlU128::<5>)),
        "tasmlib_arithmetic_u128_shift_left_static_6" => Some(Box::new(SShlU128::<6>)),
        "tasmlib_arithmetic_u128_shift_left_static_7" => Some(Box::new(SShlU128::<7>)),
        "tasmlib_arithmetic_u128_shift_left_static_8" => Some(Box::new(SShlU128::<8>)),
        "tasmlib_arithmetic_u128_shift_left_static_9" => Some(Box::new(SShlU128::<9>)),
        "tasmlib_arithmetic_u128_shift_left_static_10" => Some(Box::new(SShlU128::<10>)),
        "tasmlib_arithmetic_u128_shift_left_static_11" => Some(Box::new(SShlU128::<11>)),
        "tasmlib_arithmetic_u128_shift_left_static_12" => Some(Box::new(SShlU128::<12>)),
        "tasmlib_arithmetic_u128_shift_left_static_13" => Some(Box::new(SShlU128::<13>)),
        "tasmlib_arithmetic_u128_shift_left_static_14" => Some(Box::new(SShlU128::<14>)),
        "tasmlib_arithmetic_u128_shift_left_static_15" => Some(Box::new(SShlU128::<15>)),
        "tasmlib_arithmetic_u128_shift_left_static_16" => Some(Box::new(SShlU128::<16>)),
        "tasmlib_arithmetic_u128_shift_left_static_17" => Some(Box::new(SShlU128::<17>)),
        "tasmlib_arithmetic_u128_shift_left_static_18" => Some(Box::new(SShlU128::<18>)),
        "tasmlib_arithmetic_u128_shift_left_static_19" => Some(Box::new(SShlU128::<19>)),
        "tasmlib_arithmetic_u128_shift_left_static_20" => Some(Box::new(SShlU128::<20>)),
        "tasmlib_arithmetic_u128_shift_left_static_21" => Some(Box::new(SShlU128::<21>)),
        "tasmlib_arithmetic_u128_shift_left_static_22" => Some(Box::new(SShlU128::<22>)),
        "tasmlib_arithmetic_u128_shift_left_static_23" => Some(Box::new(SShlU128::<23>)),
        "tasmlib_arithmetic_u128_shift_left_static_24" => Some(Box::new(SShlU128::<24>)),
        "tasmlib_arithmetic_u128_shift_left_static_25" => Some(Box::new(SShlU128::<25>)),
        "tasmlib_arithmetic_u128_shift_left_static_26" => Some(Box::new(SShlU128::<26>)),
        "tasmlib_arithmetic_u128_shift_left_static_27" => Some(Box::new(SShlU128::<27>)),
        "tasmlib_arithmetic_u128_shift_left_static_28" => Some(Box::new(SShlU128::<28>)),
        "tasmlib_arithmetic_u128_shift_left_static_29" => Some(Box::new(SShlU128::<29>)),
        "tasmlib_arithmetic_u128_shift_left_static_30" => Some(Box::new(SShlU128::<30>)),
        "tasmlib_arithmetic_u128_shift_left_static_31" => Some(Box::new(SShlU128::<31>)),
        "tasmlib_arithmetic_u128_shift_left_static_32" => Some(Box::new(SShlU128::<32>)),

        "tasmlib_arithmetic_u128_shift_right_static_1" => Some(Box::new(SShrU128::<1>)),
        "tasmlib_arithmetic_u128_shift_right_static_2" => Some(Box::new(SShrU128::<2>)),
        "tasmlib_arithmetic_u128_shift_right_static_3" => Some(Box::new(SShrU128::<3>)),
        "tasmlib_arithmetic_u128_shift_right_static_4" => Some(Box::new(SShrU128::<4>)),
        "tasmlib_arithmetic_u128_shift_right_static_5" => Some(Box::new(SShrU128::<5>)),
        "tasmlib_arithmetic_u128_shift_right_static_6" => Some(Box::new(SShrU128::<6>)),
        "tasmlib_arithmetic_u128_shift_right_static_7" => Some(Box::new(SShrU128::<7>)),
        "tasmlib_arithmetic_u128_shift_right_static_8" => Some(Box::new(SShrU128::<8>)),
        "tasmlib_arithmetic_u128_shift_right_static_9" => Some(Box::new(SShrU128::<9>)),
        "tasmlib_arithmetic_u128_shift_right_static_10" => Some(Box::new(SShrU128::<10>)),
        "tasmlib_arithmetic_u128_shift_right_static_11" => Some(Box::new(SShrU128::<11>)),
        "tasmlib_arithmetic_u128_shift_right_static_12" => Some(Box::new(SShrU128::<12>)),
        "tasmlib_arithmetic_u128_shift_right_static_13" => Some(Box::new(SShrU128::<13>)),
        "tasmlib_arithmetic_u128_shift_right_static_14" => Some(Box::new(SShrU128::<14>)),
        "tasmlib_arithmetic_u128_shift_right_static_15" => Some(Box::new(SShrU128::<15>)),
        "tasmlib_arithmetic_u128_shift_right_static_16" => Some(Box::new(SShrU128::<16>)),
        "tasmlib_arithmetic_u128_shift_right_static_17" => Some(Box::new(SShrU128::<17>)),
        "tasmlib_arithmetic_u128_shift_right_static_18" => Some(Box::new(SShrU128::<18>)),
        "tasmlib_arithmetic_u128_shift_right_static_19" => Some(Box::new(SShrU128::<19>)),
        "tasmlib_arithmetic_u128_shift_right_static_20" => Some(Box::new(SShrU128::<20>)),
        "tasmlib_arithmetic_u128_shift_right_static_21" => Some(Box::new(SShrU128::<21>)),
        "tasmlib_arithmetic_u128_shift_right_static_22" => Some(Box::new(SShrU128::<22>)),
        "tasmlib_arithmetic_u128_shift_right_static_23" => Some(Box::new(SShrU128::<23>)),
        "tasmlib_arithmetic_u128_shift_right_static_24" => Some(Box::new(SShrU128::<24>)),
        "tasmlib_arithmetic_u128_shift_right_static_25" => Some(Box::new(SShrU128::<25>)),
        "tasmlib_arithmetic_u128_shift_right_static_26" => Some(Box::new(SShrU128::<26>)),
        "tasmlib_arithmetic_u128_shift_right_static_27" => Some(Box::new(SShrU128::<27>)),
        "tasmlib_arithmetic_u128_shift_right_static_28" => Some(Box::new(SShrU128::<28>)),
        "tasmlib_arithmetic_u128_shift_right_static_29" => Some(Box::new(SShrU128::<29>)),
        "tasmlib_arithmetic_u128_shift_right_static_30" => Some(Box::new(SShrU128::<30>)),
        "tasmlib_arithmetic_u128_shift_right_static_31" => Some(Box::new(SShrU128::<31>)),
        "tasmlib_arithmetic_u128_shift_right_static_32" => Some(Box::new(SShrU128::<32>)),

        // i128
        "tasmlib_arithmetic_i128_lt" => Some(Box::new(i128::lt::Lt)),
        "tasmlib_arithmetic_i128_shift_right" => Some(Box::new(i128::shift_right::ShiftRight)),

        // Hashing
        "tasmlib_hashing_absorb_multiple" => {
            Some(Box::new(hashing::absorb_multiple::AbsorbMultiple))
        }
        "tasmlib_hashing_merkle_root" => Some(Box::new(hashing::merkle_root::MerkleRoot)),
        "tasmlib_hashing_merkle_root_from_xfes" => Some(Box::new(
            hashing::merkle_root_from_xfes::MerkleRootFromXfes,
        )),
        "tasmlib_hashing_merkle_step_mem_u64_index" => {
            Some(Box::new(hashing::merkle_step_mem_u64_index::MerkleStepMemU64Index))
        },
        "tasmlib_hashing_merkle_step_u64_index" => {
            Some(Box::new(hashing::merkle_step_u64_index::MerkleStepU64Index))
        }
        "tasmlib_hashing_merkle_verify" => Some(Box::new(hashing::merkle_verify::MerkleVerify)),

        // Hashing -> algebraic hasher trait
        "tasmlib_hashing_algebraic_hasher_hash_varlen" => {
            Some(Box::new(algebraic_hasher::hash_varlen::HashVarlen))
        }
        "tasmlib_hashing_algebraic_hasher_sample_indices" => {
            Some(Box::new(algebraic_hasher::sample_indices::SampleIndices))
        }
        "tasmlib_hashing_algebraic_hasher_sample_scalars" => {
            Some(Box::new(algebraic_hasher::sample_scalars::SampleScalars))
        }

        // Hashing -> Sponge hasher trait
        "tasmlib_hashing_sponge_hasher_init" => Some(Box::new(sponge_hasher::init::Init)),
        "tasmlib_hashing_sponge_hasher_absorb" => Some(Box::new(sponge_hasher::absorb::Absorb)),
        "tasmlib_hashing_sponge_hasher_squeeze" => Some(Box::new(sponge_hasher::squeeze::Squeeze)),
        "tasmlib_hashing_sponge_hasher_pad_and_absorb_all" => {
            Some(Box::new(sponge_hasher::pad_and_absorb_all::PadAndAbsorbAll))
        }

        // io
        "tasmlib_io_read_secin___bool" => Some(Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___u32" => Some(Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___u64" => Some(Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___u128" => Some(Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___bfe" => Some(Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___xfe" => Some(Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::SecretIn,
        })),
        "tasmlib_io_read_secin___digest" => Some(Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::SecretIn,
        })),

        "tasmlib_io_read_stdin___bool" => Some(Box::new(ReadInput {
            data_type: DataType::Bool,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___u32" => Some(Box::new(ReadInput {
            data_type: DataType::U32,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___u64" => Some(Box::new(ReadInput {
            data_type: DataType::U64,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___u128" => Some(Box::new(ReadInput {
            data_type: DataType::U128,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___bfe" => Some(Box::new(ReadInput {
            data_type: DataType::Bfe,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___xfe" => Some(Box::new(ReadInput {
            data_type: DataType::Xfe,
            input_source: InputSource::StdIn,
        })),
        "tasmlib_io_read_stdin___digest" => Some(Box::new(ReadInput {
            data_type: DataType::Digest,
            input_source: InputSource::StdIn,
        })),

        "tasmlib_io_write_to_stdout___bool" => Some(Box::new(WriteToStdout {
            data_type: DataType::Bool,
        })),
        "tasmlib_io_write_to_stdout___u32" => Some(Box::new(WriteToStdout {
            data_type: DataType::U32,
        })),
        "tasmlib_io_write_to_stdout___u64" => Some(Box::new(WriteToStdout {
            data_type: DataType::U64,
        })),
        "tasmlib_io_write_to_stdout___u128" => Some(Box::new(WriteToStdout {
            data_type: DataType::U128,
        })),
        "tasmlib_io_write_to_stdout___bfe" => Some(Box::new(WriteToStdout {
            data_type: DataType::Bfe,
        })),
        "tasmlib_io_write_to_stdout___xfe" => Some(Box::new(WriteToStdout {
            data_type: DataType::Xfe,
        })),
        "tasmlib_io_write_to_stdout___digest" => Some(Box::new(WriteToStdout {
            data_type: DataType::Digest,
        })),

        // lists
        "tasmlib_list_get_element___bool" => Some(Box::new(list::get::Get::new(DataType::Bool))),
        "tasmlib_list_get_element___u32" => Some(Box::new(list::get::Get::new(DataType::U32))),
        "tasmlib_list_get_element___u64" => Some(Box::new(list::get::Get::new(DataType::U64))),
        "tasmlib_list_get_element___bfe" => Some(Box::new(list::get::Get::new(DataType::Bfe))),
        "tasmlib_list_get_element___xfe" => Some(Box::new(list::get::Get::new(DataType::Xfe))),
        "tasmlib_list_get_element___digest" => {
            Some(Box::new(list::get::Get::new(DataType::Digest)))
        }

        "tasmlib_list_pop___bool" => Some(Box::new(list::pop::Pop::new(DataType::Bool))),
        "tasmlib_list_pop___u32" => Some(Box::new(list::pop::Pop::new(DataType::U32))),
        "tasmlib_list_pop___u64" => Some(Box::new(list::pop::Pop::new(DataType::U64))),
        "tasmlib_list_pop___bfe" => Some(Box::new(list::pop::Pop::new(DataType::Bfe))),
        "tasmlib_list_pop___xfe" => Some(Box::new(list::pop::Pop::new(DataType::Xfe))),
        "tasmlib_list_pop___digest" => Some(Box::new(list::pop::Pop::new(DataType::Digest))),

        "tasmlib_list_push___bool" => Some(Box::new(list::push::Push::new(DataType::Bool))),
        "tasmlib_list_push___u32" => Some(Box::new(list::push::Push::new(DataType::U32))),
        "tasmlib_list_push___u64" => Some(Box::new(list::push::Push::new(DataType::U64))),
        "tasmlib_list_push___bfe" => Some(Box::new(list::push::Push::new(DataType::Bfe))),
        "tasmlib_list_push___xfe" => Some(Box::new(list::push::Push::new(DataType::Xfe))),
        "tasmlib_list_push___digest" => Some(Box::new(list::push::Push::new(DataType::Digest))),

        "tasmlib_list_set_element___bool" => Some(Box::new(list::set::Set::new(DataType::Bool))),
        "tasmlib_list_set_element___u32" => Some(Box::new(list::set::Set::new(DataType::U32))),
        "tasmlib_list_set_element___u64" => Some(Box::new(list::set::Set::new(DataType::U64))),
        "tasmlib_list_set_element___bfe" => Some(Box::new(list::set::Set::new(DataType::Bfe))),
        "tasmlib_list_set_element___xfe" => Some(Box::new(list::set::Set::new(DataType::Xfe))),
        "tasmlib_list_set_element___digest" => {
            Some(Box::new(list::set::Set::new(DataType::Digest)))
        }

        "tasmlib_list_new" => Some(Box::new(list::new::New)),
        "tasmlib_list_length" => Some(Box::new(list::length::Length)),
        "tasmlib_list_set_length" => Some(Box::new(list::set_length::SetLength)),

        "tasmlib_list_multiset_equality_digests" => Some(Box::new(
            list::multiset_equality_digests::MultisetEqualityDigests,
        )),
        "tasmlib_list_range" => Some(Box::new(list::range::Range)),

        // MMR
        "tasmlib_mmr_bag_peaks" => Some(Box::new(BagPeaks)),
        "tasmlib_mmr_calculate_new_peaks_from_append" => {
            Some(Box::new(CalculateNewPeaksFromAppend))
        }
        "tasmlib_mmr_calculate_new_peaks_from_leaf_mutation" => {
            Some(Box::new(MmrCalculateNewPeaksFromLeafMutationMtIndices))
        }
        "tasmlib_mmr_leaf_index_to_mt_index_and_peak_index" => {
            Some(Box::new(MmrLeafIndexToMtIndexAndPeakIndex))
        }
        "tasmlib_mmr_verify_from_memory" => Some(Box::new(MmrVerifyFromMemory)),
        "tasmlib_mmr_verify_from_secret_in_leaf_index_on_stack" => {
            Some(Box::new(MmrVerifyFromSecretInLeafIndexOnStack))
        }
        "tasmlib_mmr_verify_from_secret_in_secret_leaf_index" => {
            Some(Box::new(MmrVerifyFromSecretInSecretLeafIndex))
        }
        "tasm_lib_mmr_verify_mmr_successor" => {
            Some(Box::new(VerifyMmrSuccessor))
        }

        // recufy
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_merkleroot" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::MerkleRoot,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainmainrow" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::OutOfDomainMainRow,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainauxrow" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::OutOfDomainAuxRow,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_outofdomainquotientsegments" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::OutOfDomainQuotientSegments,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_authenticationstructure" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::AuthenticationStructure,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_mastermaintablerows" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::MasterMainTableRows,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_masterauxtablerows" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::MasterAuxTableRows,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_log2paddedheight" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::Log2PaddedHeight,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_quotientsegmentselements" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::QuotientSegmentsElements,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_fricodeword" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::FriCodeword,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_fripolynomial" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::FriPolynomial,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_dequeue_next_as_friresponse" => {
            Some(Box::new(DequeueNextAs {
                proof_item: ProofItemVariant::FriResponse,
            }))
        }
        "tasmlib_verifier_vm_proof_iter_drop" => {
            Some(Box::new(verifier::vm_proof_iter::drop::Drop))
        }
        "tasmlib_verifier_vm_proof_iter_new" => {
            Some(Box::new(verifier::vm_proof_iter::new::New))
        }

        "tasmlib_verifier_read_and_verify_own_program_digest_from_std_in" => {
            Some(Box::new(ReadAndVerifyOwnProgramDigestFromStdIn))
        }
        "tasmlib_verifier_challenges_new_empty_input_and_output_59_4" => {
            let num_challenges_to_sample = Challenges::SAMPLE_COUNT;
            let num_challenges_to_compute = Challenges::COUNT - num_challenges_to_sample;
            assert_eq!(59, num_challenges_to_sample);
            assert_eq!(4, num_challenges_to_compute);
            let challenge_snippet = NewEmptyInputAndOutput::new(
                num_challenges_to_sample,
                num_challenges_to_compute,
                challenges::shared::conventional_challenges_pointer(),
            );
            Some(Box::new(challenge_snippet))
        }

        "tasmlib_verifier_fri_barycentric_evaluation" => {
            Some(Box::new(fri::barycentric_evaluation::BarycentricEvaluation))
        }
        "tasmlib_verifier_fri_derive_from_stark" => {
            Some(Box::new(fri::derive_from_stark::DeriveFriFromStark{stark: Stark::default()}))
        }
        "tasmlib_verifier_fri_number_of_rounds" => {
            Some(Box::new(fri::number_of_rounds::NumberOfRounds))
        }
        "tasmlib_verifier_fri_verify_fri_authentication_paths" => {
            Some(Box::new(fri::verify_fri_authentication_paths::VerifyFriAuthenticationPaths))
        }

        "tasmlib_verifier_master_table_air_constraint_evaluation" => Some(Box::new(
            AirConstraintEvaluation::with_conventional_dynamic_memory_layout(),
        )),
        "tasmlib_verifier_master_table_divide_out_zerofiers" => Some(Box::new(DivideOutZerofiers)),
        "tasmlib_verifier_master_table_verify_Main_table_rows" => {
            Some(Box::new(VerifyTableRows::new(ColumnType::Main)))
        }
        "tasmlib_verifier_master_table_verify_Aux_table_rows" => {
            Some(Box::new(VerifyTableRows::new(ColumnType::Aux)))
        }
        "tasmlib_verifier_master_table_verify_Quotient_table_rows" => {
            Some(Box::new(VerifyTableRows::new(ColumnType::Quotient)))
        }

        "tasmlib_verifier_out_of_domain_points" => {
            Some(Box::new(verifier::out_of_domain_points::OutOfDomainPoints))
        }

        "tasmlib_array_inner_product_of_4_xfes" => Some(Box::new(InnerProductOfXfes::new(4))),
        WEIGHTS_QUOTIENTS_INNER_PRODUCT_ENTRYPOINT => {
            Some(Box::new(InnerProductOfXfes::new(NUM_CONSTRAINTS_TVM)))
        }
        "tasmlib_list_horner_evaluation_dynamic_length" => Some(Box::new(
            list::horner_evaluation_dynamic_length::HornerEvaluationDynamicLength,
        )),
        HORNER_EVALUATION_FOR_SUM_OF_EVALUATED_OUT_OF_DOMAIN_QUOTIENT_SEGMENTS_ENTRYPOINT => {
            Some(Box::new(HornerEvaluation::new(NUM_QUOTIENT_SEGMENTS)))
        }
        "tasmlib_verifier_own_program_digest" => Some(Box::new(OwnProgramDigest)),
        "tasmlib_array_inner_product_of_three_rows_with_weights_Bfe_mainrowelem" => Some(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Bfe),
        )),
        "tasmlib_array_inner_product_of_three_rows_with_weights_Xfe_mainrowelem" => Some(Box::new(
            InnerProductOfThreeRowsWithWeights::triton_vm_parameters(MainElementType::Xfe),
        )),
        "tasmlib_verifier_claim_instantiate_fiat_shamir_with_claim" => {
            Some(Box::new(InstantiateFiatShamirWithClaim))
        }
        CHALLENGES_NEW_FROM_DYN_CLAIM => Some(Box::new(NewGenericDynClaim::new(
            Challenges::SAMPLE_COUNT,
            ChallengeId::NUM_DERIVED_CHALLENGES,
            challenges::shared::conventional_challenges_pointer(),
        ))),

        "tasmlib_verifier_eval_arg_compute_terminal_dyn_sized_dynamic_symbols" => Some(Box::new(
            verifier::eval_arg
                ::compute_terminal_dyn_sized_dynamic_symbols::ComputeTerminalDynSizedDynamicSymbols
        )),
        "tasmlib_verifier_eval_arg_compute_terminal_from_digest" => Some(Box::new(
            verifier::eval_arg::compute_terminal_from_digest::ComputeTerminalFromDigestInitialIsOne
        )),

        // memory
        "tasmlib_memory_dyn_malloc" => Some(Box::new(DynMalloc)),
        "tasmlib_memory_memcpy" => Some(Box::new(MemCpy)),

        // FRI
        #[cfg(not(test))]
        "tasmlib_verifier_fri_verify" => {
            Some(Box::new(crate::verifier::fri::verify::FriSnippet {}))
        }

        // structure

        // mutator sets
        "tasmlib_neptune_mutator_set_commit" => Some(Box::new(Commit)),
        "tasmlib_neptune_mutator_get_swbf_indices_1048576_45" => Some(Box::new(GetSwbfIndices {
            window_size: 1048576,
            num_trials: 45,
        })),

        _ => None,
    }
}
