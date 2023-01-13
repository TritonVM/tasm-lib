use std::collections::HashMap;

use num::Zero;
use rand::{thread_rng, Rng};
use twenty_first::shared_math::b_field_element::BFieldElement;
use twenty_first::shared_math::other::random_elements;
use twenty_first::shared_math::rescue_prime_digest::Digest;
use twenty_first::shared_math::rescue_prime_regular::{RescuePrimeRegular, DIGEST_LENGTH};
use twenty_first::test_shared::mmr::get_archival_mmr_from_digests;
use twenty_first::util_types::mmr::archival_mmr::ArchivalMmr;
use twenty_first::util_types::mmr::mmr_accumulator::MmrAccumulator;
use twenty_first::util_types::mmr::mmr_membership_proof::MmrMembershipProof;
use twenty_first::util_types::mmr::mmr_trait::Mmr;

use crate::arithmetic::u32::is_odd::U32IsOdd;
use crate::arithmetic::u64::div2_u64::Div2U64;
use crate::arithmetic::u64::eq_u64::EqU64;
use crate::hashing::eq_digest::EqDigest;
use crate::hashing::load_auth_path_from_secret_in::{self, LoadAuthPathFromSecretIn};
use crate::hashing::swap_digest::SwapDigest;
use crate::library::Library;
use crate::list::u32::get::Get;
use crate::snippet::{NewSnippet, Snippet};
use crate::{get_init_tvm_stack, rust_shadowing_helper_functions, ExecutionState};

use super::leaf_index_to_mt_index::MmrLeafIndexToMtIndexAndPeakIndex;
use super::verify_from_memory::{self, MmrVerifyFromMemory};

/// First load from secret-in, then verify from memory
pub struct MmrLoadFromSecretInThenVerify();

impl NewSnippet for MmrLoadFromSecretInThenVerify {
    fn inputs() -> Vec<&'static str> {
        vec![]
    }

    fn outputs() -> Vec<&'static str> {
        vec![
            "auth_path_pointer",
            "leaf_index_hi",
            "leaf_index_lo",
            "validation_result",
        ]
    }

    fn crash_conditions() -> Vec<&'static str> {
        todo!()
    }

    fn gen_input_states() -> Vec<ExecutionState> {
        todo!()
    }
}

impl Snippet for MmrLoadFromSecretInThenVerify {
    fn stack_diff() -> isize {
        -6
    }

    fn entrypoint() -> &'static str {
        "verify_load_from_secret_in"
    }

    fn function_body(library: &mut Library) -> String {
        let entrypoint = Self::entrypoint();
        let load_auth_path_from_secret_in = library.import::<LoadAuthPathFromSecretIn>();
        let verify_from_memory = library.import::<MmrVerifyFromMemory>();

        format!(
            "
                // BEFORE: _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)]
                // AFTER: _ *auth_path leaf_index_hi leaf_index_lo validation_result
                {entrypoint}:
                    call {load_auth_path_from_secret_in}
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path

                    // prepare stack for `verify_from_memory`
                    dup7 dup7 dup12 dup8 dup8 dup8 dup8 dup8
                    // _ *peaks leaf_count_hi leaf_count_lo leaf_index_hi leaf_index_lo [digest (leaf_digest)] *auth_path ...
                    //  leaf_index_hi leaf_index_lo *peaks [digest (leaf_digest)]
                "
        )
    }

    fn rust_shadowing(
        stack: &mut Vec<BFieldElement>,
        std_in: Vec<BFieldElement>,
        secret_in: Vec<BFieldElement>,
        memory: &mut HashMap<BFieldElement, BFieldElement>,
    ) {
        todo!()
    }
}
