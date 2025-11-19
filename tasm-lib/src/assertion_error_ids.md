# Assertion Error IDs

Triton VM's instructions `assert` and `assert_vector` allow supplying context to help debugging.
One piece of context is an error ID.
For example, you can write the following Triton assembly: `push 2 assert error_id 42`.
The assertion above will fail during execution.
Triton VM will return a rust error from which the specified error ID can be queried.

This file acts as a registry for error IDs.
Its purpose is to keep error IDs within `tasm-lib` unique, and to allow looking them up.
In order to register a new error ID, simply add a row to the table below.
Feel free to grab a multiple of 10 all at once.
This way, if your assembly snippet starts asserting more stuff, you don't have to come back here as
often.

## Registry

| Error IDs | Snippet                                                                                     |
|----------:|:--------------------------------------------------------------------------------------------|
|     0..10 | [`MerkleVerify`](hashing/merkle_verify.rs)                                                  |
|    10..20 | [`MmrVerifyFromSecretInLeafIndexOnStack`](mmr/verify_from_secret_in_leaf_index_on_stack.rs) |
|    20..30 | [`MmrVerifyFromSecretInSecretLeafIndex`](mmr/verify_from_secret_in_secret_leaf_index.rs)    |
|    30..40 | [`VerifyFriAuthenticationPaths`](verifier/fri/verify_fri_authentication_paths.rs)           |
|    40..50 | [`VerifyTableRows`](verifier/master_table/verify_table_rows.rs)                             |
|    50..60 | [`Drop`](verifier/vm_proof_iter/drop.rs)                                                    |
|    60..70 | [`MemCpy`](memory/memcpy.rs)                                                                |
|    70..80 | [`DynMalloc`](memory/dyn_malloc.rs)                                                         |
|    80..90 | [`SplitOff`](list/split_off.rs)                                                             |
|   90..100 | [`MerkleRootFromXfes`](hashing/merkle_root_from_xfes.rs)                                    |
|  100..110 | [`SafeMulU64`](arithmetic/u64/safe_mul.rs)                                                  |
|  110..120 | [`u64::Decr`](arithmetic/u64/decr.rs)                                                       |
|  120..130 | [`Safepow`](arithmetic/u32/safe_pow.rs)                                                     |
|  130..140 | [`NextPowerOfTwo`](arithmetic/u32/next_power_of_two.rs)                                     |
|  140..150 | [`PrimitiveRootOfUnity`](arithmetic/bfe/primitive_root_of_unity.rs)                         |
|  150..170 | [`VerifyMmrSuccessor`](mmr/verify_mmr_successor.rs)                                         |
|  170..180 | [`SafeAddU128`](arithmetic/u128/safe_add.rs)                                                |
|  180..200 | `TasmObject` derive macro                                                                   |
|  200..210 | [`TasmObject` for `Option<T>`](structure/manual_tasm_object_implementations.rs)             |
|  210..220 | [`TasmObject` for `Vec<T>`](structure/manual_tasm_object_implementations.rs)                |
|  220..230 | [`TasmObject` for `(T, S)`](structure/manual_tasm_object_implementations.rs)                |
|  230..300 | [`StarkVerify`](verifier/stark_verify.rs)                                                   |
|  300..310 | [`vm_proof_iter::New`](verifier/vm_proof_iter/new.rs)                                       |
|  310..320 | [`u64::Add`](arithmetic/u64/add.rs)                                                         |
|  320..330 | [`i128::ShiftRight`](arithmetic/i128/shift_right.rs)                                        |
|  330..340 | [`u64::ShiftRight`](arithmetic/u64/shift_right.rs)                                          |
|  340..350 | [`u64::Sub`](arithmetic/u64/sub.rs)                                                         |
|  350..360 | [`MmrLeafIndexToMtIndexAndPeakIndex`](mmr/leaf_index_to_mt_index_and_peak_index.rs)         |
|  360..370 | [`u64::Pow2`](arithmetic/u64/pow2.rs)                                                       |
|  370..380 | [`u64::ShiftLeft`](arithmetic/u64/shift_left.rs)                                            |
|  380..390 | [`list::get`](list/get.rs)                                                                  |
|  390..400 | [`list::set`](list/set.rs)                                                                  |
|  400..410 | [`list::pop`](list/pop.rs)                                                                  |
|  410..420 | [`list::push`](list/push.rs)                                                                |
|  420..430 | [`u64::DivMod`](arithmetic/u64/div_mod.rs)                                                  |
|  430..440 | [`MerkleRoot`](hashing/merkle_root.rs)                                                      |
|  440..450 | [`u64::Incr`](arithmetic/u64/incr.rs)                                                       |
|  450..460 | [`u32::SafeAdd`](arithmetic/u32/safe_add.rs)                                                |
|  460..470 | [`u32::SafeMul`](arithmetic/u32/safe_mul.rs)                                                |
|  470..480 | [`u32::SafeSub`](arithmetic/u32/safe_sub.rs)                                                |
|  480..490 | [`u32::ShiftLeft`](arithmetic/u32/shift_left.rs)                                            |
|  490..500 | [`u32::ShiftRight`](arithmetic/u32/shift_right.rs)                                          |
|  500..520 | [`u128::SafeMul`](arithmetic/u128/safe_mul.rs)                                              |
|  520..530 | [`u128::Sub`](arithmetic/u128/sub.rs)                                                       |
|  530..540 | [`u128::ShiftLeft`](arithmetic/u128/shift_left.rs)                                          |
|  540..550 | [`u128::ShiftRight`](arithmetic/u128/shift_right.rs)                                        |
|  550..560 | [`list::Range`](list/range.rs)                                                              |
|  560..570 | [`BagPeaks`](mmr/bag_peaks.rs)                                                              |
|  570..580 | [`u160::SafeAdd`](arithmetic/u160/safe_add.rs)                                              |
|  580..590 | [`u160::SafeMul`](arithmetic/u160/safe_mul.rs)                                              |
|  590..600 | [`u160::DivMod`](arithmetic/u160/div_mod.rs)                                                |
|  600..610 | [`u192::SafeAdd`](arithmetic/u192/safe_add.rs)                                              |
