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
|   90..100 | [`MerkleRootFromXfesGeneric`](hashing/merkle_root_from_xfes_generic.rs)                     |
|  100..110 | [`SafeMulU64`](arithmetic/u64/safe_mul_u64.rs)                                              |                                              
|  110..120 | [`DecrU64`](arithmetic/u64/decr_u64.rs)                                                     |
|  120..130 | [`Safepow`](arithmetic/u32/safepow.rs)                                                      |                                                      
|  130..140 | [`NextPowerOfTwo`](arithmetic/u32/next_power_of_two.rs)                                     |
|  140..150 | [`PrimitiveRootOfUnity`](arithmetic/bfe/primitive_root_of_unity.rs)                         |
|  150..170 | [`VerifyMmrSuccessor`](mmr/verify_mmr_successor.rs)                                         |
|  170..180 | [`SafeAddU128`](arithmetic/u128/safe_add.rs)                                                |
|  180..200 | `TasmObject` derive macro                                                                   |
|  200..210 | [`TasmObject` for `Option<T>`](structure/manual_tasm_object_implementations.rs)             |
|  210..220 | [`TasmObject` for `Vec<T>`](structure/manual_tasm_object_implementations.rs)                |
|  220..230 | [`TasmObject` for `(T, S)`](structure/manual_tasm_object_implementations.rs)                |
