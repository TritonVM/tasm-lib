# tasm-lib

This repository contains a collection of functions written in Triton VM assembly (tasm).

There are two big projects to be written in tasm:
 - The consensus logic of [Neptune](https://neptune.cash/)
 - The recursive Triton VM STARK verifier (recufier)

This repository helps get an overview of the run times of the consensus code and the recufier code.

Please place code in the appropriate directories, or create them if non-existent.

The lists below keep track of improvements to individual algorithms. If the
cycle count changes because Triton VM is updated, or you find a better way of
solving a problem, then add a new line to the appropriate table, add a comment
to the two relevant lines, and add a counter to the algorithm name, or update
the TVM version when appropriate.

## Consensus Code
| Name | Author(s) | Last edited | TVM version | Tested and Rust-shadowed | Notes | Cycle Count | Asymptotic Runtime |
| ---- | :-------: | ----------: | ----------: | -----------------------: | :---: | ----------: | -----------------: |

## Recufi Code
| Name | Author(s) | Last edited | TVM version | Tested and Rust-shadowed | Notes | Cycle Count | Asymptotic Runtime |
| ---- | :-------: | ----------: | ----------: | -----------------------: | :---: | ----------: | -----------------: |

## Other Code Snippets
| Name    |      Author(s)      | Last edited | TVM version | Notes | Cycle Count | Hash table height | Asymptotic Runtime |
| ------- | :-----------------: | ----------: | ----------: | :---: | ----------: | ----------------: | -----------------: |
| bfe_add |     sword_smith     |    20221208 |       0.8.0 |       |           3 |                 0 |                    |
| hash    | sshine, sword_smith |    20221208 |       0.8.0 |       |           0 |                 9 |                    |

## u32
| Name        |  Author(s)  | Last edited | TVM version | Notes | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| ----------- | :---------: | ----------: | ----------: | :---: | -----------------: | ---------------------: | ----------------: | -----------------: |
| is_u32      | sword_smith |    20221208 |       0.8.0 |       |                 68 |                     68 |                 0 |                    |
| log_2_floor | sword_smith |    20221223 |       0.9.0 |       |                180 |                    354 |                 0 |             $O(N)$ |

## u64 (2 x u32)
| Name                 |      Author(s)      | Last edited | TVM version |                                   Notes                                   | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| -------------------- | :-----------------: | ----------: | ----------: | :-----------------------------------------------------------------------: | -----------------: | ---------------------: | ----------------: | -----------------: |
| incr_u64             | sshine, sword_smith |    20221208 |       0.8.0 |                                                                           |                  8 |                     20 |                 0 |             $O(1)$ |
| decr_u64             |     sword_smith     |    20221208 |       0.8.0 |                                                                           |                  8 |                     20 |                 0 |             $O(1)$ |
| add_u64              |     sword_smith     |    20221208 |       0.8.0 |                                                                           |                150 |                    158 |                 0 |             $O(1)$ |
| sub_u64              | sshine, sword_smith |    20221208 |       0.9.0 |                                                                           |                 84 |                     92 |                 0 |             $O(1)$ |
| lt_u64               | sshine, sword_smith |    20221209 |       0.9.0 |                                                                           |                154 |                    311 |                 0 |             $O(1)$ |
| pow2_arithmetic_flat |     sword_smith     |    20221208 |       0.8.0 | Adds 17 to cycle count for each increment of the exponent. Range: 11-1090 |                563 |                   1090 |                 0 |             $O(N)$ |
| pow2_memory_unsafe   |       sshine        |    20221208 |       0.8.0 |    Memory unsafe (assumes bounded input), exp >= 32 => +3 cycle count     |         12 (+ 165) |             15 (+ 165) |                 0 |             $O(1)$ |
| pow2_static          | sshine, sword_smith |    20221209 |       0.9.0 |                                                                           |                264 |                    264 |                 0 |             $O(1)$ |
| log2_floor_u64       |     sword_smith     |    20221223 |       0.9.0 |                                                                           |                193 |                    369 |                 0 |         $O(ln(N))$ |
| lt_standard_u64      |       sshine        |    20230111 |      0.11.0 |

## Lists (max length: `u32::MAX`)
| Name           |  Author(s)  | Last edited | TVM version |                                 Notes                                  | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| -------------- | :---------: | ----------: | ----------: | :--------------------------------------------------------------------: | -----------------: | ---------------------: | ----------------: | -----------------: |
| length (long)  | sword_smith |    20221220 |       0.9.0 |                                                                        |                  6 |                      6 |                 0 |             $O(1)$ |
| length (short) | sword_smith |    20221220 |       0.9.0 |                                                                        |                  4 |                      4 |                 0 |             $O(1)$ |
| push           | sword_smith |    20221220 |       0.9.0 | parameterized over length of list elements, $N$, 1-16 B field elements |    13 + $5\cdot N$ |        13 + $5\cdot N$ |                 0 |             $O(1)$ |
| pop            | sword_smith |    20221220 |       0.9.0 | parameterized over length of list elements, $N$, 1-16 B field elements |    17 + $5\cdot N$ |        17 + $5\cdot N$ |                 0 |             $O(1)$ |

## MMRA
An MMR is in this context always an MMR accumulator as used in the [`twenty-first`](https://github.com/Neptune-Crypto/twenty-first/blob/master/twenty-first/src/util_types/mmr/mmr_accumulator.rs). On the stack it is represented as `_ leaf_count peaks` where `leaf_count` is a u64 (two words of `u32` values) and where `peaks` is a pointer into memory where the peaks list is stored.
| Name                                   |      Author(s)      | Last edited | TVM version |                             Notes                              |  Cycle Count common | Cycle Count worst-case |   Hash table height | Asymptotic Runtime |
| -------------------------------------- | :-----------------: | ----------: | ----------: | :------------------------------------------------------------: | ------------------: | ---------------------: | ------------------: | -----------------: |
| left_child                             | sshine, sword_smith |    20221209 |       0.9.0 |       Expensive because u32 arithmetic and no u32 table        |                 354 |                    354 |                   0 |             $O(1)$ |
| right_child                            | sshine, sword_smith |    20221209 |       0.9.0 | You can call `decr` for `U32<2>` instead and save a few cycles |                  10 |                     22 |                   0 |             $O(1)$ |
| leftmost_ancestor (v0, deprecated)     | sshine, sword_smith |    20221209 |       0.9.0 |    With a `U32` table, a constant time snippet can be made     |               10953 |                  21060 |                   0 |         $O(ln(N))$ |
| leftmost_ancestor                      |     sword_smith     |    20230105 |      0.11.0 |                                                                |                 400 |                    649 |                   0 |         $O(ln(N))$ |
| right_child_and_height                 |     sword_smith     |    20221216 |       0.9.0 |        Cycle count is dominated by `leftmost_ancestor`         |                2500 |                   5000 |                   0 |         $O(ln(N))$ |
| right_ancestor_count_and_own_height    |     sword_smith     |    20230104 |      0.11.0 |                                                                |               10000 |                  40389 |                   0 |         $O(ln(N))$ |
| count_leaves                           |     sword_smith     |    20221216 |       0.9.0 |       Not really needed as leaf_count is a field in MMRA       |                   3 |                      3 |                   0 |             $O(1)$ |
| get_height_from_data_index             |     sword_smith     |    20221223 |       0.9.0 |         Needed by append through `non_leaf_nodes_left`         |                 210 |                    379 |                     |         $O(ln(N))$ |
| non_leaf_nodes_left_old                |     sword_smith     |    20221223 |      0.11.0 |      Needed by append through `data_index_to_node_index`       |               23845 |                  46929 |                   0 |         $O(ln(N))$ |
| non_leaf_nodes_left_using_and          |     sword_smith     |    20230106 |      0.11.0 |       Should be better than above version with u32 table       |               33991 |                  67155 |                   0 |         $O(ln(N))$ |
| data_index_to_node_index               |     sword_smith     |    20221223 |      0.11.0 |            Used by calculate_new_peaks_from_append             |               23845 |                  46929 |                   0 |                  ? |
| calculate_new_peaks_from_append        |     sword_smith     |    20230105 |      0.11.0 |                  Main function for appending                   | (height = 32) 65954 |   (height = 63) 131682 |  (height = 32) 3969 |         $O(ln(N))$ |
| leaf_index_to_mt_index_and_peak_index  |     sword_smith     |    20230109 |      0.11.0 |      Needed by mutating, very expensive without u32 table      | (height = 32) 42108 |    (height = 63) 78746 |                   0 |         $O(ln(N))$ |
| calculate_new_peaks_from_leaf_mutation | sshine, sword_smith |    20230110 |      0.11.0 |                   Main function for mutating                   | (height = 32) 42176 |    (height = 63) 77663 | (height = 32) 13671 |         $O(ln(N))$ |
| verify                                 |                     |             |             |         Function for verifying an authentication path          |                     |                        |                     |                    |
