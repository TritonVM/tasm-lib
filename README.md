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
| Name   |  Author(s)  | Last edited | TVM version | Notes | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| ------ | :---------: | ----------: | ----------: | :---: | -----------------: | ---------------------: | ----------------: | -----------------: |
| is_u32 | sword_smith |    20221208 |       0.8.0 |       |                 68 |                     68 |                 0 |                    |

## U32s, size 2
| Name                          |      Author(s)      | Last edited | TVM version |                                   Notes                                   | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| ----------------------------- | :-----------------: | ----------: | ----------: | :-----------------------------------------------------------------------: | -----------------: | ---------------------: | ----------------: | -----------------: |
| incr                          | sshine, sword_smith |    20221208 |       0.8.0 |                                                                           |                  8 |                     20 |                 0 |             $O(1)$ |
| decr                          |     sword_smith     |    20221208 |       0.8.0 |                                                                           |                  8 |                     20 |                 0 |             $O(1)$ |
| add                           |     sword_smith     |    20221208 |       0.8.0 |                                                                           |                150 |                    158 |                 0 |             $O(1)$ |
| sub                           | sshine, sword_smith |    20221208 |       0.9.0 |                                                                           |                 84 |                     92 |                 0 |             $O(1)$ |
| lt                            | sshine, sword_smith |    20221209 |       0.9.0 |                                                                           |                154 |                    311 |                 0 |             $O(1)$ |
| powers_of_two_arithmetic_flat |     sword_smith     |    20221208 |       0.8.0 | Adds 17 to cycle count for each increment of the exponent. Range: 11-1090 |                563 |                   1090 |                 0 |             $O(N)$ |
| powers_of_two_memory          |       sshine        |    20221208 |       0.8.0 |    Memory unsafe (assumes bounded input), exp >= 32 => +3 cycle count     |         12 (+ 165) |             15 (+ 165) |                 0 |             $O(1)$ |
| powers_of_two_static          | sshine, sword_smith |    20221209 |       0.9.0 |                                                                           |                264 |                    264 |                 0 |             $O(1)$ |

## MMR
| Name              |      Author(s)      | Last edited | TVM version |                             Notes                              | Cycle Count common | Cycle Count worst-case | Hash table height | Asymptotic Runtime |
| ----------------- | :-----------------: | ----------: | ----------: | :------------------------------------------------------------: | -----------------: | ---------------------: | ----------------: | -----------------: |
| left_child        | sshine, sword_smith |    20221209 |       0.9.0 |       Expensive because u32 arithmetic and no u32 table        |                354 |                    354 |                 0 |             $O(1)$ |
| right_child       | sshine, sword_smith |    20221209 |       0.9.0 | You can call `decr` for `U32<2>` instead and save a few cycles |                 10 |                     22 |                 0 |             $O(1)$ |
| leftmost_ancestor | sshine, sword_smith |    20221209 |       0.9.0 |    With a `U32` table, a constant time snippet can be made     |              10953 |                  21060 |                 0 |         $O(ln(N))$ |
