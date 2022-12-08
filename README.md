# tasm-snippets

This repo is a place to put your TASM snippets and for getting an overview of the run times of the consensus and the recufi code.

Please place the code in the appropriate folders.

Please use this an append-only list. So if the cycle count changes because `triton-vm` is updated, or you find a better way of solving a problem,
then add a new line to the appropriate table, add a comment to the two relevant lines, and add a counter to the algorithm name, or update the
TVM version when appropriate.

## Consensus Code
| Name | Author(s) | Last edited | TVM version | Tested and Rust-shadowed | Notes | Cycle Count |
| ---- | :-------: | ----------: | ----------: | -----------------------: | :---: | ----------: |

## Recufi Code
| Name | Author(s) | Last edited | TVM version | Tested and Rust-shadowed | Notes | Cycle Count |
| ---- | :-------: | ----------: | ----------: | -----------------------: | :---: | ----------: |

## Other Code Snippets
| Name    |      Author(s)      | Last edited | TVM version | Notes | Cycle Count | Hash table height |
| ------- | :-----------------: | ----------: | ----------: | :---: | ----------: | ----------------: |
| bfe_add |     sword_smith     |    20221208 |       0.8.0 |       |           3 |                 0 |
| hash    | sshine, sword_smith |    20221208 |       0.8.0 |       |           0 |                 9 |

## u32
| Name   |  Author(s)  | Last edited | TVM version | Notes | Cycle Count common | Cycle Count worst-case | Hash table height |
| ------ | :---------: | ----------: | ----------: | :---: | -----------------: | ---------------------: | ----------------: |
| is_u32 | sword_smith |    20221208 |       0.8.0 |       |                 68 |                     68 |                 0 |

## U32s, size 2
| Name                          |      Author(s)      | Last edited | TVM version |                                   Notes                                    | Cycle Count common | Cycle Count worst-case | Hash table height |
| ----------------------------- | :-----------------: | ----------: | ----------: | :------------------------------------------------------------------------: | -----------------: | ---------------------: | ----------------: |
| incr                          | sshine, sword_smith |    20221208 |       0.8.0 |                                                                            |                  8 |                     20 |                 0 |
| decr                          |     sword_smith     |    20221208 |       0.8.0 |                                                                            |                  8 |                     20 |                 0 |
| add                           |     sword_smith     |    20221208 |       0.8.0 |                                                                            |                150 |                    158 |                 0 |
| powers_of_two_arithmetic_flat |     sword_smith     |    20221208 |       0.8.0 | Adds 17 to cycle count for each increment of the exponent. Range: 11-1090  |                563 |                   1090 |                 0 |
| powers_of_two_memory          |       sshine        |    20221208 |       0.8.0 | Assumes bounded input, writes table to memory, exp >= 32 => +3 cycle count |                177 |                    180 |                 0 |
