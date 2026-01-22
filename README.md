# TASM Lib

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![crates.io](https://img.shields.io/crates/v/tasm-lib.svg)](https://crates.io/crates/tasm-lib)
[![GitHub CI](https://github.com/TritonVM/tasm-lib/actions/workflows/main.yml/badge.svg)](https://github.com/TritonVM/tasm-lib/actions)

This repository contains a collection of functions written in [Triton VM](https://triton-vm.org) assembly (“TASM”).
The functions supplied here can be seen as a standard library for Triton VM:
basic functions that are used a lot.

The most intricate, ready-to-use snippet contained here is a
[verifier for the zk-STARK of Triton VM](tasm-lib/src/verifier/stark_verify.rs).
You can use Triton VM to prove correct execution of this verifier.
This achieves recursion and, ultimately, Proof Carrying Data.

## Conventions

### Memory

Triton VM has $p = 2^{64} - 2^{32} + 1$ number of words in its RAM.
Each word is a prime field element in the prime $p$.[^prime]
RAM can be initialized at startup by setting non-determinism (“ND”).

[^prime]: The prime is colloquially called “0xfoi” due to its hexadecimal representation, `0xffffffff00000001`.

Memory is, by convention, divided up into chunks called pages.
All pages, except the last one, have a size of $2^{32}$ words.
The last page has a size of $1$.
Memory consists of $2^{32}$ pages.

| pages                     | region                           | size in words   | category          | purpose                          |
|---------------------------|----------------------------------|-----------------|-------------------|----------------------------------|
| $0$                       | $[0, 2^{32})$                    | $2^{32}$        | non-deterministic | for pre-loaded data              |
| $[1,2^{31})$              | $[2^{32}, 2^{63})$               | $2^{63}-2^{32}$ | dynamic           | run-time data                    |
| $[2^{31},2^{32}-4)$       | $[2^{63}, 2^{34})$               | $2^{63}-2^{34}$ | static            | not reserved                     |
| $2^{32}-4$ and $2^{32}-3$ | $[2^{64}-2^{34},2^{64}-2^{33})$  | $2^{33}$        | static            | Reserved for STARK-verifier      |
| $2^{32}-2$                | $[2^{64}-2^{33}, 2^{64}-2^{32})$ | $2^{32}$        | static            | available for `Library::kmalloc` |
| $2^{32}-1$                | $[2^{64}-2^{32}, 0)$             | $1$             | static            | state of `DynMalloc` snippet     |

- The first memory page, page 0, the address region $[0, 2^{32})$, is reserved for preloaded data.
  Note that $0$ is included and $2^{32}$ is excluded.
- The address region $[2^{32}, 2^{63})$ is used for dynamically allocated memory, memory that gets allocated at runtime.
- The address $-1$ (the last page) handles the state of the dynamic allocator.
  The dynamic allocator checks at runtime that it is not allocating outside its region.
  Note that $-1 = 2^{64} - 2^{32}$ in this prime field.
- Statically allocated memory, which is reserved by snippets and compiled code, starts at address $-2$ and grows
  downwards.
  The `kmalloc` method on `Library` can allocate from $-2$ down to (and including) $-2^{32}-1 = 2^{64}-2^{33}+1$.
  The rest of memory is also considered static, but cannot be accessed using `kmalloc`.
  Individual snippets are allowed to use the range from $[2^{63}, 2^{64}-2^{33})$.
  But the two last pages, the range $[2^{64}-2^{34},2^{64}-2^{33})$, is used by
  [the snippet that verifies the VM's own proofs](tasm-lib/src/verifier/stark_verify.rs).

The memory conventions above are enforced by the code in this library.
They are _not_ enforced by Triton VM.
If there exists non-deterministic input to Triton VM that exhibits any of the following behavior, this is considered a
bug:

1. Read before writing of any address outside the non-deterministic region $[0, 2^{32})$.
2. Write to the non-deterministic region.
3. Write to a dynamically-resolved address outside the region for dynamically allocated memory.
4. Perform any read or write in the non-reserved pages.

Additionally, a pointer that is resolved dynamically must only be resolved at most one time for each instance.
This means that memory offsets defined in memory must be resolved and kept on stack or in static memory throughout the
execution of the program.
They must not be resolved multiple times, as the memory could have changed in between two pointer resolutions.

### u64

A `u64` type is stored on the stack as two `u32` elements.
Triton VM provides native support for `u32`.
A `u64` is stored with the least significant part on top of the stack and the most significant part below it:
if the least significant part is `st0`, the most significant part is stored in `st1`.

### u128

A `u128` is represented by four `u32` values, where each value takes up a word of space in the VM.
`u128` follow a similar encoding to that of `u64`:
The least significant `u32` value is stored on top of the stack, in `st0`.

## Runtimes

Runtimes are printed in the JSON files in the [`benchmarks`](tasm-lib/benchmarks) directory.
These benchmarks count the number of clock cycles it took to execute each benchmark.
They also count coprocessor use, like [u32-table](https://triton-vm.org/spec/u32-table.html) height or
[hash-table](https://triton-vm.org/spec/hash-table.html) height.

The benchmark results are generated by tests labeled as `benchmark`.
If you manage to lower any of the numbers by changing a TASM snippet, please make a pull request;
we like efficient algorithms. 😊

All benchmark results must be deterministic.
A benchmark that does not always generate the same numbers is considered buggy.
