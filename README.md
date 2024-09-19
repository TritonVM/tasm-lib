# tasm-lib

This repository contains a collection of functions written in Triton VM assembly (tasm). The functions supplied here emulate the standard library of other programming languages: basic functions that are used a lot.

There are two big projects to be written in tasm. They are not contained in this repository. These projects are:
 - The consensus logic of [Neptune](https://neptune.cash/)
 - The recursive Triton VM STARK verifier (recufier)

This repository helps to benchmark snippets written in tasm in order to gauge the complexity of the (Triton VM) prover.

Please place code in the appropriate directories, or create them if non-existent.

## Conventions
### Memory
Triton-VM has $P = 2^{64} - 2^{32} + 1$ number of words in its RAM, where each word is a prime field element in the prime $P$, colloquially called "0xfoi" due to its hexadecimal representation. RAM can be initialized at startup by setting non-determinism (ND).

Memory is, by convention, divided up into chunks called pages. All pages, except the last one, have a size of $2^{32}$ words. The last page has a size of $1$. Memory consists of $2^{32}$ pages.

| pages                     | region                           | size in words   | category          | purpose                          |
|---------------------------|----------------------------------|-----------------|-------------------|----------------------------------|
| $0$                       | $[0, 2^{32})$                    | $2^{32}$        | non-deterministic | for pre-loaded data              |
| $[1,2^{31})$              | $[2^{32}, 2^{63})$               | $2^{63}-2^{32}$ | dynamic           | run-time data                    |
| $[2^{31},2^{32}-4)$       | $[2^{63}, 2^{34})$               | $2^{63}-2^{34}$ | static            | not reserved                     |
| $2^{32}-4$ and $2^{32}-3$ | $[2^{64}-2^{34},2^{64}-2^{33})$  | $2^{33}$        | static            | Reserved for STARK-verifier      |
| $2^{32}-2$                | $[2^{64}-2^{33}, 2^{64}-2^{32})$ | $2^{32}$        | static            | available for `Library::kmalloc` |
| $2^{32}-1$                | $[2^{64}-2^{32}, 0)$             | $1$             | static            | state of `DynMalloc` snippet     |


- The first memory page, page 0, the address region $[0, 2^{32})$, is reserved for pre-loaded data. Note that $0$ is included and $2^{32}$ is excluded.
- The address region $[2^{32}, 2^{63})$ is used for dynamically allocated memory, memory that was allocated at runtime. The address $-1$ (the last page) handles the state of the dynamic allocator. The dynamic allocator checks at runtime that it is not allocating outside of its region. Note that $-1 = 2^{64} - 2^{32}$ in this prime field.
- Statically allocated memory, which is reserved by snippets and compiled code, starts at address $-2$ and grows downwards. The `kmalloc` method on `Library` can allocate from $-2$ down to (and including) $-2^{32}-1 = 2^{64}-2^{33}+1$. The rest of memory is also considered static, but cannot be accessed using `kmalloc`. Individual snippets are allowed to use the range from $[2^{63}, 2^{64}-2^{33})$. But the two last pages, the range $[2^{64}-2^{34},2^{64}-2^{33})$, is used by [the snippet that verifies the VM's own proofs](https://github.com/TritonVM/tasm-lib/blob/master/tasm-lib/src/verifier/stark_verify.rs).

The memory conventions above are (read: should be) enforced by the code in this library. They are *not* enforced by Triton VM. This means that any of the following behaviors are considered a bug if there exists a non-deterministic input to Triton VM that allows the user to
1. Read before writing of any address outside of the ND region $[0, 2^{32})$.
2. Write to the ND region
3. Write to a dynamically-resolved address outside of the region for dynamically allocated memory.
4. Perform any read or write in the non-reserved pages.

Additionally, a pointer that is resolved dynamically must only be resolved at most one time for each instance. This means that memory offsets defined in memory must be resolved and kept on stack or in static memory thoughout the execution of the program. They must not be resolved multiple times, as the memory could have changed in between two pointer resolutions.

### U64
A `u64` type is stored on the stack as two `u32` elements. Triton VM provides native support for `u32`. A `u64` is stored with the least significant part on top of the stack and the most significant part below it. So if the least significant part is `st0`, then the most significant part is stored in `st1`.

### U128
a `u128` is represented by four `u32` values, where each value takes up a word of space in the VM. `u128` follow a similar encoding to that of `u64`: The least significant `u32` value is stored on top of the stack, in `st0`.

## Runtimes
Runtimes are printed in the JSON files in the `benchmarks` directory. These benchmarks count the number of clock cycles it took to execute each benchmark. They also count co-processor use, like u32-table height and hash-table height.

The benchmark results are generated by tests labeled as `benchmark`. If you manage to lower any of the numbers by changing a TASM snippet, please make a pull request, so we can get more efficient algorithms :)

All benchmark results must be deterministic. Any benchmark that does not always generate the same numbers is a bug.
