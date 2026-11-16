//! Keccak-f\[1600\] on a state of slices in static memory.

use itertools::Itertools;
use triton_vm::prelude::*;

use super::ROTATION_OFFSETS;
use super::ROUND_CONSTANTS;
use super::tables::rotation_table_address;
use crate::memory::load_words_from_memory_pop_pointer;
use crate::memory::load_words_from_scattered_addresses;
use crate::memory::write_words_to_memory_pop_pointer;
use crate::memory::write_words_to_scattered_addresses;
use crate::prelude::*;

/// The source slice and the rotation in bits that give slice `jo` of a lane
/// rotated left by `r`.
pub(crate) fn slice_rotation(r: u32, jo: usize) -> (usize, u32) {
    let (q, s) = (r / 8, (r % 8) as usize);
    let src = (jo + 8 - s) % 8;
    let t = (q + u32::from(jo < s)) % 8;
    (src, t)
}

/// `_ v → _ rot(v, t)` for a byte `v`, by table lookup.
pub(crate) fn rotate_byte(t: u32) -> Vec<LabelledInstruction> {
    if t.is_multiple_of(8) {
        return vec![];
    }
    let table = rotation_table_address(t % 8);
    triton_asm!(addi { table } read_mem 1 pop 1)
}

/// Keccak-f\[1600\] on the state at [`slice_address`](Self::slice_address),
/// held as slices (see [the module](super)).
///
/// The state and the permutation's scratch space are a static allocation, of
/// which the address is part of the snippet. A program that hashes with
/// several snippets lets them share one `KeccakF1600`.
///
/// The snippet has no parameters and no return values: its stack signature is
/// `_ → _`. Its input and output are the state in memory. The caller writes
/// the state's slices to [`slice_address`](Self::slice_address) before the
/// call and reads the permuted state from the same addresses after it. Every
/// access in the snippet's code uses an address that
/// [`allocate`](Self::allocate) fixed, so the code differs between
/// allocations, and so does the
/// [entrypoint](crate::traits::basic_snippet::BasicSnippet::entrypoint), which
/// contains the address.
///
/// ### Pre-conditions
///
/// - [`CheckKeccakTables`](super::tables::CheckKeccakTables) ran before
/// - every slice of the state is a byte
///
/// ### Post-conditions
///
/// - the state is Keccak-f\[1600\] of the state before, and its slices are
///   bytes
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct KeccakF1600 {
    scratch: u64,
}

impl KeccakF1600 {
    /// The state, and three buffers for θ and ρ.
    pub const NUM_SCRATCH_WORDS: u32 = 200 + 200 + 40 + 40;

    /// A permutation of which the state lives in newly allocated static
    /// memory.
    pub fn allocate(library: &mut Library) -> Self {
        let scratch = library.kmalloc(Self::NUM_SCRATCH_WORDS).write_address();
        Self {
            scratch: scratch.value(),
        }
    }

    /// The address of the first word of the state, which occupies 200
    /// consecutive words.
    pub fn state_address(&self) -> u64 {
        self.scratch
    }

    /// The address of slice `j` of lane `x + 5·y`.
    pub fn slice_address(&self, lane: usize, j: usize) -> u64 {
        self.a(lane % 5, lane / 5, j)
    }

    // The state is stored by columns: the five lanes of column x, slice j, are
    // consecutive, which θ's column sums read in one instruction.
    fn a(&self, x: usize, y: usize, j: usize) -> u64 {
        self.scratch + ((x * 8 + j) * 5 + y) as u64
    }

    // The state after ρ and π is stored by rows, which χ reads in one
    // instruction.
    fn b(&self, x: usize, y: usize, j: usize) -> u64 {
        self.scratch + 200 + ((y * 8 + j) * 5 + x) as u64
    }

    fn c(&self, x: usize, j: usize) -> u64 {
        self.scratch + 400 + (x * 8 + j) as u64
    }

    fn d(&self, x: usize, j: usize) -> u64 {
        self.scratch + 440 + (x * 8 + j) as u64
    }

    fn theta_c(&self) -> Vec<LabelledInstruction> {
        (0..40)
            .step_by(5)
            .flat_map(|batch| {
                let column_sums = (batch..batch + 5)
                    .rev()
                    .flat_map(|g| {
                        let last = self.a(g / 8, 4, g % 8);
                        triton_asm!(push { last } read_mem 5 pop 1 xor xor xor xor)
                    })
                    .collect_vec();
                let first = self.c(0, 0) + batch as u64;
                triton_asm!(
                    {&column_sums}
                    push {first}
                    write_mem 5
                    pop 1
                )
            })
            .collect()
    }

    fn theta_d(&self) -> Vec<LabelledInstruction> {
        (0..5)
            .flat_map(|x| {
                // C[x-1] on the stack with slice 0 on top. Each slice of D
                // takes the place of a slice of C at the bottom, so that D
                // ends with slice 0 on top.
                let slices = (0..8)
                    .flat_map(|jo| {
                        let (src, t) = slice_rotation(1, jo);
                        let address = self.c((x + 1) % 5, src);
                        triton_asm!(
                            push {address}
                            read_mem 1
                            pop 1
                            {&rotate_byte(t)}
                            xor
                            place 7
                        )
                    })
                    .collect_vec();
                triton_asm!(
                    push {self.c((x + 4) % 5, 7)}
                    {&load_words_from_memory_pop_pointer(8)}
                    {&slices}
                    push {self.d(x, 0)}
                    {&write_words_to_memory_pop_pointer(8)}
                )
            })
            .collect()
    }

    fn rho_pi(&self) -> Vec<LabelledInstruction> {
        let mut instructions = vec![];
        for (x, offsets) in ROTATION_OFFSETS.iter().enumerate() {
            // D[x] on the stack, slice 0 on top, for the five lanes of column x.
            instructions.extend(triton_asm!(
                push {self.d(x, 7)}
                {&load_words_from_memory_pop_pointer(8)}
            ));
            for (y, &r) in offsets.iter().enumerate() {
                let (bx, by) = (y, (2 * x + 3 * y) % 5);
                let s = (r % 8) as usize;
                for batch in [[0usize, 1, 2, 3], [4, 5, 6, 7]] {
                    let n = batch.len();
                    let sources = batch.map(|j| self.a(x, y, j));
                    instructions.extend(load_words_from_scattered_addresses(&sources));
                    // Process from the top; each result goes below the
                    // unprocessed slices, so the results end in load order.
                    for idx in (0..n).rev() {
                        let j = batch[idx];
                        let jo = (j + s) % 8;
                        let (src, t) = slice_rotation(r, jo);
                        debug_assert_eq!(src, j);
                        instructions.extend(triton_asm!(
                            dup {n + j}
                            xor
                            {&rotate_byte(t)}
                        ));
                        if idx > 0 {
                            instructions.extend(triton_asm!(place { idx }));
                        }
                    }
                    let destinations = batch.map(|j| self.b(bx, by, (j + s) % 8));
                    instructions.extend(write_words_to_scattered_addresses(&destinations));
                }
            }
            instructions.extend(triton_asm!(pop 5 pop 3));
        }
        instructions
    }

    /// χ on row `y`, and ι if `round_constant` is given (for row 0).
    ///
    /// Every B value is used four times; the last use takes it off the stack
    /// with `pick` instead of copying it with `dup`, so that only the five
    /// outputs remain.
    fn chi_row(&self, y: usize, round_constant: Option<u64>) -> Vec<LabelledInstruction> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Item {
            B(usize),
            Value,
        }
        let rc_slices = round_constant.map(super::interleave);

        // A[x][y] = B[x][y] ^ B[x+2][y] ^ (B[x+1][y] & B[x+2][y]), computed for
        // x from 4 down to 0, so that the output for x = 0 ends on top.
        let mut uses = vec![];
        for x in (0..5).rev() {
            uses.extend([(x + 1) % 5, (x + 2) % 5, (x + 2) % 5, x]);
        }
        let fetch = |stack: &mut Vec<Item>, use_index: usize| {
            let i = uses[use_index];
            let last = !uses[use_index + 1..].contains(&i);
            let index = stack.iter().rposition(|&item| item == Item::B(i)).unwrap();
            let depth = stack.len() - 1 - index;
            let instructions = if last {
                stack.remove(index);
                triton_asm!(pick { depth })
            } else {
                triton_asm!(dup { depth })
            };
            stack.push(Item::Value);
            instructions
        };

        let mut instructions = vec![];
        for j in 0..8 {
            instructions.extend(triton_asm!(push {self.b(4, y, j)} read_mem 5 pop 1));
            // _ B[4] B[3] B[2] B[1] B[0], top of the stack last
            let mut stack: Vec<Item> = (0..5).rev().map(Item::B).collect();
            for (n, x) in (0..5).rev().enumerate() {
                let u = 4 * n;
                instructions.extend(fetch(&mut stack, u));
                instructions.extend(fetch(&mut stack, u + 1));
                instructions.extend(triton_asm!(and));
                stack.pop();
                instructions.extend(fetch(&mut stack, u + 2));
                instructions.extend(triton_asm!(xor));
                stack.pop();
                instructions.extend(fetch(&mut stack, u + 3));
                instructions.extend(triton_asm!(xor));
                stack.pop();
                if let Some(slices) = rc_slices
                    && x == 0
                    && slices[j] != 0
                {
                    instructions.extend(triton_asm!(push {slices[j]} xor));
                }
            }
            debug_assert!(stack.iter().all(|&item| item == Item::Value) && stack.len() == 5);
            let destinations = (0..5).map(|x| self.a(x, y, j)).collect_vec();
            instructions.extend(write_words_to_scattered_addresses(&destinations));
        }
        instructions
    }
}

impl BasicSnippet for KeccakF1600 {
    fn parameters(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![]
    }

    fn entrypoint(&self) -> String {
        format!("tasmlib_hashing_keccak_f1600_{:x}", self.scratch)
    }

    fn code(&self, _: &mut Library) -> Vec<LabelledInstruction> {
        // The rounds are unrolled, so that ι's constants are immediates. All
        // but χ on row 0, which ι touches, is shared by every round.
        let entrypoint = self.entrypoint();
        let shared = format!("{entrypoint}_round");
        let rounds = ROUND_CONSTANTS
            .into_iter()
            .flat_map(|round_constant| {
                triton_asm!(
                    call {shared}
                    {&self.chi_row(0, Some(round_constant))}
                )
            })
            .collect_vec();
        let other_rows = (1..5).flat_map(|y| self.chi_row(y, None)).collect_vec();

        triton_asm!(
            {entrypoint}:
                {&rounds}
                return

            {shared}:
                {&self.theta_c()}
                {&self.theta_d()}
                {&self.rho_pi()}
                {&other_rows}
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::super::deinterleave;
    use super::super::interleave;
    use super::super::keccak_f1600;
    use super::super::tables::CheckKeccakTables;
    use super::super::tables::insert_tables;
    use super::*;
    use crate::linker::execute_bench;
    use crate::snippet_bencher::BenchmarkCase;
    use crate::snippet_bencher::NamedBenchmarkResult;
    use crate::snippet_bencher::write_benchmarks;
    use crate::test_prelude::*;

    /// A program that checks the tables and then applies the permutation
    /// `perms` times, and the permutation it uses.
    fn program(perms: usize) -> (Vec<LabelledInstruction>, KeccakF1600) {
        let mut library = Library::new();
        let keccak = KeccakF1600::allocate(&mut library);
        let check = library.import(Box::new(CheckKeccakTables));
        let permute = library.import(Box::new(keccak));
        let calls = vec![triton_asm!(call { permute }); perms].concat();
        let code = triton_asm!(
            call {check}
            {&calls}
            halt
            {&library.all_imports()}
        );
        (code, keccak)
    }

    fn initial_memory(
        keccak: &KeccakF1600,
        state: &[u64; 25],
    ) -> HashMap<BFieldElement, BFieldElement> {
        let mut memory = HashMap::new();
        insert_tables(&mut memory);
        for (lane, value) in state.iter().enumerate() {
            for (j, slice) in interleave(*value).iter().enumerate() {
                memory.insert(bfe!(keccak.slice_address(lane, j)), bfe!(*slice));
            }
        }
        memory
    }

    #[macro_rules_attr::apply(proptest(cases = 4))]
    fn permutes_like_the_reference(state: [u64; 25], #[strategy(1usize..3)] perms: usize) {
        let (code, keccak) = program(perms);
        let nondeterminism = NonDeterminism::default().with_ram(initial_memory(&keccak, &state));
        let mut vm_state =
            VMState::new(Program::new(&code), PublicInput::default(), nondeterminism);
        vm_state.run().unwrap();

        let mut expected = state;
        for _ in 0..perms {
            keccak_f1600(&mut expected);
        }
        for (lane, value) in expected.iter().enumerate() {
            let slices: [u64; 8] =
                std::array::from_fn(|j| vm_state.ram[&bfe!(keccak.slice_address(lane, j))].value());
            prop_assert!(slices.iter().all(|&s| s < 256));
            prop_assert_eq!(*value, deinterleave(slices.map(|s| s as u8)));
        }
    }

    #[macro_rules_attr::apply(test)]
    fn benchmark() {
        let (code_0, _) = program(0);
        let (code_1, keccak) = program(1);
        let memory = initial_memory(&keccak, &[0x0123_4567_89ab_cdef; 25]);
        let nondeterminism = NonDeterminism::default().with_ram(memory);
        let base = execute_bench(
            &code_0,
            &crate::empty_stack(),
            vec![],
            nondeterminism.clone(),
            None,
        );
        let one = execute_bench(&code_1, &crate::empty_stack(), vec![], nondeterminism, None);
        let result = crate::snippet_bencher::BenchmarkResult {
            clock_cycle_count: one.clock_cycle_count - base.clock_cycle_count,
            hash_table_height: one.hash_table_height - base.hash_table_height,
            u32_table_height: one.u32_table_height - base.u32_table_height,
            op_stack_table_height: one.op_stack_table_height - base.op_stack_table_height,
            ram_table_height: one.ram_table_height - base.ram_table_height,
        };
        write_benchmarks(vec![NamedBenchmarkResult {
            name: "tasmlib_hashing_keccak_f1600".to_string(),
            benchmark_result: result,
            case: BenchmarkCase::CommonCase,
        }]);
    }
}
