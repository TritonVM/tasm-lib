//! Keccak-f\[1600\] on a state of slices in static memory.

use triton_vm::isa::parser::to_labelled_instructions;
use triton_vm::isa::parser::tokenize;
use triton_vm::prelude::*;

use super::ROTATION_OFFSETS;
use super::ROUND_CONSTANTS;
use super::tables::rotation_table_address;
use crate::prelude::*;

/// Assembly built as text, for code whose shape the generator computes.
#[derive(Debug, Default, Clone)]
pub(crate) struct Asm {
    lines: Vec<String>,
}

impl Asm {
    pub(crate) fn e(&mut self, line: impl AsRef<str>) {
        self.lines.push(line.as_ref().to_string());
    }

    /// The field element `x mod p`, for offsets between addresses.
    pub(crate) fn fe(x: i128) -> u64 {
        x.rem_euclid(i128::from(BFieldElement::P)) as u64
    }

    /// `_ → _ m[addr]`
    pub(crate) fn load(&mut self, addr: u64) {
        self.e(format!("push {addr} read_mem 1 pop 1"));
    }

    /// `_ v → _`, and `m[addr] = v`
    pub(crate) fn store(&mut self, addr: u64) {
        self.e(format!("push {addr} write_mem 1 pop 1"));
    }

    /// `_ → _ m[a_0] m[a_1] … m[a_{n-1}]`, with one moving pointer.
    pub(crate) fn load_seq(&mut self, addrs: &[u64]) {
        self.e(format!("push {}", addrs[0]));
        for (i, addr) in addrs.iter().enumerate() {
            self.e("read_mem 1");
            if let Some(next) = addrs.get(i + 1) {
                let offset = i128::from(*next) - (i128::from(*addr) - 1);
                if offset != 0 {
                    self.e(format!("addi {}", Self::fe(offset)));
                }
            }
        }
        self.e("pop 1");
    }

    /// `_ v_{n-1} … v_1 v_0 → _`, and `m[a_i] = v_i`, with one moving pointer.
    pub(crate) fn store_seq(&mut self, addrs: &[u64]) {
        self.e(format!("push {}", addrs[0]));
        for (i, addr) in addrs.iter().enumerate() {
            self.e("write_mem 1");
            if let Some(next) = addrs.get(i + 1) {
                let offset = i128::from(*next) - (i128::from(*addr) + 1);
                if offset != 0 {
                    self.e(format!("addi {}", Self::fe(offset)));
                }
            }
        }
        self.e("pop 1");
    }

    /// `_ → _ m[last] m[last-1] … m[last-n+1]`; the lowest address ends on top.
    pub(crate) fn load_block(&mut self, last: u64, n: usize) {
        self.e(format!("push {last}"));
        self.read_mem(n);
        self.e("pop 1");
    }

    /// `read_mem` of `n` words, in chunks of at most five.
    pub(crate) fn read_mem(&mut self, n: usize) {
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            self.e(format!("read_mem {k}"));
            left -= k;
        }
    }

    /// `_ v_{n-1} … v_0 → _`, and `m[first + i] = v_i`.
    pub(crate) fn store_block(&mut self, first: u64, n: usize) {
        self.e(format!("push {first}"));
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            self.e(format!("write_mem {k}"));
            left -= k;
        }
        self.e("pop 1");
    }

    /// Pop `n` words, in chunks of at most five.
    pub(crate) fn pop(&mut self, n: usize) {
        let mut left = n;
        while left > 0 {
            let k = left.min(5);
            self.e(format!("pop {k}"));
            left -= k;
        }
    }

    /// `_ v → _ rot(v, t)` for a byte `v`, by table lookup.
    pub(crate) fn rotate_byte(&mut self, t: u32) {
        if !t.is_multiple_of(8) {
            let table = rotation_table_address(t % 8);
            self.e(format!("addi {table} read_mem 1 pop 1"));
        }
    }

    pub(crate) fn into_instructions(self) -> Vec<LabelledInstruction> {
        let code = self.lines.join("\n");
        let (rest, tokens) = tokenize(&code).expect("generated assembly must parse");
        assert!(rest.is_empty(), "generated assembly must parse completely");
        to_labelled_instructions(&tokens)
    }
}

/// The source slice and the rotation in bits that give slice `jo` of a lane
/// rotated left by `r`.
pub(crate) fn slice_rotation(r: u32, jo: usize) -> (usize, u32) {
    let (q, s) = (r / 8, (r % 8) as usize);
    let src = (jo + 8 - s) % 8;
    let t = (q + u32::from(jo < s)) % 8;
    (src, t)
}

/// Keccak-f\[1600\] on the state at [`slice_address`](Self::slice_address),
/// held as slices (see [the module](super)).
///
/// The state and the permutation's scratch space are a static allocation, of
/// which the address is part of the snippet. A program that hashes with
/// several snippets lets them share one `KeccakF1600`.
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

    fn theta_c(&self, asm: &mut Asm) {
        for batch in (0..40).step_by(5) {
            for g in (batch..batch + 5).rev() {
                let (x, j) = (g / 8, g % 8);
                let last = self.a(x, 4, j);
                asm.e(format!("push {last} read_mem 5 pop 1 xor xor xor xor"));
            }
            asm.e(format!(
                "push {} write_mem 5 pop 1",
                self.c(0, 0) + batch as u64
            ));
        }
    }

    fn theta_d(&self, asm: &mut Asm) {
        for x in 0..5 {
            // C[x-1] on the stack with slice 0 on top. Each slice of D takes
            // the place of a slice of C at the bottom, so that D ends with
            // slice 0 on top.
            asm.load_block(self.c((x + 4) % 5, 7), 8);
            for jo in 0..8 {
                let (src, t) = slice_rotation(1, jo);
                asm.load(self.c((x + 1) % 5, src));
                asm.rotate_byte(t);
                asm.e("xor place 7");
            }
            asm.store_block(self.d(x, 0), 8);
        }
    }

    fn rho_pi(&self, asm: &mut Asm) {
        for (x, offsets) in ROTATION_OFFSETS.iter().enumerate() {
            // D[x] on the stack, slice 0 on top, for the five lanes of column x.
            asm.load_block(self.d(x, 7), 8);
            for (y, &r) in offsets.iter().enumerate() {
                let (bx, by) = (y, (2 * x + 3 * y) % 5);
                let s = (r % 8) as usize;
                for batch in [[0usize, 1, 2, 3], [4, 5, 6, 7]] {
                    let n = batch.len();
                    let srcs = batch.map(|j| self.a(x, y, j));
                    asm.load_seq(&srcs);
                    // Process from the top; each result goes below the
                    // unprocessed slices, so the results end in load order.
                    for idx in (0..n).rev() {
                        let j = batch[idx];
                        let jo = (j + s) % 8;
                        let (src, t) = slice_rotation(r, jo);
                        debug_assert_eq!(src, j);
                        asm.e(format!("dup {} xor", n + j));
                        asm.rotate_byte(t);
                        if idx > 0 {
                            asm.e(format!("place {idx}"));
                        }
                    }
                    let dsts = batch.map(|j| self.b(bx, by, (j + s) % 8));
                    asm.store_seq(&dsts);
                }
            }
            asm.pop(8);
        }
    }

    /// χ on row `y`, and ι if `round_constant` is given (for row 0).
    ///
    /// Every B value is used four times; the last use takes it off the stack
    /// with `pick` instead of copying it with `dup`, so that only the five
    /// outputs remain.
    fn chi_row(&self, asm: &mut Asm, y: usize, round_constant: Option<u64>) {
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
        for j in 0..8 {
            asm.e(format!("push {} read_mem 5 pop 1", self.b(4, y, j)));
            // _ B[4] B[3] B[2] B[1] B[0], top of the stack last
            let mut stack: Vec<Item> = (0..5).rev().map(Item::B).collect();
            let fetch = |asm: &mut Asm, stack: &mut Vec<Item>, use_index: usize| {
                let i = uses[use_index];
                let last = !uses[use_index + 1..].contains(&i);
                let index = stack.iter().rposition(|&item| item == Item::B(i)).unwrap();
                let depth = stack.len() - 1 - index;
                if last {
                    stack.remove(index);
                    asm.e(format!("pick {depth}"));
                } else {
                    asm.e(format!("dup {depth}"));
                }
                stack.push(Item::Value);
            };
            for (n, x) in (0..5).rev().enumerate() {
                let u = 4 * n;
                fetch(asm, &mut stack, u);
                fetch(asm, &mut stack, u + 1);
                asm.e("and");
                stack.pop();
                fetch(asm, &mut stack, u + 2);
                asm.e("xor");
                stack.pop();
                fetch(asm, &mut stack, u + 3);
                asm.e("xor");
                stack.pop();
                if let Some(slices) = rc_slices
                    && x == 0
                    && slices[j] != 0
                {
                    asm.e(format!("push {} xor", slices[j]));
                }
            }
            debug_assert!(stack.iter().all(|&item| item == Item::Value) && stack.len() == 5);
            let dsts: Vec<u64> = (0..5).map(|x| self.a(x, y, j)).collect();
            asm.store_seq(&dsts);
        }
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
        let mut asm = Asm::default();
        asm.e(format!("{entrypoint}:"));
        for round_constant in ROUND_CONSTANTS {
            asm.e(format!("call {shared}"));
            self.chi_row(&mut asm, 0, Some(round_constant));
        }
        asm.e("return");
        asm.e(format!("{shared}:"));
        self.theta_c(&mut asm);
        self.theta_d(&mut asm);
        self.rho_pi(&mut asm);
        for y in 1..5 {
            self.chi_row(&mut asm, y, None);
        }
        asm.e("return");
        asm.into_instructions()
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
