use serde::Deserialize;
use serde::Serialize;
use triton_vm::aet::AlgebraicExecutionTrace;
use triton_vm::prelude::TableId;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub clock_cycle_count: usize,
    pub hash_table_height: usize,
    pub u32_table_height: usize,
    pub op_stack_table_height: usize,
    pub ram_table_height: usize,
}

/// The cost of a program less that of a prefix of it: the cost of the rest.
impl std::ops::Sub for BenchmarkResult {
    type Output = Self;

    fn sub(self, base: Self) -> Self {
        Self {
            clock_cycle_count: self.clock_cycle_count - base.clock_cycle_count,
            hash_table_height: self.hash_table_height - base.hash_table_height,
            u32_table_height: self.u32_table_height - base.u32_table_height,
            op_stack_table_height: self.op_stack_table_height - base.op_stack_table_height,
            ram_table_height: self.ram_table_height - base.ram_table_height,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NamedBenchmarkResult {
    pub name: String,
    pub benchmark_result: BenchmarkResult,
    pub case: BenchmarkCase,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum BenchmarkCase {
    CommonCase,
    WorstCase,
}

impl BenchmarkResult {
    pub fn new(aet: &AlgebraicExecutionTrace) -> Self {
        BenchmarkResult {
            clock_cycle_count: aet.height_of_table(TableId::Processor),
            hash_table_height: aet.height_of_table(TableId::Hash),
            u32_table_height: aet.height_of_table(TableId::U32),
            op_stack_table_height: aet.height_of_table(TableId::OpStack),
            ram_table_height: aet.height_of_table(TableId::Ram),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn write_benchmarks(benchmarks: Vec<NamedBenchmarkResult>) {
    if benchmarks.is_empty() {
        return;
    }

    let mut path = std::path::PathBuf::new();
    path.push("benchmarks");
    std::fs::create_dir_all(&path).expect("benchmarks directory should exist");

    let function_name = benchmarks[0].name.as_str();
    assert!(
        benchmarks.iter().all(|bench| bench.name == function_name),
        "all fn names must agree for benchmark writing to disk",
    );

    path.push(std::path::Path::new(function_name).with_extension("json"));
    let output = std::fs::File::create(&path).expect("open file for writing");
    serde_json::to_writer_pretty(output, &benchmarks).expect("write json to file");
}

// file access is not possible on `wasm32` architectures; ignore attempts
#[cfg(target_arch = "wasm32")]
pub fn write_benchmarks(_: Vec<NamedBenchmarkResult>) {}
