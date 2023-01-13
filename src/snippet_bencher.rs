use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};
use serde_json::to_writer_pretty;
use triton_vm::table::master_table::MasterBaseTable;

use crate::snippet::{simulate_snippet, NewSnippet, Snippet};

#[derive(Debug, Serialize, Deserialize)]
pub struct SnippetBenchmark {
    name: &'static str,
    processor_table_height: usize,
    hash_table_height: usize,
    u32_table_height: usize,
}

#[allow(dead_code)]
pub fn benchmark_snippet<T: NewSnippet>() -> Vec<SnippetBenchmark> {
    let execution_states = T::gen_input_states();
    let mut benchmarks = Vec::with_capacity(execution_states.len());

    for execution_state in execution_states {
        let (aet, inflated_clock_cycles) = simulate_snippet::<T>(execution_state);
        let benchmark = SnippetBenchmark {
            name: T::entrypoint(),
            processor_table_height: aet.processor_matrix.nrows() - inflated_clock_cycles,
            hash_table_height: aet.hash_matrix.nrows(),
            u32_table_height: MasterBaseTable::u32_table_length(&aet),
        };
        benchmarks.push(benchmark);
    }

    benchmarks
}

#[allow(dead_code)]
pub fn write_benchmarks<T: Snippet>(benchmarks: Vec<SnippetBenchmark>) {
    let mut path = PathBuf::new();
    path.push("benchmarks");
    create_dir_all(&path).expect("benchmarks directory should exist");

    path.push(Path::new(T::entrypoint()).with_extension("json"));
    let output = File::create(&path).expect("open file for writing");
    to_writer_pretty(output, &benchmarks).expect("write json to file");
}

#[allow(dead_code)]
pub fn bench_and_write<T: NewSnippet>() {
    write_benchmarks::<T>(benchmark_snippet::<T>());
}
