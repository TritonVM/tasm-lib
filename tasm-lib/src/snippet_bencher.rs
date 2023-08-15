use serde::{Deserialize, Serialize};
use serde_json::to_writer_pretty;
use std::fs::{create_dir_all, File};
use std::path::{Path, PathBuf};

use crate::snippet::DeprecatedSnippet;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub name: String,
    pub clock_cycle_count: usize,
    pub hash_table_height: usize,
    pub u32_table_height: usize,
    pub case: BenchmarkCase,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum BenchmarkCase {
    CommonCase,
    WorstCase,
}

#[allow(dead_code)]
pub fn benchmark_snippet_deprecated<T: DeprecatedSnippet>(snippet: T) -> Vec<BenchmarkResult> {
    let mut benchmarks = Vec::with_capacity(2);

    for (case, mut execution_state) in [
        (BenchmarkCase::CommonCase, snippet.common_case_input_state()),
        (BenchmarkCase::WorstCase, snippet.worst_case_input_state()),
    ] {
        let execution_result = snippet
            .link_and_run_tasm_from_state_for_bench(&mut execution_state)
            .unwrap();
        let benchmark = BenchmarkResult {
            name: snippet.entrypoint_name(),
            clock_cycle_count: execution_result.cycle_count,
            hash_table_height: execution_result.hash_table_height,
            u32_table_height: execution_result.u32_table_height,
            case,
        };
        benchmarks.push(benchmark);
    }

    benchmarks
}

#[allow(dead_code)]
pub fn write_benchmarks(benchmarks: Vec<BenchmarkResult>) {
    let mut path = PathBuf::new();
    path.push("benchmarks");
    create_dir_all(&path).expect("benchmarks directory should exist");

    let function_name = &benchmarks[0].name;
    for fnname in benchmarks.iter().map(|x| &x.name) {
        assert_eq!(
            function_name, fnname,
            "all fn names must agree for benchmark writing to disk"
        );
    }

    path.push(Path::new(&function_name).with_extension("json"));
    let output = File::create(&path).expect("open file for writing");
    to_writer_pretty(output, &benchmarks).expect("write json to file");
}

#[allow(dead_code)]
pub fn bench_and_write<T: DeprecatedSnippet>(snippet: T) {
    write_benchmarks(benchmark_snippet_deprecated(snippet));
}
