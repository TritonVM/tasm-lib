use std::cmp::min;

use anyhow::{anyhow, Result};
use triton_vm::instruction::LabelledInstruction;
use triton_vm::program::Program;
use triton_vm::{NonDeterminism, PublicInput};
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::library::Library;

pub trait CompiledProgram {
    fn rust_shadow(
        public_input: &PublicInput,
        nondeterminism: &NonDeterminism<BFieldElement>,
    ) -> Result<Vec<BFieldElement>>;

    fn program() -> Program {
        let (program_instructions, library) = Self::code();

        let library_instructions = library.all_imports();

        Program::new(&[program_instructions, library_instructions].concat())
    }

    fn run(
        public_input: &PublicInput,
        nondeterminism: &NonDeterminism<BFieldElement>,
    ) -> Result<Vec<BFieldElement>> {
        let p = Self::program();
        p.run(public_input.clone(), nondeterminism.clone())
            .map_err(|err| anyhow!(err))
    }

    fn code() -> (Vec<LabelledInstruction>, Library);

    fn crash_conditions() -> Vec<String> {
        vec![]
    }
}

pub fn test_rust_shadow<P: CompiledProgram>(
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism<BFieldElement>,
) {
    let rust_output = P::rust_shadow(public_input, nondeterminism).unwrap();
    let tasm_output = P::run(public_input, nondeterminism).unwrap();
    assert_eq!(rust_output, tasm_output);
}

pub fn bench_program<P: CompiledProgram>(
    name: String,
    case: crate::snippet_bencher::BenchmarkCase,
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism<BFieldElement>,
) {
    use std::{
        fs::{create_dir_all, File},
        path::{Path, PathBuf},
    };

    use crate::snippet_bencher::BenchmarkResult;
    use std::io::Write;

    struct AggregateProfileLine {
        label: String,
        call_depth: usize,
        cycle_count: u32,
    }

    let (program_instructions, library) = P::code();
    let library_instructions = library.all_imports();
    let all_instructions = [program_instructions, library_instructions].concat();
    let program = Program::new(&all_instructions);

    // run in trace mode to get table heights
    let benchmark = match program.trace_execution(public_input.clone(), nondeterminism.clone()) {
        Ok((aet, _output)) => BenchmarkResult {
            case,
            name: name.clone(),
            clock_cycle_count: aet.processor_table_length(),
            hash_table_height: aet.hash_table_length(),
            u32_table_height: aet.u32_table_length(),
        },
        Err(_) => panic!(),
    };

    crate::snippet_bencher::write_benchmarks(vec![benchmark]);

    // run in profile mode to get picture of call graph running times
    let (_output, profile) = program
        .profile(public_input.clone(), nondeterminism.clone())
        .unwrap();
    let mut str = format!("{name}:\n");
    str = format!("{str}\n# call graph\n");
    for line in profile.iter() {
        let indentation = vec!["  "; line.call_depth].join("");
        let label = &line.label;
        let cycle_count = line.cycle_count();
        str = format!("{str}{indentation} {label}: {cycle_count}\n");
    }
    str = format!("{str}\n# aggregated\n");
    let mut aggregated: Vec<AggregateProfileLine> = vec![];
    for line in profile {
        if let Some(agg) = aggregated.iter_mut().find(|a| a.label == line.label) {
            agg.cycle_count += line.cycle_count();
            agg.call_depth = min(agg.call_depth, line.call_depth);
        } else {
            aggregated.push(AggregateProfileLine {
                label: line.label.to_owned(),
                call_depth: line.call_depth,
                cycle_count: line.cycle_count(),
            });
        }
    }
    for aggregate_line in aggregated {
        let indentation = vec!["  "; aggregate_line.call_depth].join("");
        let label = aggregate_line.label;
        let cycle_count = aggregate_line.cycle_count;
        str = format!("{str}{indentation} {label}: {cycle_count}\n");
    }

    // write profile to standard output in case someone is watching
    println!("{str}");

    // write profile to profile file
    let mut path = PathBuf::new();
    path.push("profiles");
    create_dir_all(&path).expect("profiles directory should exist");

    path.push(Path::new(&name).with_extension("profile"));
    let mut file = File::create(&path).expect("open file for writing");
    write!(file, "{str}").unwrap();
}

#[cfg(test)]
mod test {
    use triton_vm::{triton_asm, BFieldElement, NonDeterminism, PublicInput};

    use crate::library::Library;

    use super::{test_rust_shadow, CompiledProgram};

    pub(super) struct FiboTest;

    impl CompiledProgram for FiboTest {
        fn rust_shadow(
            public_input: &PublicInput,
            _secret_input: &NonDeterminism<BFieldElement>,
        ) -> anyhow::Result<Vec<triton_vm::BFieldElement>> {
            let num_iterations = public_input.individual_tokens[0].value() as usize;
            let mut a = BFieldElement::new(0);
            let mut b = BFieldElement::new(1);
            for _ in 0..num_iterations {
                let c = a + b;
                a = b;
                b = c;
            }
            Ok(vec![b])
        }

        fn code() -> (
            Vec<triton_vm::instruction::LabelledInstruction>,
            crate::library::Library,
        ) {
            let code = triton_asm!(
                push 0
                push 1
                read_io 1
                call fibo_test_loop
                pop 1
                write_io 1
                halt

                // INVARIANT: _ a b itr
                fibo_test_loop:
                    dup 0 push 0 eq
                    skiz return

                    push -1 add

                    dup 2
                    dup 2
                    add
                    swap 1
                    recurse
            );

            (code, Library::default())
        }
    }

    #[test]
    fn test_fibo_shadow() {
        let public_input = PublicInput::new(vec![BFieldElement::new(501)]);
        let nondeterminism = NonDeterminism::new(vec![]);
        test_rust_shadow::<FiboTest>(&public_input, &nondeterminism);
    }
}

#[cfg(test)]
mod benches {
    use crate::compiled_program::test::FiboTest;
    use crate::snippet_bencher::BenchmarkCase;

    use super::*;

    #[test]
    fn bench_fibo() {
        let public_input = PublicInput::new(vec![BFieldElement::new(501)]);
        let secret_input = NonDeterminism::new(vec![]);
        bench_program::<FiboTest>(
            "fibo_test".to_string(),
            BenchmarkCase::CommonCase,
            &public_input,
            &secret_input,
        );
    }
}
