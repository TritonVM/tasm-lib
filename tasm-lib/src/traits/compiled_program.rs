use anyhow::Result;
use anyhow::anyhow;
use triton_vm::prelude::*;

use crate::library::Library;
use crate::snippet_bencher::BenchmarkResult;

pub trait CompiledProgram {
    fn rust_shadow(
        public_input: &PublicInput,
        nondeterminism: &NonDeterminism,
    ) -> Result<Vec<BFieldElement>>;

    fn program() -> Program {
        let (program_instructions, library) = Self::code();
        let library_instructions = library.all_imports();
        Program::new(&[program_instructions, library_instructions].concat())
    }

    fn run(
        public_input: &PublicInput,
        nondeterminism: &NonDeterminism,
    ) -> Result<Vec<BFieldElement>> {
        VM::run(
            Self::program(),
            public_input.clone(),
            nondeterminism.clone(),
        )
        .map_err(|err| anyhow!(err))
    }

    fn code() -> (Vec<LabelledInstruction>, Library);

    fn crash_conditions() -> Vec<String> {
        vec![]
    }
}

pub fn test_rust_shadow<P: CompiledProgram>(
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism,
) {
    let rust_output = P::rust_shadow(public_input, nondeterminism).unwrap();
    let tasm_output = P::run(public_input, nondeterminism).unwrap();
    assert_eq!(rust_output, tasm_output);
}

/// Run the program, collect benchmarkable performance statistics (including a profile),
/// and write them to disk.
pub fn bench_and_profile_program<P: CompiledProgram>(
    name: &str,
    case: crate::snippet_bencher::BenchmarkCase,
    public_input: &PublicInput,
    nondeterminism: &NonDeterminism,
) {
    use std::fs::File;
    use std::fs::create_dir_all;
    use std::io::Write;
    use std::path::Path;
    use std::path::PathBuf;

    use crate::snippet_bencher::NamedBenchmarkResult;

    let (program_instructions, library) = P::code();
    let library_instructions = library.all_imports();
    let all_instructions = [program_instructions, library_instructions].concat();
    let program = Program::new(&all_instructions);

    // run in trace mode to get table heights
    let (aet, _output) = VM::trace_execution(
        program.clone(),
        public_input.clone(),
        nondeterminism.clone(),
    )
    .unwrap();
    let benchmark_result = BenchmarkResult::new(&aet);
    let benchmark = NamedBenchmarkResult {
        name: name.to_owned(),
        benchmark_result,
        case,
    };

    crate::snippet_bencher::write_benchmarks(vec![benchmark]);

    // write profile to standard output in case someone is watching
    let profile = crate::generate_full_profile(name, program, public_input, nondeterminism);
    println!("{profile}");

    // write profile to profile file
    let mut path = PathBuf::new();
    path.push("profiles");
    create_dir_all(&path).expect("profiles directory should exist");

    path.push(Path::new(name).with_extension("profile"));
    let mut file = File::create(path).expect("open file for writing");
    write!(file, "{profile}").unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(super) struct FiboTest;

    impl CompiledProgram for FiboTest {
        fn rust_shadow(
            public_input: &PublicInput,
            _secret_input: &NonDeterminism,
        ) -> Result<Vec<BFieldElement>> {
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

        fn code() -> (Vec<LabelledInstruction>, Library) {
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
    use super::tests::FiboTest;
    use super::*;
    use crate::snippet_bencher::BenchmarkCase;

    #[test]
    fn bench_fibo() {
        let public_input = PublicInput::new(vec![BFieldElement::new(501)]);
        let secret_input = NonDeterminism::new(vec![]);
        bench_and_profile_program::<FiboTest>(
            "fibo_test",
            BenchmarkCase::CommonCase,
            &public_input,
            &secret_input,
        );
    }
}
