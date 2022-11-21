use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{execute, ExecutionResult};

fn _halt_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let code: &str = "halt";
    execute(code, stack, 0, vec![], vec![])
}

#[allow(unused_variables)]
#[allow(clippy::ptr_arg)]
fn _halt_rust(stack: &mut Vec<BFieldElement>) {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn halt() {
        // TASM
        let mut stack = vec![];
        let execution_result = _halt_tasm(&mut stack);
        assert!(
            stack.is_empty(),
            "stack must be empty after running halt in TASM"
        );
        println!("Cycle count for `halt`: {}", execution_result.cycle_count);

        // Rust
        stack = vec![];
        _halt_rust(&mut stack);
        assert!(
            stack.is_empty(),
            "stack must be empty after running halt in Rust"
        );
    }
}
