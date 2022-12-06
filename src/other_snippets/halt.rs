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
    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn halt() {
        // TASM
        let init_stack = get_init_tvm_stack();

        let mut tasm_stack = init_stack.clone();
        let execution_result = _halt_tasm(&mut tasm_stack);
        println!("Cycle count for `halt`: {}", execution_result.cycle_count);
        println!(
            "Hash table height for `hash`: {}",
            execution_result.hash_table_height
        );

        // Rust
        let mut rust_stack = init_stack.clone();
        _halt_tasm(&mut rust_stack);

        // Check that the two functions agree
        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `halt`"
        );

        // Check that the expected result is returned
        assert_eq!(init_stack, tasm_stack, "Stack must match expected value");
    }
}
