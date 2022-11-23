use triton_vm::op_stack::OP_STACK_REG_COUNT;
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{execute, ExecutionResult};

fn _halt_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let code: &str = "halt";
    execute(code, stack, OP_STACK_REG_COUNT, vec![], vec![])
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

        let mut tvm_stack = init_stack.clone();
        let execution_result = _halt_tasm(&mut tvm_stack);
        println!("Cycle count for `add`: {}", execution_result.cycle_count);

        // Rust
        let mut rust_stack = init_stack.clone();
        _halt_tasm(&mut rust_stack);

        // Check that the two functions agree
        assert_eq!(tvm_stack, rust_stack, "Rust code must match TVM for `add`");

        // Check that the expected result is returned
        assert_eq!(
            get_init_tvm_stack(),
            tvm_stack,
            "Stack must match expected value"
        );
    }
}
