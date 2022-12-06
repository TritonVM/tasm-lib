use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::snippet_trait::Snippet;

pub struct Halt();

impl Snippet for Halt {
    const STACK_DIFF: isize = 0;

    fn get_name() -> String {
        "halt".to_string()
    }

    fn get_code() -> String {
        "halt".to_string()
    }

    fn rust_shadowing(
        _stack: &mut Vec<BFieldElement>,
        _std_in: Vec<BFieldElement>,
        _secret_in: Vec<BFieldElement>,
    ) {
    }
}

#[cfg(test)]
mod tests {
    use crate::get_init_tvm_stack;

    use super::*;

    #[test]
    fn halt() {
        // TASM
        let init_stack = get_init_tvm_stack();

        let mut tasm_stack = init_stack.clone();
        let execution_result = Halt::run_tasm(&mut tasm_stack, vec![], vec![]);
        println!("Cycle count for `halt`: {}", execution_result.cycle_count);
        println!(
            "Hash table height for `hash`: {}",
            execution_result.hash_table_height
        );

        // Rust
        let mut rust_stack = init_stack.clone();
        Halt::rust_shadowing(&mut rust_stack, vec![], vec![]);

        // Check that the two functions agree
        assert_eq!(
            tasm_stack, rust_stack,
            "Rust code must match TVM for `halt`"
        );

        // Check that the expected result is returned
        assert_eq!(init_stack, tasm_stack, "Stack must match expected value");
    }
}
