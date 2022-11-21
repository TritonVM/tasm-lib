use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{execute, ExecutionResult};

fn _bfe_add_tasm(stack: &mut Vec<BFieldElement>) -> ExecutionResult {
    let code: &str = "add";
    execute(code, stack, 1, vec![], vec![])
}

fn _bfe_add_rust(stack: &mut Vec<BFieldElement>) {
    let a = stack.pop().unwrap();
    let b = stack.pop().unwrap();
    stack.push(a + b);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bfe_add_simple() {
        // TASM
        let mut stack = vec![BFieldElement::new(200), BFieldElement::new(300)];
        let execution_result = _bfe_add_tasm(&mut stack);
        println!("Cycle count for `add`: {}", execution_result.cycle_count);
        assert_eq!(vec![BFieldElement::new(500)], stack);

        // Rust
        stack = vec![BFieldElement::new(200), BFieldElement::new(300)];
        _bfe_add_rust(&mut stack);
        assert_eq!(vec![BFieldElement::new(500)], stack);
    }
}
