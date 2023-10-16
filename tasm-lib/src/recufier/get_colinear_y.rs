use rand::{rngs::StdRng, Rng, SeedableRng};
use triton_vm::triton_asm;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::{
    closure::Closure,
    get_init_tvm_stack,
    snippet::{BasicSnippet, DataType},
};

pub struct ColinearYXfe;

impl BasicSnippet for ColinearYXfe {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::XFE, "p_2_x".to_owned()),
            (DataType::XFE, "p_1_y".to_owned()),
            (DataType::XFE, "p_1_x".to_owned()),
            (DataType::XFE, "p_0_y".to_owned()),
            (DataType::XFE, "p_0_x".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(DataType::XFE, "p_2_y".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_colinear_y_xfe".to_owned()
    }

    fn code(
        &self,
        _library: &mut crate::library::Library,
    ) -> Vec<triton_vm::instruction::LabelledInstruction> {
        triton_asm!(
            {self.entrypoint()}:
                // stack: _ p2x2 p2x1 p2x0 p1y2 p1y1 p1y0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p0x2 p0x1 p0x0
                swap 9
                // stack: _ p2x2 p2x1 p2x0 p1y2 p1y1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p0x2 p0x1 p1y0
                swap 1
                // stack: _ p2x2 p2x1 p2x0 p1y2 p1y1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p0x2 p1y0 p0x1
                swap 10
                // stack: _ p2x2 p2x1 p2x0 p1y2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p0x2 p1y0 p1y1
                swap 2
                // stack: _ p2x2 p2x1 p2x0 p1y2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y1 p1y0 p0x2
                swap 11
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y1 p1y0 p1y2
                swap 2
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 p1y0 p1y1
                swap 1
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 p1y1 p1y0
                dup 5 dup 5 dup 5
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 p1y1 p1y0 p0y2 p0y1 p0y0
                push -1
                xbmul
                xxadd
                push -1
                xbmul
                // dy = p0.y-p1.y
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 p1y1 p1y0 dy2 dy1 dy0
                swap 3 pop
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 p1y1 dy0 dy2 dy1
                swap 3 pop
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 p1y2 dy1 dy0 dy2
                swap 3 pop
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 dy2 dy1 dy0
                swap 6

                swap 1
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 dy0 p0y2 p0y1 p0y0 dy2 p1x0 dy1
                swap 7
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 dy1 dy0 p0y2 p0y1 p0y0 dy2 p1x0 p1x1
                swap 2
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 dy1 dy0 p0y2 p0y1 p0y0 p1x1 p1x0 dy2
                swap 8
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 p1x1 p1x0 p1x2
                swap 2 swap 1
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 p1x2 p1x1 p1x0
                dup 11 dup 11 dup 11
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 p1x2 p1x1 p1x0 p0x2 p0x1 p0x0
                push -1
                xbmul
                xxadd
                push -1
                xbmul
                // dx = p0.x-p1.x
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 p1x2 p1x1 p1x0 dx2 dx1 dx0
                swap 3 pop
                swap 3 pop
                swap 3 pop
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 dx2 dx1 dx0
                swap 12 swap 1 swap 13 swap 2 swap 14 swap 2 swap 1
                // stack: _ dx2 dx1 dx0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p0y2 p0y1 p0y0 p2x2 p2x1 p2x0
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                // stack: _ dx2 dx1 dx0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p2x2 p2x1 p2x0 p0y2 p0y1 p0y0
                swap 12 swap 1 swap 13 swap 2 swap 14 swap 2 swap 1
                // stack: _ p0y2 p0y1 p0y0 p0x2 p0x1 p0x0 dy2 dy1 dy0 p2x2 p2x1 p2x0 dx2 dx1 dx0
                swap 9 swap 1 swap 10 swap 2 swap 11 swap 2 swap 1
                // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 dy2 dy1 dy0 p2x2 p2x1 p2x0 p0x2 p0x1 p0x0
                push -1
                xbmul
                xxadd
                 // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 dy2 dy1 dy0 p2x2 p2x1 p2x0 (p2.x-p0.x)_2 (p2.x-p0.x)_1 (p2.x-p0.x)_0
                swap 3 pop
                swap 3 pop
                swap 3 pop
                // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 dy2 dy1 dy0 (p2.x-p0.x)_2 (p2.x-p0.x)_1 (p2.x-p0.x)_0
                xxmul
                // a = (p2.x-p0.x)*dy
                // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 dy2 dy1 dy0 a2 a1 a0
                swap 3 pop swap 3 pop swap 3 pop
                  // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 a2 a1 a0
                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                // stack: _ a2 a1 a0 dx2 dx1 dx0 p0y2 p0y1 p0y0
                dup 5 dup 5 dup 5
                // stack: _ a2 a1 a0 dx2 dx1 dx0 p0y2 p0y1 p0y0 dx2 dx1 dx0
                xxmul
                // b = (p0.y)*dx
                // stack: _ a2 a1 a0 dx2 dx1 dx0 p0y2 p0y1 p0y0 b2 b1 b0
                swap 3 pop swap 3 pop swap 3 pop
                // stack: _ a2 a1 a0 dx2 dx1 dx0 b2 b1 b0
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                // stack: _ a2 a1 a0 b2 b1 b0 dx2 dx1 dx0
                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                // stack: _ dx2 dx1 dx0 b2 b1 b0 a2 a1 a0
                xxadd
                // c = a+b
                // stack: _ dx2 dx1 dx0 b2 b1 b0 c2 c1 c0
                swap 3 pop swap 3 pop swap 3 pop
                // stack: _ dx2 dx1 dx0 c2 c1 c0
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                // stack: _ c2 c1 c0 dx2 dx1 dx0
                xinvert
                xxmul
                // p2.y = c/dx
                // stack: _ c2 c1 c0 p2y2 p2y1 p2y0
                swap 3 pop swap 3 pop swap 3 pop
                // stack: _ p2y2 p2y1 p2y0
                return
        )
    }
}

impl Closure for ColinearYXfe {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        // read points and x-coordinate off stack
        let p0x = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let p0y = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let p1x = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let p1y = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
        let p2x = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);

        // use Lagrange interpolation to find colinear y
        // sum_{i} y_i prod_{j=/=i} (x-xj)/(xi-xj)
        let lagrange = |x| p0y * (x - p1x) / (p0x - p1x) + p1y * (x - p0x) / (p1x - p0x);

        // sanity checks
        assert_eq!(lagrange(p1x), p1y);
        assert_eq!(lagrange(p0x), p0y);

        // compute colinear y
        let colinear_y = lagrange(p2x);

        // push result to stack and quit
        stack.push(colinear_y.coefficients[2]);
        stack.push(colinear_y.coefficients[1]);
        stack.push(colinear_y.coefficients[0]);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        let mut rng: StdRng = SeedableRng::from_seed(seed);

        let mut stack = get_init_tvm_stack();

        // push p2x
        stack.push(rng.gen());
        stack.push(rng.gen());
        stack.push(rng.gen());

        // push p1y
        stack.push(rng.gen());
        stack.push(rng.gen());
        stack.push(rng.gen());

        // push p1x
        stack.push(rng.gen());
        stack.push(rng.gen());
        stack.push(rng.gen());

        // push p0y
        stack.push(rng.gen());
        stack.push(rng.gen());
        stack.push(rng.gen());

        // push p0x
        stack.push(rng.gen());
        stack.push(rng.gen());
        stack.push(rng.gen());

        stack
    }
}

#[cfg(test)]
mod test {
    use crate::{closure::ShadowedClosure, snippet::RustShadow};

    use super::ColinearYXfe;

    #[test]
    fn test() {
        ShadowedClosure::new(ColinearYXfe).test()
    }
}
