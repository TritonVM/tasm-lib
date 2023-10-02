use triton_vm::triton_asm;
use twenty_first::shared_math::x_field_element::XFieldElement;

use crate::{
    closure::Closure,
    snippet::{self, BasicSnippet, DataType},
};

pub struct ColinearYXfe;

impl BasicSnippet for ColinearYXfe {
    fn inputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![
            (DataType::XFE, "p_0_x".to_owned()),
            (DataType::XFE, "p_0_y".to_owned()),
            (DataType::XFE, "p_1_x".to_owned()),
            (DataType::XFE, "p_1_y".to_owned()),
            (DataType::XFE, "p_2_x".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(crate::snippet::DataType, String)> {
        vec![(DataType::XFE, "p2_y".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_colinear_y_xfe".to_owned()
    }

    fn code(
        &self,
        library: &mut crate::library::Library,
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
                xxmul
                push -1
                xbmul
                // dy = p0.y-p1.y
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 p1x0 p0y2 p0y1 p0y0 dy2 dy1 dy0
                swap 6
                // stack: _ p2x2 p2x1 p2x0 p0x2 p0x1 p0x0 p1x2 p1x1 dy0 p0y2 p0y1 p0y0 dy2 dy1 p1x0
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
                xxmul
                push -1
                xbmul
                // dx = p0.x-p1.x
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
                xxmul
                // a = (p2.x-p0.x)*dy
                // stack: _ p0y2 p0y1 p0y0 dx2 dx1 dx0 a2 a1 a0
                swap 9 swap 1 swap 10 swap 2 swap 11 swap 2 swap 1
                // stack: _ a2 a1 a0 dx2 dx1 dx0 p0y2 p0y1 p0y0
                dup 5 dup 5 dup 5
                // stack: _ a2 a1 a0 dx2 dx1 dx0 p0y2 p0y1 p0y0 dx2 dx1 dx0
                xxmul
                // b = (p0.y)*dx
                // stack: _ a2 a1 a0 dx2 dx1 dx0 b2 b1 b0
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                // stack: _ a2 a1 a0 b2 b1 b0 dx2 dx1 dx0
                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                // stack: _ dx2 dx1 dx0 b2 b1 b0 a2 a1 a0
                xxadd
                // c = a+b
                // stack: _ dx2 dx1 dx0 c2 c1 c0
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                // stack: _ c2 c1 c0 dx2 dx1 dx0 
                xinvert
                mul
                // p2.y = c/dx
                // stack: _ p2y2 p2y1 p2y0
                return
        )
    }
}

impl Closure for ColinearYXfe {
    fn rust_shadow(&self, stack: &mut Vec<triton_vm::BFieldElement>) {
        let p2x = XFieldElement::new([
            stack.pop().unwrap(),
            stack.pop().unwrap(),
            stack.pop().unwrap(),
        ]);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        bench_case: Option<crate::snippet_bencher::BenchmarkCase>,
    ) -> Vec<triton_vm::BFieldElement> {
        todo!()
    }
}
