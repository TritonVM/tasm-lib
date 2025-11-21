use triton_vm::prelude::*;

use crate::prelude::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CollinearYXfe;

impl BasicSnippet for CollinearYXfe {
    fn parameters(&self) -> Vec<(DataType, String)> {
        ["p_2_x", "p_1_y", "p_1_x", "p_0_y", "p_0_x"]
            .map(|s| (DataType::Xfe, s.to_string()))
            .to_vec()
    }

    fn return_values(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "p_2_y".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasmlib_verifier_fri_collinear_y_xfe".to_owned()
    }

    // Original:
    // let dy = p0.1 - p1.1;
    // let dx = p0.0 - p1.0;
    // let p2_y_times_dx = dy * (p2_x - p0.0) + dx * p0.1;

    // // Can we implement this without division?
    // p2_y_times_dx / dx

    // let dy = a_y - b_y;
    // let dx = a_x - b_x;
    // let p2_y_times_dx = dy * (c_x - a_x) + dx * a_y;

    // p2_y_times_dx / dx

    // So we want:
    // p2_y_times_dx = ((a_y - b_y) * (c_x - a_x) + (a_x - b_x) * a_y) / dx
    // Calculate the inner parenthesis first.
    // So first (a_y - b_y), then (c_x - a_x), then

    fn code(&self, _library: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // BEFORE: _ [p2x; 3] [p1y; 3] [p1x; 3] [p0y; 3] [p0x; 3]
            // AFTER:  _ [p2y; 3]
            {self.entrypoint()}:
                swap 9      // _ [p2x; 3] p1y2 p1y1 p0x0 [p1x; 3] [p0y; 3] p0x2 p0x1 p1y0
                swap 1      // _ [p2x; 3] p1y2 p1y1 p0x0 [p1x; 3] [p0y; 3] p0x2 p1y0 p0x1
                swap 10     // _ [p2x; 3] p1y2 p0x1 p0x0 [p1x; 3] [p0y; 3] p0x2 p1y0 p1y1
                swap 2      // _ [p2x; 3] p1y2 p0x1 p0x0 [p1x; 3] [p0y; 3] p1y1 p1y0 p0x2
                swap 11     // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] p1y1 p1y0 p1y2
                swap 2      // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] p1y2 p1y0 p1y1
                swap 1      // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y; 3]
                dup 5 dup 5 dup 5
                            // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y; 3] [p0y; 3]
                push -1
                xb_mul       // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y; 3] [-p0y; 3]
                xx_add       // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y - p0y; 3]
                push -1
                xb_mul       // dy = p0y - p1y
                            // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [dy; 3]

                swap 6      // _ [p2x; 3] [p0x; 3] p1x2 p1x1 dy0 [p0y; 3] dy2 dy1 p1x0
                swap 1      // _ [p2x; 3] [p0x; 3] p1x2 p1x1 dy0 [p0y; 3] dy2 p1x0 dy1
                swap 7      // _ [p2x; 3] [p0x; 3] p1x2 dy1 dy0 [p0y; 3] dy2 p1x0 p1x1
                swap 2      // _ [p2x; 3] [p0x; 3] p1x2 dy1 dy0 [p0y; 3] p1x1 p1x0 dy2
                swap 8      // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] p1x1 p1x0 p1x2
                swap 2      // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] p1x2 p1x0 p1x1
                swap 1      // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x; 3]
                dup 11 dup 11 dup 11
                            // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x; 3] [p0x; 3]
                push -1
                xb_mul       // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x; 3] [-p0x; 3]
                xx_add       // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x - p0x; 3]
                push -1
                xb_mul       // dx = p0x - p1x
                            // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [dx; 3]

                swap 12 swap 1 swap 13 swap 2 swap 14 swap 2 swap 1
                            // _ [dx; 3] [p0x; 3] [dy; 3] [p0y; 3] [p2x; 3]
                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                            // _ [dx; 3] [p0x; 3] [dy; 3] [p2x; 3] [p0y; 3]
                swap 12 swap 1 swap 13 swap 2 swap 14 swap 2 swap 1
                            // _ [p0y; 3] [p0x; 3] [dy; 3] [p2x; 3] [dx; 3]
                swap 9 swap 1 swap 10 swap 2 swap 11 swap 2 swap 1
                            // _ [p0y; 3] [dx; 3] [dy; 3] [p2x; 3] [p0x; 3]
                push -1
                xb_mul
                xx_add       // _ [p0y; 3] [dx; 3] [dy; 3] [p2x - p0x; 3]
                xx_mul       // a = (p2x - p0x) * dy
                            // _ [p0y; 3] [dx; 3] [a; 3]

                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                            // _ [a; 3] [dx; 3] [p0y; 3]
                dup 5 dup 5 dup 5
                            // _ [a; 3] [dx; 3] [p0y; 3] [dx; 3]
                xx_mul       // b = p0y * dx
                            // _ [a; 3] [dx; 3] [b; 3]

                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                            // _ [a; 3] [b; 3] [dx; 3]
                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                            // _ [dx; 3] [b; 3] [a; 3]
                xx_add       // c = a + b
                            // _ [dx; 3] [c; 3]

                x_invert     // _ [dx; 3] [1/c; 3]
                xx_mul       // _ [dx/c; 3]
                x_invert     // _ [c/dx; 3]
                            // _ [p2y; 3]
                return
        )
    }
}

#[cfg(test)]
mod tests {
    use twenty_first::math::polynomial::Polynomial;

    use super::*;
    use crate::test_prelude::*;

    impl Closure for CollinearYXfe {
        type Args = (
            XFieldElement,
            XFieldElement,
            XFieldElement,
            XFieldElement,
            XFieldElement,
        );

        fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
            let (p2x, p1y, p1x, p0y, p0x) = pop_encodable::<Self::Args>(stack);
            let p2y = Polynomial::get_colinear_y((p0x, p0y), (p1x, p1y), p2x);
            push_encodable(stack, &p2y);
        }

        fn pseudorandom_args(&self, seed: [u8; 32], _: Option<BenchmarkCase>) -> Self::Args {
            StdRng::from_seed(seed).random()
        }
    }

    #[test]
    fn test() {
        ShadowedClosure::new(CollinearYXfe).test()
    }
}

#[cfg(test)]
mod bench {
    use super::*;
    use crate::test_prelude::*;

    #[test]
    fn benchmark() {
        ShadowedClosure::new(CollinearYXfe).bench();
    }
}
