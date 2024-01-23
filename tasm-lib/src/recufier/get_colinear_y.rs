use rand::rngs::StdRng;
use rand::Rng;
use rand::SeedableRng;
use triton_vm::prelude::*;
use twenty_first::shared_math::polynomial::Polynomial;
use twenty_first::shared_math::x_field_element::EXTENSION_DEGREE;

use crate::data_type::DataType;
use crate::empty_stack;
use crate::library::Library;
use crate::snippet_bencher::BenchmarkCase;
use crate::traits::basic_snippet::BasicSnippet;
use crate::traits::closure::Closure;

pub struct ColinearYXfe;

impl BasicSnippet for ColinearYXfe {
    fn inputs(&self) -> Vec<(DataType, String)> {
        vec![
            (DataType::Xfe, "p_2_x".to_owned()),
            (DataType::Xfe, "p_1_y".to_owned()),
            (DataType::Xfe, "p_1_x".to_owned()),
            (DataType::Xfe, "p_0_y".to_owned()),
            (DataType::Xfe, "p_0_x".to_owned()),
        ]
    }

    fn outputs(&self) -> Vec<(DataType, String)> {
        vec![(DataType::Xfe, "p_2_y".to_owned())]
    }

    fn entrypoint(&self) -> String {
        "tasm_recufier_colinear_y_xfe".to_owned()
    }

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
                xbmul       // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y; 3] [-p0y; 3]
                xxadd       // _ [p2x; 3] [p0x; 3] [p1x; 3] [p0y; 3] [p1y - p0y; 3]
                push -1
                xbmul       // dy = p0y - p1y
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
                xbmul       // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x; 3] [-p0x; 3]
                xxadd       // _ [p2x; 3] [p0x; 3] [dy; 3] [p0y; 3] [p1x - p0x; 3]
                push -1
                xbmul       // dx = p0x - p1x
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
                xbmul
                xxadd       // _ [p0y; 3] [dx; 3] [dy; 3] [p2x - p0x; 3]
                xxmul       // a = (p2x - p0x) * dy
                            // _ [p0y; 3] [dx; 3] [a; 3]

                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                            // _ [a; 3] [dx; 3] [p0y; 3]
                dup 5 dup 5 dup 5
                            // _ [a; 3] [dx; 3] [p0y; 3] [dx; 3]
                xxmul       // b = p0y * dx
                            // _ [a; 3] [dx; 3] [b; 3]

                swap 3 swap 1 swap 4 swap 2 swap 5 swap 2 swap 1
                            // _ [a; 3] [b; 3] [dx; 3]
                swap 6 swap 1 swap 7 swap 2 swap 8 swap 2 swap 1
                            // _ [dx; 3] [b; 3] [a; 3]
                xxadd       // c = a + b
                            // _ [dx; 3] [c; 3]

                xinvert     // _ [dx; 3] [1/c; 3]
                xxmul       // _ [dx/c; 3]
                xinvert     // _ [c/dx; 3]
                            // _ [p2y; 3]
                return
        )
    }
}

impl Closure for ColinearYXfe {
    fn rust_shadow(&self, stack: &mut Vec<BFieldElement>) {
        let mut pop_xfe = || {
            let c_0 = stack.pop().unwrap();
            let c_1 = stack.pop().unwrap();
            let c_2 = stack.pop().unwrap();
            XFieldElement::new([c_0, c_1, c_2])
        };

        let p0 = (pop_xfe(), pop_xfe());
        let p1 = (pop_xfe(), pop_xfe());
        let p2x = pop_xfe();
        let p2y = Polynomial::get_colinear_y(p0, p1, p2x);

        let [c_0, c_1, c_2] = p2y.coefficients;
        stack.push(c_2);
        stack.push(c_1);
        stack.push(c_0);
    }

    fn pseudorandom_initial_state(
        &self,
        seed: [u8; 32],
        _bench_case: Option<BenchmarkCase>,
    ) -> Vec<BFieldElement> {
        const NUM_POINTS: usize = 2;
        const NUM_COORDINATES: usize = 2 * NUM_POINTS + 1;
        const NUM_ELEMENTS_PER_COORDINATE: usize = EXTENSION_DEGREE;
        const NUM_ELEMENTS: usize = NUM_ELEMENTS_PER_COORDINATE * NUM_COORDINATES;

        let mut rng: StdRng = SeedableRng::from_seed(seed);
        let elements: [BFieldElement; NUM_ELEMENTS] = rng.gen();

        [empty_stack(), elements.to_vec()].concat()
    }
}

#[cfg(test)]
mod test {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::ColinearYXfe;

    #[test]
    fn test() {
        ShadowedClosure::new(ColinearYXfe).test()
    }
}

#[cfg(test)]
mod bench {
    use crate::traits::closure::ShadowedClosure;
    use crate::traits::rust_shadow::RustShadow;

    use super::ColinearYXfe;

    #[test]
    fn bench_colinear_y() {
        ShadowedClosure::new(ColinearYXfe).bench();
    }
}
