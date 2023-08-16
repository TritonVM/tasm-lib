
use twenty_first::shared_math::b_field_element::BFieldElement;

use crate::{
    get_init_tvm_stack,
    snippet::{DataType, Snippet},
    ExecutionState,
    };

#[derive(Debug, Clone)]

impl Snippet for SafeMulU128 {

fn entrypoint(&self) -> String {

"tasm_arithmetic_u128_safe_mul".to_string()

}

fn inputs(&self) -> Vec<String> {

vec![

"rhs_3".to_string(),

"rhs_2".to_string(),

"rhs_1".to_string(),

"rhs_0".to_string(),

"lhs_3".to_string(),

"lhs_2".to_string(),

"lhs_1".to_string(),

"lhs_0".to_string(),

]

}

  

fn input_types(&self) -> Vec<DataType> {

vec![DataType::U128, DataType::U128]

}

  

fn output_types(&self) -> Vec<DataType> {

vec![DataType::U128]

}

  

fn outputs(&self) -> Vec<String> {

fn outputs(&self) -> Vec<String> {

vec![

"prod_0".to_string(),

"prod_1".to_string(),

"prod_2".to_string(),

"prod_3".to_string(),

]

}

}

  

fn stack_diff(&self) -> isize {

-4

}

  

fn function_code(&self, _library: &mut Library) -> String {

let entrypoint = self.entrypoint();

format!(

"

    // BEFORE: _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0

    // AFTER: _ prod_3 prod_2 prod_1 prod_0


    // The product limbs are defined as follows:

    // a = lhs_0*rhs_0

    // prod_0 = a_lo

    // b = (lhs_1*rhs_0 + lhs_0*rhs_1 + (lhs_0*rhs_0)_hi)

    // prod_1 = b_lo

    // c = (lhs_2*rhs_0 + lhs_1*rhs_1 + lhs_0*rhs_2 + (lhs_1*rhs_0 + lhs_0*rhs_1 + a_hi)

    // prod_2 = c_lo

    // d = (lhs_3*rhs_0 + lhs_2*rhs_1 + lhs_1*rhs_2 + lhs_0*rhs_3 + (lhs_2*rhs_0 + lhs_1*rhs_1 + lhs_0*rhs_2 + b_hi)

    // prod_3 = d_lo

    

    //The checks to be performed are:

    // 1. d_hi == 0

    // 2. lhs_1*rhs_3 == 0

    // 3. lhs_2*rhs_2 == 0

    // 3. lhs_2*rhs_3 == 0

    // 4. lhs_3*rhs_1 == 0

    // 6. lhs_3*rhs_2 == 0

    // 7. lhs_3*rhs_3 == 0

    

    {entrypoint}:

    // a = lhs_0 * rhs_lo

    dup 0 dup 5 mul

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a

    

    // split `a` into `a_hi` and `a_lo`

    split

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_hi a_lo

    

    // swap a_hi and a_lo

    swap 1

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi

    

    // lhs_1 * rhs_0

    dup 3 dup 7 mul

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo a_hi (lhs_1 * rhs_0)

    // lhs_0 * rhs_1 and add successively to produce `b`

    dup 3 dup 8 mul add add

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo (a_hi + (lhs_0 * rhs_1) + (lhs_1 * rhs_0))

    // or, in terms of b,

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b

    

    // split b

    split

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_hi b_lo

    // swap b_hi and b_lo

    swap 1

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi

    

    // lhs_2 * rhs_0

    dup 5 dup 8 mul

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi (lhs_2 * rhs_0)

    

    // lhs_1 * rhs_1 and add

    dup 5 dup 10 mul add

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo b_hi ((lhs_2 * rhs_0)+(lhs_1 * rhs_1))

    

    // lhs_0 * rhs_2 and add successively to give `c`

    dup 4 dup 11 mul add add

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo (b_hi+(lhs_2 * rhs_0)+(lhs_1 * rhs_1)+(lhs_0 * rhs_2))

    

    // split c

    split

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_hi c_lo

    

    // swap c_hi and c_lo

    swap 1

    // _ rhs_3 rhs_2 rhs_1 rhs_0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi

    

    // push 0 and swap with rhs_0

    push 0 swap 9

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi rhs_0

    // lhs_3 * rhs_0, consume rhs_0

    dup 8 mul

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 lhs_0 a_lo b_lo c_lo c_hi (lhs_3 * rhs_0)

    

    //lhs_0 * rhs_3, mul and add to consume lhs_0

    push 0 swap 6 dup 9 mul add

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo c_hi ((lhs_3 * rhs_0)+(lhs_0 * rhs_3))

    

    //lhs_1 * rhs_2

    dup 6 dup 12 mul add

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo c_hi ((lhs_3 * rhs_0)+(lhs_0 * rhs_3)+(lhs_1 * rhs_2))

    

    //lhs_2 * rhs_1 and add successively to give `d`

    dup 7 dup 11 mul add add

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo (c_hi + (lhs_3 * rhs_0)+(lhs_0 * rhs_3)+(lhs_1 * rhs_2)+(lhs_2 * rhs_1))

    

    // split d

    split

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_hi d_lo

    // swap d_hi and d_lo

    swap 1

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_lo d_hi

    // check if d_hi == 0, crash if d_hi!=0

    push 0

    eq

    assert

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_lo d_hi 0

    pop

    pop

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 lhs_1 0 a_lo b_lo c_lo d_lo

    

    // push 0 and swap with lhs_1

    push 0 swap 6

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo lhs_1

    

    // lhs_1 * rhs_3, consume lhs_1

    dup 12 mul

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo (lhs_1 * rhs_3)

    

    // check if lhs_1 * rhs_3 == 0, crash if lhs_1 * rhs_3 != 0

    push 0

    eq

    assert

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo (lhs_1 * rhs_3) 0

    

    // discard lhs_1 * rhs_3

    pop

    pop

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 lhs_2 0 0 a_lo b_lo c_lo d_lo

    

    // push 0 and swap with lhs_2

    push 0 swap 7

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2

    

    // duplicate lhs_2

    dup

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2 lhs_2

    

    // lhs_2 * rhs_2

    dup 12 mul

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2 (lhs_2 * rhs_2)

    

    //check if lhs_2 * rhs_2 == 0, crash if lhs_2 * rhs_2 != 0

    push 0

    eq

    assert

    

    // discard lhs_2 * rhs_2

    pop

    pop

    // _rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo lhs_2

    
    

    // lhs_2 * rhs_3, consume lhs_2]

    dup 12 mul

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_2 * rhs_3)

    

    // check if lhs_2 * rhs_3 == 0, crash if lhs_2 * rhs_3 != 0

    push 0

    eq

    assert

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_2 * rhs_3) 0

    

    // discard lhs_2 * rhs_3

    pop

    pop

    // _ rhs_3 rhs_2 rhs_1 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

    

    // push 0 and swap with rhs_1

    push 0 swap 10

    // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_1

    // lhs_3 * rhs_1

    dup 8 mul

    // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_1)

    

    // check if lhs_3 * rhs_1 == 0, crash if lhs_3 * rhs_1 != 0

    push 0

    eq

    assert

    // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_1) 0

    

    // discard lhs_3 * rhs_1

    pop

    pop

    // _ rhs_3 rhs_2 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

    

    // push 0 and swap with rhs_2

    push 0 swap 11

    // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_2

    

    // lhs_3 * rhs_2

    dup 8 mul

    // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_2)

    

    // check if lhs_3 * rhs_2 == 0, crash if lhs_3 * rhs_2 != 0

    push 0

    eq

    assert

    // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_2) 0

    

    // discard lhs_3 * rhs_2

    pop

    pop

    // _ rhs_3 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo

    

    // push 0 and swap with rhs_3

    push 0 swap 12

    // _ 0 0 0 0 lhs_3 0 0 0 a_lo b_lo c_lo d_lo rhs_3

    

    // push 0 and swap with lhs_3

    push 0 swap 9

    // _ 0 0 0 0 0 0 0 0 a_lo b_lo c_lo d_lo lhs_3 rhs_3

    

    // lhs_3 * rhs_3, consume both

    mul

    

    // check if lhs_3 * rhs_3 == 0, crash if lhs_3 * rhs_3 != 0

    push 0

    eq

    assert

    // _ 0 0 0 0 0 0 0 0 a_lo b_lo c_lo d_lo (lhs_3 * rhs_3) 0

    

    // discard lhs_3 * rhs_3

    pop

    pop

    

    // now swap 0s with the product limbs and pop the 0s

    swap 0 swap 1 swap 2 swap 3

    pop

    pop

    pop

    pop

    

    // _ prod_3 prod_2 prod_1 prod_0

    return

"

)

}

}