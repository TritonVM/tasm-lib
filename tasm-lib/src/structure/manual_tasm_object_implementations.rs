use itertools::Itertools;
use num_traits::Zero;
use triton_vm::prelude::*;
use twenty_first::error::BFieldCodecError;
use twenty_first::error::PolynomialBFieldCodecError;
use twenty_first::math::x_field_element::EXTENSION_DEGREE;
use twenty_first::prelude::*;

use super::tasm_object::Result;
use crate::prelude::*;

impl<const N: usize, T> TasmObject for [T; N]
where
    T: TasmObject,
{
    fn label_friendly_name() -> String {
        format!("array{N}___{}", T::label_friendly_name())
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        if let Some(static_size) = Self::static_length() {
            return triton_asm!(
                // _ *elem[0]

                pop 1
                push {static_size}
                // _ own_size
            );
        }

        todo!()
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let mut vector = Vec::with_capacity(N);
        for _ in 0..N {
            if T::static_length().is_none() {
                iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
            };
            vector.push(*T::decode_iter(iterator)?);
        }

        // `unwrap()` needs `T` to implement `Debug` – unwrap manually instead
        let Ok(array) = vector.try_into() else {
            unreachable!()
        };

        Ok(Box::new(array))
    }
}

impl<T> TasmObject for Vec<T>
where
    T: TasmObject,
{
    fn label_friendly_name() -> String {
        format!("vec___{}", T::label_friendly_name())
    }

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction> {
        if T::static_length().is_some() {
            return triton_asm!(
                // _ *list_len

                read_mem 1
                // _ list_len *elem[0]

                {&T::compute_size_and_assert_valid_size_indicator(library)}
                // _ list_len elem_size

                mul
                addi 1
                // _ (list_len * elem_size + 1)
                // _ calculated_size
            );
        }

        let loop_label = format!(
            "tasmlib_structure_tasmobject_verify_size_indicators_dyn_elem_sizes___{}",
            T::label_friendly_name()
        );

        let loop_code = triton_asm!(
            // INVARIANT: _ remaining_elements acc_size *element_si
            {loop_label}:

                dup 2
                push 0
                eq
                skiz
                    return
                // _ remaining_elements acc_size *element_si

                read_mem 1
                // _ remaining_elements acc_size element_si (*element_si-1)

                /* Verify that max allowed size is not exceeded */
                push {T::MAX_OFFSET}
                dup 2
                lt
                assert error_id 210
                // _ remaining_elements acc_size element_si (*element_si-1)

                addi 2
                // _ remaining_elements acc_size element_si *element

                dup 0
                {&T::compute_size_and_assert_valid_size_indicator(library)}
                // _ remaining_elements acc_size element_si *element calculated_elem_size

                dup 2
                eq
                assert error_id 211
                // _ remaining_elements acc_size element_si *element

                pick 2
                dup 2
                add
                // _ remaining_elements element_si *element acc_size'

                /* Account for element's size indicator, since it's dynamically sized */
                addi 1
                // _ remaining_elements element_si *element acc_size'

                place 2
                // _ remaining_elements acc_size' element_si *element

                add
                // _ remaining_elements acc_size' *next_element

                pick 2
                addi -1
                place 2
                // _ (remaining_elements-1) acc_size' *next_element

                recurse
        );

        library.explicit_import(&loop_label, &loop_code);
        triton_asm!(
            // _ *list_len

            read_mem 1
            hint remaining_elements = stack[1]
            // _ list_len (*list_len - 1)

            addi 2
            hint elem_si_ptr = stack[0]
            // _ list_len           (*list_len + 1)
            // _ remaining_elements *element[0]_si

            push 0
            hint acc_size = stack[0]
            // _ remaining_elements *element[0]_si acc_size

            place 1
            // _ remaining_elements acc_size *element[0]_si

            call {loop_label}
            // _ 0 acc_size *EOF

            pick 2
            pop 2
            // _ acc_size

            /* Add size of (outer) list's length indicator */
            addi 1
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let vec_length = iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        let mut vector = vec![];
        for _ in 0..vec_length.value() {
            if T::static_length().is_none() {
                iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
            };
            vector.push(*T::decode_iter(iterator)?);
        }

        Ok(Box::new(vector))
    }
}

impl TasmObject for BFieldElement {
    fn label_friendly_name() -> String {
        DataType::Bfe.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let word = iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        Ok(Box::new(word))
    }
}

impl TasmObject for XFieldElement {
    fn label_friendly_name() -> String {
        DataType::Xfe.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let (c_0, c_1, c_2) = iterator
            .next_tuple()
            .ok_or(BFieldCodecError::SequenceTooShort)?;

        Ok(Box::new(xfe!([c_0, c_1, c_2])))
    }
}

impl TasmObject for Digest {
    fn label_friendly_name() -> String {
        DataType::Digest.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let (d_0, d_1, d_2, d_3, d_4) = iterator
            .next_tuple()
            .ok_or(BFieldCodecError::SequenceTooShort)?;

        Ok(Box::new(Digest::new([d_0, d_1, d_2, d_3, d_4])))
    }
}

impl TasmObject for bool {
    fn label_friendly_name() -> String {
        DataType::Bool.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let the_bool = iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        match the_bool.value() {
            0 => Ok(Box::new(false)),
            1 => Ok(Box::new(true)),
            _ => Err(Box::new(BFieldCodecError::ElementOutOfRange)),
        }
    }
}

impl TasmObject for u32 {
    fn label_friendly_name() -> String {
        DataType::U32.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let word = iterator
            .next()
            .ok_or(BFieldCodecError::SequenceTooShort)?
            .try_into()
            .map_err(|_| BFieldCodecError::ElementOutOfRange)?;

        Ok(Box::new(word))
    }
}

impl TasmObject for u64 {
    fn label_friendly_name() -> String {
        DataType::U64.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let (val_lo, val_hi) = iterator
            .next_tuple()
            .ok_or(BFieldCodecError::SequenceTooShort)?;

        Ok(Self::decode(&[val_lo, val_hi])?)
    }
}

impl TasmObject for u128 {
    fn label_friendly_name() -> String {
        DataType::U128.label_friendly_name()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(pop 1 push {Self::static_length().unwrap()})
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let (lo_lo, lo_hi, hi_lo, hi_hi) = iterator
            .next_tuple()
            .ok_or(BFieldCodecError::SequenceTooShort)?;

        Ok(Self::decode(&[lo_lo, lo_hi, hi_lo, hi_hi])?)
    }
}

impl<T, S> TasmObject for (T, S)
where
    T: TasmObject,
    S: TasmObject,
{
    fn label_friendly_name() -> String {
        format!(
            "tuple_L_{}_{}_R",
            T::label_friendly_name(),
            S::label_friendly_name()
        )
    }

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction> {
        let size_left = if T::static_length().is_some() {
            T::compute_size_and_assert_valid_size_indicator(library)
        } else {
            triton_asm!(
                // _ *left_si
                hint left_si_ptr = stack[0]

                read_mem 1
                addi 2
                // _ left_si *left

                {&T::compute_size_and_assert_valid_size_indicator(library)}
                hint calculated_left = stack[0]
                // _ left_si calculated_left

                dup 1
                eq
                assert error_id 220
                // _ left_size

                addi 1
                // _ (left_size + 1)
            )
        };

        let size_right = if S::static_length().is_some() {
            S::compute_size_and_assert_valid_size_indicator(library)
        } else {
            triton_asm!(
                // _ *right_si
                hint right_si_ptr = stack[0]

                read_mem 1
                hint right_si = stack[1]
                // _ right_si (*right_si - 1)


                addi 2
                // _ right_size *right

                {&S::compute_size_and_assert_valid_size_indicator(library)}
                hint calculated_right = stack[0]
                // _ right_size calculated_right_size

                dup 1
                eq
                assert error_id 221
                // _ right_size

                /* Include size of size-indicator */
                addi 1
                // _ (right_size+1)
            )
        };

        triton_asm!(
            // _ *tuple

            dup 0
            {&size_right}
            hint right_ptr_or_right_si = stack[1]
            hint right_size_incl_pot_si = stack[0]
            // _ *right right_size'

            pick 1
            dup 1
            add
            hint left: Pointer = stack[0]
            // _ right_size' (*right + right_size')
            // _ right_size' *left

            {&size_left}
            hint left_size_incl_si = stack[0]
            // _ right_size' left_size'

            add
            // _ total_tuple_size <-- includes tuple-element's size-indicators (if dyn-sized)
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        if S::static_length().is_none() {
            iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        }
        let s = *S::decode_iter(iterator)?;

        if T::static_length().is_none() {
            iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        }
        let t = *T::decode_iter(iterator)?;

        Ok(Box::new((t, s)))
    }
}

impl TasmObject for Polynomial<'static, XFieldElement> {
    fn label_friendly_name() -> String {
        "polynomial_xfe".to_owned()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        triton_asm!(
            // _ *field_size

            addi 1
            // _ *list_length

            read_mem 2
            pop 1
            // _ list_length field_size

            push {Self::MAX_OFFSET}
            dup 1
            lt
            assert
            // _ list_length field_size

            pick 1
            push {EXTENSION_DEGREE}
            mul
            addi 1
            // _ field_size calculated_field_size

            dup 1
            eq
            assert
            // _ field_size

            /* Account for a size-indicator */
            addi 1
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        let coefficients = *Vec::<XFieldElement>::decode_iter(iterator)?;
        if coefficients.last().is_some_and(|c| c.is_zero()) {
            return Err(PolynomialBFieldCodecError::TrailingZerosInPolynomialEncoding)?;
        }

        Ok(Box::new(Self::new(coefficients)))
    }
}

impl TasmObject for Proof {
    fn label_friendly_name() -> String {
        "tvm_proof".to_owned()
    }

    fn compute_size_and_assert_valid_size_indicator(_: &mut Library) -> Vec<LabelledInstruction> {
        // Proofs are special, as the fields of a proof is only accessed through
        // the [`DequeueNextAs`](crate::verifier::vm_proof_iter::dequeue_next_as)
        // snippet which does some checks itself. So we just report the total size
        // here.

        triton_asm!(
            // _ *proof

            read_mem 1
            pop 1
            // _ field_0_len

            addi 1
            // _ own_size
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let _ = iterator.next().ok_or(BFieldCodecError::SequenceTooShort)?;
        Ok(Box::new(Self(*Vec::decode_iter(iterator)?)))
    }
}

impl<T> TasmObject for Option<T>
where
    T: TasmObject,
{
    fn label_friendly_name() -> String {
        format!("option_L_{}_R", T::label_friendly_name())
    }

    fn compute_size_and_assert_valid_size_indicator(
        library: &mut Library,
    ) -> Vec<LabelledInstruction> {
        let some_branch_label = format!(
            "tasmlib_tasmobject_size_verifier_option_some_branch___{}",
            T::label_friendly_name()
        );
        let some_branch = triton_asm!(
            {some_branch_label}:

                // _ *value 1
                pop 1

                {&T::compute_size_and_assert_valid_size_indicator(library)}
                // _ value_size

                /* Push 0 to avoid `None` branch from being taken */
                push 0

                return
        );

        let none_branch_label = "tasmlib_tasmobject_size_verifier_option_none".to_owned();
        let none_branch = triton_asm!(
            {none_branch_label}:
                // _ *ptr

                pop 1
                push 0
                // _ value_size

                return
        );

        library.explicit_import(&some_branch_label, &some_branch);
        library.explicit_import(&none_branch_label, &none_branch);

        triton_asm!(
            // _ *discriminant
            read_mem 1
            addi 2
            // _ discriminant (*discriminant + 1)

            /* Ensure discriminant has legal value */
            dup 1
            push 0
            eq
            dup 2
            push 1
            eq
            add
            // _ discriminant (*discriminant + 1) ((discriminant == 0) || (discriminant == 1))

            assert error_id 200
            // _ discriminant (*discriminant + 1)

            push 1
            // _ discriminant (*discriminant + 1) 1

            pick 2
            push 1
            eq
            // _ (*discriminant + 1) 1 (discriminant == 1)

            skiz
                call {some_branch_label}
            skiz
                call {none_branch_label}

            // _ value_size
            addi 1

            // _ total_size
        )
    }

    fn decode_iter<Itr: Iterator<Item = BFieldElement>>(iterator: &mut Itr) -> Result<Box<Self>> {
        let is_some = *bool::decode_iter(iterator)?;
        let the_option = is_some.then(|| T::decode_iter(iterator)).transpose()?;
        Ok(Box::new(the_option.map(|t| *t)))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fmt::Debug;

    use proptest::prelude::*;
    use proptest::test_runner::TestCaseResult;
    use proptest_arbitrary_interop::arb;
    use test_strategy::Arbitrary;
    use test_strategy::proptest;

    use super::*;
    use crate::memory::encode_to_memory;

    #[derive(Debug, Clone, Arbitrary)]
    struct TestObject<T>
    where
        T: TasmObject + Debug + Clone + Eq + for<'a> arbitrary::Arbitrary<'a> + 'static,
    {
        #[strategy(arb())]
        t: T,

        #[strategy(arb())]
        address: BFieldElement,
    }

    impl<T> TestObject<T>
    where
        T: TasmObject + Debug + Clone + Eq + for<'a> arbitrary::Arbitrary<'a>,
    {
        fn verify_decoding_properties(&self) -> TestCaseResult {
            let mut memory = HashMap::default();
            encode_to_memory(&mut memory, self.address, &self.t);
            let decoding_result = T::decode_from_memory(&memory, self.address);
            prop_assert!(decoding_result.is_ok());

            let obj_read = *decoding_result.unwrap();
            prop_assert_eq!(&self.t, &obj_read);

            Ok(())
        }
    }

    /// Generate a test case with the specified name for the specified type.
    macro_rules! gen_test {
        (fn $test_fn:ident for $type:ty) => {
            #[proptest]
            fn $test_fn(test_object: TestObject<$type>) {
                test_object.verify_decoding_properties()?;
            }
        };
    }

    gen_test!(fn decode_bfe for BFieldElement);
    gen_test!(fn decode_xfe for XFieldElement);
    gen_test!(fn decode_digest for Digest);
    gen_test!(fn decode_bool for bool);
    gen_test!(fn decode_u32 for u32);
    gen_test!(fn decode_u64 for u64);
    gen_test!(fn decode_u128 for u128);
    gen_test!(fn decode_poly_xfe for Polynomial<'static, XFieldElement>);
    gen_test!(fn decode_proof for Proof);

    gen_test!(fn decode_array_bfe_0 for [BFieldElement; 0]);
    gen_test!(fn decode_array_bfe_2 for [BFieldElement; 2]);
    gen_test!(fn decode_array_bfe_10 for [BFieldElement; 10]);
    gen_test!(fn decode_array_bfe_25 for [BFieldElement; 25]);
    gen_test!(fn decode_array_xfe_0 for [BFieldElement; 0]);
    gen_test!(fn decode_array_xfe_2 for [BFieldElement; 2]);
    gen_test!(fn decode_array_xfe_10 for [BFieldElement; 10]);
    gen_test!(fn decode_array_xfe_25 for [BFieldElement; 25]);
    gen_test!(fn decode_array_vec_bfe_0 for [Vec<BFieldElement>; 0]);
    gen_test!(fn decode_array_vec_bfe_2 for [Vec<BFieldElement>; 2]);
    gen_test!(fn decode_array_vec_vec_bfe_2 for [Vec<Vec<BFieldElement>>; 2]);
    gen_test!(fn decode_array_tuple_l_bfe_bfe_r_0 for [(BFieldElement, BFieldElement); 0]);
    gen_test!(fn decode_array_tuple_l_bfe_bfe_r_2 for [(BFieldElement, BFieldElement); 2]);
    gen_test!(fn decode_array_tuple_l_u64_u128_r_0 for [(u64, u128); 0]);
    gen_test!(fn decode_array_tuple_l_u64_u128_r_2 for [(u64, u128); 2]);
    gen_test!(fn decode_array_tuple_l_vec_u64_vec_u64_r_2 for [(Vec<u64>, Vec<u64>); 2]);
    gen_test!(fn decode_array_option_bfe_0 for [Option<BFieldElement>; 0]);
    gen_test!(fn decode_array_option_bfe_5 for [Option<BFieldElement>; 5]);
    gen_test!(fn decode_array_option_vec_u64_5 for [Option<Vec<u64>>; 5]);

    gen_test!(fn decode_vec_bfe for Vec<BFieldElement>);
    gen_test!(fn decode_vec_xfe for Vec<XFieldElement>);
    gen_test!(fn decode_vec_digest for Vec<Digest>);
    gen_test!(fn decode_vec_array_u32_0 for Vec<[u32; 0]>);
    gen_test!(fn decode_vec_array_u32_7 for Vec<[u32; 7]>);
    gen_test!(fn decode_vec_array_u64_0 for Vec<[u64; 0]>);
    gen_test!(fn decode_vec_array_u64_7 for Vec<[u64; 7]>);
    gen_test!(fn decode_vec_vec_digest for Vec<Vec<Digest>>);
    gen_test!(fn decode_vec_tuple_l_u32_u64_r for Vec<(u32, u64)>);
    gen_test!(fn decode_vec_vec_tuple_l_u32_u64_r for Vec<Vec<(u32, u64)>>);
    gen_test!(fn decode_vec_tuple_l_vec_u32_vec_u64_r for Vec<(Vec<u32>, Vec<u64>)>);
    gen_test!(fn decode_vec_option_bfe for Vec<Option<BFieldElement>>);
    gen_test!(fn decode_vec_option_tuple_l_u128_vec_digest_r for Vec<Option<(u128, Vec<Digest>)>>);

    gen_test!(fn decode_tuple_l_bfe_bfe_r for (BFieldElement, BFieldElement));
    gen_test!(fn decode_tuple_l_bfe_xfe_r for (BFieldElement, XFieldElement));
    gen_test!(fn decode_tuple_l_digest_xfe_r for (Digest, XFieldElement));
    gen_test!(fn decode_tuple_l_digest_vec_xfe_r for (Digest, Vec<XFieldElement>));
    gen_test!(fn decode_tuple_l_vec_digest_xfe_r for (Vec<Digest>, XFieldElement));
    gen_test!(fn decode_tuple_l_digest_array_xfe_0_r for (Digest, [XFieldElement; 0]));
    gen_test!(fn decode_tuple_l_digest_array_xfe_5_r for (Digest, [XFieldElement; 5]));
    gen_test!(fn decode_tuple_l_array_xfe_0_digest_r for ([XFieldElement; 0], Digest));
    gen_test!(fn decode_tuple_l_array_xfe_5_digest_r for ([XFieldElement; 5], Digest));
    gen_test!(fn decode_tuple_l_array_u128_5_u64_5_digest_r for ([u128; 5], [u64; 5]));
    gen_test!(fn decode_tuple_l_vec_u64_array_xfe_5_r for (Vec<u64>, [XFieldElement; 5]));
    gen_test!(fn decode_tuple_l_tuple_l_u32_u64_r_u128_r for ((u32, u64), u128));
    gen_test!(fn decode_tuple_l_u32_tuple_l_u64_u128_r_r for (u32, (u64, u128)));
    gen_test!(fn decode_tuple_l_tuple_l_u32_u64_r_tuple_l_u64_u32_r_r for ((u32, u64), (u64, u32)));
    gen_test!(fn decode_tuple_l_vec_vec_u32_vec_vec_u128_r for (Vec<Vec<u32>>, Vec<Vec<u128>>));
    gen_test!(fn decode_tuple_l_option_u32_option_u64_r for (Option<u32>, Option<u64>));
    gen_test!(fn decode_tuple_l_option_vec_u32_option_u64_r for (Option<Vec<u32>>, Option<u64>));

    gen_test!(fn decode_option_bfe for Option<BFieldElement>);
    gen_test!(fn decode_option_xfe for Option<XFieldElement>);
    gen_test!(fn decode_option_digest for Option<Digest>);
    gen_test!(fn decode_option_vec_bfe for Option<Vec<BFieldElement>>);
    gen_test!(fn decode_option_vec_xfe for Option<Vec<XFieldElement>>);
    gen_test!(fn decode_option_option_bfe for Option<Option<BFieldElement>>);
    gen_test!(fn decode_option_array_bfe_0 for Option<[BFieldElement; 0]>);
    gen_test!(fn decode_option_array_xfe_5 for Option<[XFieldElement; 5]>);
    gen_test!(fn decode_option_tuple_l_bfe_xfe_r for Option<(BFieldElement, XFieldElement)>);
    gen_test!(fn decode_option_tuple_l_u32_u64_r for Option<(u32, u64)>);
    gen_test!(fn decode_option_vec_tuple_l_bfe_u128_r for Option<Vec<(BFieldElement, u128)>>);
    gen_test!(fn decode_option_tuple_l_vec_u32_vec_u64_r for Option<(Vec<u32>, Vec<u64>)>);
    gen_test!(fn decode_option_tuple_l_array_u128_5_u64_5_r for Option<([u128; 5], [u64; 5])>);
}
