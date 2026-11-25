# Agreement of the Rust Verifier and the tasm Verifier

This module proves that two verifiers agree:

- the tasm verifier
  [`StarkVerify`](crate::verifier::stark_verify::StarkVerify), and
- the Rust verifier `triton_vm::stark::Verifier::verify`, from the `triton-vm`
  release in tasm-lib's `Cargo.lock`, at present 9.0.1.

Agreement is defined in section *Agreement of the Verifiers* of the
autoresearch ground rules (`AUTORESEARCH.md`, §1, in the repository
`triton-autoresearch`). Section [Definitions](#definitions) below restates it.

Rounds and notation are those of section *The Interactive Oracle Proof* of the
chapter
[Soundness](https://github.com/TritonVM/triton-vm/blob/master/specification/src/soundness.md).
The checks are those of its section *Verifier Checks*. That section is not yet
on `master`. It describes a Rust verifier that differs from 9.0.1 in one place
only: it rejects log₂ N ≥ 29 instead of log₂ N ≥ 30 (section
[Open Items](#open-items), item 2).

## What It Means for This Document to Hold

This document *holds* for a version of the code if the following four
conditions are true of that version.

1. **Each row is true.** Each row of section [The Checks](#the-checks) says:
   whenever the Rust check fails, the tasm instructions fail, and whenever it
   passes, they pass. The table in section [Transcript](#transcript) says the
   two verifiers absorb and squeeze the same values in the same order. Each row
   of section [Inputs](#inputs-from-nondeterminism) says the named check binds
   the value. Each of these is true of the code, including the error IDs and
   the order of operations.
2. **The lists are complete.** Section [The Checks](#the-checks) has a row for
   every way the Rust verifier can reject: every row and item of section
   *Verifier Checks*, and every panic. Section
   [Inputs](#inputs-from-nondeterminism) has a row for every value V₁ reads
   that the prover controls. A check or value missing from these lists is a gap
   that the argument does not mention.
3. **The argument is valid.** Given 1 and 2, section
   [Proof of Agreement](#proof-of-agreement) derives the two clauses of
   section [Definitions](#definitions).
4. **The open items are the only failures.** Every place where a clause fails
   is listed in section [Open Items](#open-items).

If the document holds, then V₁ accepts a false claim with at most the
probability that the chapter Soundness bounds for the Rust verifier, plus the
probability of a collision of Tip5, except where an open item says otherwise.

This is why the document matters. The chapter bounds the soundness error of
the Rust verifier only. Recursion accepts proofs through the tasm verifier.
If V₁ accepts a proof that the Rust verifier rejects, the chapter's bound does
not apply to V₁. If V₁ rejects a proof that the Rust verifier accepts, no
false claim is accepted, but a valid proof cannot be verified recursively.

The tests check condition 1 only on honest proofs (section
[Transcript](#transcript)). For proofs that are not honest, conditions 1 to 4
rest on reading the argument. At present, condition 2 is not established for
the cells of `ram` outside the two encodings (item 4). Clause 1 fails for
log₂ N < 8, which item 3 lists, so that failure is consistent with
condition 4.

## When to Update This Document

A change to either verifier updates this document in the same pull request,
and so does a change to section *Verifier Checks* of the chapter. What to
update:

- A check is added, removed, reordered, or changes its error ID: the row of
  that check in section [The Checks](#the-checks).
- A proof item is added or removed, or a squeeze moves: the table in section
  [Transcript](#transcript). The sponge test in `stark_verify.rs` fails until
  `StarkVerify::replay_rust_verifier_transcript` is changed the same way.
- The number or order of the digests read from `NonDeterminism` changes: the
  table in section [Inputs](#inputs-from-nondeterminism).
- The tasm verifier reads a new value from `NonDeterminism`: a row in that
  table, naming the check that binds the value. A value that no check binds
  makes the change unsound.
- The rows of section *Verifier Checks* change: the numbering C1 to C20.

A reviewer of such a change reads the rows it touches and checks that they
still hold. Section [Open Items](#open-items) lists where agreement does not
hold yet. Closing an item removes it.

## Notation

- N is the padded height.
- H = ⟨g⟩ is the trace domain, and g its generator.
- D = o · ⟨ω⟩ is the FRI domain, with offset o and generator ω.
- Dᵣ = oᵣ · ⟨ωᵣ⟩, with oᵣ = o^(2^r) and ωᵣ = ω^(2^r), is the domain of FRI
  round r. So D₀ = D.
- R is the number of FRI folding rounds, and t the number of FRI queries.
- fᵣ is the codeword that the prover commits to in FRI round r.
- y is the codeword that the verifier computes from the opened rows of the
  tables, at the queried points (C16).
- 𝛘 are the table-linking challenges, 𝛋 the quotient weights, α the
  out-of-domain point, and 𝛍, 𝛎, 𝛄 the DEEP weights.
- k = 4. The quotient is split into the k + 1 segments s₀, …, s_k. The prover
  claims the values σᵢ for sᵢ(αᵏ), 0 ≤ i < k, and τᵢ for sᵢ((ζα)ᵏ),
  1 ≤ i ≤ k, where ζ is a fixed constant.
- qⱼ(α) is the value of the j-th constraint at α, computed from the
  out-of-domain rows.

## Definitions

A *claim* c is a Rust value of type `Claim`. A *proof* π is a Rust value of
type `Proof`.

The *single verifier* V₁ is a Triton VM program. It reads the `BFieldCodec`
encodings of c and π from memory, calls `StarkVerify` on them, and halts.

R(c, π) holds if `Verifier::verify(c, π)` returns `Ok(())`. If the Rust
verifier panics, R(c, π) does not hold.

Two proofs π and π' are *equivalent*, written π ≃ π', if they differ at most
in these places:

- the payloads of their `AuthenticationStructure` items, and
- the field `auth_structure` of their `FriResponse` items.

The tasm verifier reads the lengths of these fields, but none of their digests.
It reads the authentication paths from `NonDeterminism` instead (section
[Inputs](#inputs-from-nondeterminism)). So agreement is stated up to
equivalence:

1. **Completeness.** If R(c, π) holds, then V₁ halts on (c, π), with the
   nondeterminism that `StarkVerify::update_nondeterminism` supplies.
2. **Soundness.** Suppose V₁ halts on (c, π), with any value of
   `NonDeterminism`. Then R(c, π') holds for some π' ≃ π, unless the prover
   has found a collision of Tip5.

The ground rules state clause 2 without equivalence, and as stated there it is
false. Take a proof that both verifiers accept, and change one digest of an
`AuthenticationStructure` item. The Rust verifier now rejects. V₁ still halts
with the same nondeterminism, because it never reads that digest. Section
[Open Items](#open-items) lists this as item 1.

Clause 1 is false too, for proofs with N < 2⁸. Section
[Proof of Agreement](#proof-of-agreement) shows why, and item 3 of the open
items lists it.

## Inputs from `NonDeterminism`

A value that V₁ reads is *bound* if changing it either changes a challenge or
makes a check fail. V₁ reads three kinds of values from `NonDeterminism`. The
table names what binds each.

| Value | Field of `NonDeterminism` | Bound by |
| :--- | :--- | :--- |
| The encoding of c | `ram` | The transcript, which absorbs c first (section [Transcript](#transcript)). |
| The encoding of π | `ram` | Items that the transcript absorbs: the transcript. Table rows: the Merkle roots of the tables, by C15. The last FRI codeword: the last FRI root, by C10. The other FRI leaves: the FRI roots, by C8 and C9. Authentication structures: only their lengths are read, and they are not bound. |
| The authentication paths | `digests` | The comparison of the computed root with the committed root: `assert_vector` with error 30 in `VerifyFriAuthenticationPaths`, for C8 and C9; `assert_vector` with error 40 in `VerifyTableRows`, for C15. |

V₁ does not read `individual_tokens`. Section [Open Items](#open-items), item 4,
concerns the cells of `ram` outside the two encodings.

V₁ reads the digests with `merkle_step`, one digest per layer of the tree. A
path from a leaf of a tree over n leaves has log₂ n digests. The paths come in
this order:

| Block | Tree | Leaves | Digests |
| :--- | :--- | :--- | :--- |
| 1 | FRI round 0 | the t sampled indices i | t · log₂ \|D\| |
| 2 + r, for r = 0, …, R − 1 | FRI round r | for each i, the index of −x^(2^r) in Dᵣ, where x is the point of index i in D | t · (log₂ \|D\| − r) |
| R + 2 | main table | the t sampled indices | t · log₂ \|D\| |
| R + 3 | auxiliary table | the t sampled indices | t · log₂ \|D\| |
| R + 4 | quotient table | the t sampled indices | t · log₂ \|D\| |

Within a FRI block, the paths come in reverse order of the sampled indices.
Within a table block, they come in the order of the sampled indices.

Block 1 and block 2 serve C8. The blocks 2 + r for r ≥ 1 serve C9. The total is
`StarkVerify::number_of_nondeterministic_digests_consumed`.

The paths are bound by the following argument. A node of a tree has a node
index; the root has index 1, and the children of node j have indices 2j and
2j + 1. One `merkle_step` does this:

- It reads a digest s, the sibling, from `NonDeterminism`.
- If the node index j is even, it replaces the current digest d by hash(d, s).
  If j is odd, by hash(s, d).
- It replaces j by ⌊j / 2⌋.

The loop stops at j = 1. Its last digest is a candidate for the root, and
`assert_vector` compares it with the committed root.

An accepted path *assigns* a digest to each node it touches: to the nodes on the
path the digests it computes, and to their siblings the digests it reads. Now
suppose two accepted paths of one tree assign different digests to the same
node. Both paths assign the committed root to node 1. Follow the two paths up
from that node. At the first node to which both assign the same digest, the two
preimages are different. That is a collision of Tip5.

So without a collision, the accepted paths of one tree assign one digest to each
node they touch. These digests contain an authentication structure for the
opened leaves, in the format of `MerkleTreeInclusionProof`. Put this
authentication structure into π in place of the one there. The result is the
proof π' ≃ π of clause 2. The Rust verifier reads the authentication
structures only in `MerkleTreeInclusionProof::verify`, and does not absorb them.
So every Rust check other than C8, C9 and C15 has the same outcome on π' as on
π.

## Transcript

Both verifiers keep one sponge, the *transcript*. Both start it in the state of
`Tip5::init()`. The Rust verifier gets it from `ProofStream::try_from`, which
sets the sponge to `Tip5::default()`, the same state. The tasm verifier calls
`sponge_init` at its entry point.

A proof item is absorbed if its `include_in_fiat_shamir_heuristic` is true.
Both verifiers absorb such an item when they dequeue it:

- The Rust verifier applies `pad_and_absorb_all` to the encoding of the item.
- `DequeueNextAs` applies `AbsorbMultiple` to the discriminant and the payload
  if the item has static size. Otherwise it applies `PadAndAbsorbAll` to the
  discriminant, the length of the payload, and the payload. In both cases it
  absorbs the same elements as the Rust verifier.

The two verifiers absorb and squeeze in this order:

| # | Rust verifier | tasm verifier |
| :--- | :--- | :--- |
| 1 | `alter_fiat_shamir_state_with(claim)` | `InstantiateFiatShamirWithClaim` |
| 2 | dequeue `Log2PaddedHeight` | `DequeueNextAs` |
| 3 | dequeue `MerkleRoot` (main) | `DequeueNextAs` |
| 4 | `sample_scalars(Challenges::SAMPLE_COUNT)`: 𝛘 | `NewGenericDynClaim` |
| 5 | dequeue `MerkleRoot` (auxiliary) | `DequeueNextAs` |
| 6 | `sample_scalars(NUM_CONSTRAINTS)`: 𝛋 | `SampleScalarsStaticLengthDynMalloc` |
| 7 | dequeue `MerkleRoot` (quotient) | `DequeueNextAs` |
| 8 | `sample_scalars(1)`: α | `SampleScalarOne` |
| 9 | dequeue 4 out-of-domain rows, 2 out-of-domain quotient segments | `DequeueNextAs`, six calls |
| 10 | `LinearCombinationWeights::sample`: 𝛍, 𝛎, 𝛄 | `SampleScalarsStaticLengthDynMalloc` |
| 11 | `Fri::verify`: dequeue a root; then R times: sample a folding challenge, dequeue a root | `FriSnippet`: the same, in `dequeue_commit_phase` |
| 12 | `Fri::verify`: dequeue `FriCodeword`, then `FriPolynomial` | `DequeueNextAs`, twice |
| 13 | `Fri::verify`: `sample_indices` | `SampleIndices` |
| 14 | `Fri::verify`: `sample_scalars(1)`, the point at which the last polynomial is evaluated | `SampleScalarsStaticLengthDynMalloc` |

The verifiers dequeue the `FriResponse` items at different times. The Rust
verifier dequeues them between steps 13 and 14, the tasm verifier after step
14. This does not change the transcript, because no `FriResponse` item is
absorbed.

After step 14, neither verifier absorbs or squeezes. So both transcripts end
after step 14. The tasm verifier then calls `sponge_init` a second time, to
hash the opened rows in `HashStaticSize`. This overwrites the sponge, but the
transcript is already complete.

The test helper `test_verify_and_report_basic_features` in `stark_verify.rs`
checks the table on honest proofs. It compares two sponge states:

- the sponge state of V₁ just before its second `sponge_init`, and
- the sponge state at the end of the Rust transcript, which
  `StarkVerify::replay_rust_verifier_transcript` computes. The replay calls the
  same `ProofStream` methods as `Verifier::verify`, in the same order, and calls
  `Fri::verify` itself.

The tests that call this helper cover both memory layouts, the expansion
factors 4 to 32, and the shortest execution. The tests
`verify_from_stored_proof_output` and `verify_two_proofs` do not call it. For
proofs that are not honest, the argument is the table above.

`SampleIndices` and `Tip5::sample_indices` compute the same indices. Both
squeeze until they have t indices. Both skip the element p − 1. Both reduce the
lower 32 bits of each remaining element modulo |D|; the tasm verifier does this
with `split`, then `and` with |D| − 1.

## The Checks

The checks are numbered as follows:

- C1 to C16 are the rows of the table in section *Verifier Checks* of the
  chapter Soundness, in order.
- C17 to C20 are the four items, in order, of the list of checks in that section
  that the chapter does not rely on.
- P1 to P5 are panics of the Rust verifier. A panic counts as a rejection.

Each row below names the tasm instructions that perform the check. A number in
parentheses is an error ID, of `assert error_id` or `assert_vector error_id`.

### Checks the Chapter Relies On

| # | Check | tasm |
| :--- | :--- | :--- |
| C1 | The proof decodes to a sequence of proof items. | `vm_proof_iter::New` checks the size indicators of the proof (300, 301). `DequeueNextAs` checks the size indicators of each item: with error 620 if the item has static size, with two `eq assert` otherwise. Decoding a `Polynomial` rejects a trailing zero coefficient; for `FriPolynomial`, `DequeueNextAs::verify_last_xfe_is_non_zero_if_payload_is_polynomial` checks this with `assert`. |
| C2 | Each item is of the kind the protocol expects next. | `DequeueNextAs` compares the discriminant with `eq assert`. The calls of `DequeueNextAs` come in the order of section [Transcript](#transcript). |
| C3 | No item is missing. | `Drop` (50, 60). If V₁ dequeues past the last item, the item count exceeds the total, and 60 fails. |
| C4 | log₂ N ≤ 28 | `push 32 dup 1 lt assert error_id 238` accepts log₂ N ≤ 31. Later instructions reject log₂ N ≥ 30 for every expansion factor, and log₂ N = 29 for an expansion factor of at least 4; see section [Proof of Agreement](#proof-of-agreement). The Rust verifier 9.0.1 checks log₂ N ≤ 29. |
| C5 | The quotient equation holds. | `AirConstraintEvaluation`, `DivideOutZerofiers` and `InnerProductOfXfes` compute Σⱼ κⱼ qⱼ(α) / Zⱼ(α), where Zⱼ is the zerofier of constraint j. Two calls of `HornerEvaluation` compute Σᵢ αⁱ σᵢ over 0 ≤ i < k, plus Σᵢ (ζα)ⁱ⁻¹ τᵢ over 1 ≤ i ≤ k. `assert_top_two_xfes_eq` (230, 231, 232) compares the two sides. The claim enters the equation through `NewGenericDynClaim`, which computes the terminals as `Challenges::new` does. |
| C6 | The last FRI codeword has \|D_R\| entries. | `FriSnippet`: `call {length_of_list}`, then `eq assert` |
| C7 | Each FRI opening reveals t values. | Round 0: `eq assert` compares the number of leaves with the number of indices, which is t by `SampleIndices`. Later rounds: `dup 14 eq assert` in `query_phase_main_loop`. |
| C8 | The opened values f₀(x) and f₀(−x) are in the tree of f₀. | `VerifyFriAuthenticationPaths` (30). Let x = o · ωⁱ be the point of index i in D. For x it starts at node index \|D\| + i. For −x it starts at node index \|D\| + i', where i' = i XOR \|D\|/2. Since ω^(\|D\|/2) = −1, i' is the index of −x. |
| C9 | For 1 ≤ r < R, the opened value fᵣ(−x^(2^r)) is in the tree of fᵣ. | `VerifyFriAuthenticationPaths` (30). It starts at node index \|Dᵣ\| + i', where i' = (i mod \|Dᵣ\|) XOR \|Dᵣ\|/2 is the index of −x^(2^r) in Dᵣ. This is the leaf that `authenticate_partial_codeword_b_for_round` uses. |
| C10 | The last codeword has the committed Merkle root. | `MerkleRootFromXfes`, then `assert_vector` against the last root |
| C11 | f_R(x^(2^R)) equals the value folded from the opened values. | `compute_c_values_loop` folds as `get_colinear_y` does: it takes the line through (a, f(a)) and (−a, f(−a)), where a = oᵣ · ωᵣ^(i mod \|Dᵣ\|), and evaluates it at the folding challenge. Then `map_reduce_indices` and `map_assert_membership` compare the result with the last codeword, using `assert_vector`. |
| C12 | The last polynomial has degree at most d, where d + 1 = \|D_R\| / expansion factor. The chapter writes \|D_R\| / 4, for the default expansion factor 4. | `lt push 0 eq assert` checks that the number of coefficients is at most d + 1. The last coefficient is not zero, by C1, so this is the same as degree ≤ d. `Fri::last_round_max_degree` is (\|D\| / expansion factor − 1) ≫ R. This equals d, because \|D\| / expansion factor is a power of two and at least 2ᴿ. |
| C13 | The last polynomial and the interpolant of the last codeword agree at a random point. | `HornerEvaluationDynamicLength` evaluates the polynomial, `BarycentricEvaluation` evaluates the interpolant, and `assert_vector` compares the two. |
| C14 | t rows of the main, auxiliary and quotient tables are opened. | `assert` with errors 235, 236 and 237, and with error 41 inside `VerifyTableRows` |
| C15 | The opened rows are in the trees of the main, auxiliary and quotient tables. | `VerifyTableRows`, once for each of `ColumnType::Main`, `Aux` and `Quotient`: `HashStaticSize` computes the leaf, a loop of `merkle_step` computes the root, and `assert_vector` (40) compares it with the committed root. |
| C16 | f₀(x) = y(x), where y(x) is computed from the opened rows | `main_loop_body` computes y(x), and `assert_top_two_xfes_eq` (230, 231, 232) compares it with f₀(x). |

`DeriveFriFromStark` computes the FRI parameters of the tasm verifier. They
equal those of `Stark::fri`: |D|, o, the expansion factor and t.
`NumberOfRounds` computes R as `Fri::num_rounds` does.

The tasm verifier performs C10, C12 and C13 before C7, C8, C9 and C11; the Rust
verifier performs them after. The order of the checks does not matter, for two
reasons. A verifier rejects if any one check fails, whatever the order. And the
order does not change the transcript (section [Transcript](#transcript)).

The chapter's section *Verifier Checks* notes, under *FRI consistency*, that
the Rust verifier checks less than the chapter assumes. It compares f₀ with y
only at x, not at −x. It opens fᵣ only at −x^(2^r), for 1 ≤ r < R. It compares
a folded value with a committed value only for f_R. The tasm verifier does
exactly the same. So that note concerns both verifiers alike, and does not
affect their agreement.

In C15, both verifiers open the rows at the indices i that FRI sampled in round
0. In the Rust verifier these are `revealed_current_row_indices`; in the tasm
verifier, `FriSnippet` returns them. The leaf of a row is `Tip5::hash_varlen`
of the row. `HashStaticSize` computes the same digest: since the row has static
length, `sponge_init`, absorbing and `sponge_squeeze` is `hash_varlen`.

In C16, y(x) uses the weights sampled in step 10 of the transcript. They lie in
memory from `*beqd_ws` on, in the order of `LinearCombinationWeights::sample`:
first 𝛍 (main columns, then auxiliary columns), then ν₀, …, ν₄, then
γ₀, …, γ₃. The value y(x) is γ₀ e₀ + γ₁ e₁ + γ₂ e₂ + γ₃ e₃, with the four DEEP
values e₀, …, e₃ below. Each is a quotient (v(β) − v(x)) / (β − x). Here v is
a linear combination of columns or segments, v(x) is its value on the opened
row at x, and v(β) is its value on the out-of-domain row or segments, as the
prover sent them.

- e₀ and e₁: v is the combination of the main and auxiliary columns with 𝛍,
  and β is α for e₀, gα for e₁. The tasm verifier computes v(x) with
  `InnerProductOfThreeRowsWithWeights` (`MainElementType::Bfe`), and v(α) and
  v(gα) once, before the loop, with `MainElementType::Xfe`. The Rust verifier
  computes v(x) with `linearly_sum_main_and_aux_row`.
- e₂: v = ν₀ s₀ + … + ν₃ s₃, and β = αᵏ.
- e₃: v = ν₁ s₁ + … + ν₄ s₄, and β = (ζα)ᵏ.

For e₂ and e₃, the tasm verifier uses `InnerProductOfXfes { length: 4 }`, on
the opened segments and on the out-of-domain segments. The Rust verifier
computes v(x) as `shared_part`, the combination of s₁, …, s₃, plus the term
of s₀ or of s₄. It computes v(β) with `dropping_back(1)` or `skip(1)` on the
segments.

`Stark::deep_update` computes (v(x) − v(β)) / (x − β). This is the same
quotient, with numerator and denominator negated.

### Checks the Chapter Does Not Rely On

| # | Check | tasm |
| :--- | :--- | :--- |
| C17 | `FriSetupError`, `ArithmeticDomainError` | `DeriveFriFromStark` fails on the same inputs as `Stark::fri`, in `NextPowerOfTwo` (130) or `PrimitiveRootOfUnity` (140). Where the Rust verifier calls `ArithmeticDomain::generator_for_length` on H, the tasm verifier calls `PrimitiveRootOfUnity` (140) on \|H\| = \|D\| / (2 · expansion factor). |
| C18 | `IncorrectNumberOfRowIndices`, `IncorrectNumberOfFRIValues` | The number of FRI values: `assert` (234). The number of indices is t by `SampleIndices`. |
| C19 | `FriValidationError::MerkleTreeError` | No check is needed. This error requires a codeword of bad length, and C6 has fixed the length. |
| C20 | `SuperfluousProofItems` | `Drop` (50, 60), in `call {drop_vm_proof_iter}`. The tasm verifier performs it after dequeuing the last item, which is before C16. The Rust verifier performs it last. |

### Panics

| # | Rust panic | tasm |
| :--- | :--- | :--- |
| P1 | A zerofier at α is 0, in `XFieldElement::inverse`. | `ZerofiersInverse`, called by `DivideOutZerofiers`: `x_invert` crashes on 0. |
| P2 | A queried point of D equals α, gα, αᵏ or (ζα)ᵏ, in `Stark::deep_update`. | `main_loop_body`: `x_invert` crashes on 0. |
| P3 | The two abscissas of a fold are equal, in `get_colinear_y`. | Cannot happen: the abscissas are a and −a, and a ≠ 0 because a is a point of Dᵣ. |
| P4 | \|D\| ≥ 2³², in `ProofStream::sample_indices`. | `NumberOfRounds` crashes: `div_mod` requires \|D\| < 2³². |
| P5 | The point of C13 lies in D_R, in `barycentric_evaluate`. | `BarycentricEvaluation`: `x_invert` crashes on 0. |

## Proof of Agreement

**A bound on |D|.** If V₁ halts, then |D| < 2³², because `NumberOfRounds`
applies `div_mod` to |D| (P4). As |D| is a power of two, |D| ≤ 2³¹. The
randomized trace has length at least 2N, so |D| ≥ 2N · expansion factor ≥ 4N.
Hence V₁ halts only if log₂ N ≤ 29. This is the bound of the Rust verifier
9.0.1. If R(c, π) holds, then |D| ≤ 2³¹ as well, by P4.

**Soundness.** Suppose V₁ halts on (c, π) with some value of `NonDeterminism`.
By section [Transcript](#transcript), V₁ dequeues the same items as the Rust
verifier, absorbs the same items, and squeezes the same challenges. By section
[The Checks](#the-checks), whenever one of C1 to C20 or P1 to P5 fails in the
Rust verifier, the tasm instructions for it fail in V₁. There are two
exceptions:

- C4: the instruction for it accepts log₂ N ≤ 31. But V₁ halts only if
  log₂ N ≤ 29, by the bound on |D|, and so C4 of the Rust verifier 9.0.1
  passes.
- C8, C9 and C15 check authentication structures, which V₁ does not read. For
  these, the statement holds with π' in place of π. Here π' ≃ π is the proof
  that section [Inputs](#inputs-from-nondeterminism) builds from the accepted
  paths, and the statement fails only if the prover has found a collision of
  Tip5. The other checks have the same outcome on π' as on π.

So the Rust verifier accepts (c, π').

**Completeness.** Suppose R(c, π) holds. `update_nondeterminism` supplies the
authentication paths that
`MerkleTreeInclusionProof::into_authentication_paths` computes from the
authentication structures of π, in the order of section
[Inputs](#inputs-from-nondeterminism). The Rust verifier accepted these
structures, so the paths reach the committed roots.

Each tasm check of section [The Checks](#the-checks) tests the same equation as
the Rust check it is listed with, so it passes when the Rust check passes. The
tasm checks also use instructions that crash on operands of 2³² or more:
`div_mod`, `lt`, `log_2_floor`, `pow`, `xor` and `merkle_step`. Their operands
are at most |D| or a node index below 2 · |D|, and |D| ≤ 2³¹ by the bound
on |D|.

The tasm verifier also performs checks that the Rust verifier does not perform.
V₁ halts on (c, π) only if π passes these as well:

- log₂ N ≥ 8 (239). The Rust verifier uses N only through the length of the
  randomized trace, `Stark::randomized_trace_len`. For the default parameters,
  this length is the same for every log₂ N ≤ 8. So a prover that writes
  log₂ N = 7 into an otherwise honest proof gets a proof that the Rust verifier
  accepts and V₁ rejects. This contradicts clause 1; see item 3. An honest
  prover never writes N < 2⁸.
- The size limits of `New` and `DequeueNextAs`:
  - the proof has fewer than 2²⁶ words (302),
  - the proof has fewer than `MAX_NUM_PROOF_ITEMS` items (303),
  - each item of dynamic size has fewer than 2²² words,
  - the proof, its length and their sum are u32 (`pop_count`).

  See item 3.
- The challenges are stored where `AirConstraintEvaluation` reads them (233).
  This depends only on the memory layout, not on c or π.

## Open Items

1. **The definition of agreement.** Clause 2 of the ground rules is false as
   stated, because the tasm verifier does not read the digests of the
   authentication structures of π (section [Definitions](#definitions)). This
   document proves clause 2 up to equivalence. The ground rules should state it
   that way.
2. **The bound on N.** The bounds differ:
   - `push 32 … assert error_id 238` accepts log₂ N ≤ 31 (C4);
   - V₁ as a whole halts only if log₂ N ≤ 29 (section
     [Proof of Agreement](#proof-of-agreement));
   - the Rust verifier 9.0.1 accepts log₂ N ≤ 29;
   - the chapter Soundness relies on log₂ N ≤ 28 (C4).

   For an expansion factor of at least 4, log₂ N = 29 gives |D| ≥ 2³². Then
   both verifiers reject: V₁ crashes in `NumberOfRounds`, and the Rust verifier
   panics in `sample_indices`. So for the default expansion factor 4, both
   verifiers meet the chapter's bound. For an expansion factor of 2, both
   accept log₂ N = 29, which the chapter excludes. Two changes close this item:
   `push 29` in place of `push 32` in `StarkVerify::code`, and a `triton-vm`
   release with `MAX_LOG2_PADDED_HEIGHT = 28`.
3. **Completeness at the edges.** Clause 1 is false: the Rust verifier accepts
   proofs with log₂ N < 8 that V₁ rejects with 239 (section
   [Proof of Agreement](#proof-of-agreement)). Either the Rust verifier should
   reject log₂ N < 8, or V₁ should accept it. No proof has been run that shows
   this; the argument is from `Verifier::verify`. The size limits are a
   second gap: no honest proof with N ≤ 2²⁸ and the parameters in use fails
   them, but this document does not show that every proof the Rust verifier
   accepts stays within them.
4. **Memory outside the encodings.** Clause 2 allows any value of
   `NonDeterminism`, including values of `ram` at addresses outside the
   encodings of c and π. This document does not show that V₁ writes each such
   cell before it reads it. For example, `DynMalloc` reads its state from
   memory before writing it. Error 70 bounds that state, so a preset value
   only selects a different free page, but the pages themselves could be
   preset too. A cell that V₁ reads before writing, and that no check binds,
   is unintended nondeterminism in the sense of the ground rules. Closing this
   item means listing every cell V₁ reads before writing, and naming the
   check that binds each.
