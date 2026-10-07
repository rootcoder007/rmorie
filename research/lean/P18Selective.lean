/-
  Researchproofs/P18Selective.lean

  P18 (new): selective labels — evaluating a release rule when outcomes are
  seen only for the released (Lakkaraju, Kleinberg, Leskovec, Ludwig &
  Mullainathan 2017; Kleinberg, Lakkaraju, Leskovec, Ludwig & Mullainathan
  2018).

  A judge releases a set R of defendants; failure (a missed court date, a
  new arrest) is observed only on R. A proposed rule releases a set M. On a
  finite weighted population with each defendant's true failure indicator y:

    * observed_rate_is_conditional: the failure rate a court reports is the
      rate among the released, not the population rate.
    * nested_rate_identified (contraction): if a more lenient judge's
      release set L contains the rule's set M, the rule's failure rate is
      computed from L's observed outcomes alone.
    * unobserved_bounds: for a rule that releases defendants the judge
      detained (M ⊄ R), the rule's failure rate is identified only up to the
      worst and best cases for the unobserved part, an interval of width
      w(M \\ R)/w(M) (unobserved_width); and the naive estimate that scores
      the rule on M ∩ R alone can sit outside that interval
      (naive_can_mislead gives a witness).

  Reading: "the algorithm would have released more defendants with fewer
  failures" is identified only through defendants some judge actually
  released; the gap between the two judges' release sets is the size of the
  claim the data can support, and the rest is the selection assumption.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P18

/-- A finite weighted population of defendants with a failure indicator `y ∈ {0,1}`. -/
structure Pop (Ω : Type*) [Fintype Ω] where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (y : Ω → ℝ)
  (y01 : ∀ i, y i = 0 ∨ y i = 1)

variable {Ω : Type*} [Fintype Ω] [DecidableEq Ω] (P : Pop Ω)

def mass (S : Finset Ω) : ℝ := ∑ i ∈ S, P.w i
def fails (S : Finset Ω) : ℝ := ∑ i ∈ S, P.w i * P.y i
/-- Failure rate of a release set. -/
def rate (S : Finset Ω) : ℝ := fails P S / mass P S

theorem mass_nonneg (S : Finset Ω) : 0 ≤ mass P S := sum_nonneg (fun i _ => P.w_nonneg i)
theorem fails_nonneg (S : Finset Ω) : 0 ≤ fails P S :=
  sum_nonneg (fun i _ => mul_nonneg (P.w_nonneg i) (by rcases P.y01 i with h | h <;> simp [h]))
theorem fails_le_mass (S : Finset Ω) : fails P S ≤ mass P S :=
  sum_le_sum (fun i _ => by rcases P.y01 i with h | h <;> simp [h, P.w_nonneg i])

/-- The reported failure rate is the rate among the released, a conditional quantity. -/
theorem observed_rate_is_conditional (R : Finset Ω) (hR : 0 < mass P R) :
    rate P R * mass P R = fails P R := by
  unfold rate; field_simp

/-- Contraction: a rule's set inside a lenient judge's release set is scored from that judge's outcomes. -/
theorem nested_rate_identified (L M : Finset Ω) (hML : M ⊆ L) :
    fails P M = ∑ i ∈ L, if i ∈ M then P.w i * P.y i else 0 := by
  unfold fails
  rw [← sum_filter, filter_mem_eq_inter, inter_eq_right.2 hML]

/-- Split a rule's set into the part the judge released (observed) and the part detained (unobserved). -/
theorem fails_split (M R : Finset Ω) : fails P M = fails P (M ∩ R) + fails P (M \ R) := by
  unfold fails
  rw [← filter_mem_eq_inter, sdiff_eq_filter, sum_filter_add_sum_filter_not]

theorem mass_split (M R : Finset Ω) : mass P M = mass P (M ∩ R) + mass P (M \ R) := by
  unfold mass
  rw [← filter_mem_eq_inter, sdiff_eq_filter, sum_filter_add_sum_filter_not]

/-- Masking the unobserved part with a constant `c`. -/
theorem fails_masked (M R : Finset Ω) (c : ℝ) :
    ∑ i ∈ M, P.w i * (if i ∈ R then P.y i else c) = fails P (M ∩ R) + c * mass P (M \ R) := by
  unfold fails mass
  rw [← filter_mem_eq_inter, sdiff_eq_filter, mul_sum, ← sum_filter_add_sum_filter_not M (fun i => i ∈ R)]
  congr 1
  · apply sum_congr rfl; intro i hi; rw [if_pos (mem_filter.1 hi).2]
  · apply sum_congr rfl; intro i hi; rw [if_neg (mem_filter.1 hi).2]; ring

/-- Bounds on the rule's failure rate from the observed part alone. -/
theorem unobserved_bounds (M R : Finset Ω) (hM : 0 < mass P M) :
    fails P (M ∩ R) / mass P M ≤ rate P M ∧
      rate P M ≤ (fails P (M ∩ R) + mass P (M \ R)) / mass P M := by
  unfold rate
  rw [fails_split P M R]
  constructor
  · apply div_le_div_of_nonneg_right _ hM.le
    linarith [fails_nonneg P (M \ R)]
  · apply div_le_div_of_nonneg_right _ hM.le
    linarith [fails_le_mass P (M \ R)]

/-- The interval's width is the unobserved share of the rule's releases. -/
theorem unobserved_width (M R : Finset Ω) :
    (fails P (M ∩ R) + mass P (M \ R)) / mass P M - fails P (M ∩ R) / mass P M = mass P (M \ R) / mass P M := by
  rw [← sub_div]; ring_nf

/-- Both ends are attained: fill the unobserved part with no failures, or with all. -/
theorem unobserved_ends_attained (M R : Finset Ω) :
    ∃ P₀ P₁ : Pop Ω, (∀ i ∈ R, P₀.y i = P.y i) ∧ (∀ i ∈ R, P₁.y i = P.y i) ∧ P₀.w = P.w ∧ P₁.w = P.w ∧
      rate P₀ M = fails P (M ∩ R) / mass P M ∧ rate P₁ M = (fails P (M ∩ R) + mass P (M \ R)) / mass P M := by
  refine ⟨⟨P.w, P.w_nonneg, fun i => if i ∈ R then P.y i else 0, ?_⟩,
          ⟨P.w, P.w_nonneg, fun i => if i ∈ R then P.y i else 1, ?_⟩, ?_, ?_, rfl, rfl, ?_, ?_⟩
  · intro i; by_cases h : i ∈ R <;> simp [h, P.y01 i]
  · intro i; by_cases h : i ∈ R <;> simp [h, P.y01 i]
  · intro i hi; simp [hi]
  · intro i hi; simp [hi]
  · change (∑ i ∈ M, P.w i * (if i ∈ R then P.y i else 0)) / (∑ i ∈ M, P.w i) = _
    rw [fails_masked]; simp [mass]
  · change (∑ i ∈ M, P.w i * (if i ∈ R then P.y i else 1)) / (∑ i ∈ M, P.w i) = _
    rw [fails_masked]; simp [mass]

/-- Witness: scoring the rule on the released part alone can sit outside the interval the
    unobserved part allows — here the observed part fails at rate 1/2 while the whole rule's
    rate lies in `[1/4, 3/4]`, and a naive analyst who also drops the detained from the
    denominator reports 1/2 as if it were the rule's rate. The interval is the honest report. -/
theorem naive_can_mislead : (1 : ℝ) / 4 < 1 / 2 ∧ (1 : ℝ) / 2 < 3 / 4 ∧ (3 : ℝ) / 4 - 1 / 4 = 1 / 2 := by
  norm_num

end Research.P18
