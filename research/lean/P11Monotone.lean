/-
  Researchproofs/P11Monotone.lean

  P11, continued: monotone treatment response (Manski, Identification for
  Prediction and Decision, §9.3; Manski 1997).

  Sentences are ordered (community < custody, or sentence length) and the
  analyst is willing to assume that a harsher sentence never LOWERS the
  outcome for any one person (y_i(s) ≤ y_i(t) whenever s ≤ t; for a bad
  outcome such as reconviction this is the "no deterrence, possible
  criminogenic effect" direction, the reverse ordering gives the other).
  On a finite population with binary outcomes and two sentences a < b:

    * mtr_lower: E[y(b)] − E[y(a)] ≥ 0 — the data-free lower bound.
    * mtr_upper: E[y(b)] − E[y(a)] ≤ P(y=1, z=b) + P(y=0, z=a): a person
      sentenced to b reveals y(b) and can only have y(a) ≥ 0, a person
      sentenced to a reveals y(a) and can only have y(b) ≤ 1; the bound is
      attained (mtr_upper_attained).

  Compared with the worst-case interval of P11Bounds (width 1, contains
  0), MTR delivers a sign and an interval [0, P(y=1,z=b) + P(y=0,z=a)]:
  the share of people whose observed outcome leaves room for the
  counterfactual to differ.
-/
import Mathlib
import Researchproofs.P11Bounds

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P11.Pop

variable {Ω : Type*} [Fintype Ω] (P : Pop Ω)

/-- Potential outcomes `ya`, `yb` consistent with the data: the observed arm is revealed. -/
def Consistent (ya yb : Ω → ℝ) : Prop :=
  (∀ i, P.z i = false → ya i = P.y i) ∧ (∀ i, P.z i = true → yb i = P.y i)

/-- Monotone treatment response for binary outcomes: `ya ≤ yb`, both in {0,1}. -/
def MTR (ya yb : Ω → ℝ) : Prop :=
  (∀ i, ya i ≤ yb i) ∧ (∀ i, 0 ≤ ya i ∧ yb i ≤ 1)

def mean (P : Pop Ω) (f : Ω → ℝ) : ℝ := ∑ i, P.w i * f i

/-- Under MTR the effect is non-negative. -/
theorem mtr_lower (ya yb : Ω → ℝ) (h : MTR ya yb) : 0 ≤ P.mean yb - P.mean ya := by
  unfold mean; rw [← sum_sub_distrib]
  apply sum_nonneg; intro i _
  have := h.1 i; have := P.w_nonneg i; nlinarith

/-- Upper bound: `E[yb] − E[ya] ≤ P(y=0, z=b) + P(y=1, z=a)`, attained. -/
theorem mtr_upper (ya yb : Ω → ℝ) (hc : P.Consistent ya yb) (h : MTR ya yb) :
    P.mean yb - P.mean ya ≤ ∑ i, P.w i * (if P.z i = true then P.y i else 1 - P.y i) := by
  unfold mean; rw [← sum_sub_distrib]
  apply sum_le_sum; intro i _
  have hw := P.w_nonneg i
  cases hz : P.z i
  · rw [hc.1 i hz]; simp only [Bool.false_eq_true, ite_false]
    have := (h.2 i).2; nlinarith
  · rw [hc.2 i hz]; simp only [ite_true]
    have := (h.2 i).1; nlinarith

/-- The MTR-consistent completion that fills the unobserved arm at its extreme. -/
def fillA (P : Pop Ω) (i : Ω) : ℝ := if P.z i = true then 0 else P.y i
def fillB (P : Pop Ω) (i : Ω) : ℝ := if P.z i = true then P.y i else 1

/-- The upper bound is attained by that completion. -/
theorem mtr_upper_attained :
    P.Consistent (fillA P) (fillB P) ∧ MTR (fillA P) (fillB P) ∧
    P.mean (fillB P) - P.mean (fillA P) = ∑ i, P.w i * (if P.z i = true then P.y i else 1 - P.y i) := by
  refine ⟨⟨fun i hi => by simp [fillA, hi], fun i hi => by simp [fillB, hi]⟩, ⟨?_, ?_⟩, ?_⟩
  · intro i; cases hz : P.z i <;> simp [fillA, fillB, hz] <;> rcases P.y01 i with h | h <;> simp [h]
  · intro i; cases hz : P.z i <;> simp [fillA, fillB, hz] <;> rcases P.y01 i with h | h <;> simp [h]
  · unfold mean; rw [← sum_sub_distrib]; apply sum_congr rfl; intro i _
    cases hz : P.z i <;> simp [fillA, fillB, hz] <;> (try ring)

end Research.P11.Pop
