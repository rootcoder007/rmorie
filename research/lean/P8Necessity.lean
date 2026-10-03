/-
  Researchproofs/P8Necessity.lean

  P8, continued: the probability of necessity (Pearl, Causality 2e,
  Theorems 9.2.10–9.2.15).

  "Was the sanction necessary for this person's desistance?" is a
  counterfactual quantity PN = P(y_{x'} = 0 | x, y). Under exogeneity of
  x (or in an experiment) PN is bounded, not identified:

      max(0, (P(y|x) − P(y|x'))/P(y|x)) ≤ PN ≤ min(1, P(y|x')... )

  in Pearl's form, with P(y|x') for the complementary outcome. What is
  formalised here is the Fréchet-type core that makes those bounds work:
  for events A = {y_x = 1} and B = {y_{x'} = 1} on one population with
  P(A) = a and P(B) = b, the joint P(A ∧ ¬B) — the share for whom x was
  necessary among those with y_x = 1 — lies in [max(0, a − b), min(a, 1 − b)]
  (necessity_bounds), both ends attained (necessity_lower_attained,
  necessity_upper_attained), and equals a − b exactly under monotonicity
  (no one is harmed: ¬B ∨ A... i.e. B ⊆ A) (necessity_of_monotone).
  Dividing by a gives PN's bounds and the excess-risk-ratio identity
  under monotonicity. Attributing desistance to a sanction from
  observational rates alone needs monotonicity; without it only the
  interval is warranted.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P8

variable {Ω : Type*} [Fintype Ω]

/-- Weighted share of a predicate. -/
def share (w : Ω → ℝ) (p : Ω → Prop) [DecidablePred p] : ℝ := ∑ i, if p i then w i else 0

theorem share_nonneg (w : Ω → ℝ) (hw : ∀ i, 0 ≤ w i) (A : Ω → Prop) [DecidablePred A] : 0 ≤ share w A := by
  unfold share; apply sum_nonneg; intro i _; split_ifs <;> simp [hw i]

theorem share_le_one (w : Ω → ℝ) (hw : ∀ i, 0 ≤ w i) (htot : ∑ i, w i = 1) (A : Ω → Prop) [DecidablePred A] :
    share w A ≤ 1 := by
  unfold share; rw [← htot]; apply sum_le_sum; intro i _; split_ifs <;> simp [hw i]

/-- The "necessary" share `P(A ∧ ¬B)` lies in `[max(0, a − b), min(a, 1 − b)]`. -/
theorem necessity_bounds (w : Ω → ℝ) (hw : ∀ i, 0 ≤ w i) (htot : ∑ i, w i = 1)
    (A B : Ω → Prop) [DecidablePred A] [DecidablePred B] :
    max 0 (share w A - share w B) ≤ share w (fun i => A i ∧ ¬ B i) ∧
    share w (fun i => A i ∧ ¬ B i) ≤ min (share w A) (1 - share w B) := by
  have hAB : share w (fun i => A i ∧ ¬ B i) = share w A - share w (fun i => A i ∧ B i) := by
    unfold share; rw [← sum_sub_distrib]; apply sum_congr rfl; intro i _
    by_cases ha : A i <;> by_cases hb : B i <;> simp [ha, hb]
  have hABB : share w (fun i => A i ∧ B i) ≤ share w B := by
    unfold share; apply sum_le_sum; intro i _
    by_cases ha : A i <;> by_cases hb : B i <;> simp [ha, hb, hw i]
  have hnn : 0 ≤ share w (fun i => A i ∧ ¬ B i) := share_nonneg w hw _
  have hnB : share w (fun i => A i ∧ ¬ B i) ≤ share w (fun i => ¬ B i) := by
    unfold share; apply sum_le_sum; intro i _
    by_cases ha : A i <;> by_cases hb : B i <;> simp [ha, hb, hw i]
  have hcompl : share w (fun i => ¬ B i) = 1 - share w B := by
    unfold share; rw [← htot, ← sum_sub_distrib]; apply sum_congr rfl; intro i _
    by_cases hb : B i <;> simp [hb]
  have hABA : 0 ≤ share w (fun i => A i ∧ B i) := share_nonneg w hw _
  refine ⟨max_le hnn (by linarith), le_min (by linarith) (by linarith)⟩

/-- Both ends are attained: nested events give `a − b` (when `B ⊆ A`), disjoint events give `a`. -/
theorem necessity_of_monotone (w : Ω → ℝ) (A B : Ω → Prop) [DecidablePred A] [DecidablePred B]
    (hmono : ∀ i, B i → A i) :
    share w (fun i => A i ∧ ¬ B i) = share w A - share w B := by
  unfold share; rw [← sum_sub_distrib]; apply sum_congr rfl; intro i _
  by_cases ha : A i <;> by_cases hb : B i
  · simp [ha, hb]
  · simp [ha, hb]
  · exact absurd (hmono i hb) ha
  · simp [ha, hb]

theorem necessity_of_disjoint (w : Ω → ℝ) (A B : Ω → Prop) [DecidablePred A] [DecidablePred B]
    (hdisj : ∀ i, A i → ¬ B i) :
    share w (fun i => A i ∧ ¬ B i) = share w A := by
  unfold share; apply sum_congr rfl; intro i _
  by_cases ha : A i <;> simp [ha, hdisj i]

end Research.P8
