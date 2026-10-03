/-
  Researchproofs/P14Slope.lean

  P14 (continued): the many-judge case — leniency as an ordered instrument
  and the slope test of monotonicity (Frandsen, Lefgren & Leslie 2023).

  J judges, each detaining a subset of a finite weighted population. Judge
  k is (weakly) harsher than judge j when everyone j detains, k detains too
  (Nested j k: the many-judge form of monotonicity). Potential outcomes
  y(0), y(1) lie in [lo, hi].

    * propensity_mono: the detention rate P is monotone along Nested pairs.
    * outcome_diff: the difference in mean observed outcomes between two
      nested judges is the effect summed over the marginal compliers — the
      people k detains and j does not — so the pair's Wald ratio is their
      average effect (the many-judge LATE).
    * slope_bound: |Y_k − Y_j| ≤ (hi − lo)(P_k − P_j): the mean outcome
      cannot move faster in leniency than the bounded effects allow.
    * violation_refutes_monotonicity: a pair with |ΔY| > (hi − lo) ΔP is not
      nested — the testable implication the slope test checks; a judge
      design whose outcome-by-leniency curve is steeper than the detention
      curve times the outcome range has non-monotone judges.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P14Slope

/-- A population, `J` judges' detention decisions, bounded potential outcomes. -/
structure Judges (Ω : Type*) [Fintype Ω] (J : ℕ) where
  (w : Ω → ℝ)
  (w_nonneg : ∀ i, 0 ≤ w i)
  (d : Ω → Fin J → Bool)
  (y0 y1 : Ω → ℝ)
  (lo hi : ℝ)
  (y0_mem : ∀ i, lo ≤ y0 i ∧ y0 i ≤ hi)
  (y1_mem : ∀ i, lo ≤ y1 i ∧ y1 i ≤ hi)

variable {Ω : Type*} [Fintype Ω] {J : ℕ} (G : Judges Ω J)

/-- Real indicator of a Boolean. -/
def ind (b : Bool) : ℝ := if b then 1 else 0

/-- Detention mass under judge `j`. -/
def P (j : Fin J) : ℝ := ∑ i, G.w i * ind (G.d i j)
/-- Observed outcome mass under judge `j`. -/
def Y (j : Fin J) : ℝ := ∑ i, G.w i * (if G.d i j then G.y1 i else G.y0 i)
/-- Judge `k` detains everyone judge `j` detains. -/
def Nested (j k : Fin J) : Prop := ∀ i, G.d i j = true → G.d i k = true

theorem ind_diff_nonneg {j k : Fin J} (h : Nested G j k) (i : Ω) :
    0 ≤ ind (G.d i k) - ind (G.d i j) := by
  unfold ind
  cases hj : G.d i j
  · cases G.d i k <;> simp
  · rw [h i hj]; simp

theorem propensity_mono {j k : Fin J} (h : Nested G j k) : P G j ≤ P G k := by
  unfold P
  apply sum_le_sum; intro i _
  have := ind_diff_nonneg G h i
  nlinarith [G.w_nonneg i]

theorem term_eq {j k : Fin J} (h : Nested G j k) (i : Ω) :
    (if G.d i k then G.y1 i else G.y0 i) - (if G.d i j then G.y1 i else G.y0 i)
      = (ind (G.d i k) - ind (G.d i j)) * (G.y1 i - G.y0 i) := by
  unfold ind
  cases hj : G.d i j
  · cases G.d i k <;> simp
  · rw [h i hj]; simp

/-- The outcome difference is the effect over the marginal compliers. -/
theorem outcome_diff {j k : Fin J} (h : Nested G j k) :
    Y G k - Y G j = ∑ i, G.w i * ((ind (G.d i k) - ind (G.d i j)) * (G.y1 i - G.y0 i)) := by
  unfold Y
  rw [← sum_sub_distrib]
  apply sum_congr rfl; intro i _
  rw [← mul_sub, term_eq G h i]

/-- The slope bound of Frandsen, Lefgren and Leslie. -/
theorem slope_bound {j k : Fin J} (h : Nested G j k) :
    |Y G k - Y G j| ≤ (G.hi - G.lo) * (P G k - P G j) := by
  rw [outcome_diff G h]
  unfold P
  rw [← sum_sub_distrib, mul_sum]
  refine (abs_sum_le_sum_abs _ _).trans (sum_le_sum fun i _ => ?_)
  have hw := G.w_nonneg i
  have hδ := ind_diff_nonneg G h i
  have h0 := G.y0_mem i
  have h1 := G.y1_mem i
  rw [abs_mul, abs_mul, abs_of_nonneg hw, abs_of_nonneg hδ]
  have hy : |G.y1 i - G.y0 i| ≤ G.hi - G.lo := by
    rw [abs_le]; constructor <;> linarith
  calc G.w i * ((ind (G.d i k) - ind (G.d i j)) * |G.y1 i - G.y0 i|)
      ≤ G.w i * ((ind (G.d i k) - ind (G.d i j)) * (G.hi - G.lo)) := by gcongr
    _ = (G.hi - G.lo) * (G.w i * ind (G.d i k) - G.w i * ind (G.d i j)) := by ring

/-- A pair that violates the slope bound is not nested: the slope test. -/
theorem violation_refutes_monotonicity {j k : Fin J}
    (hv : (G.hi - G.lo) * (P G k - P G j) < |Y G k - Y G j|) : ¬ Nested G j k :=
  fun h => absurd (slope_bound G h) (not_le.mpr hv)

end Research.P14Slope
