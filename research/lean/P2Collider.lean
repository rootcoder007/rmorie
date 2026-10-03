/-
  Researchproofs/P2Collider.lean

  P2, continued: the sign of collider-stratification bias (Hernán &
  Robins, Causal Inference: What If, Fine Point 8.2).

  Two independent risk factors A and E (prevalences a, e) and a
  background cause U (prevalence π), independent of both, produce an
  arrest Y through an "or" mechanism: Y = A ∨ E ∨ U. In the population A
  and E are independent (odds ratio 1). Among arrestees the odds ratio
  between A and E is exactly π (collider_or_eq_background): below one
  whenever the background cause is not universal, and tending to zero
  (log OR → −∞) as π → 0. Conditioning on arrest manufactures a negative
  association between independent risk factors, with size set by how
  often arrests happen for other reasons. Analyses of police records
  that condition on being in the record inherit this bias.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section

namespace Research.P2

/-- Joint probability of `(A = i, E = j, Y = 1)` under `Y = A ∨ E ∨ U` with independent factors. -/
def jointY (a e π : ℝ) (i j : Bool) : ℝ :=
  (if i then a else 1 - a) * (if j then e else 1 - e) * (if i || j then 1 else π)

/-- Odds ratio between A and E among those with Y = 1. -/
def colliderOR (a e π : ℝ) : ℝ :=
  (jointY a e π true true * jointY a e π false false) /
    (jointY a e π true false * jointY a e π false true)

theorem collider_or_eq_background (a e π : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (he0 : 0 < e) (he1 : e < 1) :
    colliderOR a e π = π := by
  unfold colliderOR jointY
  simp only [Bool.true_or, Bool.false_or, Bool.or_self, Bool.false_eq_true, ite_true, ite_false, mul_one]
  have h1 : (1 - a) ≠ 0 := by linarith
  have h2 : (1 - e) ≠ 0 := by linarith
  field_simp

theorem collider_or_lt_one (a e π : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (he0 : 0 < e) (he1 : e < 1) (hπ : π < 1) :
    colliderOR a e π < 1 := by
  rw [collider_or_eq_background a e π ha0 ha1 he0 he1]; exact hπ

/-- In the population the two factors are independent: odds ratio one. -/
theorem population_or_one (a e : ℝ) (ha0 : 0 < a) (ha1 : a < 1) (he0 : 0 < e) (he1 : e < 1) :
    (a * e * ((1 - a) * (1 - e))) / ((a * (1 - e)) * ((1 - a) * e)) = 1 := by
  have h1 : (1 - a) ≠ 0 := by linarith
  have h2 : (1 - e) ≠ 0 := by linarith
  field_simp

end Research.P2
