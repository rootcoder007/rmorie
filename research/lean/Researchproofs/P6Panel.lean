/-
  Researchproofs/P6Panel.lean

  P6, continued: what panel data add to the aggregate curve.

  Individuals come in types k with weights w_k (summing to one) and
  age-specific rates λ_k(a). The aggregate curve at each age is the
  weighted mean rate — P6AgeCrime showed that is all the aggregate
  carries. With two ages a and b observed on the SAME individuals and
  counts conditionally independent given type (Poisson trajectories), the
  cross moment E[y_a y_b] = ∑_k w_k λ_k(a) λ_k(b), so

    * cross_moment_eq: cov(y_a, y_b) = ∑_k w_k (λ_k(a) − m_a)(λ_k(b) − m_b):
      the covariance of the rates across types.
    * one_type_cov_zero: a single type has cross-age covariance zero —
      the null of the invariance test, exactly.
    * two_type_cov: for two types the covariance is w₁ w₂ (λ₁(a) − λ₂(a))
      (λ₁(b) − λ₂(b)), non-zero whenever the types differ at both ages in
      the same or opposite direction.
    * panel_separates_witness: the P6AgeCrime witness (two types with the
      same aggregate curve as one type) has non-zero cross-age
      covariance, so the panel distinguishes what the aggregate cannot —
      under the stated separation condition (types differ at both ages).
    * aggregate_blind: two type structures with the same weighted mean at
      every age have the same aggregate, whatever their covariance.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P6.Panel

variable {K : Type*} [Fintype K] (w : K → ℝ) (ra rb : K → ℝ)

/-- Aggregate rate at an age. -/
def agg (f : K → ℝ) : ℝ := ∑ k, w k * f k
/-- Cross-age covariance implied by conditional independence given type. -/
def crossCov : ℝ := (∑ k, w k * (ra k * rb k)) - agg w ra * agg w rb

theorem cross_moment_eq (hw : ∑ k, w k = 1) :
    crossCov w ra rb = ∑ k, w k * ((ra k - agg w ra) * (rb k - agg w rb)) := by
  unfold crossCov
  have hpt : ∀ k, w k * ((ra k - agg w ra) * (rb k - agg w rb))
      = w k * (ra k * rb k) - agg w rb * (w k * ra k) - agg w ra * (w k * rb k)
        + (agg w ra * agg w rb) * w k := fun k => by ring
  simp only [hpt, sum_add_distrib, sum_sub_distrib, ← mul_sum, hw, mul_one]
  unfold agg
  ring

theorem one_type_cov_zero (hw : ∑ k, w k = 1) (ra rb : ℝ) :
    crossCov w (fun _ => ra) (fun _ => rb) = 0 := by
  unfold crossCov agg
  simp only [← sum_mul, hw, one_mul]
  ring

/-- Two types with weights `w₁ + w₂ = 1`. -/
theorem two_type_cov (w₁ w₂ a₁ a₂ b₁ b₂ : ℝ) (hw : w₁ + w₂ = 1) :
    crossCov (![w₁, w₂] : Fin 2 → ℝ) ![a₁, a₂] ![b₁, b₂] = w₁ * w₂ * ((a₁ - a₂) * (b₁ - b₂)) := by
  unfold crossCov agg
  simp [Fin.sum_univ_two]
  have : w₂ = 1 - w₁ := by linarith
  subst this
  ring

/-- The two-type witness of P6AgeCrime (weights 1/2, rates 1 and 3 at age a, 3 and 1 at age b)
has the same aggregate as the one-type curve (2, 2) but a cross-age covariance of −1. -/
theorem panel_separates_witness :
    agg (![1/2, 1/2] : Fin 2 → ℝ) ![1, 3] = agg (![1/2, 1/2] : Fin 2 → ℝ) ![2, 2] ∧
    agg (![1/2, 1/2] : Fin 2 → ℝ) ![3, 1] = agg (![1/2, 1/2] : Fin 2 → ℝ) ![2, 2] ∧
    crossCov (![1/2, 1/2] : Fin 2 → ℝ) ![1, 3] ![3, 1] = -1 ∧
    crossCov (![1/2, 1/2] : Fin 2 → ℝ) ![2, 2] ![2, 2] = 0 := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;> simp [agg, crossCov, Fin.sum_univ_two] <;> norm_num

theorem aggregate_blind {K' : Type*} [Fintype K'] (w' : K' → ℝ) (ra' rb' : K' → ℝ)
    (ha : agg w ra = agg w' ra') (hb : agg w rb = agg w' rb') :
    agg w ra = agg w' ra' ∧ agg w rb = agg w' rb' := ⟨ha, hb⟩

end Research.P6.Panel
