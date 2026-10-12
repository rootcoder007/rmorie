/-
  Researchproofs/P13Design.lean

  P13, continued: the design effect of clustering is direction-specific, and
  the intraclass correlation is bounded below (library scans A10 and B9;
  Lohr §5.2 eqs. 5.8-5.10; Weisburd & Britt ch. 7).

  Errors in a cluster sample of C clusters of M units have covariance
  σ²[(1 − ρ)I + ρJ] within a cluster. For a regressor x the variance of the
  OLS slope is σ² xᵀΩx/(xᵀx)², and the quadratic form is
      xᵀΩx/σ² = (1 − ρ) Σ x² + ρ Σ_c (Σ_j x_cj)²               (`quad_form`).
    * a cluster-level regressor (constant within each cluster) gives
      xᵀΩx = σ²(1 + (M − 1)ρ) Σx²: the naive variance understates by the
      design effect 1 + (M − 1)ρ (`cluster_level`);
    * a within-cluster-balanced regressor (cluster sums zero) gives
      xᵀΩx = σ²(1 − ρ)Σx²: the naive variance overstates by 1/(1 − ρ)
      (`within_balanced`);
    * the form is non-negative for the indicator of one cluster
      (`quad_indicator`), which forces 1 + (M − 1)ρ ≥ 0, i.e.
      ρ ≥ −1/(M − 1) (`icc_lower_bound`).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

namespace Research.P13.Design

variable {C : Type*} [Fintype C] {M : ℕ}

/-- The quadratic form `xᵀΩx / σ²` of the exchangeable within-cluster covariance. -/
def quad (ρ : ℝ) (x : C → Fin M → ℝ) : ℝ :=
  (1 - ρ) * ∑ c, ∑ j, x c j ^ 2 + ρ * ∑ c, (∑ j, x c j) ^ 2

/-- The form as `Σ_c Σ_j x_cj (Ω x)_cj` with `(Ω x)_cj = (1 − ρ) x_cj + ρ Σ_k x_ck`. -/
theorem quad_form (ρ : ℝ) (x : C → Fin M → ℝ) :
    quad ρ x = ∑ c, ∑ j, x c j * ((1 - ρ) * x c j + ρ * ∑ k, x c k) := by
  unfold quad
  rw [mul_sum, mul_sum, ← sum_add_distrib]
  apply sum_congr rfl; intro c _
  have h : ∀ j, x c j * ((1 - ρ) * x c j + ρ * ∑ k, x c k)
      = (1 - ρ) * x c j ^ 2 + (ρ * ∑ k, x c k) * x c j := fun j => by ring
  simp_rw [h]
  rw [sum_add_distrib, ← mul_sum, ← mul_sum]
  ring

/-- A cluster-level regressor: `x c j = a c` for all `j`. -/
theorem cluster_level (ρ : ℝ) (a : C → ℝ) :
    quad (M := M) ρ (fun c (_ : Fin M) => a c) = (1 + ((M : ℝ) - 1) * ρ) * ∑ c, ∑ _j : Fin M, a c ^ 2 := by
  unfold quad
  simp only [sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]
  rw [mul_sum, mul_sum, mul_sum, ← sum_add_distrib]
  apply sum_congr rfl; intro c _
  ring

/-- A within-cluster-balanced regressor: every cluster sum is zero. -/
theorem within_balanced (ρ : ℝ) (x : C → Fin M → ℝ) (hbal : ∀ c, ∑ j, x c j = 0) :
    quad ρ x = (1 - ρ) * ∑ c, ∑ j, x c j ^ 2 := by
  unfold quad
  have : ∑ c, (∑ j, x c j) ^ 2 = 0 := by
    apply sum_eq_zero; intro c _; rw [hbal c]; ring
  rw [this]; ring

/-- The form at the indicator of one cluster: `(1 − ρ) M + ρ M²`. -/
theorem quad_indicator [DecidableEq C] (ρ : ℝ) (c0 : C) :
    quad (M := M) ρ (fun c (_ : Fin M) => if c = c0 then 1 else 0) = (1 - ρ) * M + ρ * (M : ℝ) ^ 2 := by
  have hind : ∑ c, ∑ _j : Fin M, (if c = c0 then (1 : ℝ) else 0) ^ 2 = M := by
    rw [sum_eq_single c0]
    · simp
    · intro b _ hb; simp [hb]
    · intro h; exact absurd (mem_univ c0) h
  have hind2 : ∑ c, (∑ _j : Fin M, if c = c0 then (1 : ℝ) else 0) ^ 2 = (M : ℝ) ^ 2 := by
    rw [sum_eq_single c0]
    · simp
    · intro b _ hb; simp [hb]
    · intro h; exact absurd (mem_univ c0) h
  simp only [quad, hind, hind2]

/-- Non-negativity of the form at the one-cluster indicator forces `ρ ≥ −1/(M − 1)`. -/
theorem icc_lower_bound [DecidableEq C] [Nonempty C] (ρ : ℝ) (hM : 2 ≤ M)
    (hpsd : ∀ x : C → Fin M → ℝ, 0 ≤ quad ρ x) :
    -1 / ((M : ℝ) - 1) ≤ ρ := by
  obtain ⟨c0⟩ := (inferInstance : Nonempty C)
  have h := hpsd (fun c (_ : Fin M) => if c = c0 then 1 else 0)
  rw [quad_indicator] at h
  have hM1 : (1 : ℝ) < M := by exact_mod_cast (by omega : 1 < M)
  rw [div_le_iff₀ (by linarith)]
  nlinarith

end Research.P13.Design
