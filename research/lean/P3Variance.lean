/-
  Researchproofs/P3Variance.lean

  P3, continued: the variance of the Horvitz–Thompson total under a
  randomised deployment, and an unbiased estimator of it.

  With joint exposure probabilities π_ij(ℓ) = P(E_i = ℓ, E_j = ℓ),

      Var T̂(ℓ) = Σ_i Σ_j (π_ij − π_i π_j) Y_i Y_j / (π_i π_j)          (ht_variance)

  and the Horvitz–Thompson variance estimator

      V̂(ℓ) = Σ_i Σ_j 1{E_i = ℓ, E_j = ℓ} (π_ij − π_i π_j) / π_ij · Y_i Y_j / (π_i π_j)

  is unbiased for it whenever every π_ij(ℓ) > 0                      (ht_variance_estimator_unbiased).

  Both are finite-sum identities over the design; no model of the
  outcomes enters. The positivity of the joint probabilities is the
  design condition that a deployment must satisfy for its uncertainty to
  be estimable at all — a design in which two places can never share an
  exposure level leaves the variance unidentified.
-/
import Mathlib
import Researchproofs.P3HorvitzThompson

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3.Design

variable {A Ω E : Type*} [Fintype A] [Fintype Ω] [Fintype E] [DecidableEq E]
variable (D : Design A Ω E)

/-- Indicator that place `i` is at exposure `ℓ` under assignment `a`. -/
def ind (ℓ : E) (a : A) (i : Ω) : ℝ := if D.e a i = ℓ then 1 else 0

/-- Joint exposure probability of places `i` and `j` at level `ℓ`. -/
def π2 (i j : Ω) (ℓ : E) : ℝ := ∑ a, D.q a * (D.ind ℓ a i * D.ind ℓ a j)

theorem π_eq_sum_ind (i : Ω) (ℓ : E) : D.π i ℓ = ∑ a, D.q a * D.ind ℓ a i := by
  unfold π ind; apply sum_congr rfl; intro a _; split_ifs <;> simp

theorem ht_eq_sum_ind (ℓ : E) (a : A) :
    D.ht ℓ a = ∑ i, D.ind ℓ a i * (D.Y ℓ i / D.π i ℓ) := by
  unfold ht ind; apply sum_congr rfl; intro i _; split_ifs <;> simp

/-- Second moment of the Horvitz–Thompson total. -/
theorem ht_second_moment (ℓ : E) :
    D.expect (fun a => D.ht ℓ a ^ 2) =
      ∑ i, ∑ j, D.π2 i j ℓ * ((D.Y ℓ i / D.π i ℓ) * (D.Y ℓ j / D.π j ℓ)) := by
  unfold expect
  simp_rw [ht_eq_sum_ind, sq, sum_mul_sum, mul_sum]
  rw [sum_comm]
  apply sum_congr rfl; intro i _
  rw [sum_comm]
  apply sum_congr rfl; intro j _
  unfold π2; rw [sum_mul]
  apply sum_congr rfl; intro a _; ring

/-- The Horvitz–Thompson variance formula. -/
theorem ht_variance (ℓ : E) (hpos : ∀ i, 0 < D.π i ℓ) :
    D.expect (fun a => D.ht ℓ a ^ 2) - (D.expect (D.ht ℓ)) ^ 2 =
      ∑ i, ∑ j, (D.π2 i j ℓ - D.π i ℓ * D.π j ℓ) * (D.Y ℓ i * D.Y ℓ j) / (D.π i ℓ * D.π j ℓ) := by
  rw [D.ht_second_moment ℓ, D.ht_unbiased ℓ hpos]
  unfold total
  rw [sq, sum_mul_sum, ← sum_sub_distrib]
  apply sum_congr rfl; intro i _
  rw [← sum_sub_distrib]
  apply sum_congr rfl; intro j _
  have hi := (hpos i).ne'; have hj := (hpos j).ne'
  field_simp

/-- The Horvitz–Thompson variance estimator under assignment `a`. -/
def htVarEst (ℓ : E) (a : A) : ℝ :=
  ∑ i, ∑ j, D.ind ℓ a i * D.ind ℓ a j * ((D.π2 i j ℓ - D.π i ℓ * D.π j ℓ) / D.π2 i j ℓ) *
    (D.Y ℓ i * D.Y ℓ j) / (D.π i ℓ * D.π j ℓ)

/-- The variance estimator is unbiased when every joint probability is positive. -/
theorem ht_variance_estimator_unbiased (ℓ : E) (hpos : ∀ i, 0 < D.π i ℓ)
    (hjoint : ∀ i j, 0 < D.π2 i j ℓ) :
    D.expect (D.htVarEst ℓ) = D.expect (fun a => D.ht ℓ a ^ 2) - (D.expect (D.ht ℓ)) ^ 2 := by
  rw [D.ht_variance ℓ hpos]
  unfold expect htVarEst
  simp_rw [mul_sum]
  rw [sum_comm]
  apply sum_congr rfl; intro i _
  rw [sum_comm]
  apply sum_congr rfl; intro j _
  have h2 : D.π2 i j ℓ ≠ 0 := (hjoint i j).ne'
  have hsum : ∑ a, D.q a * (D.ind ℓ a i * D.ind ℓ a j) = D.π2 i j ℓ := rfl
  have : ∀ a, D.q a * (D.ind ℓ a i * D.ind ℓ a j * ((D.π2 i j ℓ - D.π i ℓ * D.π j ℓ) / D.π2 i j ℓ) *
        (D.Y ℓ i * D.Y ℓ j) / (D.π i ℓ * D.π j ℓ))
      = (D.q a * (D.ind ℓ a i * D.ind ℓ a j)) *
        (((D.π2 i j ℓ - D.π i ℓ * D.π j ℓ) / D.π2 i j ℓ) * (D.Y ℓ i * D.Y ℓ j) / (D.π i ℓ * D.π j ℓ)) := by
    intro a; ring
  simp_rw [this]
  rw [← sum_mul, hsum]
  field_simp

end Research.P3.Design
