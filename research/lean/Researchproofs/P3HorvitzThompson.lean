/-
  Researchproofs/P3HorvitzThompson.lean

  P3, continued: design-based estimation under interference.

  When the analyst controls the assignment (a randomised hot-spot
  deployment) the exposure of each place is a random variable with known
  probabilities π_i(ℓ) = P(E_i = ℓ). The Horvitz–Thompson estimator of the
  population total of the potential outcome at exposure ℓ,

      T̂(ℓ) = Σ_i 1{E_i = ℓ} · Y_i(ℓ) / π_i(ℓ),

  is unbiased for T(ℓ) = Σ_i Y_i(ℓ) as long as every π_i(ℓ) is positive.
  The randomness is over a finite set of assignments with probabilities
  q(a); the exposure of place i under assignment a is e(a, i). No
  ignorability assumption is needed — the design supplies it.

  Proved:
    * ht_unbiased: E_q[T̂(ℓ)] = T(ℓ) whenever π_i(ℓ) > 0 for all i.
    * ht_contrast_unbiased: the estimated contrast T̂(ℓ₂) − T̂(ℓ₀) is
      unbiased for the total direct-plus-spillover effect, and likewise
      each part of the decomposition.

  The theorem holds for every exposure mapping; the choice of mapping
  (which ring counts as "exposed") is the analyst's, and the mapping's
  mis-specification is what P3Interference.misspecified_exposure_bias
  quantifies.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3

/-- A finite randomised design: assignments `A`, places `Ω`, exposure levels `E`. -/
structure Design (A Ω E : Type*) [Fintype A] [Fintype Ω] [Fintype E] [DecidableEq E] where
  q : A → ℝ                 -- probability of each assignment
  q_nonneg : ∀ a, 0 ≤ q a
  q_total : ∑ a, q a = 1
  e : A → Ω → E             -- exposure of each place under each assignment
  Y : E → Ω → ℝ             -- potential outcomes

namespace Design

variable {A Ω E : Type*} [Fintype A] [Fintype Ω] [Fintype E] [DecidableEq E]
variable (D : Design A Ω E)

/-- Probability that place `i` is at exposure `ℓ`. -/
def π (i : Ω) (ℓ : E) : ℝ := ∑ a, if D.e a i = ℓ then D.q a else 0

/-- Population total of the potential outcome at exposure `ℓ`. -/
def total (ℓ : E) : ℝ := ∑ i, D.Y ℓ i

/-- Horvitz–Thompson estimate of `total ℓ` under assignment `a`. -/
def ht (ℓ : E) (a : A) : ℝ := ∑ i, if D.e a i = ℓ then D.Y ℓ i / D.π i ℓ else 0

/-- Expectation over the design. -/
def expect (f : A → ℝ) : ℝ := ∑ a, D.q a * f a

/-- The Horvitz–Thompson estimator is unbiased when every exposure probability is positive. -/
theorem ht_unbiased (ℓ : E) (hpos : ∀ i, 0 < D.π i ℓ) :
    D.expect (D.ht ℓ) = D.total ℓ := by
  unfold expect ht total
  simp_rw [mul_sum]
  rw [sum_comm]
  apply sum_congr rfl
  intro i _
  have hπ : D.π i ℓ ≠ 0 := (hpos i).ne'
  -- Σ_a q a * (if e a i = ℓ then Y/π else 0) = (Y/π) * Σ_a (if e a i = ℓ then q a else 0) = Y
  have : ∀ a, D.q a * (if D.e a i = ℓ then D.Y ℓ i / D.π i ℓ else 0)
      = (D.Y ℓ i / D.π i ℓ) * (if D.e a i = ℓ then D.q a else 0) := by
    intro a; split_ifs <;> ring
  simp_rw [this]
  rw [← mul_sum]
  change D.Y ℓ i / D.π i ℓ * D.π i ℓ = D.Y ℓ i
  field_simp

/-- Unbiasedness of a contrast between two exposure levels. -/
theorem ht_contrast_unbiased (ℓ₀ ℓ₂ : E) (h0 : ∀ i, 0 < D.π i ℓ₀) (h2 : ∀ i, 0 < D.π i ℓ₂) :
    D.expect (fun a => D.ht ℓ₂ a - D.ht ℓ₀ a) = D.total ℓ₂ - D.total ℓ₀ := by
  have e : D.expect (fun a => D.ht ℓ₂ a - D.ht ℓ₀ a) = D.expect (D.ht ℓ₂) - D.expect (D.ht ℓ₀) := by
    unfold expect; rw [← sum_sub_distrib]; apply sum_congr rfl; intro a _; ring
  rw [e, D.ht_unbiased ℓ₂ h2, D.ht_unbiased ℓ₀ h0]

end Design

end Research.P3
