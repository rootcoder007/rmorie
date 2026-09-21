/-
  Researchproofs/P3SYG.lean

  P3, continued: the Sen–Yates–Grundy form of the design variance.

  For a design in which the number of places at the exposure level is
  fixed (Σ_k π_ik = m π_i for every i, with Σ_k π_k = m), the
  Horvitz–Thompson variance
      Σ_i Σ_k (π_ik − π_i π_k) c_i c_k,      c_i = t_i / π_i,
  equals the Sen–Yates–Grundy form
      ½ Σ_i Σ_k (π_i π_k − π_ik) (c_i − c_k)²,
  which is zero when t_i ∝ π_i (Lohr, Sampling: Design and Analysis,
  §6.4, Theorem 6.2). The identity is what makes the SYG estimator
  non-negative on fixed-size designs; on an exposure mapping whose
  level counts vary across assignments the hypothesis fails and the two
  forms differ, which the R function reports rather than assumes away.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P3.SYG

variable {Ω : Type*} [Fintype Ω]

/-- Fixed-size design: joint probabilities sum to `m π_i`, marginals to `m`. -/
def FixedSize (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (m : ℝ) : Prop :=
  (∀ i, ∑ k, π2 i k = m * π i) ∧ ∑ k, π k = m

def htForm (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (c : Ω → ℝ) : ℝ :=
  ∑ i, ∑ k, (π2 i k - π i * π k) * (c i * c k)

def sygForm (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (c : Ω → ℝ) : ℝ :=
  (1 / 2) * ∑ i, ∑ k, (π i * π k - π2 i k) * (c i - c k) ^ 2

theorem row_sum_zero (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (m : ℝ) (h : FixedSize π π2 m) (i : Ω) :
    ∑ k, (π i * π k - π2 i k) = 0 := by
  rw [sum_sub_distrib, ← mul_sum, h.2, h.1 i]; ring

/-- Under a fixed-size symmetric design the two variance forms agree. -/
theorem syg_eq_ht (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (c : Ω → ℝ) (m : ℝ) (h : FixedSize π π2 m)
    (hsym : ∀ i k, π2 i k = π2 k i) :
    sygForm π π2 c = htForm π π2 c := by
  unfold sygForm htForm
  have hrow : ∀ i, ∑ k, (π i * π k - π2 i k) = 0 := row_sum_zero π π2 m h
  have hcol : ∀ k, ∑ i, (π i * π k - π2 i k) = 0 := by
    intro k; rw [← hrow k]; apply sum_congr rfl; intro i _; rw [hsym i k, mul_comm]
  have expand : ∀ i k, (π i * π k - π2 i k) * (c i - c k) ^ 2
      = c i ^ 2 * (π i * π k - π2 i k) + c k ^ 2 * (π i * π k - π2 i k)
        - 2 * ((π i * π k - π2 i k) * (c i * c k)) := by intro i k; ring
  simp_rw [expand, sum_sub_distrib, sum_add_distrib]
  have t1 : ∑ i, ∑ k, c i ^ 2 * (π i * π k - π2 i k) = 0 := by
    apply sum_eq_zero; intro i _; rw [← mul_sum, hrow i, mul_zero]
  have t2 : ∑ i, ∑ k, c k ^ 2 * (π i * π k - π2 i k) = 0 := by
    rw [sum_comm]; apply sum_eq_zero; intro k _; rw [← mul_sum, hcol k, mul_zero]
  have e : ∑ i, ∑ k, 2 * ((π i * π k - π2 i k) * (c i * c k))
      = -2 * ∑ i, ∑ k, (π2 i k - π i * π k) * (c i * c k) := by
    rw [mul_sum]; apply sum_congr rfl; intro i _
    rw [mul_sum]; apply sum_congr rfl; intro k _; ring
  rw [t1, t2, e]; ring

/-- Proportional totals (`t_i = λ π_i`, so `c` constant) give zero SYG variance. -/
theorem syg_zero_of_const (π : Ω → ℝ) (π2 : Ω → Ω → ℝ) (lam : ℝ) :
    sygForm π π2 (fun _ => lam) = 0 := by
  unfold sygForm; simp

end Research.P3.SYG
