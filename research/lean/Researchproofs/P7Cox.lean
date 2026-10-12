/-
  Researchproofs/P7Cox.lean

  P7, continued: any intensity heterogeneity forces overdispersion (library
  scan C12; Grimmett & Stirzaker Problem 6.15.22(b)); the Vuong boundary
  (library scan B24; Friendly & Meyer §11.2).

  A Cox process on a finite mixture of intensities λ_k with weights w_k is a
  mixture of Poissons. By the law of total variance,
      Var N = E N + Var Λ,
  so `variance_ge_mean` (Var N ≥ E N) with equality iff Var Λ = 0, i.e. the
  intensity is constant on the support (`equality_iff_constant`). Corollary
  for P7: any random intensity pushes the zero share and the Gini above the
  Poisson null, so the P7 decomposition has to separate mixing from
  contagion rather than read "above Poisson" as contagion.

  Vuong's test for ZIP against Poisson is misapplied at the boundary: at
  π = 0 the ZIP pmf IS the Poisson pmf (`zip_zero_eq_poisson`), so every
  pointwise log-likelihood ratio is 0 and the statistic's denominator is 0,
  not N(0, 1) (`llr_zero`).
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P7.Cox

variable {K : Type*} [Fintype K]

/-- A finite mixture of Poissons: weights `w`, intensities `lam`. -/
structure Mixture (K : Type*) [Fintype K] where
  w : K → ℝ
  lam : K → ℝ
  w_nonneg : ∀ k, 0 ≤ w k
  w_sum : ∑ k, w k = 1
  lam_nonneg : ∀ k, 0 ≤ lam k

/-- Mean and second moment of the mixed count (a Poisson(λ) has `E N = λ`, `E N² = λ + λ²`). -/
def mean (m : Mixture K) : ℝ := ∑ k, m.w k * m.lam k
def secondMoment (m : Mixture K) : ℝ := ∑ k, m.w k * (m.lam k + m.lam k ^ 2)
def variance (m : Mixture K) : ℝ := secondMoment m - mean m ^ 2
/-- The variance of the intensity itself. -/
def intensityVar (m : Mixture K) : ℝ := ∑ k, m.w k * (m.lam k - mean m) ^ 2

/-- The law of total variance for the mixture: `Var N = E N + Var Λ`. -/
theorem total_variance (m : Mixture K) : variance m = mean m + intensityVar m := by
  unfold variance secondMoment intensityVar
  set μ := mean m with hμ
  have hpt : ∀ k, m.w k * (m.lam k - μ) ^ 2 = m.w k * m.lam k ^ 2 - (2 * μ) * (m.w k * m.lam k) + μ ^ 2 * m.w k := by
    intro k; ring
  have hsq : ∑ k, m.w k * (m.lam k - μ) ^ 2 = ∑ k, m.w k * m.lam k ^ 2 - 2 * μ * μ + μ ^ 2 := by
    rw [sum_congr rfl fun k _ => hpt k, sum_add_distrib, sum_sub_distrib, ← mul_sum, ← mul_sum, m.w_sum, mul_one]
    rfl
  have hsm : ∑ k, m.w k * (m.lam k + m.lam k ^ 2) = μ + ∑ k, m.w k * m.lam k ^ 2 := by
    rw [hμ]; unfold mean
    rw [← sum_add_distrib]; apply sum_congr rfl; intro k _; ring
  rw [hsq, hsm]; ring

/-- Overdispersion: the variance is at least the mean. -/
theorem variance_ge_mean (m : Mixture K) : mean m ≤ variance m := by
  rw [total_variance]
  have : 0 ≤ intensityVar m := sum_nonneg fun k _ => mul_nonneg (m.w_nonneg k) (sq_nonneg _)
  linarith

/-- Equality iff the intensity is constant on the support of the weights. -/
theorem equality_iff_constant (m : Mixture K) :
    variance m = mean m ↔ ∀ k, 0 < m.w k → m.lam k = mean m := by
  rw [total_variance]
  constructor
  · intro h
    have hz : intensityVar m = 0 := by linarith
    intro k hk
    have hterms := (sum_eq_zero_iff_of_nonneg fun j _ => mul_nonneg (m.w_nonneg j) (sq_nonneg (m.lam j - mean m))).mp hz
    have := hterms k (mem_univ k)
    rcases mul_eq_zero.mp this with h0 | h0
    · linarith
    · nlinarith [sq_nonneg (m.lam k - mean m), pow_eq_zero_iff (n := 2) (a := m.lam k - mean m) two_ne_zero |>.mp h0]
  · intro h
    have : intensityVar m = 0 := by
      unfold intensityVar
      apply sum_eq_zero
      intro k _
      rcases (m.w_nonneg k).lt_or_eq with hk | hk
      · rw [h k hk]; ring
      · rw [← hk]; ring
    linarith

/-- The zero-inflated Poisson pmf and the Poisson pmf (unnormalised by `e^{-λ}` only, so no
exponential is needed): at `π = 0` they agree at every count. -/
def zipPmf (π lam : ℝ) (n : ℕ) : ℝ :=
  (if n = 0 then π else 0) + (1 - π) * Real.exp (-lam) * lam ^ n / n.factorial
def poissonPmf (lam : ℝ) (n : ℕ) : ℝ := Real.exp (-lam) * lam ^ n / n.factorial

theorem zip_zero_eq_poisson (lam : ℝ) (n : ℕ) : zipPmf 0 lam n = poissonPmf lam n := by
  unfold zipPmf poissonPmf; simp

/-- Every pointwise log-likelihood ratio at the boundary is zero, so their sample variance is zero. -/
theorem llr_zero (lam : ℝ) (n : ℕ) (hpos : 0 < poissonPmf lam n) :
    Real.log (zipPmf 0 lam n) - Real.log (poissonPmf lam n) = 0 := by
  rw [zip_zero_eq_poisson]; ring

end Research.P7.Cox
