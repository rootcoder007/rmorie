/-
  Researchproofs/P7Mixture.lean

  P7, continued: what a Poisson mixture can and cannot look like.

  A finite mixture of Poisson counts: with probability w_i a place has
  intensity λ_i ≥ 0 (a "risk heterogeneity" model of crime places). Two
  facts follow from convexity alone, with no assumption on the mixing
  law beyond finiteness:

  * mixture_zero_ge_exp_neg_mean: the zero share of the mixture is at
    least exp(−μ) with μ the mean intensity (Jensen on exp). The Poisson
    null's zero share e^{−μ} of P7Concentration is therefore a LOWER bound
    for every heterogeneous population with the same mean: excess zeros
    are implied by heterogeneity, not evidence against Poisson counts per
    place (Friendly & Meyer §11.4.1, eq. 11.9).

  * mixture_var_ge_mean: the variance of the mixture count is at least
    its mean, the excess being the variance of the intensity (Cox
    overdispersion; Grimmett & Stirzaker Problem 6.15.22(b)). Any random
    intensity produces a dispersion index above one, so a Gini above the
    Poisson null cannot by itself separate "criminology of place" from
    heterogeneity of exposure.

  * mixture_var_eq_mean_iff: equality holds exactly when the intensity
    is constant on the support of the mixture.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P7

/-- A finite Poisson mixture: weights `w` on an index set `ι`, intensities `lam`. -/
structure Mixture (ι : Type*) [Fintype ι] where
  w : ι → ℝ
  lam : ι → ℝ
  w_nonneg : ∀ i, 0 ≤ w i
  w_total : ∑ i, w i = 1
  lam_nonneg : ∀ i, 0 ≤ lam i

namespace Mixture

variable {ι : Type*} [Fintype ι] (M : Mixture ι)

/-- Mean intensity. -/
def mean : ℝ := ∑ i, M.w i * M.lam i

/-- Zero share of the mixture count: `E[exp(-Λ)]`. -/
def zeroShare : ℝ := ∑ i, M.w i * Real.exp (-M.lam i)

/-- Second moment of the mixture count, from `E[N² | Λ] = Λ + Λ²`. -/
def secondMoment : ℝ := ∑ i, M.w i * (M.lam i + M.lam i ^ 2)

/-- Variance of the mixture count. -/
def variance : ℝ := M.secondMoment - M.mean ^ 2

/-- Variance of the intensity. -/
def intensityVariance : ℝ := ∑ i, M.w i * (M.lam i - M.mean) ^ 2

/-- Jensen: the zero share of a Poisson mixture is at least `exp(-mean)`. -/
theorem mixture_zero_ge_exp_neg_mean : Real.exp (-M.mean) ≤ M.zeroShare := by
  unfold zeroShare mean
  have h := (convexOn_exp.map_sum_le (t := univ) (w := M.w) (p := fun i => -M.lam i)
    (fun i _ => M.w_nonneg i) M.w_total (fun i _ => Set.mem_univ _))
  simp only [smul_eq_mul] at h
  have hneg : -∑ i, M.w i * M.lam i = ∑ i, M.w i * -M.lam i := by
    rw [← sum_neg_distrib]; apply sum_congr rfl; intro i _; ring
  rw [hneg]; exact h

/-- The variance decomposition: `Var N = E Λ + Var Λ`. -/
theorem variance_eq : M.variance = M.mean + M.intensityVariance := by
  unfold variance secondMoment intensityVariance
  have hw := M.w_total
  have key : ∀ i, M.w i * (M.lam i - M.mean) ^ 2
      = M.w i * M.lam i ^ 2 - (2 * M.mean) * (M.w i * M.lam i) + M.mean ^ 2 * M.w i := by
    intro i; ring
  have e1 : ∑ i, M.w i * (M.lam i + M.lam i ^ 2) = ∑ i, M.w i * M.lam i + ∑ i, M.w i * M.lam i ^ 2 := by
    rw [← sum_add_distrib]; apply sum_congr rfl; intro i _; ring
  simp_rw [key]
  rw [sum_add_distrib, sum_sub_distrib, ← mul_sum, ← mul_sum, hw, e1]
  unfold mean
  ring

/-- Cox overdispersion: the variance of a Poisson mixture is at least its mean. -/
theorem mixture_var_ge_mean : M.mean ≤ M.variance := by
  rw [variance_eq]
  have : 0 ≤ M.intensityVariance := by
    unfold intensityVariance
    apply sum_nonneg; intro i _
    exact mul_nonneg (M.w_nonneg i) (sq_nonneg _)
  linarith

/-- Equality holds iff the intensity is constant wherever the weight is positive. -/
theorem mixture_var_eq_mean_iff :
    M.variance = M.mean ↔ ∀ i, 0 < M.w i → M.lam i = M.mean := by
  rw [variance_eq]
  constructor
  · intro h i hi
    have hz : M.intensityVariance = 0 := by linarith
    unfold intensityVariance at hz
    have hterms : ∀ j ∈ univ, 0 ≤ M.w j * (M.lam j - M.mean) ^ 2 :=
      fun j _ => mul_nonneg (M.w_nonneg j) (sq_nonneg _)
    have := (sum_eq_zero_iff_of_nonneg hterms).1 hz i (mem_univ i)
    rcases mul_eq_zero.1 this with h0 | h0
    · exact absurd h0 hi.ne'
    · exact sub_eq_zero.1 (pow_eq_zero_iff (n := 2) (by norm_num) |>.1 h0)
  · intro h
    have : M.intensityVariance = 0 := by
      unfold intensityVariance
      apply sum_eq_zero; intro i _
      rcases eq_or_lt_of_le (M.w_nonneg i) with h0 | h0
      · rw [← h0]; ring
      · rw [h i h0]; ring
    linarith

end Mixture

end Research.P7
