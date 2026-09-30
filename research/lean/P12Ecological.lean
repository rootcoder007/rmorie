/-
  Researchproofs/P12Ecological.lean

  P12 (new): ecological correlation versus individual correlation.

  Split every individual value into its group mean (the "between" part)
  and the residual (the "within" part). The within part is orthogonal to
  every group-level quantity, so covariances and variances decompose
  exactly (cov_decomp, var_decomp). If the within-group covariance is
  zero, the individual covariance equals the between covariance while
  the individual variances exceed the between variances, hence

      corr(x, y)² ≤ corr(x̄_g, ȳ_g)²            (ecological_ge)

  — the group-level correlation is at least as large in magnitude as the
  individual one (Freedman, Pisani & Purves, Statistics 4e, ch. 9 §4).
  Without that hypothesis the sign can flip (robinson_reversal: a
  four-person, two-group population with negative between covariance
  and positive individual covariance). Regressing neighbourhood crime
  rates on neighbourhood composition says nothing about individuals
  unless the within-neighbourhood covariance is known.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P12

variable {Ω G : Type*} [Fintype Ω] [Fintype G] [DecidableEq G]

def fiber (g : Ω → G) (a : G) : Finset Ω := univ.filter (fun i => g i = a)
def gmean (g : Ω → G) (x : Ω → ℝ) (a : G) : ℝ := (∑ i ∈ fiber g a, x i) / (fiber g a).card
def between (g : Ω → G) (x : Ω → ℝ) (i : Ω) : ℝ := gmean g x (g i)
def within (g : Ω → G) (x : Ω → ℝ) (i : Ω) : ℝ := x i - between g x i

/-- Uncentred cross moment, and the covariance with `N = card Ω`. -/
def cov (a b : Ω → ℝ) : ℝ := ∑ i, a i * b i - (∑ i, a i) * (∑ i, b i) / (Fintype.card Ω : ℝ)
def var (a : Ω → ℝ) : ℝ := cov a a

lemma fiber_sum_within (g : Ω → G) (x : Ω → ℝ) (a : G) : ∑ i ∈ fiber g a, within g x i = 0 := by
  unfold within between
  have hc : ∀ i ∈ fiber g a, gmean g x (g i) = gmean g x a := by
    intro i hi; simp only [fiber, mem_filter] at hi; rw [hi.2]
  rw [sum_sub_distrib, sum_congr rfl hc, sum_const, nsmul_eq_mul]
  unfold gmean
  rcases Nat.eq_zero_or_pos (fiber g a).card with h | h
  · rw [card_eq_zero] at h; simp [h]
  · have : ((fiber g a).card : ℝ) ≠ 0 := by positivity
    field_simp
    ring

/-- The within part is orthogonal to every group-level function. -/
theorem within_orth (g : Ω → G) (x : Ω → ℝ) (F : G → ℝ) : ∑ i, within g x i * F (g i) = 0 := by
  rw [← sum_fiberwise_of_maps_to (s := univ) (t := univ) (g := g) (fun i _ => mem_univ (g i))]
  apply sum_eq_zero; intro a _
  have hc : ∀ i ∈ univ.filter (fun i => g i = a), within g x i * F (g i) = within g x i * F a := by
    intro i hi; simp only [mem_filter] at hi; rw [hi.2]
  rw [sum_congr rfl hc, ← sum_mul]
  change (∑ i ∈ fiber g a, within g x i) * F a = 0
  rw [fiber_sum_within, zero_mul]

theorem sum_within (g : Ω → G) (x : Ω → ℝ) : ∑ i, within g x i = 0 := by
  have := within_orth g x (fun _ => 1); simpa using this

theorem sum_between (g : Ω → G) (x : Ω → ℝ) : ∑ i, between g x i = ∑ i, x i := by
  have h := sum_within g x
  unfold within at h; rw [sum_sub_distrib] at h; linarith

theorem cross_within_between (g : Ω → G) (x y : Ω → ℝ) : ∑ i, within g x i * between g y i = 0 :=
  within_orth g x (gmean g y)

/-- Covariance decomposes exactly into between and within parts. -/
theorem cov_decomp (g : Ω → G) (x y : Ω → ℝ) :
    cov x y = cov (between g x) (between g y) + cov (within g x) (within g y) := by
  unfold cov
  have hx : ∀ i, x i = between g x i + within g x i := by intro i; unfold within; ring
  have hxy : ∑ i, x i * y i = ∑ i, between g x i * between g y i + ∑ i, within g x i * within g y i := by
    have e : ∀ i, x i * y i = between g x i * between g y i + within g x i * within g y i
        + (within g x i * between g y i + within g y i * between g x i) := by
      intro i; rw [hx i]; unfold within; ring
    simp_rw [e]
    rw [sum_add_distrib, sum_add_distrib, sum_add_distrib, cross_within_between, cross_within_between]
    ring
  rw [hxy, sum_between, sum_between, sum_within, sum_within]
  ring

theorem var_decomp (g : Ω → G) (x : Ω → ℝ) : var x = var (between g x) + var (within g x) :=
  cov_decomp g x x

theorem var_within_nonneg (g : Ω → G) (x : Ω → ℝ) : 0 ≤ var (within g x) := by
  unfold var cov
  rw [sum_within, zero_mul, zero_div, sub_zero]
  exact sum_nonneg (fun i _ => mul_self_nonneg _)

/-- Any variance is a sum of squared deviations, hence non-negative. -/
theorem var_eq_sum_sq (a : Ω → ℝ) :
    var a = ∑ i, (a i - (∑ j, a j) / (Fintype.card Ω : ℝ)) ^ 2 := by
  unfold var cov
  rcases Nat.eq_zero_or_pos (Fintype.card Ω) with h0 | h0
  · have : IsEmpty Ω := Fintype.card_eq_zero_iff.mp h0
    simp
  · have hN : (Fintype.card Ω : ℝ) ≠ 0 := by positivity
    simp_rw [sub_sq, sum_add_distrib, sum_sub_distrib, ← sum_mul, ← mul_sum, sum_const, card_univ, nsmul_eq_mul]
    field_simp
    ring

theorem var_nonneg (a : Ω → ℝ) : 0 ≤ var a := by
  rw [var_eq_sum_sq]; exact sum_nonneg (fun i _ => sq_nonneg _)

/-- With zero within-group covariance the squared ecological correlation is at least the individual one. -/
theorem ecological_ge (g : Ω → G) (x y : Ω → ℝ) (hw : cov (within g x) (within g y) = 0) :
    (cov x y) ^ 2 * (var (between g x) * var (between g y)) ≤ (cov (between g x) (between g y)) ^ 2 * (var x * var y) := by
  rw [cov_decomp g x y, hw, add_zero, var_decomp g x, var_decomp g y]
  have hx := var_nonneg (within g x)
  have hy := var_nonneg (within g y)
  have hbx := var_nonneg (between g x)
  have hby := var_nonneg (between g y)
  have h1 : var (between g x) * var (between g y) ≤ (var (between g x) + var (within g x)) * (var (between g y) + var (within g y)) := by
    nlinarith [mul_nonneg hbx hy, mul_nonneg hx hby, mul_nonneg hx hy]
  exact mul_le_mul_of_nonneg_left h1 (sq_nonneg _)

end Research.P12
