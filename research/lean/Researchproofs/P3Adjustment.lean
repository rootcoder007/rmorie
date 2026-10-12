/-
  Researchproofs/P3Adjustment.lean

  P3, continued: adding baseline covariates does affect the treatment estimate
  (library scan A9; Weisburd & Britt ch. 9 pp. 400-401; Freedman 2008; Lin 2013).

  Finite design: with M equiprobable assignments in each of which exactly m of
  the N units are treated and every unit is treated in the same number of
  assignments, the unadjusted difference in means is unbiased for the average
  treatment effect (`unadjusted_unbiased`): a statement about the design, not
  the model.

  Superpopulation asymptotics (the standard setting of Freedman 2008 and Lin
  2013): potential outcomes y(t) = a_t + b_t x + e_t with a centred covariate
  of unit variance, noise variances v_t, treated share p. Up to the common
  factor 1/n the asymptotic variances are
      unadjusted  (b₁² + v₁)/p + (b₀² + v₀)/(1 − p),
      pooled-slope ANCOVA  ((1 − p)²Δ² + v₁)/p + (p²Δ² + v₀)/(1 − p),  Δ = b₁ − b₀,
      Lin (separate slopes)  v₁/p + v₀/(1 − p).
    * `lin_le_unadjusted`, `lin_le_adjusted`: the interaction estimator is never
      worse than either.
    * `adjusted_le_unadjusted_balanced`: at p = 1/2 the pooled adjustment never
      hurts.
    * `adjusted_worse`: with b₀ = 0 the pooled adjustment is strictly worse than
      no adjustment iff p > 2/3: unequal allocation and differing slopes
      together make "will not affect the estimate" false.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

open Finset

noncomputable section

namespace Research.P3.Adjustment

variable {N : Type*} [Fintype N] [DecidableEq N]

/-- A design: a finite set of assignments (treated sets), each of size `m`, every unit treated
in exactly `c` of them. -/
structure Design (N : Type*) [Fintype N] [DecidableEq N] where
  A : Finset (Finset N)
  m : ℕ
  c : ℕ
  m_pos : 0 < m
  size : ∀ S ∈ A, S.card = m
  balanced : ∀ i : N, (A.filter fun S => i ∈ S).card = c
  nonempty : A.Nonempty

/-- Difference in means under assignment `S`. -/
def diffMeans (y1 y0 : N → ℝ) (m : ℕ) (S : Finset N) : ℝ :=
  (∑ i ∈ S, y1 i) / m - (∑ i ∈ univ \ S, y0 i) / (Fintype.card N - m : ℝ)

/-- The design average of the treated-side mean equals the population mean of `y1` times `c/m`
over `|A|`: with `c/|A| = m/N` this is the population mean. -/
theorem treated_sum_average (D : Design N) (f : N → ℝ) :
    ∑ S ∈ D.A, ∑ i ∈ S, f i = D.c * ∑ i, f i := by
  rw [sum_comm' (t' := univ) (s' := fun i => D.A.filter fun S => i ∈ S)]
  · simp only [sum_const, nsmul_eq_mul]
    rw [mul_sum]
    apply sum_congr rfl; intro i _
    rw [D.balanced i]
  · intro S i
    simp only [mem_filter, mem_univ, true_and, and_comm]

theorem unadjusted_unbiased (D : Design N) (y1 y0 : N → ℝ)
    (hNm : (D.m : ℝ) < Fintype.card N)
    (hc : (D.c : ℝ) * Fintype.card N = D.A.card * D.m) :
    (∑ S ∈ D.A, diffMeans y1 y0 D.m S) / D.A.card
      = (∑ i, y1 i) / Fintype.card N - (∑ i, y0 i) / Fintype.card N := by
  have hA : (D.A.card : ℝ) ≠ 0 := by
    have := D.nonempty; exact_mod_cast (card_pos.mpr this).ne'
  have hm : (D.m : ℝ) ≠ 0 := by exact_mod_cast D.m_pos.ne'
  have hN : (Fintype.card N : ℝ) ≠ 0 := by
    have : (0 : ℝ) ≤ D.m := by positivity
    linarith
  have hNm' : (Fintype.card N : ℝ) - D.m ≠ 0 := by linarith
  have h1 : ∑ S ∈ D.A, (∑ i ∈ S, y1 i) / D.m = D.c * (∑ i, y1 i) / D.m := by
    rw [← sum_div, treated_sum_average D y1]
  have h0 : ∑ S ∈ D.A, ∑ i ∈ univ \ S, y0 i = (D.A.card - D.c : ℝ) * ∑ i, y0 i := by
    have : ∀ S ∈ D.A, ∑ i ∈ univ \ S, y0 i = ∑ i, y0 i - ∑ i ∈ S, y0 i := by
      intro S _; rw [sum_sdiff_eq_sub (subset_univ S)]
    rw [sum_congr rfl this, sum_sub_distrib, sum_const, nsmul_eq_mul, treated_sum_average D y0]
    ring
  have h0' : ∑ S ∈ D.A, (∑ i ∈ univ \ S, y0 i) / (Fintype.card N - D.m : ℝ)
      = (D.A.card - D.c : ℝ) * (∑ i, y0 i) / (Fintype.card N - D.m : ℝ) := by
    rw [← sum_div, h0]
  have hsplit : ∑ S ∈ D.A, diffMeans y1 y0 D.m S
      = ∑ S ∈ D.A, (∑ i ∈ S, y1 i) / D.m
        - ∑ S ∈ D.A, (∑ i ∈ univ \ S, y0 i) / (Fintype.card N - D.m : ℝ) := by
    unfold diffMeans; rw [sum_sub_distrib]
  have hc' : (D.c : ℝ) = D.A.card * D.m / Fintype.card N := by
    rw [eq_div_iff hN]; exact hc
  rw [hsplit, h1, h0', hc']
  field_simp
  try ring

/-- Asymptotic variances (times n) in the superpopulation model. -/
def varUnadj (b1 b0 v1 v0 p : ℝ) : ℝ := (b1 ^ 2 + v1) / p + (b0 ^ 2 + v0) / (1 - p)
def varAdj (b1 b0 v1 v0 p : ℝ) : ℝ :=
  ((1 - p) ^ 2 * (b1 - b0) ^ 2 + v1) / p + (p ^ 2 * (b1 - b0) ^ 2 + v0) / (1 - p)
def varLin (v1 v0 p : ℝ) : ℝ := v1 / p + v0 / (1 - p)

theorem lin_le_unadjusted (b1 b0 v1 v0 p : ℝ) (hp : 0 < p) (hp1 : p < 1) :
    varLin v1 v0 p ≤ varUnadj b1 b0 v1 v0 p := by
  unfold varLin varUnadj
  have h1 : v1 / p ≤ (b1 ^ 2 + v1) / p := by
    apply div_le_div_of_nonneg_right _ hp.le; nlinarith [sq_nonneg b1]
  have h0 : v0 / (1 - p) ≤ (b0 ^ 2 + v0) / (1 - p) := by
    apply div_le_div_of_nonneg_right _ (by linarith); nlinarith [sq_nonneg b0]
  linarith

theorem lin_le_adjusted (b1 b0 v1 v0 p : ℝ) (hp : 0 < p) (hp1 : p < 1) :
    varLin v1 v0 p ≤ varAdj b1 b0 v1 v0 p := by
  unfold varLin varAdj
  have h1 : v1 / p ≤ ((1 - p) ^ 2 * (b1 - b0) ^ 2 + v1) / p := by
    apply div_le_div_of_nonneg_right _ hp.le; nlinarith [sq_nonneg ((1 - p) * (b1 - b0))]
  have h0 : v0 / (1 - p) ≤ (p ^ 2 * (b1 - b0) ^ 2 + v0) / (1 - p) := by
    apply div_le_div_of_nonneg_right _ (by linarith); nlinarith [sq_nonneg (p * (b1 - b0))]
  linarith

theorem adjusted_minus_unadjusted (b1 b0 v1 v0 p : ℝ) (hp : 0 < p) (hp1 : p < 1) :
    varAdj b1 b0 v1 v0 p - varUnadj b1 b0 v1 v0 p
      = ((1 - p) ^ 2 * (b1 - b0) ^ 2 - b1 ^ 2) / p + (p ^ 2 * (b1 - b0) ^ 2 - b0 ^ 2) / (1 - p) := by
  unfold varAdj varUnadj
  have : (1 : ℝ) - p ≠ 0 := by linarith
  field_simp
  ring

theorem adjusted_le_unadjusted_balanced (b1 b0 v1 v0 : ℝ) :
    varAdj b1 b0 v1 v0 (1 / 2) ≤ varUnadj b1 b0 v1 v0 (1 / 2) := by
  unfold varAdj varUnadj
  norm_num
  nlinarith [sq_nonneg (b1 + b0)]

/-- With an error-free slope only in the treated arm, pooled adjustment is strictly worse than
no adjustment exactly when the treated share exceeds 2/3. -/
theorem adjusted_worse (b1 v1 v0 p : ℝ) (hb : b1 ≠ 0) (hp : 0 < p) (hp1 : p < 1) :
    varUnadj b1 0 v1 v0 p < varAdj b1 0 v1 v0 p ↔ 2 / 3 < p := by
  have hd := adjusted_minus_unadjusted b1 0 v1 v0 p hp hp1
  have h1p : (0 : ℝ) < 1 - p := by linarith
  have key : varAdj b1 0 v1 v0 p - varUnadj b1 0 v1 v0 p = b1 ^ 2 * (3 * p - 2) / (1 - p) := by
    rw [hd]; field_simp; ring
  have hb2 : 0 < b1 ^ 2 := by positivity
  constructor
  · intro h
    have : 0 < b1 ^ 2 * (3 * p - 2) / (1 - p) := by linarith
    have : 0 < b1 ^ 2 * (3 * p - 2) := (div_pos_iff_of_pos_right h1p).mp this
    nlinarith
  · intro h
    have : 0 < b1 ^ 2 * (3 * p - 2) / (1 - p) := by
      apply div_pos _ h1p; apply mul_pos hb2; linarith
    linarith

end Research.P3.Adjustment
