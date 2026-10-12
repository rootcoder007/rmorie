/-
  Researchproofs/P7Bound.lean

  P7, continued: how much Gini concentration a Poisson null can show.

  For a count vector x over n places with mean x̄ > 0 the Gini index is
      G = ∑_i ∑_j |x_i − x_j| / (2 n² x̄).
  By Cauchy–Schwarz over the n² pairs and the identity
  ∑_i ∑_j (x_i − x_j)² = 2 n ∑_i (x_i − x̄)²,

    * pair_sq_identity: ∑∑ (x_i − x_j)² = 2 n ∑ (x_i − x̄)²,
    * gini_le_cv: G ≤ √(v / 2) / x̄ where v = ∑ (x_i − x̄)² / n is the
      population variance — the Gini is at most the coefficient of
      variation over √2, for every count vector.

  Under a Poisson null the variance is the mean, so the null Gini is at
  most about 1 / √(2 x̄): a "law of concentration" read from a Gini of
  0.6 on places averaging two crimes each (bound 0.5) has not left the
  neighbourhood of chance; the same Gini on places averaging fifty
  (bound 0.1) has. The R function evaluates the bound and the exact null
  distribution by simulation.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P7.Bound

variable {n : ℕ} (x : Fin n → ℝ)

/-- Mean of the counts. -/
def mean : ℝ := (∑ i, x i) / n
/-- Population variance. -/
def var : ℝ := (∑ i, (x i - mean x) ^ 2) / n
/-- The Gini index as the mean absolute pairwise difference over twice the mean. -/
def gini : ℝ := (∑ i, ∑ j, |x i - x j|) / (2 * (n : ℝ) ^ 2 * mean x)

theorem pair_sq_identity (hn : 0 < n) :
    ∑ i, ∑ j, (x i - x j) ^ 2 = 2 * n * ∑ i, (x i - mean x) ^ 2 := by
  have hsum : ∑ i, (x i - mean x) = 0 := by
    rw [sum_sub_distrib, sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]
    unfold mean
    have : (n : ℝ) ≠ 0 := by exact_mod_cast hn.ne'
    field_simp
    ring
  have hpt : ∀ i j, (x i - x j) ^ 2 = (x i - mean x) ^ 2 + (x j - mean x) ^ 2
      - 2 * ((x i - mean x) * (x j - mean x)) := fun i j => by ring
  simp only [hpt, sum_add_distrib, sum_sub_distrib, ← mul_sum, hsum, mul_zero,
    sub_zero, sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]
  ring

theorem abs_sum_le_sqrt (hn : 0 < n) :
    ∑ i, ∑ j, |x i - x j| ≤ (n : ℝ) * Real.sqrt (∑ i, ∑ j, (x i - x j) ^ 2) := by
  -- Cauchy–Schwarz over the n² pairs with the constant vector 1
  have h := Finset.sum_mul_sq_le_sq_mul_sq (univ : Finset (Fin n × Fin n))
    (fun p => |x p.1 - x p.2|) (fun _ => (1 : ℝ))
  simp only [mul_one, one_pow, sum_const, card_univ, Fintype.card_prod, Fintype.card_fin, nsmul_eq_mul,
    sq_abs] at h
  rw [← Fintype.sum_prod_type', ← Fintype.sum_prod_type']
  set A := ∑ p : Fin n × Fin n, |x p.1 - x p.2| with hAdef
  set S := ∑ p : Fin n × Fin n, (x p.1 - x p.2) ^ 2 with hSdef
  have hA : 0 ≤ A := sum_nonneg fun _ _ => abs_nonneg _
  have hS : 0 ≤ S := sum_nonneg fun _ _ => sq_nonneg _
  have hn' : (0 : ℝ) ≤ n := by positivity
  rw [Nat.cast_mul] at h
  have hsq : A ^ 2 ≤ ((n : ℝ) * Real.sqrt S) ^ 2 := by
    rw [mul_pow, Real.sq_sqrt hS]
    calc A ^ 2 ≤ S * ((n : ℝ) * n) := h
      _ = (n : ℝ) ^ 2 * S := by ring
  exact (pow_le_pow_iff_left₀ hA (by positivity) two_ne_zero).mp hsq

/-- The Gini index is at most the coefficient of variation over √2. -/
theorem gini_le_cv (hn : 0 < n) (hmean : 0 < mean x) :
    gini x ≤ Real.sqrt (var x / 2) / mean x := by
  unfold gini
  have hn' : (0 : ℝ) < n := by exact_mod_cast hn
  rw [div_le_div_iff₀ (by positivity) hmean]
  have h1 := abs_sum_le_sqrt x hn
  rw [pair_sq_identity x hn] at h1
  have hv : Real.sqrt (2 * n * ∑ i, (x i - mean x) ^ 2) = 2 * n * Real.sqrt (var x / 2) := by
    have e : 2 * (n : ℝ) * ∑ i, (x i - mean x) ^ 2 = (2 * n) ^ 2 * (var x / 2) := by
      unfold var
      field_simp
      try ring
    rw [e, Real.sqrt_mul (by positivity), Real.sqrt_sq (by positivity)]
  rw [hv] at h1
  calc (∑ i, ∑ j, |x i - x j|) * mean x ≤ ((n : ℝ) * (2 * n * Real.sqrt (var x / 2))) * mean x :=
        mul_le_mul_of_nonneg_right h1 hmean.le
    _ = Real.sqrt (var x / 2) * (2 * (n : ℝ) ^ 2 * mean x) := by ring

end Research.P7.Bound
