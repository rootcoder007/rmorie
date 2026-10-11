/-
  Researchproofs/P13HKSJ.lean

  P13 (continued): the Hartung-Knapp-Sidik-Jonkman interval (Hartung & Knapp
  2001; Sidik & Jonkman 2002; IntHout, Ioannidis & Borm 2014).

  With k studies, weights w_i > 0 and effects y_i, the pooled estimate is
  μ̂ = ∑ w y / ∑ w. The HKSJ method replaces the Wald variance 1/∑w by
  q/∑w with q = ∑ w (y − μ̂)² / (k − 1), and uses t_{k−1} quantiles.

    * hksj_wider_iff: the HKSJ variance exceeds the Wald variance exactly
      when q ≥ 1, i.e. when the weighted residual scatter is at least what
      the weights claim; q < 1 narrows the interval (the known caveat).
    * Q_eq_zero_iff: q = 0 only when every study equals the pooled value —
      the degenerate case where the HKSJ interval collapses to a point.
    * hksj_equal_weights: with equal weights the HKSJ variance is the
      one-sample t variance s²/k of the unweighted effects, so the HKSJ
      interval is then the ordinary t interval — the sense in which the
      method is "the t test applied to the effects".

  Nothing here is about the sampling distribution of y_i; the identities
  hold for any numbers, which is why the method's coverage claims
  (simulation-based in the literature) are not theorems of this file.
-/
import Mathlib

set_option linter.style.longLine false
set_option linter.style.header false

noncomputable section
open Finset

namespace Research.P13HKSJ

variable {k : ℕ} (w y : Fin k → ℝ)

/-- Total weight. -/
def W : ℝ := ∑ i, w i
/-- Weighted pooled estimate. -/
def muHat : ℝ := (∑ i, w i * y i) / W w
/-- Weighted residual sum of squares (Cochran's Q under these weights). -/
def Q : ℝ := ∑ i, w i * (y i - muHat w y) ^ 2
/-- The HKSJ scale `q = Q/(k − 1)`. -/
def q : ℝ := Q w y / ((k : ℝ) - 1)
/-- The HKSJ variance of the pooled estimate. -/
def hksjVar : ℝ := q w y / W w
/-- The Wald (inverse-variance) variance of the pooled estimate. -/
def waldVar : ℝ := 1 / W w

theorem Q_nonneg (hw : ∀ i, 0 ≤ w i) : 0 ≤ Q w y :=
  sum_nonneg fun i _ => mul_nonneg (hw i) (sq_nonneg _)

theorem hksj_var_nonneg (hw : ∀ i, 0 ≤ w i) (hk : 1 < k) : 0 ≤ hksjVar w y := by
  have hk' : (0 : ℝ) ≤ (k : ℝ) - 1 := by
    have : (1 : ℝ) < k := by exact_mod_cast hk
    linarith
  exact div_nonneg (div_nonneg (Q_nonneg w y hw) hk') (sum_nonneg fun i _ => hw i)

/-- The HKSJ interval is at least as wide as the Wald interval iff `q ≥ 1`. -/
theorem hksj_wider_iff (hW : 0 < W w) : waldVar w ≤ hksjVar w y ↔ 1 ≤ q w y := by
  unfold waldVar hksjVar
  exact div_le_div_iff_of_pos_right hW

/-- `Q = 0` exactly when every study equals the pooled estimate. -/
theorem Q_eq_zero_iff (hw : ∀ i, 0 < w i) : Q w y = 0 ↔ ∀ i, y i = muHat w y := by
  unfold Q
  rw [sum_eq_zero_iff_of_nonneg (fun i _ => mul_nonneg (hw i).le (sq_nonneg _))]
  constructor
  · intro h i
    have := h i (mem_univ i)
    rcases mul_eq_zero.mp this with h0 | h0
    · exact absurd h0 (hw i).ne'
    · exact sub_eq_zero.mp (pow_eq_zero_iff (two_ne_zero) |>.mp h0)
  · intro h i _
    rw [h i, sub_self]; ring

theorem W_const (c : ℝ) (hwc : ∀ i, w i = c) : W w = k * c := by
  unfold W; simp [hwc, sum_const, card_univ, Fintype.card_fin, nsmul_eq_mul]

theorem muHat_const (c : ℝ) (hc : c ≠ 0) (hk : k ≠ 0) (hwc : ∀ i, w i = c) :
    muHat w y = (∑ i, y i) / k := by
  unfold muHat
  rw [W_const w c hwc]
  simp only [hwc, ← mul_sum]
  have hk' : (k : ℝ) ≠ 0 := by exact_mod_cast hk
  field_simp

/-- With equal weights the HKSJ variance is the one-sample t variance `s²/k`. -/
theorem hksj_equal_weights (c : ℝ) (hc : c ≠ 0) (hk : 1 < k) (hwc : ∀ i, w i = c) :
    hksjVar w y = ((∑ i, (y i - (∑ j, y j) / k) ^ 2) / ((k : ℝ) - 1)) / k := by
  have hk0 : k ≠ 0 := by omega
  have hkr : (k : ℝ) ≠ 0 := by exact_mod_cast hk0
  have hk1 : (k : ℝ) - 1 ≠ 0 := by
    have : (1 : ℝ) < k := by exact_mod_cast hk
    linarith
  unfold hksjVar q Q
  rw [muHat_const w y c hc hk0 hwc, W_const w c hwc]
  simp only [hwc, ← mul_sum]
  generalize (∑ i, (y i - (∑ j, y j) / (k : ℝ)) ^ 2) = s
  field_simp

end Research.P13HKSJ
